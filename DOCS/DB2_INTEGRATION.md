# DB2 Integration Guide: COBOL Precompile, Compile, and Bind Process

## Overview

Integrating COBOL programs with IBM DB2 requires a specialized three-stage compilation pipeline:

1. **Precompilation** - Extract SQL and generate DBRM
2. **Compilation** - Compile modified COBOL source
3. **Binding** - Optimize SQL for execution

## Stage 1: Precompilation (DSNHPC)

### Purpose
The DSNHPC utility preprocesses COBOL source code to extract embedded SQL statements.

### Input
- COBOL source code containing EXEC SQL...END-EXEC blocks

### Process
```cobol
-- BEFORE precompilation:
EXEC SQL
  SELECT ACCOUNT_BALANCE
  INTO :WS-BALANCE
  FROM ACCOUNTS
  WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER
END-EXEC.

-- AFTER precompilation:
  CALL 'DSNHLI' USING ... -- DB2 host language interface
```

### Outputs
1. **Modified COBOL** - SQL replaced with CALL statements
2. **DBRM** (Database Request Module) - Extracted SQL statements

### JCL Step
```jcl
//PRECOMP EXEC PGM=DSNHPC,
//        PARM='PACKAGE(PKGNAME) MEMBER(PGMNAME)'
//SYSCIN   DD DSN=USER.COBOL(TRANPROC),DISP=SHR
//SYSPRT   DD SYSOUT=*
//DBRMLIB  DD DSN=USER.DBRM(TRANPROC),DISP=(,KEEP)
```

## Stage 2: Compilation (IGYCRCTL)

### Purpose
Compile the modified COBOL source into object code.

### Input
- Modified COBOL source (from precompilation)

### Process
- Standard COBOL compilation
- Includes DB2 runtime library references
- Generates object module with unresolved DB2 library symbols

### JCL Step
```jcl
//COBOL   EXEC PGM=IGYCRCTL,
//        PARM='NOGOSSIP,NOXREF,NOLIST,LIB',
//        COND=(4,LT,PRECOMP)
//SYSIN    DD DSN=USER.COBOL(TRANPRCP),DISP=SHR
//SYSLIN   DD DSN=USER.OBJ(TRANPROC),DISP=(,KEEP)
//SYSLIB   DD DSN=USER.COPYBOOKS,DISP=SHR
//         DD DSN=SYS1.COBLIB,DISP=SHR
```

## Stage 3: Link-Edit (IEWL)

### Purpose
Link object modules with DB2 runtime libraries.

### Input
- Object module from compilation
- DB2 runtime libraries (SYS1.DB2.SDSNLOAD)

### Process
- Resolves DB2 symbols
- Creates executable load module
- Links with DB2 SQL execution engine

### JCL Step
```jcl
//LINKDIT EXEC PGM=IEWL,
//        PARM='LIST,LET,NCAL',
//        COND=(4,LT,COBOL)
//SYSLIN   DD DSN=USER.OBJ(TRANPROC),DISP=SHR
//SYSLIB   DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
//         DD DSN=SYS1.COBLIB,DISP=SHR
//SYSLMOD  DD DSN=USER.LOADLIB(TRANPROC),DISP=(,KEEP)
```

## Stage 4: Binding (IKJEFT01)

### Purpose
Validate, optimize, and pre-compile SQL statements into a DB2 PLAN or PACKAGE.

### Critical Functions

**Validation**
- Verifies all tables exist in DB2 catalog
- Checks that all columns referenced are valid
- Validates data types and compatibility
- Confirms user has authorization

**Optimization**
- Analyzes table statistics (row count, index statistics)
- Determines optimal access paths for SQL statements
- Chooses between index access vs. table scan
- Optimizes join sequences for multi-table queries

**Pre-Compilation**
- Generates optimized SQL execution code
- Creates internal representation of SQL operations
- Embeds access paths for runtime execution

### PLAN vs PACKAGE

**PLAN Approach (Legacy)**
- Single execution plan for entire program
- All DBRMs bound to one PLAN
- Requires rebinding entire PLAN for any change
- Less flexible for modular development

**PACKAGE Approach (Modern, Recommended)**
- Separate package for each DBRM
- Packages grouped into COLLECTION
- Can rebind individual packages independently
- Better for maintenance and deployment
- Enables version control of SQL

### JCL Step
```jcl
//BIND    EXEC PGM=IKJEFT01,
//        PARM='DSNH BIND PLAN(BANKPLAN) +
//               PACKAGE(TRANPROC) +
//               MEMBER(TRANPROC) OWNER(DBADMIN)'
//SYSLIB   DD DSN=USER.DBRM,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//STEPLIB  DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
```

## Runtime Execution Flow

### 1. Program Load
```
TRANPROC Load Module loads into memory
  ↓
DB2 Runtime Libraries loaded
  ↓
Package TRANPROC information loaded from DB2 catalog
```

### 2. SQL Execution
```
Program encounters: EXEC SQL SELECT ... END-EXEC
  ↓
CALL statement (from precompiled code) invokes DB2
  ↓
DB2 retrieves pre-optimized access path from PACKAGE
  ↓
Execution proceeds using pre-computed best plan
  ↓
Results returned to host variables
```

### 3. Error Handling
```
After each SQL statement, SQLCODE is set:
  0   = Success
  100 = No data found
  -xxx = Error (specific code indicates issue)
```

## SQLCA Structure

The SQL Communication Area captures execution results:

```cobol
01 SQLCA.
    05 SQLCAID          PIC X(8) VALUE 'SQLCA   '.
    05 SQLCABC          PIC S9(9) COMP-5.
    05 SQLCODE          PIC S9(9) COMP-5.  -- Return code
    05 SQLERRM.                             -- Error message
        10 SQLERRML     PIC S9(9) COMP-5.
        10 SQLERRMF     PIC X(70).
    05 SQLERRP          PIC X(8).           -- Error position
    05 SQLERRD          PIC S9(9) COMP-5 OCCURS 6 TIMES.
    05 SQLWARN.                             -- Warning flags
        10 SQLWARN0     PIC X.
        10 SQLWARN1 THRU SQLWARN7 PIC X.
    05 SQLSTATE         PIC X(5).           -- SQL state code
```

## Common SQLCODE Values

| Code | Meaning | Action |
|------|---------|--------|
| 0 | Success | Continue |
| 100 | No data found | Check WHERE clause |
| -104 | Illegal character | Check SQL syntax |
| -206 | Column not found | Verify column name in DB2 |
| -305 | Null indicator needed | Add indicator variable |
| -407 | Null not allowed | Check NOT NULL constraint |
| -530 | Foreign key constraint | Check referential integrity |
| -803 | Duplicate key | Check unique constraint |
| -904 | Deadlock/timeout | Retry transaction |
| -911 | Transaction rollback | Retry entire transaction |
| -922 | Authorization failed | Check user permissions |

## Embedded SQL Patterns

### SELECT Single Row
```cobol
EXEC SQL
  SELECT ACCOUNT_BALANCE, STATUS
  INTO :WS-BALANCE :WS-BALANCE-IND
       :WS-STATUS :WS-STATUS-IND
  FROM ACCOUNTS
  WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER
END-EXEC.

IF SQLCODE = 0
    -- Successfully retrieved
ELSE IF SQLCODE = 100
    -- No rows matched
ELSE
    -- Database error
END-IF.
```

### INSERT Transaction
```cobol
EXEC SQL
  INSERT INTO TRANSACTIONS
    (TRANSACTION_ID, ACCOUNT_NUMBER, AMOUNT, STATUS)
  VALUES
    (:WS-TXN-ID, :WS-ACCOUNT, :WS-AMOUNT, 'C')
END-EXEC.
```

### UPDATE with Validation
```cobol
EXEC SQL
  UPDATE ACCOUNTS
  SET ACCOUNT_BALANCE = ACCOUNT_BALANCE + :WS-AMOUNT
      LAST_UPDATE_DATE = CURRENT_DATE
  WHERE ACCOUNT_NUMBER = :WS-ACCOUNT-NUMBER
    AND ACCOUNT_STATUS = 'A'
END-EXEC.

IF SQLCODE NOT = 0
    -- Update failed, log error
END-IF.
```

### CURSOR for Multi-Row Processing
```cobol
EXEC SQL
  DECLARE ACCT_CURSOR CURSOR FOR
  SELECT ACCOUNT_NUMBER, BALANCE
  FROM ACCOUNTS
  WHERE ACCOUNT_STATUS = 'A'
END-EXEC.

EXEC SQL OPEN ACCT_CURSOR END-EXEC.

PERFORM UNTIL SQLCODE NOT = 0
  EXEC SQL
    FETCH ACCT_CURSOR
    INTO :WS-ACCT-NUMBER, :WS-BALANCE
  END-EXEC
  
  IF SQLCODE = 0
    -- Process account
  END-IF
END-PERFORM.

EXEC SQL CLOSE ACCT_CURSOR END-EXEC.
```

## Performance Optimization

### Indexing Strategy
```sql
-- Create indexes on frequently searched columns
CREATE INDEX IDX_ACCT_CUST ON ACCOUNTS(CUSTOMER_ID);
CREATE INDEX IDX_TRN_ACCT ON TRANSACTIONS(ACCOUNT_NUMBER);
CREATE INDEX IDX_TRN_DATE ON TRANSACTIONS(TRANSACTION_DATE);

-- Bind process will prefer indexed access paths
```

### Access Path Selection
- **Index Scan**: Fast for specific lookups (account number)
- **Table Scan**: Necessary for range queries or when no index
- **Join Optimization**: Bind determines join order automatically

## Troubleshooting

### Issue: -206 SQLCODE (Column Not Found)
**Cause**: Column name doesn't exist in DB2 table
**Solution**: Verify column names match exactly (case-sensitive on some systems)

### Issue: -530 SQLCODE (Constraint Violation)
**Cause**: Foreign key or unique constraint violated
**Solution**: Check that referenced records exist and values are unique

### Issue: -904 SQLCODE (Deadlock)
**Cause**: Transaction conflict with other concurrent processes
**Solution**: Implement retry logic with exponential backoff

### Issue: Bind Fails
**Cause**: Table or column removed from DB2 since last bind
**Solution**: Rebind after schema changes

## Best Practices

1. **Check SQLCODE after every SQL statement** - Don't assume success
2. **Use indicator variables** - For nullable columns
3. **Batch operations** - Group multiple inserts/updates
4. **Use COMMIT carefully** - Balance frequency with performance
5. **Create proper indexes** - For columns in WHERE clauses
6. **Test SQL independently** - Before embedding in COBOL
7. **Document access patterns** - For maintenance and optimization
8. **Use PACKAGE binding** - For easier maintenance than PLAN

---

For more information, consult IBM DB2 documentation or execute `HELP PRECOMPILE` at z/OS prompt.