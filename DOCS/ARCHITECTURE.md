# Banking System Architecture

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────┐
│              JCL Batch Orchestration                    │
│  (COMPILE.jcl → BINDDB2.jcl → BATCH.jcl → EOD.jcl)   │
└──────────────┬──────────────────────────────────────────┘
               │
    ┌──────────┴──────────────────────────────────┐
    │                                              │
┌───▼──────────────────┐            ┌─────────────▼─────────┐
│   COBOL Programs     │            │   DB2 Database        │
│  ┌────────────────┐  │            │  ┌─────────────────┐  │
│  │ TRANPROC       │◄─┼────EXEC SQL───►│ ACCOUNTS      │  │
│  │ ACCTMSTR       │  │   ┌─────────│ │ TRANSACTIONS  │  │
│  │ RECONCIL       │  │   │         │ │ CUSTOMERS     │  │
│  │ INTCALC        │  │   │         │ │               │  │
│  └────────────────┘  │   │         │ └─────────────────┘  │
└────────────────────┘    │         └──────────────────────┘
                           │
            ┌──────────────┴──────────────┐
            │                             │
       ┌────▼──────┐              ┌───────▼────┐
       │  Copybooks│              │  DD Datasets│
       │ ACCTREC   │              │ TRANIN     │
       │ TRANREC   │              │ TRANOUT    │
       │ SQLCA     │              │ ERRLOG     │
       │ ERRHDLR   │              │ RECONCIL   │
       └───────────┘              └────────────┘
```

## Processing Flow

### 1. Transaction Input Phase
```
┌─────────────────────┐
│ Transaction File    │
│ (USER.TRANSACTIONS) │
│  (Sequential)       │
└──────────┬──────────┘
           │
           │ Read records
           ▼
    ┌──────────────┐
    │ TRANPROC.cbl │
    │ Processing   │
    └──────────────┘
```

### 2. Validation Phase
```
         ┌─────────────────────────┐
         │ For each transaction:   │
         │ 1. Parse input record   │
         │ 2. Validate action      │
         │ 3. Query DB2 for balance│
         │ 4. Check constraints    │
         └────────┬────────────────┘
                  │
        ┌─────────┴──────────┐
        │                    │
   ┌────▼────┐         ┌─────▼────┐
   │ VALID   │         │ INVALID  │
   │ Continue│         │ Log error│
   └─────────┘         └──────────┘
```

### 3. Update Phase
```
┌──────────────────────────┐
│ For valid transactions:  │
│ 1. Calculate new balance │
│ 2. UPDATE DB2 ACCOUNTS   │
│ 3. INSERT audit record   │
│ 4. COMMIT transaction    │
└────────┬─────────────────┘
         │
      ┌──┴──┐
      │ DB2 │
      └─────┘
```

### 4. Output Phase
```
┌──────────────────┐      ┌──────────────────┐
│ Success Records  │      │ Error Records    │
│ (USER.TRANOUT)   │      │ (USER.ERRLOG)    │
└────────┬─────────┘      └────────┬─────────┘
         │                         │
         └─────────┬───────────────┘
                   │
         ┌─────────▼──────────┐
         │ Reconciliation &   │
         │ Interest Posting   │
         └────────────────────┘
```

## Component Interactions

### COBOL Programs

**TRANPROC.cbl** - Main transaction processor
- Reads transaction input file
- Performs business logic validation
- Executes DB2 SQL operations
- Generates output and error logs

**ACCTMSTR.cbl** - Account master maintenance
- Create new accounts
- Update account details
- Retrieve account information
- Delete (soft) accounts

**RECONCIL.cbl** - Daily reconciliation
- Reads all processed transactions
- Accumulates totals by type
- Generates reconciliation report
- Validates ledger balances

**INTCALC.cbl** - Interest calculation
- Reads savings/MM accounts from DB2
- Calculates interest for each
- Posts interest back to DB2
- Records in transaction audit trail

### JCL Orchestration

**COMPILE.jcl**
```
PRECOMP (extract SQL) → COBOL (compile) → LINKDIT (link with DB2)
```

**BINDDB2.jcl**
```
DSNHPC → BIND (validate & optimize SQL) → PLAN/PACKAGE
```

**BATCH.jcl**
```
PREPARE → TRANPROC → REPORT → ERRREPT
```

**EOD.jcl**
```
RECONCIL → INTCALC → FINREPT
```

### DB2 Integration

**Precompile Phase**
```
COBOL source with EXEC SQL
        ↓
    DSNHPC precompiler
        ↓
Modified COBOL + DBRM
```

**Compile Phase**
```
Modified COBOL
        ↓
  IGYCRCTL compiler
        ↓
  Object module
```

**Bind Phase**
```
DSBRM + Table statistics
        ↓
  BIND utility
        ↓
PLAN/PACKAGE with optimized access paths
```

**Runtime Phase**
```
Program execution
        ↓
  EXEC SQL statement
        ↓
  DB2 runtime uses pre-bound access path
        ↓
  Results returned, SQLCODE set
```

## Data Flow Diagram

```
┌──────────────────┐
│ Input: Transaction│
│ ACC000000001D1500 │
└────────┬──────────┘
         │
         ▼
┌──────────────────────────────┐
│ TRANPROC Processing          │
│ 1. Parse: Acct + Type + Amt  │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│ DB2 Query: SELECT balance    │
│ FROM ACCOUNTS WHERE acct=... │
└────────┬─────────────────────┘
         │
         ▼
    ┌────────────────┐
    │ DB2 Response   │
    │ balance=$2500  │
    └────┬───────────┘
         │
         ▼
┌──────────────────────────────┐
│ Validation Logic             │
│ New Balance = 2500 + 1500    │
│ = 4000 (OK)                  │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│ DB2 Update: UPDATE ACCOUNTS  │
│ SET balance = 4000           │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│ DB2 Insert: INSERT TRANS     │
│ INSERT INTO TRANSACTIONS ... │
└────────┬─────────────────────┘
         │
         ▼
    ┌─────────────────┐
    │ DB2 COMMIT      │
    │ (Persistent)    │
    └────┬────────────┘
         │
         ▼
┌──────────────────────────────┐
│ Output: Success Record       │
│ USER.TRANOUT                 │
└──────────────────────────────┘
```

## Error Handling Flow

```
┌─────────────────────┐
│ SQL Operation       │
│ (SELECT/UPDATE/etc) │
└────────┬────────────┘
         │
         ▼
    ┌────────────┐
    │ SQLCODE=?  │
    └───┬────┬───┘
        │    │
   ┌────▼┐  │  ┌────▼──┐
   │=0   │  │  │<>0    │
   │     │  │  │ERROR  │
   └──┬──┘  │  └───┬───┘
      │     │      │
      │ ┌───▼──┐   │
      │ │100   │   │
      │ │no    │   │
      │ │data  │   │
      │ └──┬───┘   │
      │    │       │
  ┌───▼────▼───┐   │
  │ Log Error  │   │
  │ Continue   │   │
  │ Next Trans │   │
  └────────────┘   │
                   │
           ┌───────▼──────┐
           │ Check Return │
           │ Code Type    │
           └───────┬──────┘
                   │
        ┌──────────┼──────────┐
        │          │          │
   ┌────▼─┐  ┌────▼─┐  ┌────▼─┐
   │Retry │  │Comp  │  │Fail  │
   │Deadl │  │lete  │  │ure   │
   │ock   │  │ Succ │  │Log   │
   └──────┘  └──────┘  └──────┘
```

## Performance Considerations

### Index Strategy
```
ACCOUNTS table:
- PRIMARY KEY on ACCOUNT_NUMBER
- INDEX on CUSTOMER_ID (for lookups by customer)
- INDEX on ACCOUNT_STATUS (for filtering active accounts)

TRANSACTIONS table:
- PRIMARY KEY on TRANSACTION_ID
- INDEX on ACCOUNT_NUMBER (for audit lookups)
- INDEX on TRANSACTION_DATE (for reporting)
- INDEX on STATUS (for pending transaction searches)
```

### Batch Window Allocation
```
2:00 AM - 2:30 AM: Preparation & validation
2:30 AM - 5:00 AM: Main batch processing (up to 100K transactions)
5:00 AM - 5:30 AM: Reconciliation & interest posting
5:30 AM - 6:00 AM: Final reporting & archival

Total: 4 hours (allows buffer for error recovery)
```

### Parallel Processing
```
┌─────────┬─────────┬─────────┬─────────┐
│ Batch 1 │ Batch 2 │ Batch 3 │ Batch 4 │
│ Accts   │ Accts   │ Accts   │ Accts   │
│ 0-249K  │ 250-499K│ 500-749K│ 750-999k│
└────┬────┴────┬────┴────┬────┴────┬────┘
     │         │         │         │
     └────┬────┴────┬────┴────┬────┘
          │         │         │
     Wait for all parallel steps complete
          │         │         │
          └────┬────┴────┬────┘
               │         │
          RECONCIL + INTCALC
```

---

For detailed information on specific components, see:
- DB2_INTEGRATION.md - SQL precompile/compile/bind
- JCL_REFERENCE.md - Job control language
- DEPLOYMENT.md - Operational procedures
