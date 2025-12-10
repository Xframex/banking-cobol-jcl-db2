# Sample Data Files

## Record Formats

### TRANSACTIONS.txt - Transaction Input Record

**Record Layout** (150 characters fixed)

```
Position  Length  Type    Field Name          Description
=========================================================
1         12      CHAR    Account Number      12-digit account ID
13        1       CHAR    Transaction Type    D=Deposit, W=Withdrawal, T=Transfer, I=Interest
14        15      NUM     Amount              Amount with 2 decimal places (ZZZZ9.99)
29        20      CHAR    Description         Transaction description
49        10      CHAR    Reference Number    External reference
59        8       NUM     Transaction Date    YYYYMMDD format
67        6       NUM     Transaction Time    HHMMSS format
73        1       CHAR    Status              P=Pending, C=Complete, F=Failed
74        77      CHAR    Filler              Reserved for future use
```

**Sample Record**
```
ACC00000000D00000001500.00Deposit Check #1234    CHECK1234 20251210 090030C
```

### ACCOUNTS.txt - Account Master Record

**Record Layout** (120 characters fixed)

```
Position  Length  Type    Field Name          Description
=========================================================
1         12      CHAR    Account Number      12-digit account ID
13        8       CHAR    Customer ID         8-digit customer ID
21        1       CHAR    Account Type        C=Checking, S=Savings, M=Money Market
22        13      NUM     Account Balance     ZZZZ9999999.99
35        1       CHAR    Account Status      A=Active, I=Inactive, X=Suspended
36        5       NUM     Interest Rate       99.9999 (percentage)
41        13      NUM     Overdraft Limit     ZZZZ9999999.99 (0 for savings/MM)
54        8       NUM     Creation Date       YYYYMMDD format
62        8       NUM     Last Update Date    YYYYMMDD format
70        6       NUM     Last Update Time    HHMMSS format
76        45      CHAR    Filler              Reserved
```

**Sample Records**
```
ACC000000001CUST0001C0000002500.00A0.005000001000.0020251001202510092230PM
ACC000000002CUST0001S0000010000.00A0.022500000000.0020251001202510092230PM
ACC000000003CUST0002C0000005000.00A0.005000001500.0020251001202510092230PM
```

## Data Dictionary

### Account Types
- **C** = Checking Account (no interest, allows overdrafts)
- **S** = Savings Account (earns interest, no overdrafts)
- **M** = Money Market (high-yield savings, minimum balance)

### Transaction Types
- **D** = Deposit (adds to balance)
- **W** = Withdrawal (subtracts from balance)
- **T** = Transfer (between accounts)
- **I** = Interest (posted automatically)

### Account Status
- **A** = Active (normal operations allowed)
- **I** = Inactive (no transactions allowed)
- **X** = Suspended (pending investigation)

### Transaction Status
- **P** = Pending (awaiting processing)
- **C** = Completed (successfully processed)
- **F** = Failed (rejected)
- **R** = Reversed (undone)

## Loading Sample Data

### Into Sequential File

```jcl
//LOADDATA JOB (ACCT,001),'LOAD DATA',CLASS=A
//COPY     EXEC PGM=IEBGENER
//SYSUT1   DD DSN=this.source.file,DISP=SHR
//SYSUT2   DD DSN=USER.TRANSACTIONS,DISP=(,KEEP),
//         SPACE=(TRK,(10,5))
//SYSPRINT DD SYSOUT=*
//*
```

### Into DB2 Database

```sql
CONNECT TO BANKDB;

INSERT INTO TRANSACTIONS VALUES
  ('ACC000000001', 'D', 1500.00, CURRENT_DATE, CURRENT_TIME, 'C');

INSERT INTO TRANSACTIONS VALUES
  ('ACC000000002', 'D', 2250.00, CURRENT_DATE, CURRENT_TIME, 'C');

COMMIT;
```

## Validation Rules

### Mandatory Fields
- Account Number (cannot be null, must exist in ACCOUNTS table)
- Transaction Type (must be D, W, T, or I)
- Amount (must be numeric, positive)

### Constraints
- **Withdrawal**: Account must have sufficient balance
- **All Types**: Account must be active (status = 'A')
- **Transfer**: Both source and destination accounts must exist
- **Interest**: Only for Savings (S) and Money Market (M) accounts

### Business Rules
- Checking accounts: Allow overdrafts up to OVERDRAFT_LIMIT
- Savings accounts: No negative balances allowed
- Money Market accounts: Minimum balance of $10,000 required
- Daily withdrawal limit: $2,000 per day

## Test Scenarios

### Scenario 1: Simple Deposit
```
Account: ACC000000001
Type: D (Deposit)
Amount: 1500.00

Result: Account balance increases by $1500
Expected: SUCCESS
```

### Scenario 2: Insufficient Funds Withdrawal
```
Account: ACC000000001 (Current balance: $2500)
Type: W (Withdrawal)
Amount: 3000.00

Result: Insufficient funds check fails
Expected: REJECTION with error code
```

### Scenario 3: Account Inactive
```
Account: ACC000000099 (Status: I - Inactive)
Type: D (Deposit)
Amount: 500.00

Result: Account status check fails
Expected: REJECTION
```

### Scenario 4: Interest Calculation
```
Account: ACC000000002 (Type: S, Balance: $10,000, Rate: 2.25%)
Type: I (Interest)
Amount: (Auto-calculated: $10,000 * 0.0225 / 12 = $18.75)

Result: Interest posted, balance becomes $10,018.75
Expected: SUCCESS
```

## Performance Considerations

### Large Batch Processing
- For 100,000+ transactions, split into batches of 10,000
- Use parallel processing where possible
- Pre-sort by account number for sequential access
- Estimate 3-5ms per transaction in optimal conditions

### File Size Estimates
- Each transaction record: ~150 bytes
- 10,000 transactions: ~1.5 MB
- 100,000 transactions: ~15 MB
- Daily volume (estimated): 50,000-100,000 transactions

## Sample Test Data Creation Script

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GENDATA.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-OUTPUT-FILE       RECORD VARYING IN SIZE FROM 150 TO 150.
       01 WS-ACCT-NUMBER       PIC X(12).
       01 WS-TXN-TYPE          PIC X VALUE 'D'.
       01 WS-AMOUNT            PIC 9(13)V99 VALUE 0.
       01 WS-COUNTER           PIC 9(5).
      
       PROCEDURE DIVISION.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM VARYING WS-COUNTER FROM 1 BY 1
               UNTIL WS-COUNTER > 100
               STRING 'ACC' WS-COUNTER DELIMITED BY SIZE
                   INTO WS-ACCT-NUMBER
               COMPUTE WS-AMOUNT = FUNCTION MOD(WS-COUNTER, 3) * 1000
               WRITE WS-OUTPUT-FILE FROM WS-TXN-TYPE
               EVALUATE FUNCTION MOD(WS-COUNTER, 3)
                   WHEN 0
                       MOVE 'D' TO WS-TXN-TYPE
                   WHEN 1
                       MOVE 'W' TO WS-TXN-TYPE
                   WHEN 2
                       MOVE 'T' TO WS-TXN-TYPE
               END-EVALUATE
           END-PERFORM.
           CLOSE OUTPUT-FILE.
           STOP RUN.
```

---

**Last Updated**: December 2025
**Data Version**: 1.0.0