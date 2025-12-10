# 📊 Data Input/Output Flow - Banking System

## Overview

This document shows **exactly where data enters and exits** the banking system, including file locations, dataset names, and data flow transformations.

---

## 🔄 Complete Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                    DATA SOURCES (INPUT)                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  📄 TRANSACTIONS.txt          📄 ACCOUNTS.txt                   │
│  (10 sample transactions)     (Sample account master)           │
│  Sequential file format       Fixed-length records             │
│  150 bytes per record         120 bytes per record             │
│                                                                 │
│  OR                           OR                                │
│                                                                 │
│  USER.TRANSIN                 USER.ACCTIN                       │
│  (Production input)           (Production input)               │
│  100K+ daily transactions     Account maintenance             │
│                                                                 │
└────────────────┬──────────────────────────────────────────────┘
                 │
                 ├──────────────────────────────┐
                 │                              │
         ┌───────▼────────┐          ┌──────────▼──────┐
         │   BATCH.jcl    │          │  ACCTMSTR.cbl   │
         │  Processing    │          │  Account Maint  │
         └───────┬────────┘          └──────────┬──────┘
                 │                              │
         ┌───────▼──────────────────────────────▼─────┐
         │         DB2 DATABASE OPERATIONS             │
         ├────────────────────────────────────────────┤
         │                                            │
         │  ┌─────────────────────────────────────┐  │
         │  │      ACCOUNTS Table                  │  │
         │  │  • ACCOUNT_NUMBER (PK)              │  │
         │  │  • ACCOUNT_BALANCE (UPDATED)        │  │
         │  │  • ACCOUNT_STATUS                   │  │
         │  │  • CUSTOMER_ID                      │  │
         │  │  • LAST_UPDATE_DATE (UPDATED)       │  │
         │  └─────────────────────────────────────┘  │
         │                                            │
         │  ┌─────────────────────────────────────┐  │
         │  │    TRANSACTIONS Table                │  │
         │  │  • TRANSACTION_ID (NEW)             │  │
         │  │  • ACCOUNT_NUMBER (FK)              │  │
         │  │  • TRANSACTION_AMT                  │  │
         │  │  • TRANSACTION_DATE                 │  │
         │  │  • STATUS                           │  │
         │  └─────────────────────────────────────┘  │
         │                                            │
         │  ┌─────────────────────────────────────┐  │
         │  │    CUSTOMERS Table                  │  │
         │  │  • CUSTOMER_ID (PK)                 │  │
         │  │  • CUSTOMER_NAME                    │  │
         │  │  • ADDRESS                          │  │
         │  └─────────────────────────────────────┘  │
         │                                            │
         └────────┬──────────────────────────────────┘
                  │
        ┌─────────┴──────────┐
        │                    │
   ┌────▼─────┐        ┌────▼──────┐
   │ TRANPROC  │        │ ACCTMSTR   │
   │ Processing│        │ Account    │
   └────┬─────┘        │ Maintenance│
        │              └────┬───────┘
        │                   │
        ├───────────────────┤
        │                   │
   ┌────▼──────┐       ┌────▼──────┐
   │ RECONCIL   │       │ INTCALC    │
   │ Daily      │       │ Interest   │
   │ Reconcile  │       │ Posting    │
   └────┬───────┘       └────┬───────┘
        │                    │
        └────────┬───────────┘
                 │
   ┌─────────────▼─────────────────────────────────┐
   │      OUTPUT FILES & REPORTS                   │
   ├────────────────────────────────────────────────┤
   │                                               │
   │  📊 SUCCESS RECORDS                           │
   │  USER.TRANOUT                                 │
   │  (Processed transactions - successful)        │
   │                                               │
   │  ⚠️  ERROR LOG                                 │
   │  USER.ERRLOG                                  │
   │  (Failed transactions with error codes)       │
   │                                               │
   │  📋 RECONCILIATION REPORT                      │
   │  USER.RECONCIL                                │
   │  (Daily balance verification)                 │
   │                                               │
   │  💰 INTEREST POSTING REPORT                    │
   │  USER.INTREPORT                               │
   │  (Interest calculations & postings)           │
   │                                               │
   │  📈 MANAGEMENT REPORT                          │
   │  SYSOUT                                       │
   │  (Summary for management review)              │
   │                                               │
   └────────────────────────────────────────────────┘
```

---

## 📥 DATA INPUTS

### 1. Transaction Input File

**File Location (Dev/Test)**:
```
DATA/TRANSACTIONS.txt
```

**File Location (Production)**:
```
USER.TRANSIN
USER.PROD.TRANSIN
```

**JCL DD Statement**:
```jcl
//TRANSIN  DD DSN=USER.TRANSIN,DISP=SHR
```

**Record Format**:
```
Position  Length  Type    Field              Example
─────────────────────────────────────────────────────────
1-12      12      CHAR    ACCOUNT_NUMBER     ACC000000001
13        1       CHAR    TRANSACTION_TYPE   D (Deposit)
14-28     15      NUM     AMOUNT             000000001500.00
29-48     20      CHAR    DESCRIPTION        Deposit Check
49-58     10      CHAR    REFERENCE_NUMBER   CHECK1234
59-66     8       NUM     TRANSACTION_DATE   20251210
67-72     6       NUM     TRANSACTION_TIME   090030
73        1       CHAR    STATUS             C (Complete)
74-150    77      CHAR    FILLER             (reserved)
─────────────────────────────────────────────────────────
Total:    150 bytes per record
```

**Sample Records**:
```
ACC000000001D00000001500.00Deposit Check #1234    CHECK1234 20251210 090030C
ACC000000002W00000002500.00ATM Withdrawal....     ATM001234  20251210 091530C
ACC000000003T00000001000.00Transfer to Account2.. TRF002345  20251210 092015C
ACC000000004D00000000500.00Direct Deposit........ DD0003456  20251210 093000C
ACC000000005W00000000250.00Bill Payment........ BILL004567  20251210 094530P
```

**How It Gets Read** (TRANPROC.cbl):
```cobol
       OPEN INPUT TRANSACTION-INPUT-FILE.
       READ TRANSACTION-INPUT-FILE
           AT END MOVE 'Y' TO WS-EOF-INPUT
       END-READ.
       
       PERFORM UNTIL INPUT-EOF
           * Parse the 150-byte record
           MOVE INPUT-REC(1:12) TO WS-ACCT-NUMBER
           MOVE INPUT-REC(13:1) TO WS-TXN-TYPE
           MOVE INPUT-REC(14:15) TO WS-AMOUNT
           ...
       END-PERFORM.
       CLOSE TRANSACTION-INPUT-FILE.
```

**Volume**:
- **Dev**: 10 sample records
- **Test**: 1,000 records
- **Production**: 50,000-100,000 records per day

---

### 2. Account Master Input File

**File Location (Dev/Test)**:
```
DATA/ACCOUNTS.txt
```

**File Location (Production)**:
```
USER.ACCTIN
USER.PROD.ACCTIN
```

**JCL DD Statement**:
```jcl
//ACCTIN   DD DSN=USER.ACCTIN,DISP=SHR
```

**Record Format**:
```
Position  Length  Type    Field              Example
──────────────────────────────────────────────────────────
1         1       CHAR    ACTION             C/U/R/D
2-13      12      CHAR    ACCOUNT_NUMBER     ACC000000001
14-21     8       CHAR    CUSTOMER_ID        CUST0001
22        1       CHAR    ACCOUNT_TYPE       C/S/M
23-35     13      NUM     ACCOUNT_BALANCE    0000002500.00
36        1       CHAR    ACCOUNT_STATUS     A/I/X
37-41     5       NUM     INTEREST_RATE      2.2500
42-54     13      NUM     OVERDRAFT_LIMIT    0000001000.00
55-62     8       NUM     CREATION_DATE      20251001
63-70     8       NUM     LAST_UPDATE_DATE   20251210
71-76     6       NUM     LAST_UPDATE_TIME   092230
77-120    44      CHAR    FILLER             (reserved)
──────────────────────────────────────────────────────────
Total:    120 bytes per record
```

**Sample Records**:
```
CACC000000001CUST0001C0000002500.00A0.025000001000.0020251001202510092230
UACC000000002CUST0001S0000010000.00A0.022500000000.0020251001202510092230
RACC000000003CUST0002C0000005000.00A0.005000001500.0020251001202510092230
```

**Actions**:
- **C** = CREATE new account
- **U** = UPDATE existing account
- **R** = RETRIEVE account details
- **D** = DELETE (soft) account

---

### 3. DB2 Database Tables (Input)

**ACCOUNTS Table Query** (in TRANPROC.cbl):
```cobol
EXEC SQL
    SELECT ACCOUNT_BALANCE, ACCOUNT_STATUS, 
           ACCOUNT_TYPE, OVERDRAFT_LIMIT
    INTO :WS-BALANCE, :WS-STATUS, 
         :WS-TYPE, :WS-OVERDRAFT
    FROM ACCOUNTS
    WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER
END-EXEC.
```

**Data Retrieved**:
- Current account balance
- Account status (Active/Inactive/Suspended)
- Account type (Checking/Savings/Money Market)
- Overdraft limit for checking accounts

**CUSTOMERS Table Query** (for account inquiry):
```cobol
EXEC SQL
    SELECT CUSTOMER_NAME, PHONE, EMAIL
    INTO :WS-CUST-NAME, :WS-PHONE, :WS-EMAIL
    FROM CUSTOMERS
    WHERE CUSTOMER_ID = :WS-CUST-ID
END-EXEC.
```

---

## 📤 DATA OUTPUTS

### 1. Success Transaction Output

**File Location**:
```
USER.TRANOUT
USER.PROD.TRANOUT
```

**JCL DD Statement**:
```jcl
//TRANOUT  DD DSN=USER.TRANOUT,DISP=(NEW,KEEP),
//         SPACE=(TRK,(10,5)),UNIT=SYSALLDA
```

**Record Format** (150 bytes):
```
Position  Length  Type    Field              Example
───────────────────────────────────────────────────────
1-8       8       NUM     TIMESTAMP          20251210
9-14      6       NUM     TIME               090030
15        1       CHAR    STATUS             S (Success)
16-27     12      CHAR    ACCOUNT_NUMBER     ACC000000001
28-42     15      NUM     TRANSACTION_AMT    000000001500.00
43-80     38      CHAR    MESSAGE            Transaction processed
81-150    70      CHAR    AUDIT_INFO         (additional data)
───────────────────────────────────────────────────────
```

**Sample Output**:
```
20251210 090030 S ACC000000001 000000001500.00 Transaction processed successfully
20251210 091530 S ACC000000002 000000002500.00 Withdrawal authorized
20251210 092015 S ACC000000003 000000001000.00 Transfer completed
```

**Volume**: One line per successful transaction

---

### 2. Error Log Output

**File Location**:
```
USER.ERRLOG
USER.PROD.ERRLOG
```

**JCL DD Statement**:
```jcl
//ERRLOG   DD DSN=USER.ERRLOG,DISP=(NEW,KEEP),
//         SPACE=(TRK,(10,5)),UNIT=SYSALLDA
```

**Record Format** (200 bytes):
```
Position  Length  Type    Field              Example
────────────────────────────────────────────────────────
1-8       8       NUM     TIMESTAMP          20251210
9-14      6       NUM     TIME               090045
15-18     4       NUM     SQLCODE            -911 (deadlock)
19-30     12      CHAR    ACCOUNT_NUMBER     ACC000000099
31-60     30      CHAR    ERROR_MESSAGE      Insufficient Funds
61-90     30      CHAR    ERROR_DETAIL       Balance too low
91-150    60      CHAR    RECOVERY_ACTION    Transaction rolled back
────────────────────────────────────────────────────────
```

**Sample Errors**:
```
20251210 090045 -911 ACC000000099 Insufficient Funds         Balance too low for withdrawal         Transaction rolled back
20251210 091200 -803 ACC000000050 Duplicate Transaction      Duplicate key in TRANSACTIONS          Transaction skipped
20251210 092030 -904 ACC000000075 Account Not Found          Account number does not exist         Transaction rejected
20251210 093015 -100 ACC000000101 Account Inactive           Account status = I                     Transaction blocked
```

**SQLCODE Reference**:
```
0      = Success (no error)
+100   = No more rows (end of data)
-911   = Deadlock detected
-803   = Duplicate key value
-904   = Resource unavailable
-100   = No row found
```

---

### 3. Reconciliation Report Output

**File Location**:
```
USER.RECONCIL
USER.PROD.RECONCIL
```

**Content** (Human-readable report):
```
╔════════════════════════════════════════════════════════════╗
║         DAILY RECONCILIATION REPORT                        ║
║         December 10, 2025                                  ║
╠════════════════════════════════════════════════════════════╣
║                                                            ║
║  DEPOSITS                                                  ║
║  ════════                                                  ║
║  Count:         25,432                                     ║
║  Total Amount:  $5,234,567.89                              ║
║                                                            ║
║  WITHDRAWALS                                               ║
║  ═══════════                                               ║
║  Count:         18,921                                     ║
║  Total Amount:  $3,876,543.21                              ║
║                                                            ║
║  TRANSFERS                                                 ║
║  ═════════                                                 ║
║  Count:         5,234                                      ║
║  Total Amount:  $2,345,678.90                              ║
║                                                            ║
║  INTEREST POSTED                                           ║
║  ════════════════                                          ║
║  Count:         10,000                                     ║
║  Total Amount:  $15,432.10                                 ║
║                                                            ║
║  FAILED TRANSACTIONS                                       ║
║  ══════════════════                                        ║
║  Count:         156                                        ║
║  Total Amount:  $234,567.00                                ║
║                                                            ║
║  DISCREPANCIES FOUND:                                      ║
║  ═══════════════════                                       ║
║  Account ACC000000005: $10.00 variance                      ║
║  Account ACC000000017: Balance mismatch                     ║
║  Account ACC000000089: Pending transfer unmatched           ║
║                                                            ║
║  RECONCILIATION STATUS: ⚠️  VARIANCE DETECTED               ║
║                                                            ║
╚════════════════════════════════════════════════════════════╝
```

**Generated By**: RECONCIL.cbl
**Data Source**: TRANSACTIONS table from past 24 hours

---

### 4. Interest Posting Report

**File Location**:
```
USER.INTREPORT
USER.PROD.INTREPORT
```

**Content**:
```
╔══════════════════════════════════════════════════════════╗
║      INTEREST POSTING REPORT                             ║
║      December 10, 2025 - Monthly Posting                 ║
╠══════════════════════════════════════════════════════════╣
║                                                          ║
║  SAVINGS ACCOUNTS PROCESSED: 7,234                       ║
║  ═══════════════════════════════════                     ║
║  Total Interest Posted: $18,567.89                       ║
║  Average Interest: $2.57 per account                     ║
║  Min Interest: $0.01                                     ║
║  Max Interest: $145.32                                   ║
║                                                          ║
║  MONEY MARKET ACCOUNTS PROCESSED: 2,456                  ║
║  ═══════════════════════════════════════════             ║
║  Total Interest Posted: $9,234.56                        ║
║  Average Interest: $3.76 per account                     ║
║  Min Interest: $0.05                                     ║
║  Max Interest: $267.89                                   ║
║                                                          ║
║  CHECKING ACCOUNTS: 0 (No interest)                      ║
║  ═════════════════════════════════════                   ║
║                                                          ║
║  SUMMARY                                                 ║
║  ═══════                                                 ║
║  Total Accounts Updated: 9,690                           ║
║  Total Interest Posted: $27,802.45                       ║
║  Processing Status: ✅ SUCCESSFUL                         ║
║                                                          ║
╚══════════════════════════════════════════════════════════╝
```

**Generated By**: INTCALC.cbl
**Data Updated**: ACCOUNTS table (ACCOUNT_BALANCE)
**Data Inserted**: TRANSACTIONS table (new interest records)

---

### 5. Management Summary Report

**File Location**:
```
SYSOUT DD
(Printed to job output)
```

**Content**:
```
BANKING SYSTEM - DAILY BATCH REPORT
Date: December 10, 2025
Time: 05:30:00 EST

═════════════════════════════════════════════════════════
PROCESSING SUMMARY
═════════════════════════════════════════════════════════

Transaction Processing:
  Input Records:         98,534
  Successfully Processed: 98,378 (99.84%)
  Failed Records:           156 (0.16%)
  Skipped Records:            0

Account Updates:
  New Accounts Created:      12
  Accounts Updated:        456
  Accounts Retrieved:    1,234
  Accounts Deleted:          5

DB2 Performance:
  Queries Executed:      98,534
  Average Query Time:       4.2 ms
  Peak Time:              12.5 ms
  Database Errors:            0

Batch Timing:
  Start Time:        02:00:00
  Validation Time:   00:15:00 (02:15)
  Processing Time:   02:45:00 (05:00)
  Reconciliation:    00:15:00 (05:15)
  Interest Posting:  00:10:00 (05:25)
  Reports:           00:05:00 (05:30)
  Total Duration:    03:30:00

File Statistics:
  Input File Size:        14.8 MB
  Output File Size:       14.7 MB
  Error Log Size:         0.2 MB
  Report File Size:       0.5 MB

═════════════════════════════════════════════════════════
FINANCIAL TOTALS
═════════════════════════════════════════════════════════

Deposits:
  Count:          25,432
  Total:   $5,234,567.89

Withdrawals:
  Count:          18,921
  Total:   $3,876,543.21

Transfers:
  Count:           5,234
  Total:   $2,345,678.90

Interest Posted:
  Count:          10,000
  Total:        $27,802.45

Net Change in Assets:  $730,505.13 (0.12% growth)

═════════════════════════════════════════════════════════
STATUS: ✅ SUCCESSFUL - All processing complete
═════════════════════════════════════════════════════════
```

---

## 🔄 Data Transformation Example

### Input Transaction
```
ACC000000001D00000001500.00Deposit Check #1234    CHECK1234 20251210 090030C
├─ Account: ACC000000001
├─ Type: D (Deposit)
├─ Amount: $1,500.00
└─ Status: C (Complete)
```

### Processing in TRANPROC.cbl
```cobol
1. PARSE INPUT
   Account = ACC000000001
   Type = D
   Amount = 1500.00

2. QUERY DB2
   SELECT ACCOUNT_BALANCE, ACCOUNT_STATUS
   FROM ACCOUNTS WHERE ACCOUNT_NUMBER = 'ACC000000001'
   
   Result:
   Balance = $2,500.00
   Status = A (Active)

3. VALIDATE
   Status check: A = Active ✅
   Type check: D = Deposit ✅
   Amount: 1500.00 > 0 ✅
   
4. CALCULATE
   New Balance = 2500.00 + 1500.00 = 4000.00
   
5. UPDATE DB2
   UPDATE ACCOUNTS
   SET ACCOUNT_BALANCE = 4000.00,
       LAST_UPDATE_DATE = CURRENT_DATE,
       LAST_UPDATE_TIME = CURRENT_TIME
   WHERE ACCOUNT_NUMBER = 'ACC000000001'
   
6. INSERT AUDIT
   INSERT INTO TRANSACTIONS
   (TRANSACTION_ID, ACCOUNT_NUMBER, TRANSACTION_TYPE,
    TRANSACTION_AMT, TRANSACTION_DATE, TRANSACTION_TIME, STATUS)
   VALUES
   ('TRN20251210090030', 'ACC000000001', 'D',
    1500.00, 20251210, 090030, 'C')
```

### Output Record
```
SUCCESS OUTPUT:
20251210 090030 S ACC000000001 000000001500.00 Transaction processed successfully

DB2 STATE AFTER:
  ACCOUNTS.ACC000000001.ACCOUNT_BALANCE = 4000.00 (was 2500.00)
  TRANSACTIONS table contains new record
```

---

## 📍 File Locations Summary

### Development/Test Environment
```
Project Root/
├── DATA/
│   ├── TRANSACTIONS.txt          ← Input transactions
│   └── ACCOUNTS.txt              ← Account master input
└── Output Files (same directory in test)
    ├── output/SUCCESS.txt
    ├── output/ERRORS.txt
    ├── output/RECONCIL.txt
    └── output/INTREPORT.txt
```

### Production Environment (Mainframe)
```
USER.TRANSIN              ← Input transactions
USER.ACCTIN               ← Account master input
USER.TRANOUT              ← Success records
USER.ERRLOG               ← Error log
USER.RECONCIL             ← Reconciliation report
USER.INTREPORT            ← Interest posting report
USER.PROD.TRANSIN         ← Production input (alternate)
USER.PROD.TRANOUT         ← Production output (alternate)
```

### Database
```
DB2 Tables:
├── ACCOUNTS               ← Current balances (UPDATED)
├── TRANSACTIONS           ← Transaction history (INSERTED)
└── CUSTOMERS              ← Customer info (READ ONLY)
```

---

## 🔍 Data Flow Checklist

### Inbound Data ✅
- [ ] TRANSACTIONS.txt (or USER.TRANSIN) - Contains transaction records
- [ ] ACCOUNTS.txt (or USER.ACCTIN) - Contains account master records
- [ ] DB2 ACCOUNTS table - Current balances for validation
- [ ] DB2 CUSTOMERS table - Customer lookup data

### Processing ✅
- [ ] Parse input records
- [ ] Query DB2 for current state
- [ ] Validate business rules
- [ ] Update DB2 ACCOUNTS table
- [ ] Insert into DB2 TRANSACTIONS table
- [ ] Log success/error results

### Outbound Data ✅
- [ ] USER.TRANOUT - Successful transactions
- [ ] USER.ERRLOG - Failed transactions with error codes
- [ ] USER.RECONCIL - Daily balance verification report
- [ ] USER.INTREPORT - Interest posting report
- [ ] SYSOUT - Management summary report

### Database State Changes ✅
- [ ] ACCOUNTS.ACCOUNT_BALANCE - Updated with new balance
- [ ] ACCOUNTS.LAST_UPDATE_DATE - Set to current date
- [ ] ACCOUNTS.LAST_UPDATE_TIME - Set to current time
- [ ] TRANSACTIONS table - New record inserted for each transaction

---

## 📌 Important Notes

1. **Input Validation**: All input records must match the exact format and field lengths specified

2. **Error Handling**: Invalid records are written to ERRLOG, not rejected silently

3. **DB2 Transactions**: Each transaction has COMMIT at the end to persist changes

4. **Audit Trail**: Every change is logged in TRANSACTIONS table for regulatory compliance

5. **Report Generation**: Reports are human-readable for management review

6. **Backup**: All output files are kept for 7 days minimum per compliance requirements

---

**Last Updated**: December 2025  
**Version**: 1.0.0
