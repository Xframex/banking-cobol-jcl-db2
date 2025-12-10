# Deployment and Operational Guide

## Pre-Deployment Checklist

### Development Environment Setup
- [ ] DB2 test instance created and running
- [ ] COBOL compiler installed (v6.3+)
- [ ] Sample tables created using SCHEMA.sql
- [ ] Sample data loaded using SAMPLE_DATA.sql
- [ ] Copybooks placed in USER.COPYBOOKS library
- [ ] Test data files prepared in USER.TRANSACTIONS

### Security and Permissions
- [ ] User IDs created for batch job execution
- [ ] DB2 authorization granted for user IDs
- [ ] Library permissions set (COBOL, COPYBOOKS, LOADLIB)
- [ ] Dataset permissions configured
- [ ] Audit logging enabled

## Development Deployment

### Step 1: Create DB2 Schema

```sql
-- Connect to development DB2 database
CONNECT TO DEVDB;

-- Execute schema creation
-- Run: DB2/SCHEMA.sql

-- Verify tables created
LIST TABLES FOR SCHEMA BANKDEV;
```

### Step 2: Compile COBOL Programs

```
// Submit COMPILE.jcl for each program
// Programs: TRANPROC, ACCTMSTR, RECONCIL, INTCALC

// COMPILE TRANPROC
jsub COMPILE.jcl with PGMNAME=TRANPROC

// COMPILE ACCTMSTR
jsub COMPILE.jcl with PGMNAME=ACCTMSTR
```

**Verify successful compilation:**
- Check LOADLIB contains load modules
- Verify DBRM library created
- Review compiler messages for errors

### Step 3: Bind DB2 Programs

```
// Submit BINDDB2.jcl
jsub BINDDB2.jcl

// Verify bind successful
LIST PACKAGES FOR SCHEMA BANKDEV;
```

### Step 4: Load Sample Data

```sql
-- Insert test customers and accounts
EXECUTE SAMPLE_DATA.sql

-- Verify data loaded
SELECT COUNT(*) FROM CUSTOMERS;
SELECT COUNT(*) FROM ACCOUNTS;
```

### Step 5: Test Batch Jobs

```
// Submit BATCH.jcl with test data
jsub BATCH.jcl

// Review output
- USER.TRANOUT    (successful transactions)
- USER.ERRLOG     (errors)
- SYSOUT output   (statistics)
```

## Production Deployment

### Prerequisite: Production DB2 Setup

**Database Configuration**
```sql
CREATE DATABASE BANKPROD
  BUFFERPOOL BP32K
  INDEXBP BP16K;

-- Create production schema
CONNECT TO BANKPROD;
EXECUTE SCHEMA.sql;
```

**User and Authority Setup**
```sql
CREATE USER ID='BANKADM' NAME='Bank Administrator';
CREATE USER ID='BATCHJOB' NAME='Batch Job User';

-- Grant permissions
GRANT DBADM ON DATABASE BANKPROD TO USER BANKADM;
GRANT SELECT, INSERT, UPDATE ON ACCOUNTS TO USER BATCHJOB;
GRANT SELECT, INSERT ON TRANSACTIONS TO USER BATCHJOB;
```

### Step 1: Migrate Code to Production

**Copy load modules**
```jcl
//COPYLIB  JOB (ACCT,001),'COPY TO PROD',CLASS=A
//COPY     EXEC PGM=IEBCOPY
//SYSUT1   DD DSN=DEV.LOADLIB,DISP=SHR
//SYSUT2   DD DSN=PROD.LOADLIB,DISP=(NEW,CATLG),
//         SPACE=(TRK,(100,20))
//SYSPRINT DD SYSOUT=*
//*
```

**Copy DBRM for rebinding**
```jcl
//COPYDBRM JOB (ACCT,001),'COPY DBRM',CLASS=A
//COPY     EXEC PGM=IEBCOPY
//SYSUT1   DD DSN=DEV.DBRM,DISP=SHR
//SYSUT2   DD DSN=PROD.DBRM,DISP=(NEW,CATLG),
//         SPACE=(TRK,(50,10))
//SYSPRINT DD SYSOUT=*
//*
```

### Step 2: Bind to Production Plan

```jcl
//BINDPROD JOB (ACCT,001),'BIND PRODUCTION',CLASS=A
//BIND     EXEC PGM=IKJEFT01,
//         PARM='DSNH BIND PLAN(BANKPROD) +
//                PACKAGE(TRANPROC,ACCTMSTR,RECONCIL) +
//                OWNER(BANKADM) VALIDATE(BIND)'
//SYSLIB   DD DSN=PROD.DBRM,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//STEPLIB  DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
//*
```

### Step 3: Initialize Production Data

```sql
-- Create production customers and accounts
CONNECT TO BANKPROD;
INSERT INTO CUSTOMERS VALUES
  ('PROD001', 'Bank Operations', ...);
INSERT INTO ACCOUNTS VALUES
  ('ACC999000001', 'PROD001', 'C', 0.00, 'A', ...);
```

### Step 4: Schedule Batch Jobs

**Configure Job Scheduler (Control-M or CA-7)**

```
Job Name: DAILY
Command: jsub DAILY.jcl
Schedule: Daily at 2:00 AM
Calendar: Business days only

Notifications:
  - Success: Email to banking-ops@company.com
  - Failure: Page to on-call DBA + Email to banking-ops
  - Warning: Email if processing > 30 minutes

Dependencies:
  - Run after EOD system complete
  - Before 6:00 AM customer availability requirement
```

### Step 5: Monitor First Week

**Daily Checks**
- [ ] Job completes successfully
- [ ] All transactions processed correctly
- [ ] Reconciliation balanced
- [ ] No SQL errors in logs
- [ ] Performance acceptable (<4 hours)

**Metrics to Track**
- Transactions processed per hour
- Average transaction processing time
- Peak memory usage
- DB2 index hit ratio
- Error rate

## Operational Procedures

### Daily Execution

**Pre-Batch Window (1:30 AM)**
- Verify transaction input files ready
- Check disk space availability
- Confirm DB2 online and responsive

**During Batch Window (2:00 AM - 6:00 AM)**
- Monitor batch job progress
- Check for errors in real-time
- Adjust resources if needed

**Post-Batch (6:00 AM)**
- Review reconciliation report
- Verify account balances
- Check error logs
- Archive output files
- Notify business teams of any issues

### Troubleshooting Common Issues

**Issue: Transaction Processing Hangs**
```
Possible Causes:
- DB2 deadlock
- High system load
- Insufficient memory

Solution:
1. Check DB2 logs for deadlock messages
2. Resubmit job (may be transient)
3. Increase REGION parameter in JCL
4. Review batch window time allocation
```

**Issue: SQLCODE -904 (Deadlock)**
```
Cause: Concurrent access to accounts
Solution:
1. Implement retry logic in TRANPROC
2. Reduce batch size if possible
3. Add explicit locking hints in SQL
4. Run batch earlier to avoid overlaps
```

**Issue: Reconciliation Doesn't Balance**
```
Cause: Missing or duplicate transactions
Solution:
1. Review transaction input file
2. Check for partial transaction processing
3. Examine error log for failed updates
4. Manually verify sample accounts
5. Restore from backup if necessary
```

## Backup and Recovery

### Backup Strategy

**Before Each Batch Run**
```sql
BACKUP DATABASE BANKPROD TO '/backup/bankprod_YYYYMMDD.bak';
```

**Weekly Full Backup**
```
Every Sunday: Full database backup
Daily Mon-Sat: Incremental backups
Archive: Off-site storage after 1 month
```

### Recovery Procedure

**Restore from Point-in-Time**
```sql
RESTORE DATABASE BANKPROD FROM '/backup/bankprod_YYYYMMDD.bak'
  TO POINT IN TIME 2025-12-10 04:30:00;
```

**Verify Data Integrity**
```sql
CHECK DATABASE BANKPROD;
RORBACK FULL;
-- Verify account balances match before-batch values
```

## Performance Tuning

### DB2 Index Optimization

```sql
-- Rebuild indexes after batch load
REBUILD INDEX IDX_ACCT_NUMBER;
REBUILD INDEX IDX_TRN_DATE;

-- Update statistics
RUNSTAT ON TABLE ACCOUNTS ALL;
RUNSTAT ON TABLE TRANSACTIONS ALL;

-- Re-bind for optimized access paths
BIND PLAN(BANKPROD) REPLACE VALIDATE(BIND);
```

### JCL Optimization

```jcl
// Increase parallelism
//MAIN1    EXEC PGM=TRANPROC,PARM='BATCH=1-500'
//MAIN2    EXEC PGM=TRANPROC,PARM='BATCH=501-1000'
//MAIN3    EXEC PGM=TRANPROC,PARM='BATCH=1001-1500'

// Run RECONCIL after both complete
//RECON    EXEC RECONCIL,COND=(4,LT,MAIN1)(4,LT,MAIN2)(4,LT,MAIN3)
```

## Maintenance

### Monthly Tasks
- [ ] Review batch processing logs
- [ ] Analyze performance metrics
- [ ] Archive historical data
- [ ] Update documentation
- [ ] Test disaster recovery procedures

### Quarterly Tasks
- [ ] DB2 reorganization
- [ ] Index maintenance
- [ ] Capacity planning
- [ ] Security audit
- [ ] Software updates

### Annual Tasks
- [ ] Full system audit
- [ ] Compliance validation
- [ ] DR drill
- [ ] Performance baseline
- [ ] Strategic planning

## Contacts and Escalation

**Banking Operations Team**
- Email: banking-ops@company.com
- Phone: (555) 123-4567
- Hours: 24/7 during production

**Database Administration**
- Email: dba@company.com
- On-call: (555) 987-6543
- Page for emergencies only

**Mainframe Operations**
- Email: mainframe-ops@company.com
- Phone: (555) 345-6789

---

For additional support, consult:
- `/docs/ARCHITECTURE.md` - System design
- `/docs/DB2_INTEGRATION.md` - Database details
- IBM DB2 z/OS documentation
- COBOL programming guides