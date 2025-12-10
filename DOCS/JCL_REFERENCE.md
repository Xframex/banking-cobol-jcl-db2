# JCL Reference Guide

## Introduction to JCL

Job Control Language (JCL) is the command language for mainframe batch processing. It defines:
- What programs to execute
- What files they read/write
- In what order to run them
- How to handle errors

## JCL Syntax Basics

### Three Statement Types

**JOB Statement** - Starts a job
```jcl
//JOBNAME  JOB (ACCOUNT,CLASS),'Job Description',TIME=0010
```

**EXEC Statement** - Executes a program
```jcl
//STEPNAME EXEC PGM=PROGNAME,PARM='parameters'
```

**DD Statement** - Allocates datasets (files)
```jcl
//DDNAME   DD DSN=dataset.name,DISP=(status,action)
```

## JOB Statement

### Syntax
```jcl
//jobname JOB (acctnum,dept),'Description',keyword=value...
```

### Common Parameters

| Parameter | Purpose | Example |
|-----------|---------|----------|
| CLASS | Job priority/class | CLASS=A |
| MSGCLASS | Output destination | MSGCLASS=H |
| TIME | CPU time limit (mm:ss) | TIME=0010 |
| REGION | Memory allocation | REGION=4096K |
| COND | Job-level condition | COND=(4,LT) |
| RESTART | Restart checkpoint | RESTART=stepname |

### Example JOB Statement
```jcl
//BATCH    JOB (ACCT,001),'BATCH PROCESSING',
//         CLASS=A,MSGCLASS=H,TIME=0030,REGION=4096K
```

## EXEC Statement

### Syntax
```jcl
//stepname EXEC PGM=program,PARM='parameters',COND=condition
```

### Parameters

**PGM** - Program to execute
```jcl
EXEC PGM=TRANPROC      // Execute TRANPROC program
EXEC PGM=IEFBR14       // Null utility (used for setup)
EXEC PGM=IEBGENER      // Copy/generate utility
```

**PARM** - Parameters passed to program
```jcl
EXEC PGM=TRANPROC,PARM='PROD'     // Pass 'PROD' environment
EXEC PGM=DSNHPC,PARM='QUOTE("''")' // DB2 precompiler options
```

**COND** - Conditional execution
```jcl
COND=(code,operator)  // Execute based on prior return code
```

**REGION** - Memory allocation
```jcl
REGION=4096K    // 4096 kilobytes
REGION=0M       // Use system default
```

**COND Parameter Details**

COND=(code, operator) means:
- **code**: Return code value to compare against
- **operator**: Comparison operator (LT, LE, EQ, NE, GE, GT)
- Execute step if **CONDITION IS FALSE**

Examples:
```jcl
COND=(0,NE)    // Execute if prior step return code ≠ 0 (i.e., if error)
COND=(4,LT)    // Execute if prior step return code < 4 (i.e., if success)
COND=(8,LT)    // Execute if prior step return code < 8 (warnings OK)
```

### Multi-Step EXEC with Dependencies

```jcl
//STEP1   EXEC PGM=VALIDATE
//*
//STEP2   EXEC PGM=PROCESS,COND=(4,LT,STEP1)
//        -- Execute STEP2 only if STEP1 succeeded (return code < 4)
//*
//STEP3   EXEC PGM=REPORT,COND=(0,NE,STEP2)
//        -- Execute STEP3 only if STEP2 failed (return code ≠ 0)
```

## DD Statement

### Purpose
Defines input/output datasets for programs.

### Syntax
```jcl
//ddname DD DSN=dataset.name,DISP=(status,action),allocation...
```

### Dataset Name (DSN)
```jcl
DSN=USER.COBOL(TRANPROC)      // Member of partitioned dataset
DSN=USER.TRANSACTIONS         // Sequential dataset
DSN=USER.TRANOUT              // Output dataset
```

### Disposition (DISP)

**Status** (what to do if dataset exists):
- **SHR** - Share (existing, read-only)
- **OLD** - Exclusive use (existing)
- **NEW** - Create new dataset
- **MOD** - Modify (open for append)

**Action** (what to do after step completes):
- **KEEP** - Keep the dataset
- **DELETE** - Delete the dataset
- **CATLG** - Catalog the dataset
- **UNCATLG** - Remove from catalog

### Common Combinations
```jcl
DISP=(SHR)           // Read existing shared dataset
DISP=(OLD)           // Read/write existing dataset exclusively
DISP=(NEW,KEEP)      // Create new, keep after step
DISP=(NEW,CATLG)     // Create new, catalog for future use
DISP=(MOD,DELETE)    // Clear existing, delete when done
```

### Space Allocation

```jcl
SPACE=(TRK,(primary,secondary),RLSE)
    TRK      = Allocate in tracks
    primary  = Initial allocation (50 tracks)
    secondary= Extension per overflow (10 tracks)
    RLSE     = Release unused space
```

Example:
```jcl
SPACE=(TRK,(100,20),RLSE)  // 100 tracks initially, 20 track extensions
```

### Record Format & Length

```jcl
RECFM=FB      // Fixed Blocked
RECFM=FBA     // Fixed Blocked with ANSI carriage control
RECFM=VB      // Variable Blocked
LRECL=150     // Logical Record Length = 150 bytes
```

### Device and Unit

```jcl
UNIT=SYSDA    // Standard disk device
UNIT=3390     // IBM disk device class
UNIT=TAPE     // Magnetic tape
VOL=SER=VOL001  // Specific volume
```

### Special DD Names

```jcl
//SYSOUT   DD SYSOUT=*       // Standard output (print)
//SYSPRINT DD SYSOUT=*       // Program output
//SYSIN    DD DUMMY          // No input
//STEPLIB  DD DSN=...        // Program library
//SYSLIB   DD DSN=...        // Include library (copybooks)
```

## Practical Examples

### Input Dataset
```jcl
//TRANIN   DD DSN=USER.TRANSACTIONS,DISP=SHR
```
- Read transaction file (existing, shared access)

### Output Dataset
```jcl
//TRANOUT  DD DSN=USER.TRANOUT,DISP=(,KEEP),
//         SPACE=(TRK,(100,20),RLSE),
//         UNIT=SYSDA,RECFM=FB,LRECL=150
```
- Create new transaction output file
- Allocate 100 tracks + extensions of 20 tracks
- Keep after step completes
- Fixed block format, 150-byte records

### Program Library
```jcl
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
```
- Search USER.LOADLIB first for programs
- Then search SYS1.DB2.SDSNLOAD for DB2 libraries

### Copybook Library
```jcl
//SYSLIB   DD DSN=USER.COPYBOOKS,DISP=SHR
//         DD DSN=SYS1.COBLIB,DISP=SHR
```
- User copybooks, then system copybooks

## Complete JCL Job Example

```jcl
//BATCHJOB JOB (ACCT,001),'DAILY BATCH',
//         CLASS=A,MSGCLASS=H,TIME=0030
//*
//* Step 1: Input validation
//VALIDATE EXEC PGM=IEFBR14
//*
//* Step 2: Process transactions (only if step 1 succeeded)
//TRANSPROC EXEC PGM=TRANPROC,COND=(0,NE,VALIDATE)
//TRANIN   DD DSN=USER.TRANSACTIONS,DISP=SHR
//TRANOUT  DD DSN=USER.TRANOUT,DISP=(NEW,KEEP),
//         SPACE=(TRK,(100,20),RLSE)
//ERRLOG   DD DSN=USER.ERRLOG,DISP=(NEW,KEEP),
//         SPACE=(TRK,(50,10),RLSE)
//SYSOUT   DD SYSOUT=*
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
//*
//* Step 3: Generate report (only if step 2 succeeded)
//REPORT   EXEC PGM=IEBGENER,COND=(0,NE,TRANSPROC)
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=USER.TRANOUT,DISP=SHR
//SYSUT2   DD SYSOUT=*
//
```

## Error Handling with COND

### Logic
- COND=(code, operator) = execute if **FALSE**
- Compare against previous step's return code
- Return code 0 = success, non-zero = error

### Common Patterns

**Skip on Error**
```jcl
// EXEC PGM=VALIDATE
//*
// EXEC PGM=PROCESS,COND=(4,LT)
// -- Only run if return code < 4 (success with warnings OK)
```

**Run Only on Error**
```jcl
// EXEC PGM=PROCESS
//*
// EXEC PGM=CLEANUP,COND=(0,NE)
// -- Only run cleanup if PROCESS failed (return code ≠ 0)
```

**Chain Multiple Steps**
```jcl
// EXEC PGM=STEP1
//
// EXEC PGM=STEP2,COND=(4,LT,STEP1)    // Depend on STEP1
//
// EXEC PGM=STEP3,COND=(4,LT,STEP2)    // Depend on STEP2
// -- Creates dependency chain
```

## Useful Utilities

### IEFBR14 - Null Utility
```jcl
// EXEC PGM=IEFBR14      // Allocate/deallocate only, don't run
```

### IEBGENER - Copy/Generate
```jcl
// EXEC PGM=IEBGENER     // Copy input to output
//SYSUT1   DD ...        // Input
//SYSUT2   DD ...        // Output
//SYSIN    DD DUMMY      // No control records
```

### DSNHPC - DB2 Precompiler
```jcl
// EXEC PGM=DSNHPC       // Extract SQL from COBOL
//SYSCIN   DD ...        // COBOL source
//SYSCOMP  DD ...        // Modified COBOL output
//SYSDBLIN DD ...        // DBRM output
```

## Common Issues and Solutions

### JOB STEP (STEPNAME) EXCEEDS TIME LIMIT
**Problem**: Job took too long
**Solution**: Increase TIME parameter
```jcl
TIME=0010   // 10 minutes (mm:ss format)
```

### NOT ENOUGH SPACE FOR DATASET
**Problem**: SPACE allocation too small
**Solution**: Increase primary and secondary allocations
```jcl
SPACE=(TRK,(500,50),RLSE)  // Larger allocation
```

### CANNOT FIND PROGRAM IN STEPLIB
**Problem**: Program not in search path
**Solution**: Add DD statements for STEPLIB
```jcl
//STEPLIB  DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
//         DD DSN=USER.LOADLIB,DISP=SHR
```

---

For additional help: `HELP JCL` at z/OS prompt or consult IBM JCL documentation.