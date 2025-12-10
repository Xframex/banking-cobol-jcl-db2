//EOD      JOB (ACCT,001),'END-OF-DAY PROCESSING',
//         CLASS=A,MSGCLASS=H,TIME=0030,REGION=0M
//*
//* End-of-Day batch processing job
//* Runs reconciliation, interest calculation, and reporting
//*
//RECONCIL EXEC PGM=RECONCIL,
//        PARM='PROD',
//        REGION=4096K
//*
//* Transaction input from BATCH job
//TRANIN   DD DSN=USER.TRANOUT,DISP=SHR
//*
//* Reconciliation report output
//RECNRPT  DD DSN=USER.RECONCIL,DISP=(,KEEP),
//         SPACE=(TRK,(50,10),RLSE),
//         UNIT=SYSDA,RECFM=FB,LRECL=200
//*
//SYSOUT   DD SYSOUT=*
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
//
//INTCALC EXEC PGM=INTCALC,
//        COND=(0,NE,RECONCIL),
//        REGION=4096K
//*
//* Interest calculation report
//INTRPT   DD DSN=USER.INTRPT,DISP=(,KEEP),
//         SPACE=(TRK,(50,10),RLSE),
//         UNIT=SYSDA,RECFM=FB,LRECL=150
//*
//SYSOUT   DD SYSOUT=*
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//         DD DSN=SYS1.DB2.SDSNLOAD,DISP=SHR
//
//FINREPT EXEC PGM=IEBGENER,
//        COND=(0,NE,INTCALC),
//        REGION=2048K
//*
//* Generate final EOD report
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=USER.RECONCIL,DISP=SHR
//SYSUT2   DD SYSOUT=*
//