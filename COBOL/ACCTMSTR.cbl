       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMSTR.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-INPUT-FILE
               ASSIGN TO UT-S-ACCTIN
               ORGANIZATION IS LINE SEQUENTIAL.
      
           SELECT ACCOUNT-OUTPUT-FILE
               ASSIGN TO UT-S-ACCTOUT
               ORGANIZATION IS LINE SEQUENTIAL.
      
       DATA DIVISION.
       FILE SECTION.
       FD ACCOUNT-INPUT-FILE.
       01 ACC-INPUT-RECORD            PIC X(100).
      
       FD ACCOUNT-OUTPUT-FILE.
       01 ACC-OUTPUT-RECORD           PIC X(150).
      
       WORKING-STORAGE SECTION.
      
       01 WS-PROGRAM-INFO.
           05 WS-PROGRAM-NAME         PIC X(8) VALUE 'ACCTMSTR'.
           05 WS-VERSION              PIC X(5) VALUE '1.0.0'.
           05 WS-EXECUTION-DATE       PIC 9(8).
           05 WS-EXECUTION-TIME       PIC 9(6).
      
       01 WS-FILE-CONTROL.
           05 WS-EOF-ACCTIN           PIC X VALUE 'N'.
               88 ACCTIN-EOF              VALUE 'Y'.
           05 WS-RECORD-COUNT         PIC 9(8) VALUE 0.
           05 WS-SUCCESS-COUNT        PIC 9(8) VALUE 0.
           05 WS-ERROR-COUNT          PIC 9(8) VALUE 0.
      
       COPY ACCTREC.
       COPY SQLCA.
       COPY ERRHDLR.
      
       01 WS-ACCOUNT-VARS.
           05 WS-ACCT-NUMBER          PIC X(12).
           05 WS-CUST-ID              PIC X(8).
           05 WS-ACCT-TYPE            PIC X(1).
           05 WS-ACCT-STATUS          PIC X(1).
           05 WS-ACCT-BALANCE         PIC S9(11)V99 COMP-3 VALUE 0.
           05 WS-INTEREST-RATE        PIC 9V9(4) COMP-3.
           05 WS-OVERDRAFT-LIMIT      PIC S9(11)V99 COMP-3.
      
       01 WS-ACTION-FLAG.
           05 WS-ACTION               PIC X(1).
               88 ACTION-CREATE           VALUE 'C'.
               88 ACTION-UPDATE           VALUE 'U'.
               88 ACTION-RETRIEVE         VALUE 'R'.
               88 ACTION-DELETE           VALUE 'D'.
      
       01 WS-DATE-TIME.
           05 WS-CURRENT-DATE-YMD     PIC 9(8).
           05 WS-CURRENT-TIME-HMS     PIC 9(6).
      
       PROCEDURE DIVISION.
      
       000-MAIN-PROCEDURE.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-ACCOUNTS.
           PERFORM 300-FINALIZATION.
           STOP RUN.
      
       100-INITIALIZATION.
           ACCEPT WS-EXECUTION-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-EXECUTION-TIME FROM TIME.
           MOVE 0 TO WS-RECORD-COUNT.
           MOVE 0 TO WS-SUCCESS-COUNT.
           MOVE 0 TO WS-ERROR-COUNT.
      
           OPEN INPUT ACCOUNT-INPUT-FILE.
           OPEN OUTPUT ACCOUNT-OUTPUT-FILE.
      
           DISPLAY 'ACCTMSTR: Starting Account Master Maintenance'.
           DISPLAY 'Date: ' WS-EXECUTION-DATE ' Time: ' WS-EXECUTION-TIME.
      
       200-PROCESS-ACCOUNTS.
           READ ACCOUNT-INPUT-FILE
               AT END MOVE 'Y' TO WS-EOF-ACCTIN
           END-READ.
      
           PERFORM UNTIL ACCTIN-EOF
               ADD 1 TO WS-RECORD-COUNT
      
               PERFORM 210-PARSE-INPUT-RECORD
               PERFORM 220-VALIDATE-ACTION
      
               IF WS-ACTION = 'C'
                   PERFORM 230-CREATE-ACCOUNT
               ELSE IF WS-ACTION = 'U'
                   PERFORM 240-UPDATE-ACCOUNT
               ELSE IF WS-ACTION = 'R'
                   PERFORM 250-RETRIEVE-ACCOUNT
               ELSE IF WS-ACTION = 'D'
                   PERFORM 260-DELETE-ACCOUNT
               ELSE
                   PERFORM 270-LOG-INVALID-ACTION
               END-IF
      
               READ ACCOUNT-INPUT-FILE
                   AT END MOVE 'Y' TO WS-EOF-ACCTIN
               END-READ
           END-PERFORM.
      
       210-PARSE-INPUT-RECORD.
           MOVE ACC-INPUT-RECORD(1:1) TO WS-ACTION.
           MOVE ACC-INPUT-RECORD(2:12) TO WS-ACCT-NUMBER.
           MOVE ACC-INPUT-RECORD(14:8) TO WS-CUST-ID.
           MOVE ACC-INPUT-RECORD(22:1) TO WS-ACCT-TYPE.
      
       220-VALIDATE-ACTION.
           IF WS-ACTION NOT = 'C' AND
              WS-ACTION NOT = 'U' AND
              WS-ACTION NOT = 'R' AND
              WS-ACTION NOT = 'D'
               MOVE 'N' TO WS-VALID-TRANSACTION
           END-IF.
      
       230-CREATE-ACCOUNT.
      *>    Insert new account into DB2
           EXEC SQL
               INSERT INTO ACCOUNTS
                   (ACCOUNT_NUMBER, CUSTOMER_ID, ACCOUNT_TYPE,
                    ACCOUNT_BALANCE, ACCOUNT_STATUS,
                    CREATION_DATE, LAST_UPDATE_DATE, LAST_UPDATE_TIME)
               VALUES
                   (:WS-ACCT-NUMBER, :WS-CUST-ID, :WS-ACCT-TYPE,
                    0.00, 'A', CURRENT_DATE, CURRENT_DATE, CURRENT_TIME)
           END-EXEC.
      
           IF SQLCODE = 0
               PERFORM 235-LOG-CREATE-SUCCESS
               ADD 1 TO WS-SUCCESS-COUNT
           ELSE
               PERFORM 236-LOG-CREATE-ERROR
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
      
       235-LOG-CREATE-SUCCESS.
           STRING WS-EXECUTION-DATE DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-EXECUTION-TIME DELIMITED BY SIZE
               ': CREATE SUCCESS - Account: ' DELIMITED BY SIZE
               WS-ACCT-NUMBER DELIMITED BY SIZE
               ' Customer: ' DELIMITED BY SIZE
               WS-CUST-ID DELIMITED BY SIZE
               INTO ACC-OUTPUT-RECORD
           END-STRING.
           WRITE ACC-OUTPUT-RECORD.
      
       236-LOG-CREATE-ERROR.
           STRING WS-EXECUTION-DATE DELIMITED BY SIZE
               ' ' DELIMITED BY SIZE
               WS-EXECUTION-TIME DELIMITED BY SIZE
               ': CREATE ERROR - SQLCODE=' DELIMITED BY SIZE
               SQLCODE DELIMITED BY SIZE
               INTO ACC-OUTPUT-RECORD
           END-STRING.
           WRITE ACC-OUTPUT-RECORD.
      
       240-UPDATE-ACCOUNT.
      *>    Update existing account in DB2
           EXEC SQL
               UPDATE ACCOUNTS
               SET ACCOUNT_STATUS = :WS-ACCT-STATUS,
                   LAST_UPDATE_DATE = CURRENT_DATE,
                   LAST_UPDATE_TIME = CURRENT_TIME
               WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER
           END-EXEC.
      
           IF SQLCODE = 0
               ADD 1 TO WS-SUCCESS-COUNT
               STRING WS-EXECUTION-DATE DELIMITED BY SIZE
                   ' UPDATE SUCCESS - ' DELIMITED BY SIZE
                   WS-ACCT-NUMBER DELIMITED BY SIZE
                   INTO ACC-OUTPUT-RECORD
               END-STRING
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               STRING WS-EXECUTION-DATE DELIMITED BY SIZE
                   ' UPDATE ERROR - SQLCODE=' DELIMITED BY SIZE
                   SQLCODE DELIMITED BY SIZE
                   INTO ACC-OUTPUT-RECORD
               END-STRING
           END-IF.
           WRITE ACC-OUTPUT-RECORD.
      
       250-RETRIEVE-ACCOUNT.
      *>    Retrieve account details from DB2
           EXEC SQL
               SELECT CUSTOMER_ID, ACCOUNT_TYPE, ACCOUNT_BALANCE,
                      ACCOUNT_STATUS
               INTO :WS-CUST-ID, :WS-ACCT-TYPE,
                    :WS-ACCT-BALANCE, :WS-ACCT-STATUS
               FROM ACCOUNTS
               WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER
           END-EXEC.
      
           IF SQLCODE = 0
               ADD 1 TO WS-SUCCESS-COUNT
               STRING WS-EXECUTION-DATE DELIMITED BY SIZE
                   ' RETRIEVE SUCCESS - Balance: ' DELIMITED BY SIZE
                   WS-ACCT-BALANCE DELIMITED BY SIZE
                   INTO ACC-OUTPUT-RECORD
               END-STRING
           ELSE
               ADD 1 TO WS-ERROR-COUNT
               STRING WS-EXECUTION-DATE DELIMITED BY SIZE
                   ' RETRIEVE ERROR - Account not found' DELIMITED BY SIZE
                   INTO ACC-OUTPUT-RECORD
               END-STRING
           END-IF.
           WRITE ACC-OUTPUT-RECORD.
      
       260-DELETE-ACCOUNT.
      *>    Mark account as inactive (soft delete)
           EXEC SQL
               UPDATE ACCOUNTS
               SET ACCOUNT_STATUS = 'I',
                   LAST_UPDATE_DATE = CURRENT_DATE,
                   LAST_UPDATE_TIME = CURRENT_TIME
               WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER
           END-EXEC.
      
           IF SQLCODE = 0
               ADD 1 TO WS-SUCCESS-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
           END-IF.
      
       270-LOG-INVALID-ACTION.
           STRING WS-EXECUTION-DATE DELIMITED BY SIZE
               ' INVALID ACTION - ' DELIMITED BY SIZE
               WS-ACTION DELIMITED BY SIZE
               INTO ACC-OUTPUT-RECORD
           END-STRING.
           WRITE ACC-OUTPUT-RECORD.
           ADD 1 TO WS-ERROR-COUNT.
      
       300-FINALIZATION.
           CLOSE ACCOUNT-INPUT-FILE.
           CLOSE ACCOUNT-OUTPUT-FILE.
      
           DISPLAY 'ACCTMSTR: Processing Complete'.
           DISPLAY 'Records Processed: ' WS-RECORD-COUNT.
           DISPLAY 'Successful: ' WS-SUCCESS-COUNT.
           DISPLAY 'Failed: ' WS-ERROR-COUNT.
      
           IF WS-ERROR-COUNT > 0
               MOVE 8 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.