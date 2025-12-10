       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTCALC.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INTEREST-REPORT
               ASSIGN TO UT-S-INTRPT
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INTEREST-REPORT.
       01 INTEREST-REPORT-LINE        PIC X(150).

       WORKING-STORAGE SECTION.

       01 WS-PROGRAM-INFO.
           05 WS-PROGRAM-NAME         PIC X(8) VALUE 'INTCALC'.
           05 WS-EXECUTION-DATE       PIC 9(8).
           05 WS-EXECUTION-TIME       PIC 9(6).

       01 WS-INTEREST-CALC.
           05 WS-ACCT-NUMBER          PIC X(12).
           05 WS-ACCOUNT-BALANCE      PIC S9(11)V99 COMP-3.
           05 WS-INTEREST-RATE        PIC 9V9(4) COMP-3.
           05 WS-INTEREST-AMOUNT      PIC S9(11)V99 COMP-3.
           05 WS-NEW-BALANCE          PIC S9(11)V99 COMP-3.
           05 WS-ACCT-TYPE            PIC X(1).
           05 WS-ACCT-STATUS          PIC X(1).

       01 WS-SUMMARY.
           05 WS-ACCT-COUNT           PIC 9(8) VALUE 0.
           05 WS-TOTAL-INTEREST       PIC S9(13)V99 COMP-3 VALUE 0.
           05 WS-TOTAL-NEW-BALANCES   PIC S9(13)V99 COMP-3 VALUE 0.

       COPY ACCTREC.
       COPY SQLCA.

       PROCEDURE DIVISION.

       000-MAIN-PROCEDURE.
           PERFORM 100-INITIALIZATION.
           PERFORM 200-PROCESS-INTEREST.
           PERFORM 300-FINALIZATION.
           STOP RUN.

       100-INITIALIZATION.
           ACCEPT WS-EXECUTION-DATE FROM DATE YYYYMMDD.
           ACCEPT WS-EXECUTION-TIME FROM TIME.
           MOVE 0 TO WS-ACCT-COUNT.
           MOVE 0 TO WS-TOTAL-INTEREST.

           OPEN OUTPUT INTEREST-REPORT.

           DISPLAY 'INTCALC: Starting Interest Calculation'.
           DISPLAY 'Date: ' WS-EXECUTION-DATE.

       200-PROCESS-INTEREST.
      *>    Retrieve all savings/money market accounts and calculate interest
           EXEC SQL
               DECLARE INTEREST_CURSOR CURSOR FOR
               SELECT ACCOUNT_NUMBER, ACCOUNT_BALANCE, INTEREST_RATE,
                      ACCOUNT_TYPE, ACCOUNT_STATUS
               FROM ACCOUNTS
               WHERE ACCOUNT_STATUS = 'A'
               AND (ACCOUNT_TYPE = 'S' OR ACCOUNT_TYPE = 'M')
           END-EXEC.

           EXEC SQL OPEN INTEREST_CURSOR END-EXEC.

           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH INTEREST_CURSOR
                   INTO :WS-ACCT-NUMBER, :WS-ACCOUNT-BALANCE,
                        :WS-INTEREST-RATE, :WS-ACCT-TYPE,
                        :WS-ACCT-STATUS
               END-EXEC

               IF SQLCODE = 0
                   ADD 1 TO WS-ACCT-COUNT
                   PERFORM 210-CALCULATE-INTEREST
                   PERFORM 220-UPDATE-ACCOUNT-INTEREST
                   PERFORM 230-LOG-INTEREST-POST
               END-IF
           END-PERFORM.

           EXEC SQL CLOSE INTEREST_CURSOR END-EXEC.

       210-CALCULATE-INTEREST.
           COMPUTE WS-INTEREST-AMOUNT =
               WS-ACCOUNT-BALANCE * WS-INTEREST-RATE / 12.
           COMPUTE WS-NEW-BALANCE =
               WS-ACCOUNT-BALANCE + WS-INTEREST-AMOUNT.

           ADD WS-INTEREST-AMOUNT TO WS-TOTAL-INTEREST.

       220-UPDATE-ACCOUNT-INTEREST.
           EXEC SQL
               UPDATE ACCOUNTS
               SET ACCOUNT_BALANCE = :WS-NEW-BALANCE
                   LAST_UPDATE_DATE = CURRENT_DATE
                   LAST_UPDATE_TIME = CURRENT_TIME
               WHERE ACCOUNT_NUMBER = :WS-ACCT-NUMBER
           END-EXEC.

           IF SQLCODE = 0
      *>        Insert transaction record
               EXEC SQL
                   INSERT INTO TRANSACTIONS
                       (TRANSACTION_ID, ACCOUNT_NUMBER,
                        TRANSACTION_TYPE, TRANSACTION_AMT,
                        TRANSACTION_DATE, TRANSACTION_TIME, STATUS)
                   VALUES
                       ('INT' || :WS-EXECUTION-DATE ||\n                        LPAD(:WS-ACCT-COUNT, 4, '0'),
                        :WS-ACCT-NUMBER, 'I', :WS-INTEREST-AMOUNT,
                        CURRENT_DATE, CURRENT_TIME, 'C')
               END-EXEC
           END-IF.

       230-LOG-INTEREST-POST.
           STRING 'Account: ' WS-ACCT-NUMBER DELIMITED BY SIZE
               ' Balance: ' WS-NEW-BALANCE DELIMITED BY SIZE
               ' Interest: ' WS-INTEREST-AMOUNT DELIMITED BY SIZE
               INTO INTEREST-REPORT-LINE.
           WRITE INTEREST-REPORT-LINE.

       300-FINALIZATION.
           CLOSE INTEREST-REPORT.

           DISPLAY 'INTCALC: Interest Calculation Complete'.
           DISPLAY 'Accounts Processed: ' WS-ACCT-COUNT.
           DISPLAY 'Total Interest Posted: ' WS-TOTAL-INTEREST.

           MOVE 0 TO RETURN-CODE.