       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-TXN-PROCESSOR-PLUS.
       AUTHOR. GEMINI-AI.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TXN-FILE ASSIGN TO 'TXNINPUT.DAT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT MASTER-FILE ASSIGN TO 'MASTER.DAT'
               ORGANIZATION IS INDEXED
               ACCESS IS RANDOM
               RECORD KEY IS MA-ACCOUNT-NUM.

           SELECT REPORT-FILE ASSIGN TO 'REPORT.OUT'
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT AUDIT-FILE ASSIGN TO 'AUDIT.OUT'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  TXN-FILE.
       01  TXN-RECORD.
           05  TXN-ACCOUNT-NUM         PIC 9(8).
           05  TXN-TYPE                PIC X(1).
               88 TXN-DEPOSIT          VALUE 'D'.
               88 TXN-WITHDRAW         VALUE 'W'.
               88 TXN-TRANSFER         VALUE 'T'.
               88 VALID-TXN-TYPE       VALUES 'D' 'W' 'T'.
           05  TXN-AMOUNT              PIC 9(7)V99.
           05  TXN-TARGET-ACCOUNT      PIC 9(8).

       FD  MASTER-FILE.
       01  MASTER-RECORD.
           05  MA-ACCOUNT-NUM          PIC 9(8).
           05  MA-CUSTOMER-NAME        PIC X(30).
           05  MA-ACCOUNT-TYPE         PIC X(1).
               88 IS-REGULAR           VALUE 'R'.
               88 IS-BUSINESS          VALUE 'B'.
               88 IS-VIP               VALUE 'V'.
           05  MA-ACCOUNT-STATUS       PIC X(1).
               88 ACCT-ACTIVE          VALUE 'A'.
               88 ACCT-BLOCKED         VALUE 'B'.
               88 ACCT-DORMANT         VALUE 'D'.
           05  MA-BALANCE              PIC S9(9)V99.
           05  MA-YEARS-ACTIVE         PIC 9(2).
           05  MA-DAILY-WD-AMT         PIC 9(7)V99.
           05  MA-DAILY-WD-COUNT       PIC 9(3).

       FD  REPORT-FILE.
       01  REPORT-RECORD               PIC X(132).

       FD  AUDIT-FILE.
       01  AUDIT-RECORD                PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-FLAGS.
           05  WS-EOF-FLG              PIC X VALUE 'N'.
               88 END-OF-FILE          VALUE 'Y'.

           05  WS-SOURCE-FOUND-FLG     PIC X VALUE 'N'.
               88 SOURCE-FOUND         VALUE 'Y'.

           05  WS-TARGET-FOUND-FLG     PIC X VALUE 'N'.
               88 TARGET-FOUND         VALUE 'Y'.

           05  WS-VALID-FLG            PIC X VALUE 'N'.
               88 TXN-VALID            VALUE 'Y'.
               88 TXN-INVALID          VALUE 'N'.

           05  WS-VIP-FLG              PIC X VALUE 'N'.
               88 VIP-CUSTOMER         VALUE 'Y'.

           05  WS-OVERDRAFT-FLG        PIC X VALUE 'N'.
               88 OVERDRAFT-HIT        VALUE 'Y'.

           05  WS-FEE-FLG              PIC X VALUE 'N'.
               88 FEE-APPLIED          VALUE 'Y'.

       01  WS-STATUS.
           05  WS-PROCESS-STATUS       PIC X(2) VALUE '00'.
               88 ST-OK                VALUE '00'.
               88 ST-ACCT-NOT-FOUND    VALUE '01'.
               88 ST-TARGET-NOT-FOUND  VALUE '02'.
               88 ST-INVALID-TYPE      VALUE '03'.
               88 ST-INVALID-AMOUNT    VALUE '04'.
               88 ST-ACCOUNT-BLOCKED   VALUE '05'.
               88 ST-INSUFF-FUNDS      VALUE '06'.
               88 ST-LIMIT-EXCEEDED    VALUE '07'.
               88 ST-SAME-ACCOUNT      VALUE '08'.

       01  WS-AMOUNTS.
           05  WS-SRC-OLD-BAL          PIC S9(9)V99 VALUE 0.
           05  WS-SRC-NEW-BAL          PIC S9(9)V99 VALUE 0.
           05  WS-TGT-OLD-BAL          PIC S9(9)V99 VALUE 0.
           05  WS-TGT-NEW-BAL          PIC S9(9)V99 VALUE 0.
           05  WS-FEE-AMOUNT           PIC 9(5)V99 VALUE 0.
           05  WS-STANDARD-FEE         PIC 9(3)V99 VALUE 2.50.
           05  WS-BUSINESS-FEE         PIC 9(3)V99 VALUE 1.00.
           05  WS-OVERDRAFT-PENALTY    PIC 9(5)V99 VALUE 50.00.
           05  WS-DAILY-WD-LIMIT       PIC 9(7)V99 VALUE 5000.00.

       01  WS-SUMMARY.
           05  WS-TXN-COUNT            PIC 9(7) VALUE 0.
           05  WS-VALID-COUNT          PIC 9(7) VALUE 0.
           05  WS-ERROR-COUNT          PIC 9(7) VALUE 0.
           05  WS-DEPOSIT-COUNT        PIC 9(7) VALUE 0.
           05  WS-WITHDRAW-COUNT       PIC 9(7) VALUE 0.
           05  WS-TRANSFER-COUNT       PIC 9(7) VALUE 0.
           05  WS-TOTAL-DEPOSITS       PIC 9(9)V99 VALUE 0.
           05  WS-TOTAL-WITHDRAWALS    PIC 9(9)V99 VALUE 0.
           05  WS-TOTAL-TRANSFERS      PIC 9(9)V99 VALUE 0.
           05  WS-TOTAL-FEES           PIC 9(9)V99 VALUE 0.

       01  WS-SOURCE-REC.
           05  WS-SRC-ACCOUNT-NUM      PIC 9(8).
           05  WS-SRC-CUSTOMER-NAME    PIC X(30).
           05  WS-SRC-ACCOUNT-TYPE     PIC X(1).
           05  WS-SRC-ACCOUNT-STATUS   PIC X(1).
           05  WS-SRC-BALANCE          PIC S9(9)V99.
           05  WS-SRC-YEARS-ACTIVE     PIC 9(2).
           05  WS-SRC-DAILY-WD-AMT     PIC 9(7)V99.
           05  WS-SRC-DAILY-WD-COUNT   PIC 9(3).

       01  WS-TARGET-REC.
           05  WS-TGT-ACCOUNT-NUM      PIC 9(8).
           05  WS-TGT-CUSTOMER-NAME    PIC X(30).
           05  WS-TGT-ACCOUNT-TYPE     PIC X(1).
           05  WS-TGT-ACCOUNT-STATUS   PIC X(1).
           05  WS-TGT-BALANCE          PIC S9(9)V99.
           05  WS-TGT-YEARS-ACTIVE     PIC 9(2).
           05  WS-TGT-DAILY-WD-AMT     PIC 9(7)V99.
           05  WS-TGT-DAILY-WD-COUNT   PIC 9(3).

       01  WS-REPORT-LINE.
           05  RL-ACCT                 PIC 9(8).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-TYPE                 PIC X(1).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-NAME                 PIC X(20).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-AMOUNT               PIC ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-OLD-BAL              PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-NEW-BAL              PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-FEE                  PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  RL-NOTES                PIC X(40).

       01  WS-AUDIT-LINE               PIC X(132).

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INITIALIZE
           PERFORM UNTIL END-OF-FILE
               PERFORM 2000-PROCESS-TXN
               PERFORM 2100-READ-NEXT-TXN
           END-PERFORM
           PERFORM 9000-TERMINATE
           GOBACK.

       1000-INITIALIZE.
           OPEN INPUT TXN-FILE
           OPEN I-O MASTER-FILE
           OPEN OUTPUT REPORT-FILE
           OPEN OUTPUT AUDIT-FILE
           PERFORM 2100-READ-NEXT-TXN.

       2100-READ-NEXT-TXN.
           READ TXN-FILE
               AT END
                   SET END-OF-FILE TO TRUE
           END-READ.

       2000-PROCESS-TXN.
           ADD 1 TO WS-TXN-COUNT
           PERFORM 2200-RESET-WORK-AREAS
           PERFORM 3000-READ-SOURCE-ACCOUNT
           IF TXN-TRANSFER AND ST-OK
               PERFORM 3100-READ-TARGET-ACCOUNT
           END-IF
           IF ST-OK
               PERFORM 4000-VALIDATE-TXN
           END-IF
           IF ST-OK
               PERFORM 5000-APPLY-TXN
               PERFORM 6000-CALCULATE-FEES
               PERFORM 7000-UPDATE-MASTER
               ADD 1 TO WS-VALID-COUNT
           ELSE
               ADD 1 TO WS-ERROR-COUNT
           END-IF
           PERFORM 8000-WRITE-DETAIL-REPORT
           PERFORM 8100-WRITE-AUDIT.

       2200-RESET-WORK-AREAS.
           MOVE 'N' TO WS-SOURCE-FOUND-FLG
           MOVE 'N' TO WS-TARGET-FOUND-FLG
           MOVE 'N' TO WS-VALID-FLG
           MOVE 'N' TO WS-VIP-FLG
           MOVE 'N' TO WS-OVERDRAFT-FLG
           MOVE 'N' TO WS-FEE-FLG
           MOVE '00' TO WS-PROCESS-STATUS
           MOVE 0 TO WS-FEE-AMOUNT
           MOVE 0 TO WS-SRC-OLD-BAL WS-SRC-NEW-BAL
           MOVE 0 TO WS-TGT-OLD-BAL WS-TGT-NEW-BAL
           MOVE SPACES TO WS-REPORT-LINE WS-AUDIT-LINE
           MOVE SPACES TO WS-SOURCE-REC WS-TARGET-REC.

       3000-READ-SOURCE-ACCOUNT.
           MOVE TXN-ACCOUNT-NUM TO MA-ACCOUNT-NUM
           READ MASTER-FILE
               INVALID KEY
                   SET ST-ACCT-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET SOURCE-FOUND TO TRUE
                   MOVE MASTER-RECORD TO WS-SOURCE-REC
                   MOVE MA-BALANCE TO WS-SRC-OLD-BAL
                   IF MA-ACCOUNT-TYPE = 'V'
                      OR (MA-ACCOUNT-TYPE = 'R' AND MA-YEARS-ACTIVE > 10)
                       SET VIP-CUSTOMER TO TRUE
                   END-IF
           END-READ.

       3100-READ-TARGET-ACCOUNT.
           IF TXN-ACCOUNT-NUM = TXN-TARGET-ACCOUNT
               SET ST-SAME-ACCOUNT TO TRUE
           ELSE
               MOVE TXN-TARGET-ACCOUNT TO MA-ACCOUNT-NUM
               READ MASTER-FILE
                   INVALID KEY
                       SET ST-TARGET-NOT-FOUND TO TRUE
                   NOT INVALID KEY
                       SET TARGET-FOUND TO TRUE
                       MOVE MASTER-RECORD TO WS-TARGET-REC
                       MOVE MA-BALANCE TO WS-TGT-OLD-BAL
               END-READ
           END-IF.

       4000-VALIDATE-TXN.
           IF NOT VALID-TXN-TYPE
               SET ST-INVALID-TYPE TO TRUE
           ELSE
               IF TXN-AMOUNT <= 0
                   SET ST-INVALID-AMOUNT TO TRUE
               ELSE
                   IF WS-SRC-ACCOUNT-STATUS = 'B'
                       SET ST-ACCOUNT-BLOCKED TO TRUE
                   END-IF
               END-IF
           END-IF

           IF ST-OK
               EVALUATE TRUE
                   WHEN TXN-WITHDRAW
                       IF TXN-AMOUNT > WS-SRC-BALANCE + 500.00
                           SET ST-INSUFF-FUNDS TO TRUE
                       END-IF
                       IF WS-SRC-DAILY-WD-AMT + TXN-AMOUNT
                          > WS-DAILY-WD-LIMIT
                          AND WS-SRC-ACCOUNT-TYPE NOT = 'V'
                           SET ST-LIMIT-EXCEEDED TO TRUE
                       END-IF
                   WHEN TXN-TRANSFER
                       IF WS-TGT-ACCOUNT-STATUS = 'B'
                           SET ST-ACCOUNT-BLOCKED TO TRUE
                       END-IF
                       IF TXN-AMOUNT > WS-SRC-BALANCE + 500.00
                           SET ST-INSUFF-FUNDS TO TRUE
                       END-IF
               END-EVALUATE
           END-IF.

       5000-APPLY-TXN.
           SET TXN-VALID TO TRUE
           EVALUATE TRUE
               WHEN TXN-DEPOSIT
                   MOVE WS-SRC-OLD-BAL TO WS-SRC-NEW-BAL
                   ADD TXN-AMOUNT TO WS-SRC-NEW-BAL
                   ADD 1 TO WS-DEPOSIT-COUNT
                   ADD TXN-AMOUNT TO WS-TOTAL-DEPOSITS

               WHEN TXN-WITHDRAW
                   MOVE WS-SRC-OLD-BAL TO WS-SRC-NEW-BAL
                   SUBTRACT TXN-AMOUNT FROM WS-SRC-NEW-BAL
                   ADD TXN-AMOUNT TO WS-SRC-DAILY-WD-AMT
                   ADD 1 TO WS-SRC-DAILY-WD-COUNT
                   ADD 1 TO WS-WITHDRAW-COUNT
                   ADD TXN-AMOUNT TO WS-TOTAL-WITHDRAWALS

               WHEN TXN-TRANSFER
                   MOVE WS-SRC-OLD-BAL TO WS-SRC-NEW-BAL
                   MOVE WS-TGT-OLD-BAL TO WS-TGT-NEW-BAL
                   SUBTRACT TXN-AMOUNT FROM WS-SRC-NEW-BAL
                   ADD TXN-AMOUNT TO WS-TGT-NEW-BAL
                   ADD 1 TO WS-TRANSFER-COUNT
                   ADD TXN-AMOUNT TO WS-TOTAL-TRANSFERS
           END-EVALUATE

           IF WS-SRC-NEW-BAL < 0
               SET OVERDRAFT-HIT TO TRUE
           END-IF.

       6000-CALCULATE-FEES.
           IF TXN-WITHDRAW OR TXN-TRANSFER
               IF NOT VIP-CUSTOMER
                   IF WS-SRC-ACCOUNT-TYPE = 'B'
                       ADD WS-BUSINESS-FEE TO WS-FEE-AMOUNT
                   ELSE
                       ADD WS-STANDARD-FEE TO WS-FEE-AMOUNT
                   END-IF
                   SET FEE-APPLIED TO TRUE
               END-IF
           END-IF

           IF OVERDRAFT-HIT
               ADD WS-OVERDRAFT-PENALTY TO WS-FEE-AMOUNT
               SET FEE-APPLIED TO TRUE
           END-IF

           IF WS-FEE-AMOUNT > 0
               SUBTRACT WS-FEE-AMOUNT FROM WS-SRC-NEW-BAL
               ADD WS-FEE-AMOUNT TO WS-TOTAL-FEES
           END-IF.

       7000-UPDATE-MASTER.
           MOVE WS-SRC-ACCOUNT-NUM TO MA-ACCOUNT-NUM
           READ MASTER-FILE
               INVALID KEY
                   SET ST-ACCT-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   MOVE WS-SRC-CUSTOMER-NAME TO MA-CUSTOMER-NAME
                   MOVE WS-SRC-ACCOUNT-TYPE TO MA-ACCOUNT-TYPE
                   MOVE WS-SRC-ACCOUNT-STATUS TO MA-ACCOUNT-STATUS
                   MOVE WS-SRC-NEW-BAL TO MA-BALANCE
                   MOVE WS-SRC-YEARS-ACTIVE TO MA-YEARS-ACTIVE
                   MOVE WS-SRC-DAILY-WD-AMT TO MA-DAILY-WD-AMT
                   MOVE WS-SRC-DAILY-WD-COUNT TO MA-DAILY-WD-COUNT
                   REWRITE MASTER-RECORD
                       INVALID KEY
                           SET ST-ACCT-NOT-FOUND TO TRUE
                   END-REWRITE
           END-READ

           IF TXN-TRANSFER AND ST-OK
               MOVE WS-TGT-ACCOUNT-NUM TO MA-ACCOUNT-NUM
               READ MASTER-FILE
                   INVALID KEY
                       SET ST-TARGET-NOT-FOUND TO TRUE
                   NOT INVALID KEY
                       MOVE WS-TGT-CUSTOMER-NAME TO MA-CUSTOMER-NAME
                       MOVE WS-TGT-ACCOUNT-TYPE TO MA-ACCOUNT-TYPE
                       MOVE WS-TGT-ACCOUNT-STATUS TO MA-ACCOUNT-STATUS
                       MOVE WS-TGT-NEW-BAL TO MA-BALANCE
                       MOVE WS-TGT-YEARS-ACTIVE TO MA-YEARS-ACTIVE
                       MOVE WS-TGT-DAILY-WD-AMT TO MA-DAILY-WD-AMT
                       MOVE WS-TGT-DAILY-WD-COUNT TO MA-DAILY-WD-COUNT
                       REWRITE MASTER-RECORD
                           INVALID KEY
                               SET ST-TARGET-NOT-FOUND TO TRUE
                       END-REWRITE
               END-READ
           END-IF.

       8000-WRITE-DETAIL-REPORT.
           MOVE TXN-ACCOUNT-NUM TO RL-ACCT
           MOVE TXN-TYPE TO RL-TYPE
           MOVE WS-SRC-CUSTOMER-NAME TO RL-NAME
           MOVE TXN-AMOUNT TO RL-AMOUNT
           MOVE WS-SRC-OLD-BAL TO RL-OLD-BAL
           MOVE WS-SRC-NEW-BAL TO RL-NEW-BAL
           MOVE WS-FEE-AMOUNT TO RL-FEE

           EVALUATE TRUE
               WHEN ST-OK AND TXN-TRANSFER
                   MOVE 'TRANSFER COMPLETED' TO RL-NOTES
               WHEN ST-OK AND OVERDRAFT-HIT AND FEE-APPLIED
                   MOVE 'OVERDRAFT + FEES APPLIED' TO RL-NOTES
               WHEN ST-OK AND FEE-APPLIED
                   MOVE 'TRANSACTION OK - FEES CHARGED' TO RL-NOTES
               WHEN ST-OK AND VIP-CUSTOMER
                   MOVE 'TRANSACTION OK - VIP NO FEE' TO RL-NOTES
               WHEN ST-ACCT-NOT-FOUND
                   MOVE 'SOURCE ACCOUNT NOT FOUND' TO RL-NOTES
               WHEN ST-TARGET-NOT-FOUND
                   MOVE 'TARGET ACCOUNT NOT FOUND' TO RL-NOTES
               WHEN ST-INVALID-TYPE
                   MOVE 'INVALID TRANSACTION TYPE' TO RL-NOTES
               WHEN ST-INVALID-AMOUNT
                   MOVE 'INVALID TRANSACTION AMOUNT' TO RL-NOTES
               WHEN ST-ACCOUNT-BLOCKED
                   MOVE 'ACCOUNT BLOCKED' TO RL-NOTES
               WHEN ST-INSUFF-FUNDS
                   MOVE 'INSUFFICIENT FUNDS' TO RL-NOTES
               WHEN ST-LIMIT-EXCEEDED
                   MOVE 'DAILY LIMIT EXCEEDED' TO RL-NOTES
               WHEN ST-SAME-ACCOUNT
                   MOVE 'SOURCE = TARGET' TO RL-NOTES
               WHEN OTHER
                   MOVE 'UNKNOWN STATUS' TO RL-NOTES
           END-EVALUATE

           WRITE REPORT-RECORD FROM WS-REPORT-LINE.

       8100-WRITE-AUDIT.
           STRING
               'ACCT=' TXN-ACCOUNT-NUM DELIMITED BY SIZE
               ' TYPE=' TXN-TYPE DELIMITED BY SIZE
               ' AMT=' TXN-AMOUNT DELIMITED BY SIZE
               ' STATUS=' WS-PROCESS-STATUS DELIMITED BY SIZE
               ' FEE=' WS-FEE-AMOUNT DELIMITED BY SIZE
               INTO WS-AUDIT-LINE
           END-STRING
           WRITE AUDIT-RECORD FROM WS-AUDIT-LINE.

       9000-TERMINATE.
           WRITE REPORT-RECORD FROM
             '================ END OF RUN SUMMARY ================'
           WRITE REPORT-RECORD FROM
             'TOTAL TXNS     : '
           WRITE REPORT-RECORD FROM
             'VALID TXNS     : '
           WRITE REPORT-RECORD FROM
             'ERROR TXNS     : '
           CLOSE TXN-FILE
           CLOSE MASTER-FILE
           CLOSE REPORT-FILE
           CLOSE AUDIT-FILE.