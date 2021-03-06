       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program1.
       AUTHOR.     Frederic Proulx.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENT-FILE-IN
               ASSIGN "C:\Users\Freddo\INVENT2B.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
                   
           SELECT INTENTORY-TRANSACTION-FILE
               ASSIGN "C:\Users\Freddo\TRANSFIL.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.

           SELECT INVENT-REPORT-OUT
               ASSIGN "C:\Users\Freddo\INVREPRT.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           
           SELECT RO-REPORT-OUT
               ASSIGN "C:\Users\Freddo\ROREPRT.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  INVENT-FILE-IN.
       01  INVENTORY-RECORD-IN.
           05  PART-NUMBER-IN      PIC 9(5).
           05  PART-NAME-IN        PIC X(20).
           05  QTY-ON-HAND-IN      PIC 9(3).
           05  QTY-RECEIVED-IN     PIC 9(3).
           05  AMT-SHIPPED-IN      PIC 9(3).
           05  UNIT-PRICE-IN       PIC 9(4)V99.
           05  RE-ORDER-POINT-IN   PIC 9(3).
           
       FD  INTENTORY-TRANSACTION-FILE.
       01  INVENTORY-TRANSACTION-IN
           05  PART-NUMBER-IN      PIC 9(5).
           05  TRANSACTION-TYPE-IN PIC 9(1).
           05  TRANSACTION-AMMOUNT-IN PIC 9(3)

       FD  INVENT-REPORT-OUT.
       01  INVENTORY-REPORT-OUT    PIC X(85).
       
       FD  RO-REPORT-OUT.
       01  RE-ORDER-REPORT-OUT     PIC x(85).
       
       

       WORKING-STORAGE SECTION.

      *    =================================================
      *    Each of the record structures used in the program
      *    is declared in working storage. As required by
      *    the process, the record structure is moved to the
      *    output line (INVENTORY-REPORT-OUT) adn written
      *    =================================================
      *
      *
       01  BLANK-LINE  PIC X(132)  VALUE SPACES.
       
       01  INVENTORY-DETAIL-LINE.
           05  FILLER               PIC X(1)    VALUE   SPACES.
           05  PART-NUMBER-OUT      PIC X(5).
           05  FILLER               PIC X(3)   VALUE  SPACES.
           05  PART-NAME-OUT        PIC X(20).
           05  FILLER               PIC X(2)   VALUE  SPACES.
           05  QUANTITY-ON-HAND-OUT PIC ZZ9.
           05  FILLER               PIC X(1)   VALUE  SPACES.
           05  AMOUNT-RECEIVED-OUT  PIC ZZ9.
           05  FILLER               PIC X(1)   VALUE  SPACES.
           05  AMT-SHIPPED-OUT      PIC ZZ9.
           05  FILLER               PIC X(2)   VALUE  SPACES.
           05  CURRENT-OUT	        PIC ZZZ9.
           05  FILLER               PIC X(1)   VALUE  SPACES.
           05  UNIT-PRICE-OUT       PIC ZZ9.99.
           05  FILLER               PIC X(2)   VALUE  SPACES.
           05  UNIT-VALUE-OUT       PIC $$$,$$9.99.

       01  INVENTORY-COLUMN-HEADER.
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(7)  VALUE   "PART NO".
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(9)  VALUE   "PART NAME".
           05  FILLER      PIC X(13).
           05  FILLER      PIC X(2)  VALUE   "OH".
           05  FILLER      PIC X(2).
           05  FILLER      PIC X(3)  VALUE   "REC".
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(4)   VALUE   "SHIP".
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(4)   VALUE   "CURR".
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(5)   VALUE   "PRICE".
           05  FILLER      PIC X(6).
           05  FILLER      PIC X(5)  VALUE     "VALUE".
           
       01  RE-ORDER-DETAIL-LINE.
           05  FILLER               PIC X(1)    VALUE   SPACES.
           05  PART-NUMBER-OUT-RO      PIC X(5).
           05  FILLER               PIC X(3)   VALUE  SPACES.
           05  PART-NAME-OUT-RO        PIC X(20).
           05  FILLER              PIC X(3)    VALUE SPACES.
           05  PART-CURRENT-STOCK-RO   PIC ZZZ9.
           
       01 RE-ORDER-COLUMN-HEADER.
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(7)  VALUE   "PART NO".
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(9)  VALUE   "PART NAME".
           05  FILLER      PIC X(14).
           05  FILLER      PIC X(13)  VALUE   "CURRENT STOCK".
       
       01 DATE-WS.
            05 YR pic 99.
            05 MNTH pic 99.

       01  FLAGS-AND-COUNTERS.
           05  EOF-FLAG            PIC X(3)    VALUE "NO".
           
       01  INVENTORY-HEADER-DATE.
           05  FILLER      PIC X(9)    VALUE SPACES.
           05  FILLER      PIC X(20)   VALUE "INVENTORY REPORT for".
           05  FILLER      PIC X(3)    VALUE SPACES.
           05  MONTH       PIC 99   VALUE ZERO.
           05  FILLER      PIC X(1)    VALUE SPACES.
           05  YEAR        PIC 99   VALUE ZERO.
           
       01  RE-ORDER-HEADER.
           05  FILLER      PIC X(9)    VALUE SPACES.
           05  FILLER      PIC X(20)   VALUE "RE ORDER REPORT for".

       01  INVENTORY-SUMMARY.
           05  FILLER      PIC X(2)     VALUE SPACES.
           05  FILLER      PIC X(11)    VALUE "TOTAL VALUE".
           05  FILLER      PIC X(2)     VALUE SPACES.
           05  INV-TOTAL-VALUE PIC $$$$,$$9.99 VALUE ZERO.

       01  AUDIT-TRAIL1.
           05  FILLER          PIC X(2)    VALUE SPACES.
           05  FILLER          PIC X(15)   VALUES "RECORDS READ  ".
           05  CTR-RECORDS-IN  PIC ZZZ9    VALUE ZERO.
           
       01  AUDIT-TRAIL2.
           05  FILLER          PIC X(2)    VALUE SPACES.
           05  FILLER          PIC X(15)   VALUE "RECORDS WRITTEN  ".
           05  CTR-RECORDS-OUT PIC ZZZ9    VALUE ZERO.


       01  ACCUMULATORS.
           05  CURRENT-WS      PIC 9(3)  VALUE ZERO.
           05  UNIT-VALUE-WS   PIC 9(6)  VALUE ZERO.
           05  CTR-RECORDS-IN-WS   PIC 9(4) VALUE ZERO.
           05  CTR-RECORDS-OUT-WS   PIC 9(4) VALUE ZERO.
           05 INV-TOTAL-VALUE-WS   PIC 9(7)V99 VALUE ZERO.

       PROCEDURE DIVISION.

       100-PRODUCE-INVENTORY-REPORT.

      *    ==================================================
      *    This is the high level program control that is
      *    shown on the hierarcchy chart.
      *    ==================================================

           PERFORM  200-INITIATE-INVENTORY-REPORT.
           PERFORM  200-PRODUCE-INVENTORY-REPORT
                   UNTIL EOF-FLAG = "YES".
           PERFORM  200-TERMINATE-INVENTORY-REPORT.

           STOP RUN.


       200-INITIATE-INVENTORY-REPORT.
      *    ==================================================
      *    You will note that the initiation module includes
      *    the priming read to start the process. If there
      *    are no records on the file, then the mainline
      *    process is bypassed.
      *    ==================================================

           PERFORM  700-OPEN-INVENTORY-FILES.
           PERFORM  700-INITIALIZE-COUNTERS.
           PERFORM  700-READ-INVENTORY-RECORD.
           PERFORM  700-PRINT-FILE-HEADER.
           PERFORM  700-PRINT-COLUMN-HEADER.

       200-PRODUCE-INVENTORY-REPORT.
      *    ==================================================
      *    This is the mainline process which is repeated for
      *    each record on the file. Since the priming read
      *    picked up the first record, the read in the
      *    mainline is the last function to be executed.
      *    This provides easy testing for the repetition of
      *    the mainline in the upper higher level control
      *    module
      *    ==================================================

           PERFORM  700-CALCULATE-INVENTORY-VALUE.
           PERFORM  700-CHECK-RE-ORDER.
           PERFORM  700-PRINT-INVENTORY-DETAIL.
           PERFORM  700-CALCULATE-GRAND-TOTALS.
           PERFORM  700-READ-INVENTORY-RECORD.

       200-TERMINATE-INVENTORY-REPORT.

      *    ==========================================================
      *    The termination module carries out those function to be
      *    performed once all records have been processed. Control of
      *    the execution of this module is at the high level module.
      *    ==========================================================

           PERFORM 700-PRINT-TOTAL-VALUES.
           PERFORM 700-WRITE-AUDIT-TRAIL.
           PERFORM 700-CLOSE-INVENTORY-FILES.

      *    =======================================================
      *    All of the level 700 modules are those that actually do
      *    work. You will note that modules prior to these strictly
      *    control the execution of lower level modules.
      *    This aspect of cohesion suggests that a module either
      *    controls lower level modules or does work - not both.
      *    you will also note that this code is dtructured in a
      *    layered structure in synch with the hierarchy chart.
      *    This approach facilitates maintenance as modules can
      *    be easily located if changes are required.
      *    =======================================================


       700-OPEN-INVENTORY-FILES.
           OPEN INPUT  INVENT-FILE-IN
                OUTPUT INVENT-REPORT-OUT
                OUTPUT RO-REPORT-OUT.

       700-INITIALIZE-COUNTERS.
            INITIALIZE  CTR-RECORDS-IN-WS
                        CTR-RECORDS-OUT-WS.

       700-READ-INVENTORY-RECORD.
           READ INVENT-FILE-IN
               AT END MOVE "YES" TO EOF-FLAG
                   NOT AT END ADD 1 TO CTR-RECORDS-IN-WS.
                   
       700-PRINT-FILE-HEADER.
           ACCEPT DATE-WS FROM DATE.
           MOVE YR TO YEAR.
           MOVE MNTH TO MONTH.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE RE-ORDER-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT
               FROM INVENTORY-HEADER-DATE.
           WRITE RE-ORDER-REPORT-OUT
               FROM RE-ORDER-HEADER.

       700-PRINT-COLUMN-HEADER.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT
                   FROM INVENTORY-COLUMN-HEADER.
           WRITE RE-ORDER-REPORT-OUT FROM BLANK-LINE.
           WRITE RE-ORDER-REPORT-OUT FROM BLANK-LINE.
           WRITE RE-ORDER-REPORT-OUT FROM BLANK-LINE.
           WRITE RE-ORDER-REPORT-OUT
                   FROM RE-ORDER-COLUMN-HEADER.
                   
       700-PRINT-RE-ORDER-REPORT.
           MOVE    PART-NUMBER-IN
               TO  PART-NUMBER-OUT-RO.
           MOVE  PART-NAME-IN
                   TO  PART-NAME-OUT-RO.
           MOVE CURRENT-WS
               TO PART-CURRENT-STOCK-RO.
           WRITE RE-ORDER-REPORT-OUT
                  FROM  RE-ORDER-DETAIL-LINE.

       700-PRINT-INVENTORY-DETAIL.
           MOVE    PART-NUMBER-IN
               TO  PART-NUMBER-OUT.
           MOVE  PART-NAME-IN
                   TO  PART-NAME-OUT.
           MOVE  QTY-ON-HAND-IN
                   TO  QUANTITY-ON-HAND-OUT.
           MOVE  QTY-RECEIVED-IN
                   TO  AMOUNT-RECEIVED-OUT.
           MOVE  AMT-SHIPPED-IN
                   TO  AMT-SHIPPED-OUT.
           MOVE  UNIT-PRICE-IN
                   TO UNIT-PRICE-OUT.
           MOVE  CURRENT-WS
                   TO CURRENT-OUT.
           MOVE  UNIT-VALUE-WS
                   TO UNIT-VALUE-OUT.
           WRITE INVENTORY-REPORT-OUT
                  FROM  INVENTORY-DETAIL-LINE.
           ADD 1 TO CTR-RECORDS-OUT-WS.

       700-CALCULATE-INVENTORY-VALUE.
           ADD QTY-ON-HAND-IN
               QTY-RECEIVED-IN
                   GIVING CURRENT-WS.
           SUBTRACT AMT-SHIPPED-IN
               FROM  CURRENT-WS.
           MULTIPLY CURRENT-WS
               BY UNIT-PRICE-IN
                   GIVING  UNIT-VALUE-WS.
                   
       700-CHECK-RE-ORDER.
           IF CURRENT-WS <= RE-ORDER-POINT-IN THEN
               PERFORM 700-PRINT-RE-ORDER-REPORT.

       700-CALCULATE-GRAND-TOTALS.
           ADD UNIT-VALUE-WS TO INV-TOTAL-VALUE-WS.

       700-PRINT-TOTAL-VALUES.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           MOVE    INV-TOTAL-VALUE-WS TO INV-TOTAL-VALUE.
           MOVE   INVENTORY-SUMMARY  TO INVENTORY-REPORT-OUT.
           WRITE  INVENTORY-REPORT-OUT.

       700-WRITE-AUDIT-TRAIL.
           MOVE CTR-RECORDS-OUT-WS TO CTR-RECORDS-OUT.
           MOVE CTR-RECORDS-IN-WS TO CTR-RECORDS-IN.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE INVENTORY-REPORT-OUT FROM BLANK-LINE.
           WRITE  INVENTORY-REPORT-OUT
               FROM   AUDIT-TRAIL1.
           WRITE  INVENTORY-REPORT-OUT
               FROM   AUDIT-TRAIL2.

       700-CLOSE-INVENTORY-FILES.
           CLOSE INVENT-FILE-IN
                 INVENT-REPORT-OUT
                 RO-REPORT-OUT.
                 
