       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program2.
       AUTHOR.     Frederic Proulx, Luke Bailey, Kyle Gervais.
       
       DATA DIVISION.
          LINKAGE SECTION.
          01 QTY-ON-HAND-IN      PIC 9(3).
          01 QTY-RECEIVED-IN     PIC 9(3).
          01 CURRENT-WS          PIC 9(3).
          01 AMT-SHIPPED-IN      PIC 9(3).
          01 UNIT-PRICE-IN       PIC 9(4)V99.
          01 UNIT-VALUE-WS        PIC 9(6).
          
       
       PROCEDURE DIVISION USING QTY-ON-HAND-IN, QTY-RECEIVED-IN,
           CURRENT-WS, AMT-SHIPPED-IN, UNIT-PRICE-IN, UNIT-VALUE-WS.
           
       PERFORM 700-CALCULATE-INVENTORY-VALUE.
       EXIT PROGRAM.
       
       700-CALCULATE-INVENTORY-VALUE.
           ADD QTY-ON-HAND-IN
               QTY-RECEIVED-IN
                   GIVING CURRENT-WS.
           SUBTRACT AMT-SHIPPED-IN
               FROM  CURRENT-WS.
           MULTIPLY CURRENT-WS
               BY UNIT-PRICE-IN
                   GIVING  UNIT-VALUE-WS.
                   
          