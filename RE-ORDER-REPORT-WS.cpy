       01  RE-ORDER-DETAIL-LINE.
           05  FILLER                  PIC X(1)    VALUE   SPACES.
           05  PART-NUMBER-OUT-RO      PIC X(5).
           05  FILLER                  PIC X(3)    VALUE  SPACES.
           05  PART-NAME-OUT-RO        PIC X(20).
           05  FILLER                  PIC X(3)    VALUE SPACES.
           05  PART-CURRENT-STOCK-RO   PIC ZZZ9.
       
       01 RE-ORDER-COLUMN-HEADER.
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(7)    VALUE   "PART NO".
           05  FILLER      PIC X(1).
           05  FILLER      PIC X(9)    VALUE   "PART NAME".
           05  FILLER      PIC X(14).
           05  FILLER      PIC X(13)   VALUE   "CURRENT STOCK".
           
       01  RE-ORDER-HEADER.
           05  FILLER      PIC X(9)    VALUE SPACES.
           05  FILLER      PIC X(20)   VALUE "RE ORDER REPORT".
       
