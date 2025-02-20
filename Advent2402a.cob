       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENT2402A.
      *-----------------------------------------------------------------
      * THIS PROGRAM CHECKS FOR EACH LINE IF THE NUMBERS ARE "SAFE"
      * WHICH IS DEFINED AS ALL NUMBERS ASCENDING OR DESCENDING
      * WITHIN THE LINE AND GIVE A TOTAL OF LINES "SAFE"
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL ADVENT-FILE
               ASSIGN TO "Advent2402.dat"
               FILE STATUS IS WS-FILE-STATUS-A
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ADVENT-FILE
           LABEL RECORDS ARE STANDARD.
       01  LIST-RECORD PIC X(23).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS-A PIC X(2).
       01  WS-NUMBERS.
           05 NUM1          PIC 9(2).    
           05 NUM2          PIC 9(2).
           05 NUM3          PIC 9(2).
           05 NUM4          PIC 9(2).
           05 NUM5          PIC 9(2).
           05 NUM6          PIC 9(2).
           05 NUM7          PIC 9(2).
           05 NUM8          PIC 9(2).
       01  TOTAL-COUNT      PIC 9(10).

       PROCEDURE DIVISION.

       PROGRAM-BEGIN.
           OPEN INPUT ADVENT-FILE.
           PERFORM READ-INPUT-FILE UNTIL WS-FILE-STATUS-A = '10'.
           CLOSE ADVENT-FILE.
           DISPLAY TOTAL-COUNT.

       PROGRAM-END.
           STOP RUN.

       READ-INPUT-FILE.
           READ ADVENT-FILE NEXT RECORD.
           UNSTRING LIST-RECORD DELIMITED BY SPACE
           INTO NUM1, NUM2, NUM3, NUM4, NUM5, NUM6, NUM7, NUM8
           END-UNSTRING.

           IF WS-FILE-STATUS-A NOT = '10' AND
           NUM6 = '00' AND NUM7 = '00' AND NUM8 = '00' THEN
           IF (NUM1 < NUM2 AND NUM1 + 3 >= NUM2
           AND NUM2 < NUM3 AND NUM2 + 3 >= NUM3
           AND NUM3 < NUM4 AND NUM3 + 3 >= NUM4
           AND NUM4 < NUM5 AND NUM4 + 3 >= NUM5) OR
              (NUM5 < NUM4 AND NUM5 + 3 >= NUM4
           AND NUM4 < NUM3 AND NUM4 + 3 >= NUM3
           AND NUM3 < NUM2 AND NUM3 + 3 >= NUM2
           AND NUM2 < NUM1 AND NUM2 + 3 >= NUM1) THEN
               COMPUTE TOTAL-COUNT = TOTAL-COUNT + 1.

           IF WS-FILE-STATUS-A NOT = '10' AND
           NUM6 NOT = '00' AND NUM7 = '00' AND NUM8 = '00' THEN
           IF (NUM1 < NUM2 AND NUM1 + 3 >= NUM2
           AND NUM2 < NUM3 AND NUM2 + 3 >= NUM3
           AND NUM3 < NUM4 AND NUM3 + 3 >= NUM4
           AND NUM4 < NUM5 AND NUM4 + 3 >= NUM5
           AND NUM5 < NUM6 AND NUM5 + 3 >= NUM6) OR
              (NUM6 < NUM5 AND NUM6 + 3 >= NUM5
           AND NUM5 < NUM4 AND NUM5 + 3 >= NUM4
           AND NUM4 < NUM3 AND NUM4 + 3 >= NUM3
           AND NUM3 < NUM2 AND NUM3 + 3 >= NUM2
           AND NUM2 < NUM1 AND NUM2 + 3 >= NUM1) THEN
               COMPUTE TOTAL-COUNT = TOTAL-COUNT + 1.
           
           IF WS-FILE-STATUS-A NOT = '10' AND
           NUM6 NOT = '00' AND NUM7 NOT = '00' AND NUM8 = '00' THEN
           IF (NUM1 < NUM2 AND NUM1 + 3 >= NUM2
           AND NUM2 < NUM3 AND NUM2 + 3 >= NUM3
           AND NUM3 < NUM4 AND NUM3 + 3 >= NUM4
           AND NUM4 < NUM5 AND NUM4 + 3 >= NUM5
           AND NUM5 < NUM6 AND NUM5 + 3 >= NUM6
           AND NUM6 < NUM7 AND NUM6 + 3 >= NUM7) OR
              (NUM7 < NUM6 AND NUM7 + 3 >= NUM6 
           AND NUM6 < NUM5 AND NUM6 + 3 >= NUM5
           AND NUM5 < NUM4 AND NUM5 + 3 >= NUM4
           AND NUM4 < NUM3 AND NUM4 + 3 >= NUM3
           AND NUM3 < NUM2 AND NUM3 + 3 >= NUM2
           AND NUM2 < NUM1 AND NUM2 + 3 >= NUM1) THEN
               COMPUTE TOTAL-COUNT = TOTAL-COUNT + 1.
           
           IF WS-FILE-STATUS-A NOT = '10' AND
           NUM6 NOT = '00' AND NUM7 NOT = '00' AND NUM8 NOT = '00' 
           THEN
           IF (NUM1 < NUM2 AND NUM1 + 3 >= NUM2
           AND NUM2 < NUM3 AND NUM2 + 3 >= NUM3
           AND NUM3 < NUM4 AND NUM3 + 3 >= NUM4
           AND NUM4 < NUM5 AND NUM4 + 3 >= NUM5
           AND NUM5 < NUM6 AND NUM5 + 3 >= NUM6
           AND NUM6 < NUM7 AND NUM6 + 3 >= NUM7
           AND NUM7 < NUM8 AND NUM7 + 3 >= NUM8) OR
              (NUM8 < NUM7 AND NUM8 + 3 >= NUM7
           AND NUM7 < NUM6 AND NUM7 + 3 >= NUM6
           AND NUM6 < NUM5 AND NUM6 + 3 >= NUM5 
           AND NUM5 < NUM4 AND NUM5 + 3 >= NUM4
           AND NUM4 < NUM3 AND NUM4 + 3 >= NUM3
           AND NUM3 < NUM2 AND NUM3 + 3 >= NUM2
           AND NUM2 < NUM1 AND NUM2 + 3 >= NUM1) THEN
               COMPUTE TOTAL-COUNT = TOTAL-COUNT + 1.
