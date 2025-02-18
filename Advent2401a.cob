       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENT2401A.
      *-----------------------------------------------------------------
      * THIS PROGRAM TAKES TWO LISTS OF NUMBERS AND OUTPUTS THE SUM OF
      * THE DISTANCES BETWEEN THE ORDERED NUMBERS ON EACH SIDE OF THE 
      * LIST.
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL ADVENT-FILE
               ASSIGN TO "Advent2401.dat"
               FILE STATUS IS WS-FILE-STATUS-A
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL LEFT-FILE
               ASSIGN TO "Advent2401Left.dat"
               FILE STATUS IS WS-FILE-STATUS-L
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OPTIONAL RIGHT-FILE
               ASSIGN TO "Advent2401Right.dat"
               FILE STATUS IS WS-FILE-STATUS-R
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT WORK-FILE ASSIGN TO SORT-WORK.
       
       DATA DIVISION.
       FILE SECTION.
       FD  ADVENT-FILE
           LABEL RECORDS ARE STANDARD.
       01  LIST-RECORD.
           05 LIST-LEFT  PIC X(5).
           05 LIST-SPACE PIC X(3).
           05 LIST-RIGHT PIC X(5).
       SD  WORK-FILE
           LABEL RECORDS ARE STANDARD.
       01  LIST-RECORD-W.
           05 LIST-LEFT-W  PIC X(5).
           05 LIST-SPACE-W PIC X(3).
           05 LIST-RIGHT-W PIC X(5).
       FD  LEFT-FILE
           LABEL RECORDS ARE STANDARD.
       01  LIST-RECORD-L.
           05 LIST-LEFT-L  PIC X(5).
           05 LIST-SPACE-L PIC X(3).
           05 LIST-RIGHT-L PIC X(5).
       FD  RIGHT-FILE
           LABEL RECORDS ARE STANDARD.
       01  LIST-RECORD-R.
           05 LIST-LEFT-R  PIC X(5).
           05 LIST-SPACE-R PIC X(3).
           05 LIST-RIGHT-R PIC X(5).

       WORKING-STORAGE SECTION.
       01  WS-LEFT-LIST     PIC X(5).
       01  WS-RIGHT-LIST    PIC X(5).
       01  AMOUNT-LEFT      PIC 9(5).
       01  AMOUNT-RIGHT     PIC 9(5).
       01  AMOUNT-TOTAL     PIC 9(15).
       01  WS-RIGHT-LIST    PIC X(5).
       01  WS-FILE-STATUS-A PIC X(2).
       01  WS-FILE-STATUS-L PIC X(2).
       01  WS-FILE-STATUS-R PIC X(2).
       01  WS-FILE-STATUS-W PIC X(2).  
       01  IDX01            PIC 9(4).  


       PROCEDURE DIVISION.

       PROGRAM-BEGIN.
           SORT WORK-FILE
           ON ASCENDING KEY LIST-LEFT-W
           WITH DUPLICATES IN ORDER
           USING ADVENT-FILE GIVING LEFT-FILE.
           SORT WORK-FILE
           ON ASCENDING KEY LIST-RIGHT-W
           WITH DUPLICATES IN ORDER
           USING ADVENT-FILE GIVING RIGHT-FILE.
           CLOSE LEFT-FILE.
           CLOSE RIGHT-FILE.
           OPEN INPUT LEFT-FILE.
           OPEN INPUT RIGHT-FILE.
           PERFORM VARYING IDX01 FROM 1 BY 1 UNTIL IDX01 = 1001
               READ LEFT-FILE NEXT RECORD
               READ RIGHT-FILE NEXT RECORD
               MOVE LIST-LEFT-L TO AMOUNT-LEFT
               MOVE LIST-RIGHT-R TO AMOUNT-RIGHT
               COMPUTE AMOUNT-TOTAL = AMOUNT-TOTAL +
               FUNCTION ABS(AMOUNT-LEFT - AMOUNT-RIGHT)
           END-PERFORM.
           CLOSE LEFT-FILE.
           CLOSE RIGHT-FILE.
           DISPLAY AMOUNT-TOTAL.

       PROGRAM-END.
           STOP RUN.
