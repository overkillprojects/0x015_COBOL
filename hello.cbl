      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-A PIC 9(9) VALUE 23.
           01 WS-B PIC 9(9) VALUE 137.
           01 WS-C PIC 9(9).

           01 WS-IDX PIC 9(9).

           01 WS-VAL PIC 9(9).
           01 WS-DIV PIC 9(9).
           01 WS-REM PIC 9(9).
           01 WS-MULT PIC 9(18).
       PROCEDURE DIVISION.
           MAIN-PROCEDURE.
               MOVE 23 TO WS-C

               ADD WS-A TO WS-B GIVING WS-VAL
               DIVIDE WS-VAL BY WS-C GIVING WS-DIV REMAINDER WS-REM

               DISPLAY "VAL:   " WS-VAL
               DISPLAY "DIV:   " WS-DIV
               DISPLAY "REM:   " WS-REM

               PERFORM 100-MULTIPLY-VAL-REM

               DISPLAY "VAL times REM is " WS-MULT.

               PERFORM 200-FACTORIAL
                   VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-C.

               DISPLAY WS-C "! is " WS-MULT.

               STOP RUN.

           100-MULTIPLY-VAL-REM.
               MULTIPLY WS-VAL BY WS-REM GIVING WS-MULT.

           200-FACTORIAL.
               IF WS-IDX IS EQUAL TO 1 THEN
                   MOVE 1 TO WS-MULT
               ELSE
                   COMPUTE WS-MULT = WS-MULT * WS-IDX
               END-IF.


       END PROGRAM HELLO.
