      ******************************************************************
      * Author:    Walter Jacob
      * Date:      04/10/2020
      * Purpose:   Longest Increasing Subsequence Demo
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LongestSubseq.
       DATA DIVISION.
           FILE SECTION.
           WORKING-STORAGE SECTION.
           01 WS-ARRAY-LENGTH PIC 99 VALUE 17.
           01 WS-CURRENT-DATE-DATA.
	              05 WS-CURRENT-DATE.
	                  10 WS-CURRENT-YEAR	 PIC 9(4).
	                  10 WS-CURRENT-MONTH PIC 9(2).
	                  10 WS-CURRENT-DAY PIC 9(2).
	              05 WS-CURRENT-TIME.
	       	          10 WS-CURRENT-HOURS PIC 9(2).
	       	          10 WS-CURRENT-MINUTE PIC 9(2).
	       	          10 WS-CURRENT-SECOND PIC 9(2).
	       	          10 WS-CURRENT-MILLISECONDS PIC 9(2).
	              05 WS-DIFF-FROM-GMT	 PIC S9(4).
           01 WS-I PIC 99.
           01 WS-J PIC 99.
           01 WS-LIS PIC 9.
           01 WS-ARRAY.
               05 WS-VALUE OCCURS 1 TO 50 TIMES
               DEPENDING ON WS-ARRAY-LENGTH PIC 9.
           01 WS-LENGTH-ARRAY.
               05 WS-LENGTH-VALUE OCCURS 1 TO 50 TIMES
               DEPENDING ON WS-ARRAY-LENGTH PIC 9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           100-BEGIN.
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
               COMPUTE WS-I = (FUNCTION RANDOM(WS-CURRENT-MILLISECONDS)) * 9.
               PERFORM 200-FILL-ARRAY
                   VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ARRAY-LENGTH.
               PERFORM 300-LIS.
           DISPLAY "N:  " WS-ARRAY-LENGTH.
           DISPLAY WS-ARRAY.
           DISPLAY WS-LIS.

           STOP RUN.

           200-FILL-ARRAY.
               COMPUTE WS-VALUE (WS-I) = (FUNCTION RANDOM()) * 9.

           210-FILL-ARRAY-ZERO.
               MOVE 0 TO WS-LENGTH-VALUE (WS-I).

           300-LIS.
               PERFORM 210-FILL-ARRAY-ZERO
                   VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-ARRAY-LENGTH.
               MOVE 1 TO WS-LENGTH-VALUE(1).
               PERFORM VARYING WS-I FROM 2 BY 1
                   UNTIL WS-I > WS-ARRAY-LENGTH
                   PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > WS-I
                       IF WS-VALUE(WS-J) < WS-VALUE(WS-I)
                       AND WS-LENGTH-VALUE(WS-J) > WS-LENGTH-VALUE(WS-I)
                       THEN
                           MOVE WS-LENGTH-VALUE(WS-J)
                               TO WS-LENGTH-VALUE(WS-I)
                       END-IF
                   END-PERFORM
                   COMPUTE
                       WS-LENGTH-VALUE(WS-I) = WS-LENGTH-VALUE(WS-I) + 1
               END-PERFORM.
               MOVE 0 TO WS-LIS.
               PERFORM 310-MAX-LIS
                   VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > WS-ARRAY-LENGTH.

           310-MAX-LIS.
               IF WS-LIS < WS-LENGTH-VALUE(WS-I) THEN
                   MOVE WS-LENGTH-VALUE(WS-I) TO WS-LIS
               END-IF.

       END PROGRAM LongestSubseq.
