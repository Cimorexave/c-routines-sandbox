       IDENTIFICATION DIVISION.
       PROGRAM-ID. AWFUNCT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Idx                PIC 9(02) COMP-3 VALUE 1.
       01 Idz                PIC 9(02) COMP-3 VALUE 1.
       01 Ws-Return-Code     PIC S9(04) COMP  VALUE 0.
       01 Ws-Module-Name     PIC X(8).

       *> BitAnd - Variable
       01 WS-BitAnd-Len          PIC 9(04) COMP.
       01 WS-BitAnd-Result       PIC X(04).
       01 WS-BitAnd-Value1       PIC X(04).
       01 WS-BitAnd-Value2       PIC X(04).
       01 WS-BitAnd-Expected     PIC X(04).

       *> BitGet - Variable
       01 WS-BitGet-Len          PIC 9(04) COMP.
       01 WS-BitGet-Position     PIC 9(02) COMP.
       01 WS-BitGet-Array        PIC X(04).
       01 WS-BitGet-Result       PIC X VALUE SPACES.
       01 WS-BitGet-Expected     PIC X.

       *> BitInv - Variable
       01 WS-BitInv-Len          PIC 9(04) COMP.
       01 WS-BitInv-Array        PIC X(04).
       01 WS-BitInv-Position     PIC 9(02) COMP.
       01 WS-BitInv-Expected     PIC X(04).

       *> BitIs - Variable
       01 WS-BitIs-Len           PIC 9(04) COMP.
       01 WS-BitIs-Array         PIC X(04).
       01 WS-BitIs-Position      PIC 9(02) COMP.
       01 WS-BitIs-Result        PIC X VALUE SPACES.
       01 WS-BitIs-Expected      PIC X.

       *> BitOr - Variable
       01 WS-BitOr-Len           PIC 9(04) COMP.
       01 WS-BitOr-Result        PIC X(04).
       01 WS-BitOr-Value1        PIC X(04).
       01 WS-BitOr-Value2        PIC X(04).
       01 WS-BitOr-Expected      PIC X(04).

       *> BitSet - Variable
       01 WS-BitSet-Len          PIC 9(04) COMP.
       01 WS-BitSet-Array        PIC X(04).
       01 WS-BitSet-Position     PIC 9(02) COMP.
       01 WS-BitSet-Value        PIC X.
       01 WS-BitSet-Expected     PIC X(04).

       *> BitSlr - Variable
       01 BitSlr-Tab.
         03 BitSlr-TestCases     OCCURS 4 TIMES.
           05 BitSlr-Array       PIC X(04).
           05 BitSlr-BitCount    PIC 9(02) COMP.
           05 BitSlr-Expected    PIC X(04).
       01 WS-BitSlr-Len          PIC 9(04) COMP.
       01 WS-BitSlr-Array        PIC X(04).
       01 WS-BitSlr-BitCount     PIC 9(02) COMP.
       01 WS-BitSlr-Expected     PIC X(04).

       *> BitSll - Variable
       01 WS-BitSll-Len          PIC 9(04) COMP.
       01 WS-BitSll-Array        PIC X(04).
       01 WS-BitSll-BitCount     PIC 9(02) COMP.
       01 WS-BitSll-Expected     PIC X(04).

       *> DOFY - Variable
       01 DOFY-Date-Str      PIC X(10).
       01 DOFY-Tab.
         03 DOFY-TestCases       OCCURS 23 TIMES.
           05 DOFY-Year          PIC 9(04) COMP.
           05 DOFY-Day-of-Year   PIC 9(03) COMP.
           05 DOFY-Expected-Date PIC X(10).
       01 WS-DOFY-Year           PIC 9(04) COMP.
       01 WS-DOFY-Day-of-Year    PIC 9(03) COMP.

       LINKAGE SECTION.
       01 pParm.
        05 pParm-Len    PIC S9(4) COMP.
        05 pParm-Value  PIC X(100).

       PROCEDURE DIVISION USING pParm.
           MOVE 0 TO RETURN-CODE
           EVALUATE TRUE
             WHEN pParm-Value(1:1) = '*'
               PERFORM Test_BitAnd
               PERFORM Test_BitGet
               PERFORM Test_BitInv
               PERFORM Test_BitIs
               PERFORM Test_BitOr
               PERFORM Test_BitSet
             WHEN pParm-Value(1:6) = 'BITAND'
               PERFORM Test_BitAnd
             WHEN pParm-Value(1:6) = 'BITGET'
               PERFORM Test_BitGet
             WHEN pParm-Value(1:6) = 'BITINV'
               PERFORM Test_BitInv
             WHEN pParm-Value(1:5) = 'BITIS'
               PERFORM Test_BitIs
             WHEN pParm-Value(1:5) = 'BITOR'
               PERFORM Test_BitOr
             WHEN pParm-Value(1:6) = 'BITSET'
               PERFORM Test_BitSet
             WHEN pParm-Value(1:6) = 'BITSLR'
               PERFORM Test_BitSlr
             WHEN pParm-Value(1:6) = 'BITSLL'
               PERFORM Test_BitSll
             WHEN pParm-Value(1:4) = 'DOFY'
               PERFORM Test_DOFY
             WHEN OTHER
               DISPLAY "AWFUNCT: Test fuer '" pParm-Value "'"
                       " nicht unterstuetzt."
               MOVE 8 TO RETURN-CODE
           END-EVALUATE

           DISPLAY "AWFUNCT: RC " RETURN-CODE
           GOBACK.

      ******************************************************************
       Test_BitAnd SECTION.
      ******************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitAnd - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
             PERFORM SetBitAndTestValues
             PERFORM RunBitAndTest
             PERFORM DisplayBitAndResults
             PERFORM VerifyBitAndResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitAnd - End"
           .

       SetBitAndTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitAnd-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                             WS-BitAnd-Value1
               MOVE FUNCTION BIT-TO-CHAR('1001111110101010') TO
                             WS-BitAnd-Value2
               MOVE FUNCTION BIT-TO-CHAR('0001111100001010') TO
                             WS-BitAnd-Expected
             WHEN 2
               MOVE 2       TO WS-BitAnd-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                             WS-BitAnd-Value1
               MOVE FUNCTION BIT-TO-CHAR('0000000011111111') TO
                             WS-BitAnd-Value2
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                             WS-BitAnd-Expected
             WHEN 3
               MOVE 2       TO WS-BitAnd-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                             WS-BitAnd-Value1
               MOVE FUNCTION BIT-TO-CHAR('0101010110101010') TO
                             WS-BitAnd-Value2
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                             WS-BitAnd-Expected
             WHEN 4
               MOVE 2       TO WS-BitAnd-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                             WS-BitAnd-Value1
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                             WS-BitAnd-Value2
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                             WS-BitAnd-Expected
           END-EVALUATE
           .

       RunBitAndTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " start:"
           DISPLAY "WS-BitAnd-Value1: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitAnd-Value1(1:WS-BitAnd-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitAnd-Value1(1:WS-BitAnd-Len))
           DISPLAY "WS-BitAnd-Value2: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitAnd-Value2(1:WS-BitAnd-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitAnd-Value2(1:WS-BitAnd-Len))
                    "' "

           CALL 'BITAND' USING BY VALUE     WS-BitAnd-Len
                               BY REFERENCE WS-BitAnd-Result
                               BY REFERENCE WS-BitAnd-Value1
                               BY REFERENCE WS-BitAnd-Value2
                         RETURNING WS-Return-Code
           DISPLAY "BITAND-RC: " WS-Return-Code
           .

       DisplayBitAndResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "WS-BitAnd-Result: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitAnd-Result(1:WS-BitAnd-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitAnd-Result(1:WS-BitAnd-Len))
                    "' "
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitAnd-Len
              DISPLAY "Byte ", Idz, ": ",
                    "X'"
                    FUNCTION HEX-OF(WS-BitAnd-Result(Idz:1))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitAnd-Result(Idz:1))
                    "' "
           END-PERFORM
           .

       VerifyBitAndResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitAnd-Len
             IF WS-BitAnd-Result(Idz:1) NOT = WS-BitAnd-Expected(Idz:1)
               DISPLAY "Mismatch at byte ", Idz, ": got ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitAnd-Result(Idz:1)),
                       "' B'"
                       FUNCTION BIT-OF(WS-BitAnd-Result(Idz:1))
                       "' expected ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitAnd-Expected(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitAnd-Expected(Idz:1))
                       "'"

               MOVE 9 TO RETURN-CODE
             ELSE
               DISPLAY "Byte ", Idz, " ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitAnd-Result(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitAnd-Result(Idz:1))
                       "' matches."
             END-IF
           END-PERFORM
           .

      ******************************************************************
       Test_BitGet SECTION.
      ******************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitGet - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
               PERFORM SetBitGetTestValues
               PERFORM RunBitGetTest
               PERFORM DisplayBitGetResults
               PERFORM VerifyBitGetResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitGet - End"
           .

       SetBitGetTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitGet-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                              WS-BitGet-Array
               MOVE 3       TO WS-BitGet-Position
               MOVE '1'     TO WS-BitGet-Expected
             WHEN 2
               MOVE 2       TO WS-BitGet-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                              WS-BitGet-Array
               MOVE 8       TO WS-BitGet-Position
               MOVE '1'     TO WS-BitGet-Expected
             WHEN 3

               MOVE 2       TO WS-BitGet-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                              WS-BitGet-Array
               MOVE 4       TO WS-BitGet-Position
               MOVE '0'     TO WS-BitGet-Expected
             WHEN 4
               MOVE 2       TO WS-BitGet-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                              WS-BitGet-Array
               MOVE 16      TO WS-BitGet-Position
               MOVE '1'     TO WS-BitGet-Expected
           END-EVALUATE
           .

       RunBitGetTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " Start:"
           DISPLAY "WS-BitGet-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitGet-Array(1:WS-BitGet-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitGet-Array(1:WS-BitGet-Len))
                    "' "
           DISPLAY "WS-BitGet-Position: " WS-BitGet-Position

           CALL 'BITGET' USING BY VALUE     WS-BitGet-Len
                               BY REFERENCE WS-BitGet-Array
                               BY VALUE     WS-BitGet-Position
                               BY REFERENCE WS-BitGet-Result
                         RETURNING WS-Return-Code
           DISPLAY "BITGET-RC: " WS-Return-Code
           .

       DisplayBitGetResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "Bit value at position ", WS-BitGet-Position,
                   " is ", WS-BitGet-Result
           .

       VerifyBitGetResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           IF WS-BitGet-Result NOT = WS-BitGet-Expected
               DISPLAY "Mismatch: got ", WS-BitGet-Result,
                       ", expected ", WS-BitGet-Expected
               MOVE 9 TO RETURN-CODE
           ELSE
               DISPLAY "Test case ", Idx, " matches."
           END-IF
           .

      ******************************************************************
       Test_BitInv SECTION.
      ******************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitInv - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
               PERFORM SetBitInvTestValues
               PERFORM RunBitInvTest
               PERFORM DisplayBitInvResults
               PERFORM VerifyBitInvResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitInv - End"
           .

       SetBitInvTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitInv-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                              WS-BitInv-Array
               MOVE 3       TO WS-BitInv-Position
               MOVE FUNCTION BIT-TO-CHAR('0001111100001111') TO
                              WS-BitInv-Expected
             WHEN 2
               MOVE 2       TO WS-BitInv-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                              WS-BitInv-Array
               MOVE 8       TO WS-BitInv-Position
               MOVE FUNCTION BIT-TO-CHAR('1111111000000000') TO
                              WS-BitInv-Expected
             WHEN 3

               MOVE 2       TO WS-BitInv-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                              WS-BitInv-Array
               MOVE 4       TO WS-BitInv-Position
               MOVE FUNCTION BIT-TO-CHAR('1011101001010101') TO
                              WS-BitInv-Expected
             WHEN 4
               MOVE 2       TO WS-BitInv-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                              WS-BitInv-Array
               MOVE 16      TO WS-BitInv-Position
               MOVE FUNCTION BIT-TO-CHAR('1111111111111110') TO
                              WS-BitInv-Expected
           END-EVALUATE
           .

       RunBitInvTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " Start:"
           DISPLAY "WS-BitInv-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitInv-Array(1:WS-BitInv-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitInv-Array(1:WS-BitInv-Len))
                    "' "
           DISPLAY "WS-BitInv-Position: " WS-BitInv-Position

           CALL 'BITINV' USING BY VALUE     WS-BitInv-Len
                               BY REFERENCE WS-BitInv-Array
                               BY VALUE     WS-BitInv-Position
                         RETURNING WS-Return-Code
           .
           DISPLAY "BITINV-RC: " WS-Return-Code
           .

       DisplayBitInvResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "WS-BitInv-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitInv-Array(1:WS-BitInv-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitInv-Array(1:WS-BitInv-Len))
                    "' "
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitInv-Len
              DISPLAY "Byte ", Idz, ": ",
                    "X'"
                    FUNCTION HEX-OF(WS-BitInv-Array(Idz:1))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitInv-Array(Idz:1))
                    "' "
           END-PERFORM
           .

       VerifyBitInvResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitInv-Len
             IF WS-BitInv-Array(Idz:1) NOT = WS-BitInv-Expected(Idz:1)
               DISPLAY "Mismatch at byte ", Idz, ": got ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitInv-Array(Idz:1)),
                       "' B'"
                       FUNCTION BIT-OF(WS-BitInv-Array(Idz:1))
                       "' expected ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitInv-Expected(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitInv-Expected(Idz:1))
                       "'"

               MOVE 9 TO RETURN-CODE
             ELSE
               DISPLAY "Byte ", Idz, " ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitInv-Array(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitInv-Array(Idz:1))
                       "' matches."
             END-IF
           END-PERFORM
           .
      ******************************************************************
       Test_BitIs SECTION.
      ******************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitIs - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
               PERFORM SetBitIsTestValues
               PERFORM RunBitIsTest
               PERFORM DisplayBitIsResults
               PERFORM VerifyBitIsResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitIs - End"
           .

       SetBitIsTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitIs-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                              WS-BitIs-Array
               MOVE 3       TO WS-BitIs-Position
               MOVE '1'     TO WS-BitIs-Expected
             WHEN 2
               MOVE 2       TO WS-BitIs-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                              WS-BitIs-Array
               MOVE 9       TO WS-BitIs-Position
               MOVE '0'     TO WS-BitIs-Expected
             WHEN 3
               MOVE 2       TO WS-BitIs-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                              WS-BitIs-Array
               MOVE 4       TO WS-BitIs-Position
               MOVE '0'     TO WS-BitIs-Expected
             WHEN 4
               MOVE 2       TO WS-BitIs-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                              WS-BitIs-Array
               MOVE 16      TO WS-BitIs-Position
               MOVE '1'     TO WS-BitIs-Expected
           END-EVALUATE
           .

       RunBitIsTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " Start:"
           DISPLAY "WS-BitIs-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitIs-Array(1:WS-BitIs-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitIs-Array(1:WS-BitIs-Len))
                    "' "
           DISPLAY "WS-BitIs-Position: " WS-BitIs-Position

           CALL 'BITIS' USING BY VALUE      WS-BitIs-Len
                               BY REFERENCE WS-BitIs-Array
                               BY VALUE     WS-BitIs-Position
                               BY REFERENCE WS-BitIs-Result
                         RETURNING WS-Return-Code
           DISPLAY "BITIS-RC: " WS-Return-Code
           .

       DisplayBitIsResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "Bit value at position ", WS-BitIs-Position,
                   " is ", WS-BitIs-Result
           .

       VerifyBitIsResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           IF WS-BitIs-Result NOT = WS-BitIs-Expected
               DISPLAY "Mismatch: got ", WS-BitIs-Result,
                       ", expected ", WS-BitIs-Expected
               MOVE 9 TO RETURN-CODE
           ELSE
               DISPLAY "Test case ", Idx, " matches."
           END-IF
           .

      *****************************************************************
       Test_BitOr SECTION.
      *****************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitOr - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
             PERFORM SetBitOrTestValues
             PERFORM RunBitOrTest
             PERFORM DisplayBitOrResults
             PERFORM VerifyBitOrResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitOr - End"
           .

       SetBitOrTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitOr-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                             WS-BitOr-Value1
               MOVE FUNCTION BIT-TO-CHAR('1001111110101010') TO
                             WS-BitOr-Value2
               MOVE FUNCTION BIT-TO-CHAR('1011111110101111') TO
                             WS-BitOr-Expected
             WHEN 2
               MOVE 2       TO WS-BitOr-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                             WS-BitOr-Value1
               MOVE FUNCTION BIT-TO-CHAR('0000000011111111') TO
                             WS-BitOr-Value2
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                             WS-BitOr-Expected
             WHEN 3
               MOVE 2       TO WS-BitOr-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                             WS-BitOr-Value1
               MOVE FUNCTION BIT-TO-CHAR('0101010110101010') TO
                             WS-BitOr-Value2
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                             WS-BitOr-Expected
             WHEN 4
               MOVE 2       TO WS-BitOr-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                             WS-BitOr-Value1
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                             WS-BitOr-Value2
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                             WS-BitOr-Expected
           END-EVALUATE
           .

       RunBitOrTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " start:"
           DISPLAY "WS-BitOr-Value1: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitOr-Value1(1:WS-BitOr-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitOr-Value1(1:WS-BitOr-Len))
           DISPLAY "WS-BitOr-Value2: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitOr-Value2(1:WS-BitOr-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitOr-Value2(1:WS-BitOr-Len))
                    "' "

           CALL 'BITOR' USING BY VALUE     WS-BitOr-Len
                              BY REFERENCE WS-BitOr-Result
                              BY REFERENCE WS-BitOr-Value1
                              BY REFERENCE WS-BitOr-Value2
                         RETURNING WS-Return-Code
           DISPLAY "BITOR-RC: " WS-Return-Code
           .

       DisplayBitOrResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "WS-BitOr-Result: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitOr-Result(1:WS-BitOr-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitOr-Result(1:WS-BitOr-Len))
                    "' "
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitOr-Len
              DISPLAY "Byte ", Idz, ": ",
                    "X'"
                    FUNCTION HEX-OF(WS-BitOr-Result(Idz:1))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitOr-Result(Idz:1))
                    "' "
           END-PERFORM
           .

       VerifyBitOrResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitOr-Len
             IF WS-BitOr-Result(Idz:1) NOT = WS-BitOr-Expected(Idz:1)
               DISPLAY "Mismatch at byte ", Idz, ": got ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitOr-Result(Idz:1)),
                       "' B'"
                       FUNCTION BIT-OF(WS-BitOr-Result(Idz:1))
                       "' expected ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitOr-Expected(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitOr-Expected(Idz:1))
                       "'"

               MOVE 9 TO RETURN-CODE
             ELSE
               DISPLAY "Byte ", Idz, " ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitOr-Result(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitOr-Result(Idz:1))
                       "' matches."
             END-IF
           END-PERFORM
           .

      ******************************************************************
       Test_BitSet SECTION.
      ******************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitSet - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
               PERFORM SetBitSetTestValues
               PERFORM RunBitSetTest
               PERFORM DisplayBitSetResults
               PERFORM VerifyBitSetResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitSet - End"
           .

       SetBitSetTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitSet-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                              WS-BitSet-Array
               MOVE 3       TO WS-BitSet-Position
               MOVE '0'     TO WS-BitSet-Value
               MOVE FUNCTION BIT-TO-CHAR('0001111100001111') TO
                              WS-BitSet-Expected
             WHEN 2
               MOVE 2       TO WS-BitSet-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                              WS-BitSet-Array
               MOVE 9       TO WS-BitSet-Position
               MOVE '0'     TO WS-BitSet-Value
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                              WS-BitSet-Expected
             WHEN 3
               MOVE 2       TO WS-BitSet-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                              WS-BitSet-Array
               MOVE 4       TO WS-BitSet-Position
               MOVE '1'     TO WS-BitSet-Value
               MOVE FUNCTION BIT-TO-CHAR('1011101001010101') TO
                              WS-BitSet-Expected
             WHEN 4
               MOVE 2       TO WS-BitSet-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                              WS-BitSet-Array
               MOVE 16      TO WS-BitSet-Position
               MOVE '0'     TO WS-BitSet-Value
               MOVE '1'     TO WS-BitSet-Expected
               MOVE FUNCTION BIT-TO-CHAR('1111111111111110') TO
                              WS-BitSet-Expected
           END-EVALUATE
           .

       RunBitSetTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " Start:"
           DISPLAY "WS-BitSet-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitSet-Array(1:WS-BitSet-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitSet-Array(1:WS-BitSet-Len))
                    "' "
           DISPLAY "WS-BitSet-Position: " WS-BitSet-Position
           DISPLAY "WS-BitSet-Value: " WS-BitSet-Value

           CALL 'BITSET' USING BY VALUE     WS-BitSet-Len
                               BY REFERENCE WS-BitSet-Array
                               BY VALUE     WS-BitSet-Position
                               BY VALUE     WS-BitSet-Value
                         RETURNING WS-Return-Code
           DISPLAY "BITSET-RC: " WS-Return-Code
           .
       DisplayBitSetResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "WS-BitSet-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitSet-Array(1:WS-BitSet-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitSet-Array(1:WS-BitSet-Len))
                    "' "
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitSet-Len
              DISPLAY "Byte ", Idz, ": ",
                    "X'"
                    FUNCTION HEX-OF(WS-BitSet-Array(Idz:1))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitSet-Array(Idz:1))
                    "' "
           END-PERFORM
           .
       VerifyBitSetResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitSet-Len
             IF WS-BitSet-Array(Idz:1) NOT = WS-BitSet-Expected(Idz:1)
               DISPLAY "Mismatch at byte ", Idz, ": got ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitSet-Array(Idz:1)),
                       "' B'"
                       FUNCTION BIT-OF(WS-BitSet-Array(Idz:1))
                       "' expected ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitSet-Expected(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitSet-Expected(Idz:1))
                       "'"

               MOVE 9 TO RETURN-CODE
             ELSE
               DISPLAY "Byte ", Idz, " ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitSet-Array(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitSet-Array(Idz:1))
                       "' matches."
             END-IF
           END-PERFORM
           .

      ******************************************************************
       Test_BitSlr SECTION.
      ******************************************************************
           DISPLAY "AWFUNCT/TEST_BitSlr - Start"

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
               PERFORM SetBitSlrTestValues
               PERFORM RunBitSlrTest
               PERFORM DisplayBitSlrResults
               PERFORM VerifyBitSlrResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitSlr - End"
           .

       SetBitSlrTestValues SECTION.
           EVALUATE Idx
               WHEN 1
                   MOVE X"3F0F" TO BitSlr-Array (Idx)
                   MOVE 3 TO BitSlr-BitCount (Idx)
                   MOVE X"07E1" TO BitSlr-Expected (Idx)
               WHEN 2
                   MOVE X"FF00" TO BitSlr-Array (Idx)
                   MOVE 9 TO BitSlr-BitCount (Idx)
                   MOVE X"007F" TO BitSlr-Expected (Idx)
               WHEN 3
                   MOVE X"AA55" TO BitSlr-Array (Idx)
                   MOVE 4 TO BitSlr-BitCount (Idx)
                   MOVE X"0AA5" TO BitSlr-Expected (Idx)
               WHEN 4
                   MOVE X"FFFF" TO BitSlr-Array (Idx)
                   MOVE 16 TO BitSlr-BitCount (Idx)
                   MOVE X"0000" TO BitSlr-Expected (Idx)
           END-EVALUATE
           .

       RunBitSlrTest SECTION.
           CALL 'BITSLR' USING BY VALUE LENGTH OF WS-BitSlr-Array
                                  BY REFERENCE WS-BitSlr-Array
                                  BY VALUE WS-BitSlr-BitCount
                         RETURNING WS-Return-Code
           .

       DisplayBitSlrResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "Array after logical right shift by ",
                   WS-BitSlr-BitCount, " bits is ", WS-BitSlr-Array
           .

       VerifyBitSlrResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           IF WS-BitSlr-Array NOT = WS-BitSlr-Expected
               DISPLAY "Mismatch: got ", WS-BitSlr-Array,
                       ", expected ", WS-BitSlr-Expected
           ELSE
               DISPLAY "Test case ", Idx, " matches."
           END-IF
           .

      ******************************************************************
       Test_BitSll SECTION.
      ******************************************************************
           DISPLAY "=================================================="
           DISPLAY "AWFUNCT/TEST_BitSll - Start"
           DISPLAY "=================================================="

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 4
               PERFORM SetBitSllTestValues
               PERFORM RunBitSllTest
               PERFORM DisplayBitSllResults
               PERFORM VerifyBitSllResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_BitSll - End"
           .

       SetBitSllTestValues SECTION.
           EVALUATE Idx
             WHEN 1
               MOVE 2       TO WS-BitSll-Len
               MOVE FUNCTION BIT-TO-CHAR('0011111100001111') TO
                              WS-BitSll-Array
               MOVE 3       TO WS-BitSll-BitCount
               MOVE FUNCTION BIT-TO-CHAR('1111100001111000') TO
                               WS-BitSll-Expected
             WHEN 2
               MOVE 2       TO WS-BitSll-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111100000000') TO
                              WS-BitSll-Array
               MOVE 8       TO WS-BitSll-BitCount
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                               WS-BitSll-Expected
             WHEN 3
               MOVE 2       TO WS-BitSll-Len
               MOVE FUNCTION BIT-TO-CHAR('1010101001010101') TO
                              WS-BitSll-Array
               MOVE 4       TO WS-BitSll-BitCount
               MOVE FUNCTION BIT-TO-CHAR('0100101010100000') TO
                               WS-BitSll-Expected
             WHEN 4
               MOVE 2       TO WS-BitSll-Len
               MOVE FUNCTION BIT-TO-CHAR('1111111111111111') TO
                              WS-BitSll-Array
               MOVE 16      TO WS-BitSll-BitCount
               MOVE FUNCTION BIT-TO-CHAR('0000000000000000') TO
                               WS-BitSll-Expected
           END-EVALUATE
           .

       RunBitSllTest SECTION.
           DISPLAY "--------------------------------------------------"
           DISPLAY "Test case ", Idx, " Start:"
           DISPLAY "WS-BitSll-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitSll-Array(1:WS-BitSll-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitSll-Array(1:WS-BitSll-Len))
                    "' "
           DISPLAY "WS-BitSll-BitCount: " WS-BitSll-BitCount

           CALL 'BitSll' USING BY VALUE     WS-BitSll-Len
                               BY REFERENCE WS-BitSll-Array
                               BY VALUE     WS-BitSll-BitCount
                         RETURNING WS-Return-Code
           DISPLAY "BITGET-RC: " WS-Return-Code
           .

       DisplayBitSllResults SECTION.
           DISPLAY "Test case ", Idx, " results:"
           DISPLAY "WS-BitSll-Array: "
                    "X'"
                    FUNCTION HEX-OF(WS-BitSll-Array(1:WS-BitSll-Len))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitSll-Array(1:WS-BitSll-Len))
                    "' "
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitSll-Len
              DISPLAY "Byte ", Idz, ": ",
                    "X'"
                    FUNCTION HEX-OF(WS-BitSll-Array(Idz:1))
                    "' "
                    "B'"
                    FUNCTION BIT-OF(WS-BitSll-Array(Idz:1))
                    "' "
           END-PERFORM
           .
       VerifyBitSllResults SECTION.
           DISPLAY "Verifying results for test case ", Idx, ":"
           PERFORM VARYING Idz FROM 1 BY 1 UNTIL Idz > WS-BitSll-Len
             IF WS-BitSll-Array(Idz:1) NOT = WS-BitSll-Expected(Idz:1)
               DISPLAY "Mismatch at byte ", Idz, ": got ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitSll-Array(Idz:1)),
                       "' B'"
                       FUNCTION BIT-OF(WS-BitSll-Array(Idz:1))
                       "' expected ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitSll-Expected(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitSll-Expected(Idz:1))
                       "'"

               MOVE 9 TO RETURN-CODE
             ELSE
               DISPLAY "Byte ", Idz, " ",
                       "X'"
                       FUNCTION HEX-OF(WS-BitSll-Array(Idz:1))
                       "' B'"
                       FUNCTION BIT-OF(WS-BitSll-Array(Idz:1))
                       "' matches."
             END-IF
           END-PERFORM
           .

      ******************************************************************
       Test_DOFY SECTION.
      ******************************************************************
           DISPLAY "AWFUNCT/TEST_DOFY - Start"

           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > 23
               PERFORM SetDOFYTestValues
               PERFORM RunDOFYTest
               PERFORM DisplayDOFYResults
               PERFORM VerifyDOFYResults
           END-PERFORM

           DISPLAY "AWFUNCT/TEST_DOFY - End"
           .
       Test_DOFY-Exit.
           EXIT.

       SetDOFYTestValues SECTION.
           EVALUATE Idx
               WHEN 1
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 1 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-01-01" TO DOFY-Expected-Date (Idx)
               WHEN 2
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 31 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-01-31" TO DOFY-Expected-Date (Idx)
               WHEN 3
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 59 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-02-28" TO DOFY-Expected-Date (Idx)
               WHEN 4
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 60 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-03-01" TO DOFY-Expected-Date (Idx)
               WHEN 5
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 100 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-04-10" TO DOFY-Expected-Date (Idx)
               WHEN 6
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 250 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-09-07" TO DOFY-Expected-Date (Idx)
               WHEN 7
                   MOVE 2024 TO DOFY-Year (Idx)
                   MOVE 60 TO DOFY-Day-of-Year (Idx)
                   MOVE "2024-02-29" TO DOFY-Expected-Date (Idx)
               WHEN 8
                   MOVE 2024 TO DOFY-Year (Idx)
                   MOVE 61 TO DOFY-Day-of-Year (Idx)
                   MOVE "2024-03-01" TO DOFY-Expected-Date (Idx)
               WHEN 9
                   MOVE 2024 TO DOFY-Year (Idx)
                   MOVE 366 TO DOFY-Day-of-Year (Idx)
                   MOVE "2024-12-31" TO DOFY-Expected-Date (Idx)
               WHEN 10
                   MOVE 2023 TO DOFY-Year (Idx)
                   MOVE 365 TO DOFY-Day-of-Year (Idx)
                   MOVE "2023-12-31" TO DOFY-Expected-Date (Idx)
               WHEN 11
                   MOVE 2020 TO DOFY-Year (Idx)
                   MOVE 366 TO DOFY-Day-of-Year (Idx)
                   MOVE "2020-12-31" TO DOFY-Expected-Date (Idx)
               WHEN 12
                   MOVE 2019 TO DOFY-Year (Idx)
                   MOVE 1 TO DOFY-Day-of-Year (Idx)
                   MOVE "2019-01-01" TO DOFY-Expected-Date (Idx)
               WHEN 13
                   MOVE 2000 TO DOFY-Year (Idx)
                   MOVE 60 TO DOFY-Day-of-Year (Idx)
                   MOVE "2000-02-29" TO DOFY-Expected-Date (Idx)
               WHEN 14
                   MOVE 1900 TO DOFY-Year (Idx)
                   MOVE 60 TO DOFY-Day-of-Year (Idx)
                   MOVE "1900-03-01" TO DOFY-Expected-Date (Idx)
               WHEN 15
                   MOVE 2100 TO DOFY-Year (Idx)
                   MOVE 59 TO DOFY-Day-of-Year (Idx)
                   MOVE "2100-02-28" TO DOFY-Expected-Date (Idx)
               WHEN 16
                   MOVE 2022 TO DOFY-Year (Idx)
                   MOVE 1 TO DOFY-Day-of-Year (Idx)
                   MOVE "2022-01-01" TO DOFY-Expected-Date (Idx)
               WHEN 17
                   MOVE 2022 TO DOFY-Year (Idx)
                   MOVE 365 TO DOFY-Day-of-Year (Idx)
                   MOVE "2022-12-31" TO DOFY-Expected-Date (Idx)
               WHEN 18
                   MOVE 2004 TO DOFY-Year (Idx)
                   MOVE 60 TO DOFY-Day-of-Year (Idx)
                   MOVE "2004-02-29" TO DOFY-Expected-Date (Idx)
               WHEN 19
                   MOVE 2004 TO DOFY-Year (Idx)
                   MOVE 61 TO DOFY-Day-of-Year (Idx)
                   MOVE "2004-03-01" TO DOFY-Expected-Date (Idx)
               WHEN 20
                   MOVE 2001 TO DOFY-Year (Idx)
                   MOVE 31 TO DOFY-Day-of-Year (Idx)
                   MOVE "2001-01-31" TO DOFY-Expected-Date (Idx)
               WHEN 21
                   MOVE 2001 TO DOFY-Year (Idx)
                   MOVE 32 TO DOFY-Day-of-Year (Idx)
                   MOVE "2001-02-01" TO DOFY-Expected-Date (Idx)
               WHEN 22
                   MOVE 1980 TO DOFY-Year (Idx)
                   MOVE 366 TO DOFY-Day-of-Year (Idx)
                   MOVE "1980-12-31" TO DOFY-Expected-Date (Idx)
               WHEN 23
                   MOVE 1981 TO DOFY-Year (Idx)
                   MOVE 1 TO DOFY-Day-of-Year (Idx)
                   MOVE "1981-01-01" TO DOFY-Expected-Date (Idx)
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .

       RunDOFYTest SECTION.
           MOVE DOFY-Year (Idx)        TO WS-DOFY-Year
           MOVE DOFY-Day-of-Year (Idx) TO WS-DOFY-Day-of-Year
           CALL 'DOFY' USING BY VALUE WS-DOFY-Year
                             BY VALUE WS-DOFY-Day-of-Year
                             BY REFERENCE DOFY-Date-Str
                         RETURNING WS-Return-Code
           .

       DisplayDOFYResults SECTION.
           DISPLAY "Test case ", Idx, ": Year = ", DOFY-Year (Idx),
                   ", Day of Year = ", DOFY-Day-of-Year (Idx),
                   ", Expected Date = ", DOFY-Expected-Date (Idx),
                   ", Actual Date = ", DOFY-Date-Str
           .

       VerifyDOFYResults SECTION.
           IF DOFY-Date-Str NOT = DOFY-Expected-Date (Idx) THEN
               DISPLAY "ERROR: Test case ", Idx, " failed!"
               MOVE 9 TO RETURN-CODE
           ELSE
               DISPLAY "Test case ", Idx, " passed."
           END-IF
           .
