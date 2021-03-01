      *    *** A-Z,0-9,!-|���L�� ���o���ϊ� �T�u���[�`��
      *    *** 

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE07.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** A-Z,0-9 �f�[�^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE07".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE "DECODE07.PIN1".
      *         "�����f�U�C��\A.txt".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-LINE-G.
             05  WK-LINE       OCCURS 16.
               07              OCCURS 80.
                 09  WK-MOJI   PIC  X(016).
                 09            PIC  X(001).

           03  WK-ASCII-LEN    BINARY-LONG SYNC VALUE ZERO.

           COPY    CPFILEDUMP REPLACING ==:##:== BY ==WFD==.

       01  TBL-AREA01-G.
      *    ***
           03  TBL01-AREA.
             05  TBL01-ASCII-TBL.
               07              PIC  X(002) VALUE
                 "!""".
               07              PIC  X(013) VALUE
                 "#$%&'()*+,-./".
               07              PIC  X(017) VALUE
                 "0123456789:;<=>?@".
               07              PIC  X(026) VALUE
                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
               07              PIC  X(006) VALUE
                 "[\]^_`".
               07              PIC  X(026) VALUE
                 "abcdefghijklmnopqrstuvwxyz".
               07              PIC  X(004) VALUE
                 "{|}~".
      *    ***
             05  TBL01-ASCII-R REDEFINES TBL01-ASCII-TBL.
               07  TBL01-AREA2 OCCURS 94
                               ASCENDING KEY IS TBL01-ASCII
                               INDEXED BY TBL01-IDX.
                 09  TBL01-ASCII PIC  X(001).

       01  TBL-AREA02-G.
      *    *** A-Z,0-9,!-|���L�� �ް�
           03  TBL02-AREA      OCCURS 94.
             05  TBL02-AREA2   OCCURS 16.
               07  TBL02-LINE  PIC  X(016) VALUE SPACE.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I1              BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  I3              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       LINKAGE                 SECTION.
           COPY    CPDECODE07 REPLACING ==:##:== BY ==LDE07==.

       PROCEDURE   DIVISION    USING   LDE07-DECODE07-AREA
           .
       M100-10.

           EVALUATE LDE07-ID
               WHEN "OPEN  "
      *    *** OPEN
                   PERFORM S010-10     THRU    S010-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL WK-PIN1-EOF   =         HIGH-VALUE
      *    *** PIN1 DATA �ı�
                           PERFORM S030-10     THRU      S030-EX
      *    *** READ PIN1
                           PERFORM S020-10     THRU      S020-EX
                   END-PERFORM

               WHEN "CHANGE"
                   MOVE    SPACE       TO    LDE07-LINE-G
                                             WK-LINE-G

                   PERFORM VARYING I1 FROM 1 BY 1
                           UNTIL I1 > 80
      *    *** SEARCH
                           PERFORM S040-10     THRU      S040-EX
                   END-PERFORM
      *    *** �p�^�[���ύX
                   PERFORM S050-10     THRU      S050-EX

               WHEN "CLOSE "
                   PERFORM S900-10     THRU    S900-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDE07-ID PARA ERROR="
                           LDE07-ID
                  DISPLAY WK-PGM-NAME " LDE07-ID OPEN,CHANGE,CLOSE �w��"
                   STOP    RUN
           END-EVALUATE

           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** OPEN
       S010-10.

           OPEN    INPUT       PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F OPEN ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           SET     TBL01-IDX   TO      1
           MOVE    1           TO      I1
           MOVE    ZERO        TO      I2

      *****     CALL "COBDUMP" USING  WK-DATA
           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.

           READ    PIN1-F
               AT  END
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               NOT AT  END
                   ADD     1           TO      WK-PIN1-CNT
                   IF      PIN1-REC (17:3) NOT = SPACE
                           DISPLAY WK-PGM-NAME " PIN1-REC (1:20) ERROR="
                                   PIN1-REC (1:20)
                                   " WK-PIN1-CNT=" WK-PIN1-CNT
                           STOP    RUN
                   END-IF
           END-READ
           .
       S020-EX.
           EXIT.

      *    *** PIN1 DATA �ı�
       S030-10.

           ADD     1          TO       I2
           IF      I2          >       16
                   ADD     1           TO      I1
                   MOVE    1           TO      I2
           END-IF

           IF      I1         >        94
                OR I2         >        16
                   DISPLAY WK-PGM-NAME " TBL01 OVER I1=" I1 " I2=" I2
                   STOP    RUN
           END-IF

           MOVE     PIN1-REC (1:16) TO TBL02-LINE (I1 I2)
           .
       S030-EX.
           EXIT.

      *    *** SEARCH
       S040-10.

           SEARCH  ALL TBL01-AREA2
               AT END
                    CONTINUE
               WHEN TBL01-ASCII (TBL01-IDX) =  LDE07-ASCII-TBL (I1)

                    PERFORM VARYING I2 FROM 1 BY 1
                            UNTIL I2 > 16
                            MOVE    TBL02-LINE (TBL01-IDX I2) TO
                                    WK-MOJI (I2 I1) 
                    END-PERFORM
           END-SEARCH

           .
       S040-EX.
           EXIT.

      *    *** �p�^�[���ύX
       S050-10.


      *    *** MAIN �� COBSAM02.CBL �ɃT���v���L
           EVALUATE LDE07-PTN

      *    *** �ʏ�o�͎�
               WHEN 1
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I)
                   END-PERFORM

      *    *** �㑤�E�ցA�������ւP
               WHEN 2
                   MOVE     4          TO      I2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I) (I2:)

                       IF      I           =       4 OR 8 OR 12
                               ADD     -1          TO      I2
                       END-IF
                   END-PERFORM

      *    *** �㑤�E�ցA�������ւQ
               WHEN 3
                   MOVE     8          TO      I2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I) (I2:)
                       IF      I           =       2 OR 4 OR 6 OR 8 
                                                OR 10 OR 12 OR 14
                               ADD     -1          TO      I2
                       END-IF
                   END-PERFORM

      *    *** �㑤�E�ցA�������ւR
               WHEN 4
                   MOVE    16          TO      I2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I) (I2:)
                       ADD     -1          TO      I2
                   END-PERFORM

      *    *** �㑤���ցA�����E�ւP
               WHEN 5
                   MOVE    1           TO      I2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I) (I2:)
                       IF      I           =       4 OR 8 OR 12
                               ADD     1           TO      I2
                       END-IF
                   END-PERFORM

      *    *** �㑤���ցA�����E�ւQ
               WHEN 6
                   MOVE    1           TO      I2
                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I) (I2:)
                       IF      I           =       2 OR 4 OR 6 OR 8
                                                OR 10 OR 12 OR 14
                               ADD     1           TO      I2
                       END-IF
                   END-PERFORM

      *    *** �㑤���ցA�����E�ւR
               WHEN 7
                   MOVE    1           TO      I2
                   PERFORM VARYING I FROM 1 BY 1
                          UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I) (I2:)
                       ADD     1           TO      I2
                   END-PERFORM

      *    *** �㑤�E�ցA�������ց@�㉺�t�]
               WHEN 8
                   MOVE    1           TO      I2
                   PERFORM VARYING I FROM 16 BY -1
                           UNTIL I < 1
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I2)
                       ADD     1           TO      I2
                   END-PERFORM

      *    *** ���E�t�]
               WHEN 9
                   MOVE    80          TO      WK-ASCII-LEN
                   PERFORM VARYING I FROM 80 BY -1
                           UNTIL I < 2
                              OR LDE07-ASCII-TBL (I) NOT = SPACE
                           IF    LDE07-ASCII-TBL (I) = SPACE
                                 ADD     -1        TO      WK-ASCII-LEN
                           END-IF
                   END-PERFORM
                   COMPUTE I3 = WK-ASCII-LEN * 17

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       PERFORM VARYING I2 FROM 1 BY 1
                               UNTIL I2 > WK-ASCII-LEN * 17
                           MOVE    WK-LINE (I) (I2:1) 
                                       TO      LDE07-LINE (I) (I3:1)
                           ADD     -1          TO      I3
                       END-PERFORM
                       COMPUTE I3 = WK-ASCII-LEN * 17
                   END-PERFORM

      *    *** ���]
               WHEN 10
                   MOVE    80          TO      WK-ASCII-LEN
                   PERFORM VARYING I FROM 80 BY -1
                           UNTIL I < 2
                              OR LDE07-ASCII-TBL (I) NOT = SPACE
                           IF    LDE07-ASCII-TBL (I) = SPACE
                                 ADD     -1        TO      WK-ASCII-LEN
                           END-IF
                   END-PERFORM

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I)
                       PERFORM VARYING I2 FROM 1 BY 1
                           UNTIL I2 > WK-ASCII-LEN * 17
                           IF      LDE07-LINE (I) (I2:1) =   SPACE
                               MOVE   "*"   TO LDE07-LINE (I) (I2:1)
                           ELSE
                               MOVE   SPACE TO LDE07-LINE (I) (I2:1)
                           END-IF
                       END-PERFORM
                   END-PERFORM
               WHEN OTHER
      *             DISPLAY WK-PGM-NAME " �p�^�[���G���[ ="
      *                     LDE07-PTN
      *             STOP    RUN
      *    *** PTN�@�͈͊O�́A�ʏ�Ɠ����ɂ���

                   PERFORM VARYING I FROM 1 BY 1
                           UNTIL I > 16
                       MOVE    WK-LINE (I) TO      LDE07-LINE (I)
                   END-PERFORM

           END-EVALUATE

           .
       S050-EX.
           EXIT.

      *    *** CLOSE
       S900-10.

           CLOSE   PIN1-F
           IF      WK-PIN1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " PIN1-F CLOSE ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
           END-IF

           DISPLAY WK-PGM-NAME " PIN1 �ݽ� = " WK-PIN1-CNT
                   " (" WK-PIN1-F-NAME ")"
           .
       S900-EX.
           EXIT.
