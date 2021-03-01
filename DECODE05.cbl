      *    *** SJIS <=> UTF8 �ϊ� �T�u���[�`��
      *    *** 
      *    *** https://www.seiai.ed.jp/sys/text/java/utf8table.html ������p
      *    *** 00-7x	�P�o�C�g����	US-ASCII�ɂ��Ȃ�
      *    *** 8x,9x,Ax,Bx	���o�C�g�����̂Q�o�C�g�ڈȍ~	
      *    *** Cx,Dx	2�o�C�g�����̊J�n�o�C�g	
      *    *** Ex	3�o�C�g�����̊J�n�o�C�g	�����͂����ނ˂���ŊJ�n
      *    *** Fx	4�o�C�g�ȏ�̕����̊J�n�o�C�g	
      *    *** F0-F7��4�o�C�g�AF8-FB��5�o�C�g�AFC-FD�͂U�o�C�g
      *    *** 
      *    *** SJIS=>UTF8 �̎��A���L�͈͂ɂ��鎞�A�ϊ��A
      *    *** �Ή�����R�[�h����"�H"���Z�b�g
      *    *** MODE=AA �w�莞�A1�o�C�g�ÂZ�b�g
      *    *** MODE=AK �w�莞�AX"2020"�̎��́AX"E38080"�Z�b�g
      *    *** ���̑��̃R�[�h��1�o�C�g�ÂZ�b�g

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE05.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** ���̎w�肪�����ƃR���p�C���G���[�ɂȂ�
      *    *** MOVE LENGTH(LDE05-BUF2) TO WK-BUF-LEN2 �̏�
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

      *    *** SJIS,UTF8 �f�[�^
       SELECT PIN1-F           ASSIGN   WK-PIN1-F-NAME
                               STATUS   WK-PIN1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  PIN1-F
           LABEL RECORDS ARE STANDARD
           RECORD VARYING DEPENDING ON WK-PIN1-LEN.
       01  PIN1-REC.
           03  FILLER          PIC  X(100).

       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE05".

           03  WK-PIN1-F-NAME  PIC  X(032) VALUE
               "KANJI\KANJI1.txt".

           03  WK-PIN1-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-PIN1-EOF     PIC  X(001) VALUE LOW-VALUE.

           03  WK-PIN1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-PIN1-CNT     BINARY-LONG SYNC VALUE ZERO.

      *     03  WK-SJIS         PIC  X(002) VALUE SPACE.
           03  WK-UTF8         PIC  X(003) VALUE SPACE.
      *    *** SPACE
           03  WK-20           PIC  X(001) VALUE X"20".
      *    *** SPACE
           03  WK-2020         PIC  X(002) VALUE X"2020".
      *    *** �H SJIS
           03  WK-8148         PIC  X(002) VALUE X"8148".
      *    *** SPACE UTF8
           03  WK-E38080       PIC  X(003) VALUE X"E38080".
      *    *** �H UTF8
           03  WK-EFBC9F       PIC  X(003) VALUE X"EFBC9F".
           03  WK-UTF8-BYTE    BINARY-LONG SYNC VALUE ZERO.
           03  WK-DATA.
             05                PIC  X(001) VALUE "%".
             05  WK-DATA-L1    PIC  X(001) VALUE ZERO.
             05  WK-DATA-R1    PIC  X(001) VALUE ZERO.
             05                PIC  X(001) VALUE "%".
             05  WK-DATA-L2    PIC  X(001) VALUE ZERO.
             05  WK-DATA-R2    PIC  X(001) VALUE ZERO.
             05                PIC  X(001) VALUE "%".
             05  WK-DATA-L3    PIC  X(001) VALUE ZERO.
             05  WK-DATA-R3    PIC  X(001) VALUE ZERO.
           03  WK-BUF1-L       BINARY-LONG SYNC VALUE ZERO.
           03  WK-BUF2-L       BINARY-LONG SYNC VALUE ZERO.

      *    *** �ϊ��O�̃f�[�^�̒���
      *     03  LDE05-BUF1-LEN     BINARY-LONG SYNC VALUE ZERO.

      *    *** �ϊ���̃f�[�^�̒���
      *     03  WK-BUF2-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  WK-HEX-DIGITS   VALUE '0123456789ABCDEF'.
             05  WK-HEX-DIGIT  OCCURS 16
                               PIC  X(001).

           03  WK-PIC.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-PIC-X      PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIC-HALF     REDEFINES WK-PIC 
                               PIC  9(004) COMP-X.

           COPY    CPFILEDUMP REPLACING ==:##:== BY ==WFD==.

       01  TBL-AREA01-G.
      *    *** SJIS �ŏ���
           03  TBL01-AREA      OCCURS 10000
                               ASCENDING KEY IS TBL01-SJIS
                               INDEXED BY TBL01-IDX.
      *    ***
             05  TBL01-SJIS    PIC  X(002) VALUE HIGH-VALUE.
      *    ***
             05  TBL01-UTF8-BYTE BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-UTF8    PIC  X(003) VALUE SPACE.

       01  TBL-AREA02-G.
      *    *** UTF8 �ŏ���
           03  TBL02-AREA      OCCURS 10000
                               ASCENDING KEY IS TBL02-UTF8
                               INDEXED BY TBL02-IDX.
      *    ***
             05  TBL02-SJIS    PIC  X(002) VALUE SPACE.
      *    ***
             05  TBL02-UTF8-BYTE BINARY-LONG SYNC VALUE ZERO.
             05  TBL02-UTF8    PIC  X(003) VALUE HIGH-VALUE.

       01  TBL-AREA03-G.
      *    *** UTF8 �ǉ��� 1�o�C�g�nUTF8=>SJIS
           03  TBL03-AREA.
             05  TBL03-AREA2.
               07              PIC  X(003) VALUE X"208140".
               07              PIC  X(003) VALUE X"218149".
               07              PIC  X(003) VALUE X"22814E".
               07              PIC  X(003) VALUE X"238194".
               07              PIC  X(003) VALUE X"248190".
               07              PIC  X(003) VALUE X"258193".
               07              PIC  X(003) VALUE X"268195".
               07              PIC  X(003) VALUE X"27818C".
               07              PIC  X(003) VALUE X"288169".
               07              PIC  X(003) VALUE X"29816A".
               07              PIC  X(003) VALUE X"2A8196".
               07              PIC  X(003) VALUE X"2B817B".
               07              PIC  X(003) VALUE X"2C8143".
               07              PIC  X(003) VALUE X"2D817C".
               07              PIC  X(003) VALUE X"2E8144".
               07              PIC  X(003) VALUE X"2F815E".

               07              PIC  X(003) VALUE X"30824F".
               07              PIC  X(003) VALUE X"318250".
               07              PIC  X(003) VALUE X"328251".
               07              PIC  X(003) VALUE X"338252".
               07              PIC  X(003) VALUE X"348253".
               07              PIC  X(003) VALUE X"358254".
               07              PIC  X(003) VALUE X"368255".
               07              PIC  X(003) VALUE X"378256".
               07              PIC  X(003) VALUE X"388257".
               07              PIC  X(003) VALUE X"398258".
               07              PIC  X(003) VALUE X"3A8146".
               07              PIC  X(003) VALUE X"3B8147".
               07              PIC  X(003) VALUE X"3C8183".
               07              PIC  X(003) VALUE X"3D8181".
               07              PIC  X(003) VALUE X"3E8184".
               07              PIC  X(003) VALUE X"3F8148".

               07              PIC  X(003) VALUE X"408197".
               07              PIC  X(003) VALUE X"418260".
               07              PIC  X(003) VALUE X"428261".
               07              PIC  X(003) VALUE X"438262".
               07              PIC  X(003) VALUE X"448263".
               07              PIC  X(003) VALUE X"458264".
               07              PIC  X(003) VALUE X"468265".
               07              PIC  X(003) VALUE X"478266".
               07              PIC  X(003) VALUE X"488267".
               07              PIC  X(003) VALUE X"498268".
               07              PIC  X(003) VALUE X"4A8269".
               07              PIC  X(003) VALUE X"4B826A".
               07              PIC  X(003) VALUE X"4C826B".
               07              PIC  X(003) VALUE X"4D826C".
               07              PIC  X(003) VALUE X"4E826D".
               07              PIC  X(003) VALUE X"4F826E".

               07              PIC  X(003) VALUE X"50826F".
               07              PIC  X(003) VALUE X"518270".
               07              PIC  X(003) VALUE X"528271".
               07              PIC  X(003) VALUE X"538272".
               07              PIC  X(003) VALUE X"548273".
               07              PIC  X(003) VALUE X"558274".
               07              PIC  X(003) VALUE X"568275".
               07              PIC  X(003) VALUE X"578276".
               07              PIC  X(003) VALUE X"588277".
               07              PIC  X(003) VALUE X"598278".
               07              PIC  X(003) VALUE X"5A8279".
               07              PIC  X(003) VALUE X"5B816D".
               07              PIC  X(003) VALUE X"5C815F".
               07              PIC  X(003) VALUE X"5D816E".
               07              PIC  X(003) VALUE X"5E814F".
               07              PIC  X(003) VALUE X"5F8151".

               07              PIC  X(003) VALUE X"60814D".
               07              PIC  X(003) VALUE X"618281".
               07              PIC  X(003) VALUE X"628282".
               07              PIC  X(003) VALUE X"638283".
               07              PIC  X(003) VALUE X"648284".
               07              PIC  X(003) VALUE X"658285".
               07              PIC  X(003) VALUE X"668286".
               07              PIC  X(003) VALUE X"678287".
               07              PIC  X(003) VALUE X"688288".
               07              PIC  X(003) VALUE X"698289".
               07              PIC  X(003) VALUE X"6A828A".
               07              PIC  X(003) VALUE X"6B828B".
               07              PIC  X(003) VALUE X"6C828C".
               07              PIC  X(003) VALUE X"6D828D".
               07              PIC  X(003) VALUE X"6E828E".
               07              PIC  X(003) VALUE X"6F828F".

               07              PIC  X(003) VALUE X"708290".
               07              PIC  X(003) VALUE X"718291".
               07              PIC  X(003) VALUE X"728292".
               07              PIC  X(003) VALUE X"738293".
               07              PIC  X(003) VALUE X"748294".
               07              PIC  X(003) VALUE X"758295".
               07              PIC  X(003) VALUE X"768296".
               07              PIC  X(003) VALUE X"778297".
               07              PIC  X(003) VALUE X"788298".
               07              PIC  X(003) VALUE X"798299".
               07              PIC  X(003) VALUE X"7A829A".
               07              PIC  X(003) VALUE X"7B816F".
               07              PIC  X(003) VALUE X"7C8162".
               07              PIC  X(003) VALUE X"7D8170".
               07              PIC  X(003) VALUE X"7E8160".

             05  TBL03-AREA2-R REDEFINES TBL03-AREA2
                               OCCURS 95
                               INDEXED BY TBL03-IDX.
               07  TBL03-UTF8  PIC  X(001).
               07  TBL03-SJIS  PIC  X(002).

       01  IDX-AREA.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.

       01  CNS-AREA.
           03  CNS-1           BINARY-LONG SYNC VALUE 1.
           03  CNS-2           BINARY-LONG SYNC VALUE 2.
           03  CNS-3           BINARY-LONG SYNC VALUE 3.
           03  CNS-20          BINARY-LONG SYNC VALUE 20.

       LINKAGE                 SECTION.

           COPY    CPDECODE05 REPLACING ==:##:== BY ==LDE05==.

       01  LDE05-BUF1         PIC  X(001) ANY LENGTH.

       01  LDE05-BUF2         PIC  X(001) ANY LENGTH.

       PROCEDURE   DIVISION    USING   LDE05-DECODE05-AREA
                                       LDE05-BUF1
                                       LDE05-BUF2
           .
       M100-10.

           EVALUATE LDE05-ID
               WHEN "OPEN  "
      *    *** OPEN
                   PERFORM S010-10     THRU    S010-EX
      *    *** READ PIN1
                   PERFORM S020-10     THRU    S020-EX

                   PERFORM UNTIL WK-PIN1-EOF   =         HIGH-VALUE
      *    *** PIN1 DATA �ı�
      *    *** ��
                           IF      PIN1-REC (1:3) =      X"E58CBA"
                                OR PIN1-REC (22:1) =     X"09"
                                OR WK-PIN1-CNT <         15
                                OR WK-PIN1-LEN =         ZERO
                                   CONTINUE
                           ELSE
                                   PERFORM S030-10     THRU      S030-EX
                           END-IF
      *    *** READ PIN1
                           PERFORM S020-10     THRU      S020-EX
                   END-PERFORM

      *    *** UTF8 �ǉ��X�g�A�[
                   PERFORM S040-10     THRU    S040-EX

                   SORT    TBL01-AREA
                           ASCENDING  KEY TBL01-SJIS

      *     MOVE    "P"         TO    WFD-ID
      *     CALL    "FILEDUMP" USING  WFD-FILEDUMP-AREA
      *                               TBL-AREA01-G

                   SORT    TBL02-AREA
                           ASCENDING  KEY TBL02-UTF8


               WHEN "CHANGE"

                   MOVE    LENGTH(LDE05-BUF2) TO     LDE05-BUF2-LEN
                   MOVE    SPACE
                           TO LDE05-BUF2 (1:LENGTH(LDE05-BUF2))

      *    *** SJIS => UTF8
                   EVALUATE LDE05-HENKAN
                       WHEN "SU"
                            MOVE    1           TO      L2
      *    *** SJIS 2�o�C�g�n�̂ݑΉ��A2�o�C�g�����Ƀ`�F�b�N����
                            PERFORM VARYING L1 FROM 1 BY 1
                                UNTIL L1 > LDE05-BUF1-LEN - 1
                              EVALUATE TRUE
                                WHEN ( LDE05-BUF1 (L1:2) >= X"8140" AND 
                                       LDE05-BUF1 (L1:2) <= X"9FFC" ) OR
                                     ( LDE05-BUF1 (L1:2) >= X"E040" AND 
                                       LDE05-BUF1 (L1:2) <= X"EAA4" )
                                     PERFORM S100-10     THRU    S100-EX
                                     ADD     1           TO      L1
                                WHEN OTHER
                                   IF LDE05-MODE = "AA"
                                      MOVE    LDE05-BUF1 (L1:1) 
                                              TO LDE05-BUF2 (L2:1)
                                      ADD     1        TO  L2
                                   ELSE
                                     IF LDE05-BUF1 (L1:CNS-2) =  WK-2020
                                       ADD     L2 2        GIVING L3
                                       IF      L3     <=  LDE05-BUF2-LEN
      *                              MOVE    X"E38080"      TO
      *    *** X"E38080" ����MOVE���e���ʂ̏퐔�ɂȂ��Ă��܂�
                                           MOVE    WK-E38080     TO
                                                LDE05-BUF2 (L2:CNS-3)
      *                           CALL "COBDUMP" USING LDE05-BUF2 (L2:CNS-3)
                                       END-IF
                                       ADD     1           TO      L1
                                       ADD     3           TO      L2
                                   ELSE
                                      MOVE    LDE05-BUF1 (L1:1) 
                                              TO LDE05-BUF2 (L2:1)
                                      ADD     1        TO  L2
                                   END-IF
                              END-EVALUATE
                           END-PERFORM

      *    *** UTF8 => SJIS �S�C�T�C�U�o�C�g�n�͍l�����Ȃ�
                       WHEN "US"
                            MOVE    1           TO      L2
                            PERFORM VARYING L1 FROM 1 BY 1
                                UNTIL L1 > LDE05-BUF1-LEN
                                EVALUATE TRUE

      *    *** UTF8 �R�o�C�g�n
                                    WHEN LDE05-BUF1 (L1:1) >= X"E0"
                                                       AND <= X"EF"
                                        IF      L1  > LDE05-BUF1-LEN - 2
                                            CONTINUE
                                        ELSE
                                            MOVE    3    TO WK-UTF8-BYTE
                                            PERFORM S200-10 THRU S200-EX
                                        END-IF
                                        ADD     2           TO      L1

      *    *** UTF8 �Q�o�C�g�n
                                    WHEN LDE05-BUF1 (L1:1) >= X"C0"
                                                       AND <= X"DF"
                                        IF      L1  > LDE05-BUF1-LEN - 1
                                            CONTINUE
                                        ELSE
                                            MOVE    2    TO WK-UTF8-BYTE
                                            PERFORM S200-10 THRU S200-EX
                                        END-IF
                                        ADD     1           TO      L1

      *    *** UTF8 �P�o�C�g�n
      *                              WHEN LDE05-BUF1 (L1:1) >= X"00" 
      *                                                 AND <= X"7F"
                                    WHEN OTHER
                                      IF      LDE05-MODE  =  "AA"
                                        MOVE    LDE05-BUF1 (L1:1) 
                                                TO LDE05-BUF2 (L2:1)
                                        ADD     1        TO  L2
                                      ELSE
                                        MOVE    1        TO WK-UTF8-BYTE
                                        PERFORM S200-10 THRU    S200-EX
                                      END-IF
                                END-EVALUATE
                            END-PERFORM

                       WHEN OTHER
                            DISPLAY WK-PGM-NAME 
                                    " LDE05-HENKAN PARA ERROR="
                                    LDE05-HENKAN
                            DISPLAY WK-PGM-NAME
                                    " LDE05-HENKAN SU(SJIS=>"
                                    "UTF8) US(UTF8=>SJIS) �w��"
                            STOP    RUN
                   END-EVALUATE

                   PERFORM VARYING L4 FROM LDE05-BUF2-LEN BY -1
                           UNTIL L4 < 1
                              OR LDE05-BUF2 (L4:1) NOT = SPACE
                           CONTINUE
                   END-PERFORM
      *    *** �ϊ��㒷���Z�b�g
                   MOVE    L4          TO      LDE05-BUF2-LEN

               WHEN "CLOSE "
                   PERFORM S900-10     THRU    S900-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDE05-ID PARA ERROR="
                           LDE05-ID
                   DISPLAY WK-PGM-NAME 
                           " LDE05-ID OPEN,CHANGE,CLOSE �w��"
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
           SET     TBL02-IDX   TO      1

           .
       S010-EX.
           EXIT.

      *    *** READ PIN1
       S020-10.
           READ    PIN1-F

           IF      WK-PIN1-STATUS =    ZERO
                   ADD     1           TO      WK-PIN1-CNT
           ELSE
               IF  WK-PIN1-STATUS =    10
                   MOVE    HIGH-VALUE  TO      WK-PIN1-EOF
               ELSE
                   DISPLAY WK-PGM-NAME " PIN1-F READ ERROR STATUS="
                           WK-PIN1-STATUS
                   STOP    RUN
               END-IF
           END-IF
           .
       S020-EX.
           EXIT.

      *    *** PIN1 DATA �ı�
       S030-10.

      *    *** MAIN��OPEN,CLOSE ���Ă����,FILEDUMP �g�p�\
      *    *** ����ς�A�g���Ȃ��HFILEDUMP �I�������\������邪�A
      *    *** �t�@�C�����e�o�͂���Ă��Ȃ�
      *     IF (WK-PIN1-CNT >= 50   AND <= 50 ) 
      *     IF (WK-PIN1-CNT >= 8000 AND <= 8500 )
      *     IF (WK-PIN1-CNT >= 8000)
      *        MOVE    "P"         TO      WFD-ID
      *        MOVE    2           TO      WFD-SU
      *        CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
      *                                    PIN1-REC
      *        CALL    "COBDUMP"   USING   PIN1-REC (1:80)
      *     END-IF

      *    *** SJIS DATA
           MOVE    PIN1-REC (12:1) TO  WK-DATA-L1
           MOVE    PIN1-REC (13:1) TO  WK-DATA-R1
           MOVE    PIN1-REC (14:1) TO  WK-DATA-L2
           MOVE    PIN1-REC (15:1) TO  WK-DATA-R2
           MOVE    "2"         TO      WK-DATA-L3
           MOVE    "0"         TO      WK-DATA-R3
      *     MOVE    9           TO      WK-BUF1-L
      *     MOVE    2           TO      WK-BUF2-L
      *    *** %LR%LR... 16�i�ϊ��T�u���[�`�����p����
           CALL    "DECODE02"  USING   WK-DATA
                                       WK-BUF1-L
                                       TBL01-SJIS (TBL01-IDX)
                                       WK-BUF2-L
           MOVE    TBL01-SJIS (TBL01-IDX) TO
                                       TBL02-SJIS (TBL02-IDX)

      *    *** UTF8 DATA
           MOVE    PIN1-REC (22:1) TO  WK-DATA-L1
           MOVE    PIN1-REC (23:1) TO  WK-DATA-R1

           IF      PIN1-REC (24:1) =   X"09"
                   MOVE    "2"         TO     WK-DATA-L2
                   MOVE    "0"         TO     WK-DATA-R2
                   MOVE    "2"         TO     WK-DATA-L3
                   MOVE    "0"         TO     WK-DATA-R3
                   MOVE    1           TO
                           TBL01-UTF8-BYTE (TBL01-IDX)
                           TBL02-UTF8-BYTE (TBL02-IDX)
           ELSE
               MOVE    PIN1-REC (24:1) TO  WK-DATA-L2
               MOVE    PIN1-REC (25:1) TO  WK-DATA-R2

               IF      PIN1-REC (26:1) =   X"09"
                   MOVE    "2"         TO     WK-DATA-L3
                   MOVE    "0"         TO     WK-DATA-R3
                   MOVE    2           TO
                           TBL01-UTF8-BYTE (TBL01-IDX)
                           TBL02-UTF8-BYTE (TBL02-IDX)
               ELSE
                   MOVE    PIN1-REC (26:1) TO  WK-DATA-L3
                   MOVE    PIN1-REC (27:1) TO  WK-DATA-R3
                   MOVE    3           TO
                           TBL01-UTF8-BYTE (TBL01-IDX)
                           TBL02-UTF8-BYTE (TBL02-IDX)
               END-IF
           END-IF

      *    *** %LR%LR... 16�i�ϊ��T�u���[�`�����p����
      *     MOVE    9           TO      WK-BUF1-L
      *     MOVE    3           TO      WK-BUF2-L
           CALL    "DECODE02"  USING   WK-DATA
                                       WK-BUF1-L
                                       TBL01-UTF8 (TBL01-IDX)
                                       WK-BUF2-L
           MOVE    TBL01-UTF8 (TBL01-IDX) TO
                                       TBL02-UTF8 (TBL02-IDX)

      *    *** ���� �P���Z���Ă���
           SET     TBL01-IDX   UP BY   1
           SET     TBL02-IDX   UP BY   1
           .
       S030-EX.
           EXIT.

      *    *** UTF8 1�o�C�g�n�ǉ�
       S040-10.

           SET     TBL03-IDX   TO      1

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL I > 95
                   MOVE    TBL03-UTF8 (TBL03-IDX) TO
                           TBL02-UTF8 (TBL02-IDX)
                   MOVE    1           TO      
                           TBL02-UTF8-BYTE (TBL02-IDX)
                   MOVE    TBL03-SJIS (TBL03-IDX) TO
                           TBL02-SJIS (TBL02-IDX)

      *    *** ���� �P���Z���Ă���
                   SET     TBL03-IDX   UP BY   1
                   SET     TBL02-IDX   UP BY   1
           END-PERFORM
           .
       S040-EX.
           EXIT.

      *    *** CHANGE SJIS => UTF8
       S100-10.

           SEARCH  ALL TBL01-AREA
               AT END
      *    *** �H �Z�b�g
                   ADD     L2 2        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    WK-EFBC9F TO    LDE05-BUF2 (L2:CNS-3)
                   END-IF
                   ADD     3           TO      L2

      *    *** �����Q�Ƃ�AND�w��Ŏw�肷��ƁA���e����Ă��g�h�s���Ă��܂�
               WHEN TBL01-SJIS (TBL01-IDX) =   LDE05-BUF1 (L1:CNS-2)
                   EVALUATE TBL01-UTF8-BYTE (TBL01-IDX)
                       WHEN 3
                           ADD     L2 2        GIVING  L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               MOVE TBL01-UTF8 (TBL01-IDX) TO
                                    LDE05-BUF2 (L2:CNS-3)
                           END-IF
                           ADD     3           TO      L2
                       WHEN 2
                           ADD     L2 1        GIVING  L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               MOVE TBL01-UTF8 (TBL01-IDX) TO
                                    LDE05-BUF2 (L2:CNS-2)
                           END-IF
                           ADD     2           TO      L2
                       WHEN 1
                           ADD     L2          GIVING  L3
                           IF      L3          <=      LDE05-BUF2-LEN
                               MOVE TBL01-UTF8 (TBL01-IDX) TO
                                    LDE05-BUF2 (L2:CNS-1)
                           END-IF
                           ADD     1           TO      L2
                   END-EVALUATE

           END-SEARCH

           .
       S100-EX.
           EXIT.

      *    *** CHANGE UTF8 => SJIS �R�o�C�g�n �t�s�e�W
       S200-10.

           EVALUATE WK-UTF8-BYTE
               WHEN 3
                   MOVE    LDE05-BUF1 (L1:CNS-3) TO   WK-UTF8
               WHEN 2
                   MOVE    LDE05-BUF1 (L1:CNS-2) TO   WK-UTF8 (1:2)
                   MOVE    WK-20              TO   WK-UTF8 (3:1)
               WHEN 1
                   MOVE    LDE05-BUF1 (L1:CNS-1) TO   WK-UTF8 (1:1)
                   MOVE    WK-2020            TO   WK-UTF8 (2:2)
           END-EVALUATE

           SEARCH  ALL TBL02-AREA
               AT END
      *    *** �H �Z�b�g
                   ADD     L2 1        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    WK-8148 TO      LDE05-BUF2 (L2:CNS-2)
                   END-IF
      *    *** debug �ׁ̈Adisplay �c���Ă���
                   DISPLAY WK-PGM-NAME
                           " S200-10 " " BUF1-CNT=" LDE05-BUF1-CNT
                           " L1=" L1 " L2=" L2
                   CALL    "COBDUMP"   USING   WK-UTF8
                   ADD     2           TO      L2

                   MOVE    "P"         TO      WFD-ID
                   MOVE    "UTF8"      TO      WFD-KANJI
                   CALL    "FILEDUMP"  USING   WFD-FILEDUMP-AREA
                                               LDE05-BUF1
                                                 (1:LDE05-BUF1-LEN)

      *    *** �����Q�Ƃ�AND�w��Ŏw�肷��ƁA���e����Ă��g�h�s���Ă��܂�
               WHEN TBL02-UTF8 (TBL02-IDX) = WK-UTF8

                   ADD     L2 1        GIVING  L3
                   IF      L3          <=      LDE05-BUF2-LEN
                       MOVE    TBL02-SJIS (TBL02-IDX) TO
                               LDE05-BUF2 (L2:CNS-2)
                   END-IF
                   ADD     2           TO      L2
           END-SEARCH
           .
       S200-EX.
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
