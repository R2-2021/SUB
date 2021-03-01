      *    *** FILEDUMP �T�u���[�`��
      *    *** OPEN COBOL SOURCE ���ǂ����@�����������͂قكI���W�i��
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             FILEDUMP.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.

       SELECT POT1-F           ASSIGN   WK-POT1-F-NAME
                               STATUS   WK-POT1-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       SELECT POT2-F           ASSIGN   WK-POT2-F-NAME
                               STATUS   WK-POT2-STATUS
           ORGANIZATION LINE   SEQUENTIAL.

       DATA                    DIVISION.
       FILE                    SECTION.

       FD  POT1-F
           LABEL RECORDS ARE STANDARD.
       01  POT1-REC            PIC  X(200).

       FD  POT2-F
           LABEL RECORDS ARE STANDARD.
       01  POT2-REC            PIC  X(200).
      
       WORKING-STORAGE         SECTION.
       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "FILEDUMP".

           03  WK-POT1-F-NAME  PIC  X(032) VALUE "FILEDUMP.POT1".
           03  WK-POT2-F-NAME  PIC  X(032) VALUE "FILEDUMP.POT2".
           03  WK-POTN-F-NAME  PIC  X(032) VALUE SPACE.

           03  WK-POT1-STATUS  PIC  9(002) VALUE ZERO.
           03  WK-POT2-STATUS  PIC  9(002) VALUE ZERO.

           03  WK-POT1-CNT     BINARY-LONG SYNC VALUE ZERO.
           03  WK-POT2-CNT     BINARY-LONG SYNC VALUE ZERO.

           03  WK-CNT.
             05 FILLER         PIC  X(001) VALUE LOW-VALUE.
             05 WK-CNT-X       PIC  X(001) VALUE LOW-VALUE.
           03  WK-CNT-R        REDEFINES   WK-CNT
                               PIC  9(004) COMP-X.

      *    *** �X�y�[�X�N���A�[���Ȃ�,INPUT �ϒ�������̂ŁAMAIN�ł͍��ڒ���
      *    *** �ꏊ�̂ݎQ�ƂƂ���A�X�y�[�X�N���A�[�Ɏ��Ԃ��|���邽��

       01  WK-BUF2.
           03  WK-BUF2-L-TBL.
             05  WK-BUF2-L     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-R-TBL.
             05  WK-BUF2-R     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
           03  WK-BUF2-LR-TBL.
             05  WK-BUF2-LR-TBL2 OCCURS 65536.
               07  WK-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  WK-BUF2-R2  PIC  X(001) VALUE SPACE.

       01  Addr-Pointer        POINTER.

       01  Addr-Sub            BINARY-CHAR.

       01  Addr-Value          BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Length       BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Sub          BINARY-LONG SYNC VALUE ZERO.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16 TIMES
                               PIC  X(001).

       01  Left-Nibble         COMP-5
                               PIC  9(002).
       01  Nibble              REDEFINES Left-Nibble 
                               BINARY-CHAR.

       01  Right-Nibble        COMP-5 
                               PIC  9(002).

       01  Output-Detail1.
           05  OD-ASCII        OCCURS 101 TIMES
                               PIC  X(001).
           05                  PIC  X(002).
           05  OD-I12          PIC  X(024).

       01  Output-Detail12.
           05  OD-ASCII-X.
             07  OD-ASCII2     OCCURS 180 TIMES
                               PIC  X(001).
           05  OD-I122         REDEFINES OD-ASCII-X
                               PIC  X(180).

       01  OD-I11.
           05  OD-Byte         PIC  ZZ,ZZ9
           05  OD-SURA         PIC  X(001)
           05  OD-LENG         PIC  ZZ,ZZ9
           05  OD-SEQ          PIC  ZZZ,ZZZ,ZZ9.

       01  Output-Detail2.
           05  OD-Hex          OCCURS 100 TIMES.
             10  OD-Hex-1      PIC  X(001).

       01  Output-Detail3.
           05  OD-Hex          OCCURS 100 TIMES.
             10  OD-Hex-2      PIC  X(001).

       01  Output-Detail4.
           05  OD4-SEQ         PIC  ZZZ,ZZZ,ZZ9.
           05  FILLER          PIC  X(001).
           05  OD4-ITEM        PIC  X(010).
           05  FILLER          PIC  X(001).
           05  OD4-ASCII-X.
             07  OD4-ASCII     OCCURS 40 TIMES
                               PIC  X(001).
           05  FILLER          PIC  X(001).
           05  OD4-I12         PIC  X(080).
           05  FILLER          PIC  X(001).
           05  OD4-I13         PIC  X(024).

       01  Output-Detail42.
           05  OD4-SEQ2        PIC ZZZ,ZZZ,ZZ9.
           05  FILLER          PIC  X(001).
           05  OD4-ITEM2       PIC  X(010).
           05  FILLER          PIC  X(001).
           05  OD4-ASCII2-X.
             07  OD4-ASCII2    OCCURS 180 TIMES 
                               PIC X(001).
           05  OD4-I22         REDEFINES OD4-ASCII2-X
                               PIC X(180).

       01  OD4-I11.
           05  OD4-Hex         OCCURS 40 TIMES.
             10  OD4-Hex-1     PIC  X(001).
             10  OD4-Hex-2     PIC  X(001).

       01  Output-Sub          BINARY-LONG SYNC VALUE ZERO.

       01  Output-Header-1.
           05  FILLER          PIC  X(010) VALUE "         1".
           05  FILLER          PIC  X(010) VALUE "         2".
           05  FILLER          PIC  X(010) VALUE "         3".
           05  FILLER          PIC  X(010) VALUE "         4".
           05  FILLER          PIC  X(010) VALUE "         5".
           05  FILLER          PIC  X(010) VALUE "         6".
           05  FILLER          PIC  X(010) VALUE "         7".
           05  FILLER          PIC  X(010) VALUE "         8".
           05  FILLER          PIC  X(010) VALUE "         9".
           05  FILLER          PIC  X(010) VALUE "         0".

       01  Output-Header-2.
           05  FILLER          PIC  X(100) VALUE ALL "1234567890".
           05  FILLER          PIC  X(003) VALUE SPACE.
           05  FILLER          PIC  X(020) VALUE "  Byte/   LEN       ".
           05  FILLER          PIC  X(004) VALUE " SEQ".

       01  Output-Header-3.
           05  FILLER          PIC  X(023) VALUE SPACE.
           05  FILLER          PIC  X(040) VALUE ALL "1234567890".
           05  FILLER          PIC  X(001) VALUE SPACE.
           05  FILLER          PIC  X(010) VALUE " 1 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(010) VALUE "11 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(010) VALUE "21 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(010) VALUE "31 2 3 4 5".
           05  FILLER          PIC  X(010) VALUE " 6 7 8 9 *".
           05  FILLER          PIC  X(001) VALUE SPACE.
           05  FILLER          PIC  X(020) VALUE "  Byte/   LEN       ".
           05  FILLER          PIC  X(004) VALUE " SEQ".

       01  PIC-XX.
           05  FILLER          PIC  X(001) VALUE LOW-VALUES.
           05  PIC-X           PIC  X(001).
       01  PIC-Halfword        REDEFINES PIC-XX 
                               PIC  9(004) COMP-X.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.
           03  P1              BINARY-LONG SYNC VALUE ZERO.
           03  P2              BINARY-LONG SYNC VALUE ZERO.
           03  P1-2            BINARY-LONG SYNC VALUE ZERO.
           03  P2-2            BINARY-LONG SYNC VALUE ZERO.
           03  CNS-50          BINARY-LONG SYNC VALUE 50.

       01  SW-AREA.
           05  SW-KANJI        PIC  X(001) VALUE ZERO.
           05  SW-KANJI2       PIC  X(001) VALUE ZERO.
           05  SW-UTF8         PIC  X(001) VALUE "N"..

       LINKAGE                 SECTION.

           COPY    CPFILEDUMP REPLACING ==:##:== BY ==LFD==.

       01  Buffer              PIC X ANY LENGTH.

      *01  Buffer-Len          BINARY-LONG.
      
      *    *** Buffer-Len �w�薳���́A���̃��R�[�h�A���ڂ̒����ŏo��
       PROCEDURE   DIVISION    USING     LFD-FILEDUMP-AREA,
                                         Buffer,
                                         OPTIONAL LFD-LEN.
       M100-10.

      *    *** OPEN
           EVALUATE TRUE
               WHEN LFD-ID      =       "O"
                   PERFORM S010-10      THRU      S010-EX
      *    *** RECORD�̏o��
               WHEN LFD-ID      =       "P"
                   PERFORM S100-10      THRU      S100-EX
      *    *** ���ڂ̏o��
               WHEN LFD-ID      =       "X"
                   PERFORM S200-10      THRU      S200-EX
      *    *** CLOSE
               WHEN LFD-ID      =       "C"
                   PERFORM S900-10      THRU      S900-EX
      *    *** ERROR
               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LFD-ID ERROR LFD-ID=" LFD-ID
                   DISPLAY WK-PGM-NAME " ID=O(OPEN),P(RECORD-PUT),"
                           "X(ITEM-PUT),C(CLOSE) �w��"
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.
      *
       S010-10.

           IF      LFD-PGM     NOT = SPACE
                   MOVE    WK-POT1-F-NAME TO   WK-POTN-F-NAME
                   STRING
                      LFD-PGM DELIMITED BY SPACE
                      "." DELIMITED BY SIZE
                      WK-POTN-F-NAME
                      INTO WK-POT1-F-NAME
                   END-STRING 

                   MOVE    WK-POT2-F-NAME TO   WK-POTN-F-NAME
                   STRING
                      LFD-PGM DELIMITED BY SPACE
                      "." DELIMITED BY SIZE
                      WK-POTN-F-NAME
                      INTO WK-POT2-F-NAME
                   END-STRING 
           END-IF

           OPEN    OUTPUT      POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F OPEN ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           OPEN    OUTPUT      POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F OPEN ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

      *    *** 1�o�C�g�ڂ���o�ׁ͂̈A�T�N��EDIT�ł͖ڐ���\�����邽��
      *    *** �K��l�@�擪�ɂP�������w�b�_�|�o��
           IF      LFD-HED     =       "Y"
                   WRITE   POT1-REC    FROM    Output-Header-1
                   WRITE   POT1-REC    FROM    Output-Header-2
                   WRITE   POT1-REC    FROM    Output-Header-3
                   ADD     3            TO     WK-POT1-CNT

                   WRITE   POT2-REC    FROM    Output-Header-1
                   WRITE   POT2-REC    FROM    Output-Header-2
                   WRITE   POT2-REC    FROM    Output-Header-3
                   ADD     3            TO     WK-POT2-CNT
           END-IF
           .
       S010-EX.
           EXIT.

      *    *** RECORD�̏o��
      *    *** LFD-SU��POT1,POT2�ǂ���ɏo�͂��邩�w��
       S100-10.
           IF NUMBER-OF-CALL-PARAMETERS = 3
      *        MOVE Buffer-Len     TO Buffer-Length
              MOVE LFD-LEN     TO Buffer-Length
      *    *** �w�肳�ꂽ�������A���ڒ��𒴂��Ă鎞�A���ڒ��ɂ���
      *        IF   Buffer-Len >   LENGTH(Buffer)  OR
      *             Buffer-Len =   0
              IF   LFD-LEN >   LENGTH(Buffer)
      *          OR
      *             LFD-LEN =   0
      *    *** ���ڒ��[�����L���Ƃ���A�o�͑ΏۊO�Ƃ���
                   MOVE LENGTH(Buffer) TO Buffer-Length
              END-IF
           ELSE
      *    *** ��3�p�����[�^(LFD-LEN)�������A�����n���ꂽ���ڂ̒����Ƃ���
              MOVE LENGTH(Buffer) TO Buffer-Length
           END-IF

           MOVE SPACE             TO Output-Detail1
                                     Output-Detail12
                                     Output-Detail2
                                     Output-Detail3
                                     OD-I11 OD-I12

           SET Addr-Pointer       TO ADDRESS OF Buffer

           MOVE    ZERO        TO      I
           MOVE    ZERO        TO      SW-KANJI SW-KANJI2

           MOVE    104         TO      P1
           MOVE    "N"         TO      SW-UTF8

           MOVE    ZERO        TO      Output-Sub
                                       P2

           PERFORM VARYING Buffer-Sub FROM 1 BY 1
                   UNTIL   Buffer-Sub > Buffer-Length

                   ADD 1 TO Output-Sub
                            P2
      *    *** �������肵���Ƃ��A�P�񔻒�X�L�b�v 
                   IF  LFD-KANJI = "SJIS"
                     IF  SW-KANJI = "1"
      *    *** SW-KANJI=1�̂Ƃ��A�Q�o�C�g��
                       IF  SW-KANJI2 =  "1"
                           MOVE   ZERO   TO  SW-KANJI2
                           ADD    1      TO  I
                       ELSE
                           ADD    1      TO  I
                           MOVE   ZERO   TO  SW-KANJI
                           PERFORM  S210-10 THRU S210-EX
                       END-IF
                     ELSE
      *    *** �����`�F�b�N
                       ADD   1   TO   I
                       PERFORM  S210-10 THRU S210-EX
                     END-IF
                   END-IF

                   IF Output-Sub = 1
                      MOVE Buffer-Sub    TO OD-Byte
                                            L2
                      MOVE "/"           TO OD-SURA
                      MOVE Buffer-Length TO OD-LENG
                   END-IF

                   IF Buffer-Sub = 1
                      CALL    "DECODE03" USING Buffer
                                               Buffer-Length
                                               WK-BUF2
                   END-IF

                   MOVE Buffer (Buffer-Sub : 1) TO PIC-X

      *    *** UTF-8 �Ǝv���銿���̎��A�p�f�B���O�������X�y�[�X�ɕύX����
                   IF  LFD-KANJI = "UTF8"
                     IF  ( PIC-X >= X"E0" AND PIC-X <= X"EF" ) AND
                         ( Buffer (Buffer-Sub + 1: 1) >= X"80" AND
                           Buffer (Buffer-Sub + 1: 1) <= X"BF" )

                         MOVE    "Y"       TO     SW-UTF8
                         ADD     1         TO     P1
                         ADD     1         TO     P2

      *    *** �ŏI�o�C�g �R�[�h�R�o�C�g�������A���̐�̊����o�C�g�Z�b�g�Ή�
                         IF  Output-Sub = 99
                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF

                         IF  Output-Sub = 100
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 1)

                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF
                     END-IF
                   END-IF

      *    *** X"20"=SPACE ,X"7E"=~
      *    *** X"7E" �ł̓J�^�J�i�o�Ȃ��̂ŁAX"FD"�ɕύX
                   IF   (PIC-X < X"20")
                     OR (PIC-X = X"7F")
      *               OR (PIC-X > X"7E" AND PIC-X < X"A1")
      *               OR (PIC-X > X"DF")) AND
      *****             SW-KANJI = ZERO
                        MOVE SPACE TO OD-ASCII (Output-Sub)
                        MOVE SPACE TO OD-ASCII2 (P2)
                   ELSE
                        MOVE PIC-X TO OD-ASCII (Output-Sub)
                        MOVE PIC-X TO OD-ASCII2 (P2)
                   END-IF

      *             IF  LFD-TYPE = "M"

      *                 DIVIDE PIC-Halfword BY 16
      *                        GIVING Left-Nibble
      *                        REMAINDER Right-Nibble
      *                 ADD 1 TO Left-Nibble Right-Nibble
      *                 MOVE Hex-Digit (Left-Nibble)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE Hex-Digit (Right-Nibble)
      *                         TO OD-Hex-2 (Output-Sub)

      *    *** CALL "DECODE03" �ɕύX����
      *                 MOVE WK-BUF2-L (Buffer-Sub)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE WK-BUF2-R (Buffer-Sub)
      *                         TO OD-Hex-2 (Output-Sub)
      *             END-IF

                   IF  Output-Sub = 100
                       IF  SW-KANJI = "1"
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII(101)
                       END-IF

                       MOVE  LFD-SEQ  TO OD-SEQ

                       MOVE  OD-I11   TO OD-I12
                       MOVE  OD-I11   TO OD-I122 (P1:24)

                       IF  LFD-SU = 1
                           MOVE  SPACE    TO   POT1-REC
                           WRITE POT1-REC
                           ADD   1        TO   WK-POT1-CNT

                           IF  LFD-HED  =  "A"
                             WRITE   POT1-REC    FROM    Output-Header-1
                             WRITE   POT1-REC    FROM    Output-Header-2
                             ADD   2        TO   WK-POT1-CNT
                           END-IF

                           IF  SW-UTF8 = "Y"
                               WRITE POT1-REC FROM Output-Detail12
      *                         CALL "COBDUMP" USING Output-Detail12
                           ELSE
                               WRITE POT1-REC FROM Output-Detail1
                           END-IF
                           ADD   1        TO   WK-POT1-CNT
                       ELSE
                           MOVE  SPACE    TO   POT2-REC
                           WRITE POT2-REC
                           ADD   1        TO   WK-POT2-CNT

                           IF  LFD-HED  =  "A"
                             WRITE   POT2-REC    FROM    Output-Header-1
                             WRITE   POT2-REC    FROM    Output-Header-2
                             ADD   2        TO   WK-POT2-CNT
                           END-IF
                           IF  SW-UTF8 = "Y"
                               WRITE POT2-REC FROM Output-Detail12
                           ELSE
                               WRITE POT2-REC FROM Output-Detail1
                           END-IF
                           ADD   1        TO   WK-POT2-CNT
                       END-IF

                       IF  LFD-TYPE = "M"

      *    *** CALL "DECODE03" �ɕύX����
                           MOVE WK-BUF2-L-TBL (L2:100)
                                   TO Output-Detail2
                           MOVE WK-BUF2-R-TBL (L2:100)
                                   TO Output-Detail3
                           IF  LFD-SU  =  1
                               WRITE POT1-REC FROM Output-Detail2
                               WRITE POT1-REC FROM Output-Detail3
                               ADD   2        TO   WK-POT1-CNT
                           ELSE
                               WRITE POT2-REC FROM Output-Detail2
                               WRITE POT2-REC FROM Output-Detail3
                               ADD   2        TO   WK-POT2-CNT
                           END-IF
                       END-IF

      
                       MOVE SPACE   TO Output-Detail1
                                       Output-Detail12
                                       Output-Detail2
                                       Output-Detail3
                                       OD-I11 OD-I12

                       MOVE    ZERO        TO      I
                       MOVE    ZERO        TO      SW-KANJI SW-KANJI2

                       MOVE    104         TO      P1
                       MOVE    "N"         TO      SW-UTF8
                       MOVE    0           TO      Output-Sub
                                                   P2

      *                 SET Addr-Pointer UP BY 100
                   END-IF
           END-PERFORM

           IF  Output-Sub > 0
               MOVE  LFD-SEQ  TO OD-SEQ

               MOVE  OD-I11   TO OD-I12
                                 OD-I122 (P1:24)

               IF  LFD-SU = 1
                   MOVE  SPACE    TO   POT1-REC
                   WRITE POT1-REC
                   ADD   1        TO   WK-POT1-CNT

                   IF  LFD-HED  =  "A"
                       WRITE   POT1-REC    FROM    Output-Header-1
                       WRITE   POT1-REC    FROM    Output-Header-2
                       ADD   2        TO   WK-POT1-CNT
                   END-IF

                   IF  SW-UTF8 = "Y"
                       WRITE POT1-REC FROM Output-Detail12
                   ELSE
                       WRITE POT1-REC FROM Output-Detail1
                   END-IF
                   ADD   1        TO   WK-POT1-CNT
               ELSE
                   MOVE  SPACE    TO   POT2-REC
                   WRITE POT2-REC
                   ADD   1        TO   WK-POT2-CNT

                   IF  LFD-HED  =  "A"
                       WRITE   POT2-REC    FROM    Output-Header-1
                       WRITE   POT2-REC    FROM    Output-Header-2
                       ADD   2        TO   WK-POT2-CNT
                   END-IF

                   IF  SW-UTF8 = "Y"
                       WRITE POT2-REC FROM Output-Detail12
                   ELSE
                       WRITE POT2-REC FROM Output-Detail1
                   END-IF
                   ADD   1        TO   WK-POT2-CNT
               END-IF

               IF  LFD-TYPE = "M"
                   MOVE WK-BUF2-L-TBL (L2:Output-Sub)
                           TO Output-Detail2
                   MOVE WK-BUF2-R-TBL (L2:Output-Sub)
                           TO Output-Detail3
                   IF  LFD-SU  =  1
                       WRITE POT1-REC FROM Output-Detail2
                       WRITE POT1-REC FROM Output-Detail3
                       ADD   2        TO   WK-POT1-CNT
                   ELSE
                       WRITE POT2-REC FROM Output-Detail2
                       WRITE POT2-REC FROM Output-Detail3
                       ADD   2        TO   WK-POT2-CNT
                   END-IF
               END-IF
           END-IF
           . 
       S100-EX.
           EXIT.
      *
      *    *** ID=X�p �ŏ���100�o�C�g���A���ڂ��Ƃ��o��
      *    *** LFD-TYPE=A(ASCII�����̂݁j�A=M(hex)���o��
      *    *** LFD-SEQ=RECORD.NO MAIN�Ŏw�肷��
      *    *** LFD-ITEM�@���ږ�10�o�C�g�ȓ��Ŏw��
       S200-10.
           IF NUMBER-OF-CALL-PARAMETERS = 3
      *        MOVE Buffer-Len     TO Buffer-Length
              MOVE LFD-LEN     TO Buffer-Length
      *    *** �w�肳�ꂽ�������A���ڒ��𒴂��Ă鎞�A���ڒ��ɂ���
      *        IF   Buffer-Len >   LENGTH(Buffer)  OR
      *             Buffer-Len =   0
              IF   LFD-LEN >   LENGTH(Buffer) 
      *    *** ���ڒ��[�����L���Ƃ���A�o�͑ΏۊO�Ƃ���
      *         OR
      *             LFD-LEN =   0
                   MOVE LENGTH(Buffer) TO Buffer-Length
              END-IF
           ELSE
              MOVE LENGTH(Buffer) TO Buffer-Length
           END-IF

           MOVE SPACE             TO Output-Detail1
                                     Output-Detail12
                                     Output-Detail2
                                     Output-Detail3
                                     Output-Detail4
                                     Output-Detail42
                                     OD-I11 OD-I12
                                     OD4-I12
                                     OD4-I13
                                     OD4-I11

      *     SET Addr-Pointer       TO ADDRESS OF Buffer

           MOVE    ZERO        TO      I
           MOVE    ZERO        TO      SW-KANJI SW-KANJI2

           MOVE    104         TO      P1
           MOVE    42          TO      P1-2

           MOVE    "N"         TO      SW-UTF8

           MOVE    ZERO        TO      Output-Sub
                                       P2
                                       P2-2

           PERFORM VARYING Buffer-Sub FROM 1 BY 1
                   UNTIL   Buffer-Sub > Buffer-Length
      *    *** ID=X �̎��A�l�`�w���P�O�O�o�C�g�܂�
                        OR Output-Sub = 100

                   ADD 1 TO Output-Sub
                            P2
                            P2-2
      *    *** �������肵���Ƃ��A�P�񔻒�X�L�b�v 
                   IF  LFD-KANJI = "SJIS"
                     IF  SW-KANJI = "1"
      *    *** SW-KANJI=1�̂Ƃ��A�Q�o�C�g��
                       IF  SW-KANJI2 =  "1"
                           MOVE   ZERO   TO  SW-KANJI2
                           ADD    1      TO  I
                       ELSE
                           ADD    1      TO  I
                           MOVE   ZERO   TO  SW-KANJI
                           PERFORM  S210-10 THRU S210-EX
                       END-IF
                     ELSE
      *    *** �����`�F�b�N
                       ADD   1   TO   I
                       PERFORM  S210-10 THRU S210-EX
                     END-IF
                   END-IF

                   IF Output-Sub = 1
                      MOVE Buffer-Sub    TO OD-Byte
                                            L2
                      MOVE "/"           TO OD-SURA
                      MOVE Buffer-Length TO OD-LENG
                   END-IF

                   IF Buffer-Sub = 1
      *    *** ID=X �� �P�O�O�o�C�g�܂ł����\�����Ȃ���
                      IF  Buffer-Length > 100
                          MOVE    100  TO  L4
                          CALL    "DECODE03" USING Buffer
                                                   L4
                                                   WK-BUF2
                      ELSE
                          CALL    "DECODE03" USING Buffer
                                                   Buffer-Length
                                                   WK-BUF2
                      END-IF
                   END-IF

                   IF  Output-Sub = 100
                       IF  SW-KANJI = "1"
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII(101)
                       END-IF
                   END-IF

                   MOVE Buffer (Buffer-Sub : 1) TO PIC-X

      *    *** UTF-8 �Ǝv���銿���̎��A�p�f�B���O�������X�y�[�X�ɕύX����
                   IF  LFD-KANJI = "UTF8"
                     IF  ( PIC-X >= X"E0" AND PIC-X <= X"EF" ) AND
                         ( Buffer (Buffer-Sub + 1: 1) >= X"80" AND
                                                      <= X"BF" )
                         MOVE    "Y"       TO     SW-UTF8
                         ADD     1         TO     P1
                                                  P2
                                                  P2-2
      *    *** �Ō�̕������� �łȂ����A���炳�Ȃ�
                         IF ( Buffer-Length     = Output-Sub ) OR
                            ( Buffer-Length - 1 = Output-Sub )
                             CONTINUE
                         ELSE
                             ADD     1         TO     P1-2
                         END-IF

                         IF  Output-Sub = 99

                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF

                         IF  Output-Sub = 100

                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 1)

                           ADD   Buffer-Sub 2 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII2(P2 + 2)
                         END-IF
                     END-IF
                   END-IF

      *    *** X"20"=SPACE ?,X"7E"=~
      *    *** X"7E" �ł̓J�^�J�i�o�Ȃ��̂ŁAX"FD"�ɕύX
                   IF    (PIC-X < X"20")
                      OR (PIC-X = X"7F")
      *               OR (PIC-X > X"7E" AND PIC-X < X"A1")
      *               OR (PIC-X > X"DF")) AND
      *****               SW-KANJI = ZERO
                        MOVE SPACE TO OD-ASCII (Output-Sub)
                                      OD-ASCII2 (P2)

                        IF Buffer-Length <= 40
                           MOVE SPACE TO OD4-ASCII (Output-Sub)
                                         OD4-ASCII2 (P2-2)
                        END-IF
                   ELSE
                        MOVE PIC-X TO OD-ASCII (Output-Sub)
                                      OD-ASCII2 (P2)

                        IF Buffer-Length <= 40
                           MOVE PIC-X TO OD4-ASCII (Output-Sub)
                                         OD4-ASCII2 (P2-2)
                        END-IF
                   END-IF

      *             IF  LFD-TYPE = "M" OR 
      *             IF  Buffer-Length <= 40

      *                 DIVIDE PIC-Halfword BY 16
      *                        GIVING Left-Nibble
      *                        REMAINDER Right-Nibble
      *                ADD 1 TO Left-Nibble Right-Nibble
      *                 MOVE Hex-Digit (Left-Nibble)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE Hex-Digit (Right-Nibble)
      *                         TO OD-Hex-2 (Output-Sub)

      *    *** CALL "DECODE03" �ɕύX����
      *                 MOVE WK-BUF2-L (Buffer-Sub)
      *                         TO OD-Hex-1 (Output-Sub)
      *                 MOVE WK-BUF2-R (Buffer-Sub)
      *                         TO OD-Hex-2 (Output-Sub)

      *                 IF Buffer-Length <= 40
      *                     MOVE Hex-Digit (Left-Nibble)
      *                         TO OD4-Hex-1 (Output-Sub)
      *                     MOVE Hex-Digit (Right-Nibble)
      *                         TO OD4-Hex-2 (Output-Sub)

      *                     MOVE WK-BUF2-L (Buffer-Sub)
      *                             TO OD4-Hex-1 (Output-Sub)
      *                     MOVE WK-BUF2-R (Buffer-Sub)
      *                             TO OD4-Hex-2 (Output-Sub)
      *                 END-IF
      *             END-IF
           END-PERFORM

           IF  Output-Sub > 0
               MOVE  LFD-SEQ  TO OD4-SEQ
                                 OD4-SEQ2
                                 OD-SEQ
               MOVE  LFD-ITEM TO OD4-ITEM
                                 OD4-ITEM2
               MOVE  OD-I11   TO OD-I12
                                 OD4-I13
                                 OD-I122 (P1:24)
                                 OD4-I22 (P1-2 + 81:24)

      *    *** ID=X �Ł@40�o�C�g�ȉ��̎��ATYPE=A �ł�16�i�� �o�͂���
               IF Buffer-Length <= 40
                   COMPUTE L3 = OUTPUT-SUB * 2
                   MOVE  WK-BUF2-LR-TBL(1:L3) TO OD4-I11 (1:L3)
                   MOVE  OD4-I11  TO OD4-I12
                                     OD4-I22 (P1-2:80)
                   IF  LFD-SU = 1
                       MOVE  SPACE    TO   POT1-REC
                       WRITE POT1-REC
                       ADD   1        TO   WK-POT1-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT1-REC    FROM    Output-Header-3
                           ADD   1        TO   WK-POT1-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
      *     IF ( LFD-SEQ = 10 ) OR
      *        ( LFD-SEQ >=49 AND <= 57 ) 
      *       CALL "COBDUMP" USING Output-Detail42
      *     END-IF
                           WRITE POT1-REC FROM Output-Detail42
                       ELSE
                           WRITE POT1-REC FROM Output-Detail4
                       ADD   1        TO   WK-POT1-CNT
                       END-IF
                   ELSE
                       MOVE  SPACE    TO   POT2-REC
                       WRITE POT2-REC
                       ADD   1        TO   WK-POT2-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT2-REC   FROM    Output-Header-3
                           ADD   1        TO   WK-POT2-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
                           WRITE POT2-REC FROM Output-Detail42
                       ELSE
                           WRITE POT2-REC FROM Output-Detail4
                       END-IF

                       ADD   1        TO   WK-POT2-CNT
                   END-IF
               ELSE
                   MOVE  SPACE    TO   OD4-I12
                                       OD4-I13
                                       OD4-I11
                                       OD4-ASCII-X
                                       OD4-ASCII2-X

                   IF  LFD-SU = 1
                       MOVE  SPACE    TO   POT1-REC
                       WRITE POT1-REC
                       WRITE POT1-REC FROM Output-Detail4
                       ADD   2        TO   WK-POT1-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT1-REC    FROM    Output-Header-1
                           WRITE   POT1-REC    FROM    Output-Header-2
                           ADD   2        TO   WK-POT1-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
                           WRITE POT1-REC FROM Output-Detail12
                       ELSE
                           WRITE POT1-REC FROM Output-Detail1
                       END-IF
                       ADD   1        TO   WK-POT1-CNT
                   ELSE
                       MOVE  SPACE    TO   POT2-REC
                       WRITE POT2-REC
                       WRITE POT2-REC FROM Output-Detail4
                       ADD   2        TO   WK-POT2-CNT

                       IF  LFD-HED  =  "A"
                           WRITE   POT2-REC    FROM    Output-Header-1
                           WRITE   POT2-REC    FROM    Output-Header-2
                           ADD   2        TO   WK-POT2-CNT
                       END-IF

                       IF  SW-UTF8 = "Y"
                           WRITE POT2-REC FROM Output-Detail12
                       ELSE
                           WRITE POT2-REC FROM Output-Detail1
                       END-IF
                       ADD   1        TO   WK-POT2-CNT
                   END-IF
                   IF  LFD-TYPE = "M"
                       MOVE WK-BUF2-L-TBL (L2:Output-Sub)
                               TO Output-Detail2
                       MOVE WK-BUF2-R-TBL (L2:Output-Sub)
                               TO Output-Detail3
                       IF  LFD-SU  =  1
                           WRITE POT1-REC FROM Output-Detail2
                           WRITE POT1-REC FROM Output-Detail3
                           ADD   2        TO   WK-POT1-CNT
                       ELSE
                           WRITE POT2-REC FROM Output-Detail2
                           WRITE POT2-REC FROM Output-Detail3
                           ADD   2        TO   WK-POT2-CNT
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       S200-EX.
           EXIT.

      *    *** SJIS �p
       S210-10.
      *    *** SJIS �̎��A�P�O�O�o�C�g�ڂ̊�����\�������邽��
           IF    ( Buffer (I:2) >= X"8140" AND 
                   Buffer (I:2) <= X"9FFC" )   OR
                 ( Buffer (I:2) >= X"E040" AND 
                   Buffer (I:2) <= X"EAA4" )
                   MOVE   "1"  TO     SW-KANJI
                                      SW-KANJI2
           ELSE
                   MOVE   ZERO TO     SW-KANJI
           END-IF
           .
       S210-EX.
           EXIT.
      *
       S900-10.

           CLOSE   POT1-F
           IF      WK-POT1-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT1-F CLOSE ERROR STATUS="
                           WK-POT1-STATUS
                   STOP    RUN
           END-IF

           CLOSE   POT2-F
           IF      WK-POT2-STATUS NOT =  ZERO
                   DISPLAY WK-PGM-NAME " POT2-F CLOSE ERROR STATUS="
                           WK-POT2-STATUS
                   STOP    RUN
           END-IF

           DISPLAY WK-PGM-NAME " POT1 �ݽ� = " WK-POT1-CNT
                   " (" WK-POT1-F-NAME ")"
           DISPLAY WK-PGM-NAME " POT2 �ݽ� = " WK-POT2-CNT
                   " (" WK-POT2-F-NAME ")"

           .
       S900-EX.
           EXIT.
