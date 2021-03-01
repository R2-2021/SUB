      *    *** 16�i���ϊ� �T�u���[�`��
      *    *** �P�������łȂ��ABuffer�S����ϊ�
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE03.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** ���̎w�肪�����ƃR���p�C���G���[�ɂȂ�
      *    *** MOVE LENGTH(LI-BUF2) TO WK-BUF-LEN2 �̏�
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE03".

           03  WK-L            BINARY-LONG SYNC VALUE ZERO.
           03  WK-R            BINARY-LONG SYNC VALUE ZERO.

      *     03  WK-BUF-LEN1     BINARY-LONG SYNC VALUE ZERO.
      *     03  WK-BUF-LEN2     BINARY-LONG SYNC VALUE ZERO.

           03  WK-HEX-DIGITS   VALUE '0123456789ABCDEF'.
             05  WK-HEX-DIGIT  OCCURS 16
                               PIC  X(001).

           03  WK-PIC.
             05  FILLER        PIC  X(001) VALUE LOW-VALUE.
             05  WK-PIC-X      PIC  X(001) VALUE LOW-VALUE.
           03  WK-PIC-HALF     REDEFINES WK-PIC 
                               PIC  9(004) COMP-X.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 256
                               ASCENDING KEY IS TBL01-PIC-X
      *    *** TBL01-IDX ����INDEXED BY ���w�肵�� INDEX �͒�`�s�v
                               INDEXED BY TBL01-IDX.

      *    *** 0-255 ��16�i���������Ă���
             05  TBL01-PIC-X   PIC  X(001) VALUE ZERO.
      *    *** 0-255 ��16�i���AASCII �����������Ă���
             05  TBL01-LR.
               07  TBL01-L     PIC  X(001) VALUE ZERO.
               07  TBL01-R     PIC  X(001) VALUE ZERO.

       01  IDX-AREA.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       LINKAGE                 SECTION.

      *    *** �ϊ��O �������Ă���f�[�^
       01  LI-BUF1             PIC  X(001) ANY LENGTH.

      *    *** �ϊ��O�̃f�[�^�̒���
       01  LI-BUF1-LEN         BINARY-LONG SYNC VALUE ZERO.

      *    *** 16�i�� �ϊ��� �������Ă���f�[�^
      *    *** �x�m�ʂ�NETCOBOL�̎����ɂ��ƁA���ڍő咷��64770�o�C�g�ł���
       01  LI-BUF2.
      *    *** LLL...
           03  LI-BUF2-L-TBL.
             05  LI-BUF2-L     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** RRR...
           03  LI-BUF2-R-TBL.
             05  LI-BUF2-R     OCCURS 65536
                               PIC  X(001) VALUE SPACE.
      *    *** LRLR...
           03  LI-BUF2-LR-TBL.
             05  LI-BUF2-LR-TBL2 OCCURS 65536.
               07  LI-BUF2-L2  PIC  X(001) VALUE SPACE.
               07  LI-BUF2-R2  PIC  X(001) VALUE SPACE.

       PROCEDURE   DIVISION    USING   LI-BUF1
                                       LI-BUF1-LEN
                                       LI-BUF2
           .
       M100-10.

           IF      SW-FIRST    =       "Y"
                   PERFORM S010-10     THRU    S010-EX
           END-IF

      *    *** PIN1 LINE SEQUENCE �� ���R�[�h�������ƁC�k�d�m�Z�b�g���Ă�
      *    *** �ς̃��R�[�h���ɂȂ�Ȃ��ׁA�l�`�h�m�Ń��R�[�h���Z�b�g�ɂ���
      *    *** ���ڃN���A�[���������x�グ�邽�߂ɂ��Ă��Ȃ��A�����O�X�𒴂���
      *    *** �Q�Ƃ��Ȃ��R���g���[���͂l�`�h�m�ōs���A���l�̗��R���ڒ�������
      *    *** �`�F�b�N�����Ȃ�
      *     MOVE    LENGTH(LI-BUF1) TO  WK-BUF-LEN1

      *     IF      LI-BUF1-LEN >       65536
      *             DISPLAY WK-PGM-NAME " BUF1 DATA MAX-LEN OVER 65536"
      *             STOP    RUN
      *     END-IF

      *     DISPLAY "BUF-LEN1=" WK-BUF-LEN1

      *     MOVE    LENGTH(LI-BUF2) TO  WK-BUF-LEN2

      *
           MOVE    ZERO        TO      L2
           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL   L1 > LI-BUF1-LEN

               ADD     1           TO      L2

      *    *** TEST18�łP�O�O�O����A�P�o�C�g��r�łO�D�S�R�b�|����
      *    *** �P�O�O�����A�P�O�O�O�o�C�g�łS�R�b�|����v�Z
               IF    TBL01-PIC-X(TBL01-IDX) =   LI-BUF1 (L1:1)
                           MOVE    TBL01-L (TBL01-IDX)
                                               TO      LI-BUF2-L (L2)
                                                       LI-BUF2-L2(L2)
                           MOVE    TBL01-R (TBL01-IDX)
                                               TO      LI-BUF2-R (L2)
                                                       LI-BUF2-R2(L2)
               ELSE
                   SEARCH  ALL TBL01-AREA
                       AT END
                           MOVE    SPACE       TO      LI-BUF2 (L2:1)

                       WHEN TBL01-PIC-X(TBL01-IDX) =   LI-BUF1 (L1:1)
                           MOVE    TBL01-L (TBL01-IDX)
                                               TO      LI-BUF2-L (L2)
                                                       LI-BUF2-L2(L2)
                           MOVE    TBL01-R (TBL01-IDX)
                                               TO      LI-BUF2-R (L2)
                                                       LI-BUF2-R2(L2)
                   END-SEARCH
               END-IF
           END-PERFORM
           .
       M100-EX.
           EXIT    PROGRAM.

       S010-10.

           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > 255
                   MOVE    I           TO      WK-PIC-HALF
                   ADD     I 1         GIVING  I2

                   MOVE    WK-PIC-X    TO      TBL01-PIC-X (I2)

                   DIVIDE  WK-PIC-HALF BY 16
                           GIVING    WK-L
                           REMAINDER WK-R

                   ADD     1           TO      WK-L
                                               WK-R

                   MOVE    WK-HEX-DIGIT (WK-L)
                                       TO      TBL01-L (I2)

                   MOVE    WK-HEX-DIGIT (WK-R)
                                       TO      TBL01-R (I2)
           END-PERFORM

           MOVE    "N"         TO      SW-FIRST

      *    *** ���� SPACE�̏ꏊ�Z�b�g���Ă���
           SET     TBL01-IDX   TO      33
           .
       S010-EX.
           EXIT.
