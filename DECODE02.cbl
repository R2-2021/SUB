      *    *** DECODE01 ���� �T�u���[�`��
      *    *** �P�������łȂ��ABuffer�S����ϊ�
      *    *** �C���^�[�l�b�g HTML %XX UTF-8 �����ϊ�
      *    *** 
      *    *** MAIN �łQ�J���ȏ�ŁA�g�p�̎��ALI-BUF2-L ���ݒ肾�ƁA
      *    *** �ŏ���CALL�Œ����������ŃZ�b�g���邽�߁A�����̈ႤCALL
      *    *** �̎��A�\�����ʌ��ʂɂȂ�̂ŁA���ӂ���
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE02.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
      *    *** ���̎w�肪�����ƃR���p�C���G���[�ɂȂ�
      *    *** MOVE LENGTH(LI-BUF2) TO WK-BUF-LEN2 �̏�
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
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

           COPY    CPFILEDUMP REPLACING ==:##:== BY ==WFD==.

       01  TBL-AREA.
           03  TBL01-AREA      OCCURS 256
                               ASCENDING KEY IS TBL01-LR
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
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       01  CNS-AREA.
           03  CNS-9           BINARY-LONG SYNC VALUE 9.
           03  CNS-3           BINARY-LONG SYNC VALUE 3.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "Y".

       LINKAGE                 SECTION.

      *    *** %XX%XX... �������Ă���f�[�^
       01  LI-BUF1             PIC  X(001) ANY LENGTH.
      *    *** MAIN �ŃZ�b�g���邱�ƁA�ϊ������������Z�b�g
       01  LI-BUF1-L           BINARY-LONG SYNC.
      *    *** �ϊ��� �������Ă���f�[�^
       01  LI-BUF2             PIC  X(001) ANY LENGTH.
      *    *** MAIN �ŃZ�b�g���邱�ƁARETURN��͕ϊ��㒷���Z�b�g
       01  LI-BUF2-L           BINARY-LONG SYNC.

       PROCEDURE   DIVISION    USING   LI-BUF1
                                       LI-BUF1-L
                                       LI-BUF2
                                       LI-BUF2-L
           .
       M100-10.

           IF      SW-FIRST    =       "Y"
                   PERFORM S010-10     THRU    S010-EX
           END-IF

      *     MOVE    LENGTH(LI-BUF1) TO  WK-BUF-LEN1
      *     MOVE    LENGTH(LI-BUF2) TO  WK-BUF-LEN2
      *     IF      LI-BUF1-L   =       ZERO
                   MOVE    LENGTH(LI-BUF1) TO  LI-BUF1-L
      *     END-IF
      *     IF      LI-BUF2-L   =       ZERO
                   MOVE    LENGTH(LI-BUF2) TO  LI-BUF2-L
      *     END-IF

      *    *** UTF-8  0xe08080�`0xefbfbf
      *    *** Egg%XX%YY => EggXY �ɑΉ� ANK,%XX ���݂ɑΉ�
           MOVE    ZERO        TO      L2
                                       L3
           PERFORM VARYING L1 FROM 1 BY 1
                   UNTIL   L1 >  LI-BUF1-L
                        OR L2 >= LI-BUF2-L

               ADD     1           TO      L2
               IF      LI-BUF1 (L1:1) =    "%"

      *    *** SEARCH ALL INDEX �� SET �s�v
      *             SET     TBL01-IDX   TO      1
                   SEARCH  ALL TBL01-AREA
                       AT END

      *    *** 256����TBL�ɑS������̂ŁAAT END �͂��肦�Ȃ�
                           MOVE    SPACE       TO      LI-BUF2 (L2:1)
                       WHEN TBL01-LR (TBL01-IDX) = LI-BUF1 (L1 + 1:2)
                           MOVE    TBL01-PIC-X (TBL01-IDX)
                                               TO      LI-BUF2 (L2:1)
                   END-SEARCH

                   ADD     2           TO      L1
                   ADD     1           TO      L3
               ELSE
      *    *** % �ȊO��ANK�Ǝv����̂ŁA���̂܂܃Z�b�g
      *    *** Egg���܂�+[22/7] +�ȍ~���s�v�̎��AMAIN�ŃJ�b�g����
      *    *** "/results?search_query=Egg%E3%81%9F%E3%81%BE%E3%81%94+
      *    *** %E3%80%9022%2F7%E5%85%AC%E5%BC%8F%E3%80%91"
      *    *** �Ȃ̂ŁAX"20"�E�X�y�[�X�������J�b�g����
                   MOVE    LI-BUF1 (L1:1) TO   LI-BUF2 (L2:1)
                   IF      LI-BUF1 (L1:1) NOT = SPACE
                       ADD     1           TO      L3
                   END-IF
               END-IF
           END-PERFORM

      *    *** L3 �͕ϊ��㒷��
           MOVE    L3          TO      LI-BUF2-L

           .
       M100-EX.
           EXIT    PROGRAM.

       S010-10.

           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > 255
                   MOVE    I           TO      WK-PIC-HALF
                   MOVE    WK-PIC-X    TO      TBL01-PIC-X (I + 1)
                   
                   DIVIDE  WK-PIC-HALF BY 16
                           GIVING    WK-L
                           REMAINDER WK-R

                   ADD     1           TO      WK-L
                                               WK-R

                   MOVE    WK-HEX-DIGIT (WK-L)
                                       TO      TBL01-L (I + 1)

                   MOVE    WK-HEX-DIGIT (WK-R)
                                       TO      TBL01-R (I + 1)
           END-PERFORM

           MOVE    "N"         TO      SW-FIRST
           .
       S010-EX.
           EXIT.
