      *    *** �o�C�i���[�t�@�C�� ���R�[�h���o�� �T�u���[�`��

       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             DECODE04.

       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DECODE04".

       01  IDX-AREA.
           03  L1              BINARY-LONG SYNC VALUE ZERO.
           03  L2              BINARY-LONG SYNC VALUE ZERO.
           03  L3              BINARY-LONG SYNC VALUE ZERO.
           03  L4              BINARY-LONG SYNC VALUE ZERO.
           03  I               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "N".

       01  SAVE-AREA.
           03  SV-BUF1         PIC  X(65536) VALUE SPACE.

       LINKAGE                 SECTION.


      *    *** LI-SHORI="FIRST" MAIN�ŃZ�b�g�A����CALL��
      *    *** LI-SHORI="COND " SUB �ŃZ�b�g�A���R�[�h�o�͒�
      *    *** LI-SHORI="READ " SUB �ŃZ�b�g�A���R�[�h�P�ʂɃZ�b�g��A
      *    ***   MAIN����READ�˗�
      *    *** LI-SHORI="END  " SUB �ŃZ�b�g,X"FF"�����āA
      *    ***   ���R�[�h�G���h�̎�
      *    *** 
       01  LI-AREA.
           03  LI-SHORI        PIC  X(005) VALUE SPACE.
           03  LI-BUF1-LEN     BINARY-LONG SYNC VALUE ZERO.

           03  LI-REC-LEN      BINARY-LONG SYNC VALUE ZERO.

      *    *** �ϊ��O �o�C�i���[�t�@�C���@���R�[�h�������Ă���f�[�^
       01  LI-BUF1             PIC  X(001) ANY LENGTH.

      *    *** �ϊ��� ���R�[�h�P�ʂɃZ�b�g����
       01  LI-BUF2.
           03  LI-BUF2-TBL     OCCURS 65536
                               PIC  X(001) VALUE SPACE.

       PROCEDURE   DIVISION    USING   LI-AREA
                                       LI-BUF1
                                       LI-BUF2
           .
       M100-10.

      *    *** L1,BUF1 ��o�����A���̊J�n�ʒu
      *    *** L2�BUF2 �o�͑��A�J�n�ʒu
      *    *** L3,BUF1,BUF2 ��o���A�Z�b�g���钷��
      *    *** L4,BUF1 ��o�����A���X�̊J�n�ʒu

      *    *** LI-SHORI="FIRST" �ŏ���CALL��
           IF      LI-SHORI    =       "FIRST"
                   PERFORM S010-10     THRU    S010-EX

      *    *** X"0D0A" �������̂ŁACOND �Z�b�g�@�B��
                   MOVE    "COND "     TO      LI-SHORI
           END-IF



      *    *** LI-SHORI="COND " ���R�[�h���o����
      *    *** LI-SHORI="READ " ���R�[�h���o���AREAD����A�擪
           IF      LI-SHORI    =       "READ "
      *    *** L3=0 ��BUF1 �Ōオ X"0A0D" �̎�
                   IF      L3          =       ZERO

      *    *** �@
                           MOVE    LI-BUF1 (L1:LI-REC-LEN) TO
                                   LI-BUF2 (1:LI-REC-LEN)
                           MOVE    "COND "     TO      LI-SHORI

      *    *** L1= ���J�n�ʒu
      *    *** ���̃��R�[�h���o���ʒu�ݒ�
                           ADD     L1 LI-REC-LEN GIVING L1
                   ELSE

      *    *** �A
      *    *** READ ����Ă����A�c��̃��R�[�h��������
                           MOVE    SV-BUF1 (1:L3) TO   LI-BUF2 (1:L3)
                           MOVE    LI-BUF1 ( 1:LI-REC-LEN - L3) TO
                                   LI-BUF2 (L2:LI-REC-LEN - L3)
                           MOVE    "COND "     TO      LI-SHORI

      *    *** L1= ���J�n�ʒu
      *    *** ���̃��R�[�h���o���ʒu�ݒ�
                           COMPUTE L1 = 1 + ( LI-REC-LEN - L3 )

                   END-IF
           ELSE
      *    *** �B
                   MOVE    LI-BUF1 (L1:LI-REC-LEN) 
                                       TO      LI-BUF2 (1:LI-REC-LEN)
                   MOVE    "COND "     TO      LI-SHORI

      *    *** ���̃��R�[�h���o���ʒu�ݒ�
                   ADD     L1 LI-REC-LEN GIVING L1

           END-IF



      *    *** �P�[�X�Ƃ��āA�R�������Ȃ����A
      *    *** �@�A�A�A�B���R�[�h���ɂ��ׂĎ��܂��Ă������A���X�A�J�n�ʒu
      *    *** ����ł��ACOND�ŇB���R�[�h�Z�b�g������AL1 > LI-BUF1-LEN
      *    *** ��������������

           IF      L1         >       LI-BUF1-LEN
      *    *** LI-BUF1-LEN ���ɍŌ�̃��R�[�h���@�����Ă������A�B�ōŌ��
      *    *** ���R�[�h�Z�b�g���Ă���A���R�[�h������̂�,MAIN��READ�˗�
                   MOVE    "READ "      TO      LI-SHORI
                   MOVE    1            TO      L1
                   MOVE    1            TO      L2
                   MOVE    ZERO         TO      L3
      *    *** ����CALL���A�@��
           ELSE
      *    IF      L1         <=      LI-BUF1-LEN

      *    *** L4= ���X�A�J�n�ʒu
                   COMPUTE L4 = L1 + LI-REC-LEN

      *    *** L4 - 1 ���X���R�[�h���ALI-BUF1-LEN���Ɏ��܂��Ă��邩�A����
                   IF      L4 - 1      >       LI-BUF1-LEN

      *    *** ���R�[�h���ALI-BUF1-LEN���Ɏ��܂��Ă��Ȃ���
      *    *** ���C���������ɂq�d�`�c�˗�����
                           MOVE    "READ "      TO      LI-SHORI

      *    *** LI-BUF1-LEN �ōŌ�̃��R�[�h�����̃��R�[�h�ƕ�����Ă��鎞
                           COMPUTE L3 = LI-BUF1-LEN - L1 + 1
                           COMPUTE L2 = L3 + 1

      *    *** ���R�[�h�G���h�܂ł̃G���A�ޔ�����
                           MOVE    LI-BUF1 (L1:L3) TO   SV-BUF1 (1:L3)

      *    *** ����CALL���A�A��
                   ELSE

      *    *** ���R�[�h���ALI-BUF1-LEN���Ɏ��܂��Ă��鎞
      *        IF      L4 - 1 <= LI-BUF1-LEN

                           IF      L4 - 1 = LI-BUF1-LEN
                                   CONTINUE

      *    *** ���R�[�h�����x�ŏI�o�C�g�ŏI�������
      *    *** ����CALL���A�B��
                           ELSE
                                   CONTINUE
      *    *** ����CALL���A�B��
                           END-IF
                   END-IF
           END-IF



           IF      LI-BUF2 (1:1) =     X"FF"

      *    *** LI-SHORI="END  " X"FF"�̎�
                   MOVE    "END  "     TO      LI-SHORI
           ELSE
               IF  LI-BUF2 (LI-REC-LEN - 1:2) NOT = X"0D0A"
                   DISPLAY WK-PGM-NAME " BUF1 DATA X'0D0A' ���� ERROR"
                                       " �Œ蒷�̂� ������"
                   CALL    "COBDUMP" USING LI-BUF2 (LI-REC-LEN - 1:2)

                   DISPLAY WK-PGM-NAME " ���R�[�h��=" LI-REC-LEN
                   DISPLAY WK-PGM-NAME " ���R�[�h���A���J�nPOS=" L1
                   DISPLAY WK-PGM-NAME " ���R�[�h0D0A  POS=" LI-REC-LEN
                                       "-1"

                   CALL    "COBDUMP" USING LI-BUF2 (1:LI-REC-LEN)
                   STOP    RUN
               END-IF
           END-IF

           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** X"0D0A" ���݃`�F�b�N
       S010-10.

           IF      LI-BUF1-LEN >       65536 OR
                   LI-BUF1-LEN <=      ZERO
                   DISPLAY WK-PGM-NAME " BUF1 DATA MAX-LEN OVER 65536"
                                       ",ZERO,OR,MINUS ERROR"
                   DISPLAY WK-PGM-NAME " BUF1-LEN=" LI-BUF1-LEN
                   STOP    RUN
           END-IF

           PERFORM VARYING I FROM 1 BY 1
                   UNTIL   I >  LI-BUF1-LEN OR
                           SW-FIRST    =       "Y"

      *    *** �ŏ��Ɍ��ꂽX"0D0A"�܂ł̒��������R�[�h���ɃZ�b�g
                   IF      LI-BUF1 (I:2) =     X"0D0A"
                           ADD     1 I         GIVING  LI-REC-LEN
                           MOVE    "Y"         TO      SW-FIRST

      *    *** 1���ڂ�1�o�C�g�ڂ�����o��
                           MOVE    1           TO      L1
                   END-IF
           END-PERFORM

           IF      SW-FIRST    =       "N"
                   DISPLAY WK-PGM-NAME " BUF1 DATA X'0D0A' ���� ERROR"
                   STOP    RUN
           END-IF

           .
       S010-EX.
           EXIT.
