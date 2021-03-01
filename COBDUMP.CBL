      *    *** Open COBOL ������
      *    *** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDUMP.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
       FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Addr-Number         BINARY-LONG SYNC VALUE ZERO.
       01  Addr-Pointer        REDEFINES Addr-Number
                               POINTER.

       01  Addr-Sub            BINARY-LONG SYNC VALUE ZERO.

       01  Addr-Value          BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Length       BINARY-LONG SYNC VALUE ZERO.

       01  Buffer-Sub          BINARY-LONG SYNC VALUE ZERO.

       01  Hex-Digits          VALUE '0123456789ABCDEF'.
           05  Hex-Digit       OCCURS 16
                               PIC  X(001).

       01  Left-Nibble         PIC  9(002) COMP-5 VALUE ZERO.
       01  Nibble              REDEFINES Left-Nibble BINARY-CHAR.

       01  Right-Nibble        PIC  9(002) COMP-5 VALUE ZERO.

       01  Output-Detail       VALUE SPACE.
           05  OD-Addr.
             10  OD-Addr-Hex   OCCURS 8
                               PIC  X(001).
           05  FILLER          PIC  X(001).
           05  OD-Byte         PIC  Z(4)9.
           05  FILLER          PIC  X(0001).
           05  OD-Hex          OCCURS 16.
             10  OD-Hex-1      PIC  X(001).
             10  OD-Hex-2      PIC  X(001).
             10  FILLER        PIC  X(001).
           05  OD-ASCII        OCCURS 17
                               PIC  X(001).

       01  Output-Sub          PIC  9(002) COMP-5 VALUE ZERO.

       01  Output-Header-1.
           05  FILLER          PIC  X(080) VALUE
               '<-Addr->  Byte ' &
               '<---------------- Hexadecimal ----------------> ' &
               '<---- Char ---->'. 

       01  Output-Header-2.
           05  FILLER          PIC X(080) VALUE
               '======== ===== ' &
      *         '=============================================== ' &
      *         '================'.
               '01=02=03=04=05=06=07=08=09=10=11=12=13=14=15=16 ' &
               '====5====1====5='.

       01  PIC-XX.
           05  FILLER          PIC  X(001) VALUE LOW-VALUE.
           05  PIC-X           PIC  X(001) VALUE LOW-VALUE.
       01  PIC-Halfword        REDEFINES PIC-XX
                               PIC  9(004) COMP-X.

       01  IDX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  J2              BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.
           03  L               BINARY-LONG SYNC VALUE ZERO.
           03  M               BINARY-LONG SYNC VALUE ZERO.

       01  SW-AREA.
           05  SW-KANJI        PIC  X(001) VALUE ZERO.
           05  SW-KANJI2       PIC  X(001) VALUE ZERO.
           05  SW-KANJI3       PIC  X(001) VALUE ZERO.

       LINKAGE SECTION.

       01  Buffer              PIC  X(001) ANY LENGTH.

       01  Buffer-Len          BINARY-LONG SYNC.

       PROCEDURE DIVISION USING Buffer, OPTIONAL Buffer-Len.

       000-COBDUMP.

      *    ***  Buffer�݂̂� NUMBER-OF-CALL-PARAMETERS=1
           IF NUMBER-OF-CALL-PARAMETERS = 1
              MOVE LENGTH(Buffer) TO Buffer-Length
           ELSE
              MOVE Buffer-Len     TO Buffer-Length
      *    *** �w�肳�ꂽ�������A���ڒ��𒴂��Ă鎞�A���ڒ��ɂ���
              IF   Buffer-Len >   LENGTH(Buffer)  OR
                   Buffer-Len =   0
                   MOVE LENGTH(Buffer) TO Buffer-Length
              END-IF
           END-IF

           MOVE SPACES            TO Output-Detail
           SET Addr-Pointer       TO ADDRESS OF Buffer

           MOVE    ZERO        TO      I.
           MOVE    ZERO        TO      SW-KANJI SW-KANJI2 SW-KANJI3.

           PERFORM 100-Generate-Address
           MOVE 0 TO Output-Sub

           DISPLAY Output-Header-1 UPON SYSERR
           DISPLAY Output-Header-2 UPON SYSERR

           PERFORM VARYING Buffer-Sub FROM 1 BY 1
                   UNTIL   Buffer-Sub > Buffer-Length

                   ADD 1 TO Output-Sub

      *    *** �������肵���Ƃ��A�P�񔻒�X�L�b�v 
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

                   IF Output-Sub = 1
                      MOVE Buffer-Sub TO OD-Byte
                   END-IF

                   MOVE Buffer (Buffer-Sub : 1) TO PIC-X

      *    *** X"20"=SPACE ANK �ȊOSPACE�Z�b�g 
                   IF    ( PIC-X < X"20")
                      OR ( PIC-X = X"7F")
                      OR ( PIC-X = X"A0")
                      OR ( PIC-X >= X"FD" AND <= X"FF")
      *****                SW-KANJI = ZERO
      *****           OR ( PIC-X = X"81")
      *    *** �����\����������
      *    *** X"81" ������ƕ\�����Ȃ��ׁA�X�y�[�X�ɂ���
      *               OR ( PIC-X > X"7E" AND PIC-X < X"A1")
      *               OR ( PIC-X > X"DF")
      *    *** SPACE �N���A�[���Ă���̂ŁA�s�v
                            MOVE SPACE TO OD-ASCII (Output-Sub)
                   ELSE
                       IF   SW-KANJI3 = "1" AND
                            OUTPUT-SUB = 1
                            MOVE SPACE TO OD-ASCII (Output-Sub)
                            MOVE ZERO  TO SW-KANJI3
                       ELSE
                            MOVE PIC-X TO OD-ASCII (Output-Sub)
                       END-IF
                   END-IF

                   DIVIDE PIC-Halfword BY 16
                          GIVING Left-Nibble
                          REMAINDER Right-Nibble

                   ADD 1 TO Left-Nibble Right-Nibble

                   MOVE Hex-Digit (Left-Nibble)
                           TO OD-Hex-1 (Output-Sub)

                   MOVE Hex-Digit (Right-Nibble)
                           TO OD-Hex-2 (Output-Sub)

                   IF  Output-Sub = 16
                       IF  SW-KANJI = "1"
                           ADD   Buffer-Sub 1 GIVING I2
                           MOVE  Buffer (I2:1)  TO OD-ASCII(17)
                           MOVE  "1"        TO     SW-KANJI3
                       END-IF

                       DISPLAY Output-Detail UPON SYSERR END-DISPLAY

                       MOVE SPACES TO Output-Detail
                       MOVE 0 TO Output-Sub

                       SET Addr-Pointer UP BY 16
                       PERFORM 100-Generate-Address
                   END-IF
           END-PERFORM

           IF  Output-Sub > 0
               DISPLAY Output-Detail UPON SYSERR
           END-IF 
       EXIT PROGRAM.
           EXIT.

       100-Generate-Address.
      *    *** �A�h���X�g�d�w8�����A4�o�C�g��
           MOVE 8 TO Addr-Sub
           MOVE Addr-Number TO Addr-Value

      *    *** Addr-Value�ɂ́A�_���v�Ώۂ̃A�h���X�����Ă���
           MOVE ALL '0' TO OD-Addr

           PERFORM WITH TEST BEFORE 
                   UNTIL Addr-Value = 0

      *    *** �A�h���X16�Ŋ��闝�R�A10�i������16�i���ɕϊ����Ă���A
      *    *** Nibble�͗]��Ȃ̂ŁA�Y���Ɏg���Ă���

                   DIVIDE Addr-Value BY 16
                          GIVING Addr-Value
                          REMAINDER Nibble

                   ADD 1 TO Nibble
                   MOVE Hex-Digit (Nibble)
                        TO OD-Addr-Hex (Addr-Sub)
                   SUBTRACT 1 FROM Addr-Sub
           END-PERFORM.
       100-EX.
           EXIT.

       S210-10.

      *    *** SJIS �����͈�
           IF    ( Buffer (I:2) >= X"8140" AND 
                   Buffer (I:2) <= X"9FFC" )   OR
                 ( Buffer (I:2) >= X"E040" AND 
                   Buffer (I:2) <= X"EAA4" )
                   MOVE   "1"  TO     SW-KANJI
                                      SW-KANJI2
           ELSE
                   MOVE   ZERO TO     SW-KANJI
           END-IF.
       S210-EX.
           EXIT.
