      *    *** YYYYMMDD����j���v�Z
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         DATEWEEK.

       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.

       DATA                DIVISION.
       WORKING-STORAGE     SECTION.

       01  WORK-AREA.
           03  WK-PGM-NAME     PIC  X(008) VALUE "DATEWEEK".

           03  WK-URUU         PIC  X(001) VALUE SPACE.
           03  WK-R-NISUU      BINARY-LONG SYNC VALUE ZERO.
           03  WK-NISUU2       BINARY-LONG SYNC VALUE ZERO.
           03  WK-SHOU         BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY         BINARY-LONG SYNC VALUE ZERO.
           03  WK-YYYY-2       BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI-400    BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI-100    BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI-4      BINARY-LONG SYNC VALUE ZERO.
           03  WK-AMARI        BINARY-LONG SYNC VALUE ZERO.

       01  INDEX-AREA.
           03  I               BINARY-LONG SYNC VALUE ZERO.
           03  I2              BINARY-LONG SYNC VALUE ZERO.
           03  J               BINARY-LONG SYNC VALUE ZERO.
           03  K               BINARY-LONG SYNC VALUE ZERO.

       01  TBL-AREA.
      *    *** 0000�N(1)�ɃZ�b�g,9999�N(10000)�ɃZ�b�g
           03  TBL01-AREA      OCCURS 10000.
             05  TBL01-YYYY    BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-R-NISUU BINARY-LONG SYNC VALUE ZERO.
             05  TBL01-URUU    PIC  X(001) VALUE SPACE.

       01  SW-AREA.
           03  SW-FIRST        PIC  X(001) VALUE "N".

       LINKAGE             SECTION.
       
           COPY    CPDATEWEEK REPLACING ==:##:== BY ==LDW==.

      *
       PROCEDURE           DIVISION    USING   LDW-DATEWEEK-AREA.
       M100-10.

           IF      SW-FIRST    =       "N"
      *    *** TBL01 SET,�ݐϓ����Z�b�g
                   PERFORM S010-10     THRU    S010-EX
           END-IF

           EVALUATE TRUE

      *    *** LDW-DATE2-YMD ����j�� LDW-DATE2-WEEK ���߂�
               WHEN LDW-DATE2-ID = "A"
      *    *** LDW-DATE2-YMD ���̓����Z�b�g
                   PERFORM S200-10     THRU    S200-EX

      *    *** �[�N�̓������A�v�Z����
      *             PERFORM S100-10     THRU    S100-EX

      *    *** �j���v�Z����
      *    *** ���j�F1�A���j�F�Q�A�c�A�y�j�F�V
      *    *** LDW-DATE2-WEEK SET
                   PERFORM S210-10     THRU    S210-EX

      *    *** LDW-NISUU ���� LDW-DATE2-YMD ���߂�
               WHEN LDW-DATE2-ID = "R"
      *    *** LDW-DATE2-YMD ���̓����Z�b�g
                   PERFORM S300-10     THRU    S300-EX

               WHEN OTHER
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID ERROR" 
                          " LDW-DATE2-ID=" LDW-DATE2-ID
                   STOP    RUN
           END-EVALUATE
           .
       M100-EX.
           EXIT    PROGRAM.

      *    *** TBL01 SET,�ݐϓ����Z�b�g
       S010-10.
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > 9999

                   MOVE    I           TO      WK-YYYY
      *    *** �[�N����
                   PERFORM S011-10     THRU    S011-EX
                   ADD     I 1         GIVING  I2
                   MOVE    I           TO      TBL01-YYYY (I2)
                   MOVE    WK-URUU     TO      TBL01-URUU (I2)
                   IF      WK-URUU     =       "Y"
                           ADD     366         TO     WK-R-NISUU
                   ELSE
                           ADD     365         TO     WK-R-NISUU
                   END-IF
      *    *** ���̔N��12��31���܂ł̓����A0000�N12��31����366�Ƃ��āA
                   MOVE    WK-R-NISUU    TO   TBL01-R-NISUU (I2)
           END-PERFORM
           MOVE    "Y"         TO      SW-FIRST
           .
       S010-EX.
           EXIT.

      *    *** �[�N����
       S011-10.

      *    *** ���邤�N����A�S�O�O�N�Ŋ���؂��N�́A�Q���͂Q�X��
      *    *** ���邤�N����A�P�O�O�N�Ŋ���؂��N�́A�Q���͂Q�W��
      *    *** ���邤�N����A�@�@�S�N�Ŋ���؂��N�́A�Q���͂Q�X��
           DIVIDE  WK-YYYY BY 400 GIVING WK-YYYY-2
                   REMAINDER WK-AMARI-400

           IF      WK-AMARI-400 =      ZERO
                   MOVE    "Y"         TO      WK-URUU
           ELSE
               DIVIDE  WK-YYYY BY 100 GIVING WK-YYYY-2
                       REMAINDER WK-AMARI-100

               IF      WK-AMARI-100 =      ZERO
                       MOVE    "N"         TO      WK-URUU
               ELSE
                   DIVIDE  WK-YYYY BY 4   GIVING WK-YYYY-2
                           REMAINDER WK-AMARI-4

                   IF      WK-AMARI-4 =    ZERO
                       MOVE    "Y"         TO      WK-URUU
                   ELSE
                       MOVE    "N"         TO      WK-URUU
                   END-IF
               END-IF
           END-IF
           .
       S011-EX.
           EXIT.

      *    *** �Z�o���@�ύX�AS100-10 ���g�p
      *    *** �[�N�̓������A�v�Z����
       S100-10.

      *    *** LW-NISUU �̊�� 1582.1.1 => 0000.1.1 �Ƃ���
      *    *** 1582�N�ȑO�́A��O���S���I��łȂ��̂ŁA�ڈ��ł���
           MOVE    ZERO        TO      LDW-NISUU
      *     PERFORM VARYING I FROM 1582 BY 1
           PERFORM VARYING I FROM 0 BY 1
                   UNTIL I > LDW-DATE2-YYYY

                   MOVE    I           TO      WK-YYYY
      *    *** �[�N����
                   PERFORM S011-10     THRU    S011-EX

                   IF      I           =       LDW-DATE2-YYYY
                       PERFORM VARYING J FROM 1 BY 1
                              UNTIL J > LDW-DATE2-MM
                           IF      J           =       LDW-DATE2-MM
                               ADD     LDW-DATE2-DD TO      LDW-NISUU
                           ELSE
                               ADD     LDW-DATE2-DD2(J) TO  LDW-NISUU
                           END-IF
                       END-PERFORM
                   ELSE
                       IF      LDW-URUU         =       "Y"
                           ADD     366         TO      LDW-NISUU
                       ELSE
                           ADD     365         TO      LDW-NISUU
                       END-IF
                   END-IF
           END-PERFORM
           .

       S100-EX.
           EXIT.

      *    *** LDW-DATE2-YMD ���̓����Z�b�g
       S200-10.

           IF      LDW-DATE2-YMD IS    NUMERIC
               AND LDW-DATE2-MM >=     01
               AND LDW-DATE2-MM <=     12
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = A"
                           " LDW-DATE2-YMD NOT NUMERIC ERROR OR"
                           " LDW-DATE2-MM < 01 OR > 12 ERROR"
                           " LDW-DATE2-YMD=" LDW-DATE2-YMD
                   STOP    RUN
           END-IF

           MOVE    LDW-DATE2-YYYY TO   I
      *    *** �O�N�܂ł̗ݐϓ����Z�b�g
           IF      LDW-DATE2-YYYY =    ZERO
                   MOVE    ZERO        TO      LDW-NISUU
           ELSE
                   MOVE    TBL01-R-NISUU (I) TO  LDW-NISUU
           END-IF

           ADD     I 1         GIVING  I2

           IF      TBL01-URUU (I2) =   "Y"
                   MOVE    29          TO      LDW-DATE2-DD2(2)
           ELSE
                   MOVE    28          TO      LDW-DATE2-DD2(2)
           END-IF
           MOVE    TBL01-URUU (I2) TO  LDW-URUU

           IF      LDW-DATE2-DD >=     01
               AND LDW-DATE2-DD <=     LDW-DATE2-DD2(LDW-DATE2-MM)
                   CONTINUE
           ELSE
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = A"
                           " LDW-DATE2-DD < 01 OR > 28,29,30,31 ERROR"
                           " LDW-DATE2-YMD=" LDW-DATE2-YMD
                   STOP    RUN
           END-IF

      *    *** ���N�̌����܂ł̓����Z�b�g
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > LDW-DATE2-MM
                   IF      J           =       LDW-DATE2-MM
                           ADD     LDW-DATE2-DD TO      LDW-NISUU
                   ELSE
                           ADD     LDW-DATE2-DD2(J) TO  LDW-NISUU
                   END-IF
           END-PERFORM
           .
       S200-EX.
           EXIT.

      *    *** 1900.1.1 �͓��j�G1�A�j���v�Z����
      *    *** ���j�F1�A���j�F�Q�A�c�A�y�j�F�V
       S210-10.

           DIVIDE  LDW-NISUU BY 7 GIVING WK-SHOU
                   REMAINDER LDW-DATE2-WEEK

      *    *** �␳���� 0000�N1��1�������y�j���i7�j
      *    *** LDW-DATE2-WEEK => 1(��) + 6 => 7 �y

           ADD      6          TO      LDW-DATE2-WEEK

           EVALUATE TRUE
               WHEN LDW-DATE2-WEEK = 8
                   MOVE     1           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 9
                   MOVE     2           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 10
                   MOVE     3           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 11
                   MOVE     4           TO      LDW-DATE2-WEEK
               WHEN LDW-DATE2-WEEK = 12
                   MOVE     5           TO      LDW-DATE2-WEEK
               WHEN OTHER
                   CONTINUE
           END-EVALUATE
           .
       S210-EX.
           EXIT.

      *    *** LDW-NISUU ���� LDW-DATE2-YMD ���Z�b�g
       S300-10.

           MOVE    ZERO        TO      WK-NISUU2
           PERFORM TEST AFTER
                   VARYING I2 FROM 1 BY 1
                   UNTIL I2 > 10000
                    OR TBL01-R-NISUU (I2) >= LDW-NISUU
               IF      TBL01-R-NISUU (I2) >= LDW-NISUU
                   ADD     I2 -1       GIVING  I
                   MOVE    TBL01-YYYY (I2) TO  LDW-DATE2-YYYY

                   IF      TBL01-URUU (I2) =   "Y"
                        MOVE    29          TO      LDW-DATE2-DD2(2)
                   ELSE
                        MOVE    28          TO      LDW-DATE2-DD2(2)
                   END-IF

                   IF      I           =       ZERO
                       MOVE    LDW-NISUU   TO      WK-NISUU2
                   ELSE
      *    *** TBL01-R-NISUU (I) 1�N�O�̗ݐϓ����������A���N�̓������߂�
                       COMPUTE WK-NISUU2 = LDW-NISUU - TBL01-R-NISUU (I)
                   END-IF

                   PERFORM VARYING J FROM 1 BY 1
                           UNTIL J > 12
                              OR WK-NISUU2 <= LDW-DATE2-DD2(J)
                           COMPUTE WK-NISUU2 = WK-NISUU2 
                                             - LDW-DATE2-DD2(J)
                   END-PERFORM

                   IF      J          >       12
                           DISPLAY WK-PGM-NAME " LDW-DATE2-ID = R"
                                   " LDW-NISUU OVER ERROR LDW-NISUU="
                                   LDW-NISUU " J=" J
                           STOP    RUN
                   END-IF

                   MOVE    J           TO      LDW-DATE2-MM
                   MOVE    WK-NISUU2   TO      LDW-DATE2-DD
               END-IF
           END-PERFORM

           IF      I2          >       10000
                   DISPLAY WK-PGM-NAME " LDW-DATE2-ID = R"
                           " LDW-NISUU OVER ERROR LDW-NISUU=" LDW-NISUU
                           " I2=" I2
                   STOP    RUN
           END-IF

      *    DISPLAY LDW-NISUU " " LDW-DATE2-YMD
           .
       S300-EX.
           EXIT.
