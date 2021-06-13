       IDENTIFICATION DIVISION.
       PROGRAM-ID. ArqSequncial.
       AUTHOR. Geraldo Jr.
       DATE-WRITTEN. 13/06/2021.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Arq-Mov ASSIGN TO "posto.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FStatus.

	   DATA DIVISION.
       FILE SECTION.
       FD Arq-Mov.
       01  Abast.
           88  Fim-Mov         VALUE HIGH-VALUES.
           02  Mov-Data        PIC X(8).
           02  Mov-Tipo        PIC X.
           02  Mov-Litros      PIC 9(3)V99.
	   WORKING-STORAGE SECTION.
       01  FStatus     PIC XX.
       01  GValor      PIC 99V99   VALUE 3.30.
       01  GDesc1      PIC 9V99    VALUE 0.04.
       01  GDesc2      PIC 9V99    VALUE 0.06.
       01  AValor      PIC 99V99   VALUE 3.30.
       01  ADesc1      PIC 9V99    VALUE 0.04.
       01  ADesc2      PIC 9V99    VALUE 0.06.
       01  Desconto    PIC 9V99.
       01  SubT        PIC 9(4)V99.
       01  VDesc       PIC 9(4)V99.
       01  Tot         PIC 9(4)V99.
       01  TotSD       PIC 9(4)V99 VALUE ZEROS.
       01  TotCD       PIC 9(4)V99 VALUE ZEROS.
       01  TotD        PIC 9(4)V99 VALUE ZEROS.

       PROCEDURE DIVISION.
           DISPLAY "Básico Arquivo Sequencial"
           DISPLAY "========================="
           OPEN INPUT Arq-Mov.
           IF FStatus NOT = "35" THEN
               DISPLAY "Data      T  Litros  SubT     VDesc    Tot"
               DISPLAY "=============================================="
               READ Arq-Mov
                   AT END SET Fim-Mov TO TRUE
               END-READ
               PERFORM UNTIL Fim-Mov
                   EVALUATE Mov-Tipo
                       WHEN "G"
                       WHEN "g"                       
                           IF Mov-Litros <= 20 THEN
                               MOVE GDesc1 TO Desconto
                           ELSE
                               MOVE GDesc2 TO Desconto
                           END-IF
                           COMPUTE SubT = Mov-Litros * GValor
                           COMPUTE VDesc = SubT * Desconto
                           COMPUTE Tot = Subt - VDesc
                       WHEN "A"
                       WHEN "a"                       
                           IF Mov-Litros <= 20 THEN
                               MOVE ADesc1 TO Desconto
                           ELSE
                               MOVE ADesc2 TO Desconto
                           END-IF
                           COMPUTE SubT = Mov-Litros * AValor
                           COMPUTE VDesc = SubT * Desconto
                           COMPUTE Tot = Subt - VDesc
                   END-EVALUATE
                   DISPLAY Mov-Data "  " Mov-Tipo "  " Mov-Litros
                           "  " SubT "  " VDesc "  " Tot
                   COMPUTE TotSD = TotSD + SubT
                   COMPUTE TotCD = TotCD + Tot
                   COMPUTE TotD = TotD + VDesc
                   READ Arq-Mov
                       AT END SET Fim-Mov TO TRUE
                   END-READ
               END-PERFORM
               DISPLAY "----------------------------------------------"
               DISPLAY "Total                " TotSD "  " TotD
                       "  " TotCD
               DISPLAY "----------------------------------------------"
           ELSE
               DISPLAY "Arquivo não encontrado."
           END-IF
           CLOSE Arq-Mov.
           STOP RUN.
