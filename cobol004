       IDENTIFICATION DIVISION.
       PROGRAM-ID. BuscaQtdNum.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-Limite   PIC 9(4)    VALUE 100.
       77  WS-Busca    PIC X       VALUE "0".
       77  WS-Num      PIC 9(4).
       77  WS-Qtd      PIC 9(4).
       77  WS-Total    PIC 9(4)    VALUE 0.
       77  WS-NumFMT   PIC ZZZ9.

       PROCEDURE DIVISION.
           PERFORM VARYING WS-Num From 1 BY 1 UNTIL WS-Num > WS-Limite
              MOVE WS-Num TO WS-NumFMT
              INSPECT WS-NumFMT TALLYING WS-Qtd FOR ALL WS-Busca
              COMPUTE WS-Total = WS-Total + WS-Qtd
              MOVE 0 TO WS-Qtd
           END-PERFORM

           DISPLAY "=========================="
           DISPLAY "Quantide de números : " WS-Busca
           DISPLAY "De 0 até            : " WS-Limite
           DISPLAY "Total               : " WS-Total
           DISPLAY "=========================="

           STOP RUN.
