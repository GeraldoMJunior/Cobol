	IDENTIFICATION DIVISION.
	PROGRAM-ID. prog-002.

	DATA DIVISION.
	WORKING-STORAGE SECTION.
	01  Num1    PIC 99.
	02  Num2    PIC 99.

	PROCEDURE DIVISION.
           DISPLAY "Numero 1 : " WITH NO ADVANCING
           ACCEPT Num1
           DISPLAY "Numero 2 : " WITH NO ADVANCING
           ACCEPT Num2
           IF Num1 > Num2 THEN
               DISPLAY "Número 1 é o maior."
	   ELSE
               IF Num2 > Num1 THEN 
                   DISPLAY "Número 2 é o maior."
               ELSE
                   DISPLAY "Números são iguais."
               END-IF
           END-IF
           STOP RUN.
