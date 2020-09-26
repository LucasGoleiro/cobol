       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALURA01.
       AUTHOR. LUCAS COELHO DOS SANTOS.

      **************************************
      * TESTES CURSO ALURA   *
      **************************************

      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WRK-DATA.
           02 WRK-ANO          PIC 9(04) VALUE ZEROS.
           02 WRK-MES          PIC 9(02) VALUE ZEROS.
           02 WRK-DIA          PIC 9(02) VALUE ZEROS.

      *> 01  WRK-TESTE           PIC X(30) VALUE "LUCAS".
           
      *----------------------------------------------------------------

       PROCEDURE DIVISION.
  
       P-INICIO.
           ACCEPT WRK-DATA FROM DATE YYYYMMDD.
      *>     MOVE "20200922" TO WRK-DATA.
           DISPLAY WRK-DATA.
      *>     ACCEPT WRK-TESTE FROM CONSOLE.
      *>     MOVE "LARISSA" TO WRK-TESTE.
      *>     DISPLAY "NOME: " WRK-TESTE.

     
       P-FIM-EXIT.
           EXIT PROGRAM.

       P-FIM-STOP-RUN.
           STOP RUN.
       