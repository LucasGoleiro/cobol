       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP002.
       AUTHOR. LUCAS COELHO DOS SANTOS.

      *******************************************
      * PROGRAMA PARA CADASTRO DE CID(DOENCA)  *
      *******************************************
       
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCID ASSIGN TO DISK
                     ORGANIZATION IS INDEXED
                     ACCESS MODE IS DYNAMIC
                     RECORD KEY IS CODIGO
                     ALTERNATE RECORD KEY IS DENOMI WITH DUPLICATES
                     FILE STATUS IS ST-ERRO.

      *----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.
       FD CADCID
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCID.DAT". 
       01 REGCID.
           03 CODIGO              PIC 9(04).
           03 DENOMI              PIC X(30).
           
      *----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 TXTTPCID      PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.

      *-----------------------------------------------------------------

       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05 LINE 01 COLUMN 10 VALUE "*** CADASTRO DE CID***".
           05  LINE 05 COLUMN 01 
               VALUE  "CODIGO DA CID:".
           05  LINE 07 COLUMN 01 
               VALUE  "DENOMINACAO DA CID:".
              
      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
       INICIO.

       R0.
           OPEN I-O CADCID
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCID
                 CLOSE CADCID
                 MOVE "*** ARQUIVO CADCID FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCID" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE ZEROS TO CODIGO.
           MOVE SPACES TO DENOMI.      
           DISPLAY TELA2.

       R2.
           ACCEPT(05, 17) CODIGO.
           ACCEPT(07, 20) DENOMI.         
           WRITE REGCID.

       ROT-FIM.
           CLOSE CADCID.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]----------------------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
        