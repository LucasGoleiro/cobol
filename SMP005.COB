       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP005.
       AUTHOR. LUCAS COELHO DOS SANTOS.

      *******************************************
      * PROGRAMA PARA CADASTRO DE CEP  *
      *******************************************
       
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODIGO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES
                    FILE STATUS IS ST-ERRO.

      *----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
           03 CODIGO           PIC 9(08).
           03 ENDERECO         PIC X(30).
           03 BAIRRO           PIC X(20).
           03 CIDADE           PIC X(20).
           03 ESTADO           PIC X(02).

      *----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 IND           PIC 9(02) VALUE ZEROS.
       
       01 TABESTX.
           03 FILLER     PIC X(25) VALUE "ACRE".
           03 FILLER     PIC X(25) VALUE "ALAGOAS".
           03 FILLER     PIC X(25) VALUE "AMAPA".
           03 FILLER     PIC X(25) VALUE "AMAZONAS".
           03 FILLER     PIC X(25) VALUE "BAHIA".
           03 FILLER     PIC X(25) VALUE "CEARA".
           03 FILLER     PIC X(25) VALUE "DISTRITO FEDERAL".
           03 FILLER     PIC X(25) VALUE "ESPIRITO SANTO".
           03 FILLER     PIC X(25) VALUE "GOIAS".
           03 FILLER     PIC X(25) VALUE "MARANHAO".
           03 FILLER     PIC X(25) VALUE "MATO GROSSO".
           03 FILLER     PIC X(25) VALUE "MATO GROSSO DO SUL".
           03 FILLER     PIC X(25) VALUE "MINAS GERAIS".
           03 FILLER     PIC X(25) VALUE "PARA".
           03 FILLER     PIC X(25) VALUE "PARAIBA".
           03 FILLER     PIC X(25) VALUE "PARANA".
           03 FILLER     PIC X(25) VALUE "PERNAMBUCO".
           03 FILLER     PIC X(25) VALUE "PIAUI".
           03 FILLER     PIC X(25) VALUE "RIO DE JANEIRO".
           03 FILLER     PIC X(25) VALUE "RIO GRANDE DO NORTE".
           03 FILLER     PIC X(25) VALUE "RIO GRANDE DO SUL".
           03 FILLER     PIC X(25) VALUE "RONDONIA".
           03 FILLER     PIC X(25) VALUE "RORAIMA".
           03 FILLER     PIC X(25) VALUE "SANTA CATARINA".
           03 FILLER     PIC X(25) VALUE "SAO PAULO".
           03 FILLER     PIC X(25) VALUE "SERGIPE".
           03 FILLER     PIC X(25) VALUE "TOCANTINS".

       01 TABEST REDEFINES TABESTX.
           03 TBEST   PIC X(25) OCCURS 27 TIMES.
       
      *----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "CODIGO: ". 

           05  LINE 02  COLUMN 01 
               VALUE  "ENDERECO: ". 

           05  LINE 03  COLUMN 01 
               VALUE  "BAIRRO: ". 

           05  LINE 04  COLUMN 01 
               VALUE  "CIDADE: ".

           05  LINE 05  COLUMN 01 
               VALUE  "ESTADO: ".   

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "*** ARQUIVO CADCEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE ZEROS TO CODIGO.
           MOVE SPACES TO ENDERECO BAIRRO CIDADE ESTADO.      
           DISPLAY TELA2.

       R2.
           ACCEPT(01, 20) CODIGO. 
           ACCEPT(02, 20) ENDERECO.
           ACCEPT(03, 20) BAIRRO.
           ACCEPT(04, 20) CIDADE.
           ACCEPT(05, 20) ESTADO.
           WRITE REGCEP.

       ROT-FIM.
           CLOSE CADCEP.
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