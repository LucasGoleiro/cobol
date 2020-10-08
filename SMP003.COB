       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP003.
       AUTHOR. LUCAS COELHO DOS SANTOS.

      *******************************************
      * PROGRAMA PARA CADASTRO DE CONVENIO  *
      *******************************************
       
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODIGO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES
                    FILE STATUS IS ST-ERRO.

      *----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.
       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
           03 CODIGO           PIC 9(04).
           03 NOME             PIC X(30).
           03 PLANO            PIC 9(02).

      *----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 TXTTPMED      PIC X(10) VALUE SPACES.
       01 IND           PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       PIC X(12) VALUE SPACES.     

       01 TABCONV.
          03 TBCONV    PIC X(10) OCCURS 10 TIMES.

       01 TABCONVX.
          03 FILLER     PIC X(20) VALUE "ENF. REGIONAL".
          03 FILLER     PIC X(20) VALUE "ENF. NACIONAL".
          03 FILLER     PIC X(20) VALUE "ENF. INTERNACIONAL".
          03 FILLER     PIC X(20) VALUE "APTO PADRAO REGIONAL".
          03 FILLER     PIC X(20) VALUE "APTO PADRAO NACIONAL".
          03 FILLER     PIC X(20) VALUE "APTO PADRAO INTER".
          03 FILLER     PIC X(20) VALUE "EMERG. REGIONAL".
          03 FILLER     PIC X(20) VALUE "EMERG. NACIONAL".
          03 FILLER     PIC X(20) VALUE "EMERG. INTERNACIONAL".
          03 FILLER     PIC X(20) VALUE "PLANO GLOBAL".
           

      *-----------------------------------------------------------------

       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05 LINE 01 COLUMN 10 VALUE "*** CADASTRO DE CONVENIO***".
           05  LINE 03 COLUMN 01 
               VALUE  "CODIGO:".
           05  LINE 04 COLUMN 01 
               VALUE  "NOME:".
           05  LINE 05 COLUMN 01 
               VALUE  "PLANO:".
          
       01  TELATA.
           05  LINE 14  COLUMN 41 VALUE  "01-ENF. REGIONAL".
           05  LINE 15  COLUMN 41 VALUE  "02-ENF. NACIONAL".
           05  LINE 16  COLUMN 41 VALUE  "03-ENF. INTERNACIONAL".
           05  LINE 17  COLUMN 41 VALUE  "04-APTO PADRAO REGIONAL".
           05  LINE 18  COLUMN 41 VALUE  "05-APTO PADRAO NACIONAL".
           05  LINE 19  COLUMN 41 VALUE  "06-APTO PADRAO INTER".
           05  LINE 20  COLUMN 41 VALUE  "07-EMERG. REGIONAL".
           05  LINE 21  COLUMN 41 VALUE  "08-EMERG. NACIONAL".
           05  LINE 22  COLUMN 41 VALUE  "09-EMERG. INTERNACIONAL".
           05  LINE 23  COLUMN 41 VALUE  "10-PLANO GLOBAL".

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
       INICIO.
           MOVE "ENF. REGIONAL"        TO TBCONV(1)
           MOVE "ENF. NACIONAL   "     TO TBCONV(2)
           MOVE "ENF. INTERNACIONAL"   TO TBCONV(3)
           MOVE "APTO PADRAO REGIONAL" TO TBCONV(4)
           MOVE "APTO PADRAO NACIONA"  TO TBCONV(5)
           MOVE "APTO PADRAO INTE"     TO TBCONV(6)
           MOVE "EMERG. REGIONAL"      TO TBCONV(7)
           MOVE "EMERG. NACIONAL"      TO TBCONV(8)
           MOVE "EMERG. INTERNACIONAL" TO TBCONV(9)
           MOVE "PLANO GLOBAL"         TO TBCONV(10).
       
       R0.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCONV
                 CLOSE CADCONV
                 MOVE "*** ARQUIVO CADCONV FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
           MOVE SPACES TO NOME
           MOVE ZEROS TO CODIGO PLANO
           DISPLAY TELA2
           DISPLAY TELATA.

       R2.
           ACCEPT(03, 15) CODIGO.
           ACCEPT(04, 15) NOME.         
           ACCEPT(05, 15) PLANO.
           WRITE REGCONV.

       ROT-FIM.
           CLOSE CADCONV.
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