       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP001.
       AUTHOR. LUCAS COELHO DOS SANTOS.

      *******************************************
      * PROGRAMA PARA CADASTRO DE VEICULOS  *
      *******************************************
       
      *----------------------------------------------------------------
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADMED ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CRM
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    FILE STATUS IS ST-ERRO.

      *----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.
       FD CADMED
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADMED.DAT". 
       01 REGMED.
           03 CRM              PIC 9(06).
           03 NOME             PIC X(30).
           03 ESPECIALIDADE    PIC 9(02).
           03 SEXO             PIC X(01).
           03 DATANASC         PIC 9(08).
           03 EMAIL            PIC X(30).
           03 TELEFONE         PIC 9(09).

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

       01 TABMED.
          03 TBMED    PIC X(10) OCCURS 9 TIMES.

       01 TABSEXOX.
          03 FILLER     PIC X(15) VALUE "FFEMININO".
          03 FILLER     PIC X(15) VALUE "MMASCULINO".
          
       01 TABSEXO REDEFINES TABSEXOX.
          03 TBSEXO   PIC X(15) OCCURS 2 TIMES.
       01 TXTSEXO.
          03 TXTSEXO1 PIC X(01) VALUE SPACES.
          03 TXTSEXO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------

       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01 COLUMN 01 
               VALUE  "CRM:".
           05  LINE 02 COLUMN 01 
               VALUE  "NOME:".
           05  LINE 03 COLUMN 01 
               VALUE  "ESPECIALIDADE:".
           05  LINE 04 COLUMN 01 
               VALUE  "SEXO:".
           05  LINE 05 COLUMN 01 
               VALUE  "DATA DE NASCMENTO:".
           05  LINE 06 COLUMN 01 
               VALUE  "EMAIL:".
           05  LINE 07  COLUMN 01
               VALUE  "TELEFONE:".
  
           05  TSEXO
               LINE 11  COLUMN 14  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 11  COLUMN 16  PIC X(15)
               USING  TEXSEXO
               HIGHLIGHT.
      
       01  TELATA.
           05  LINE 14  COLUMN 41 VALUE  "1-CLINICA MEDICA".
           05  LINE 15  COLUMN 41 VALUE  "2-UROLOGIA".
           05  LINE 16  COLUMN 41 VALUE  "3-GINICOLOGISTA".
           05  LINE 17  COLUMN 41 VALUE  "4-PEDIATRIA".
           05  LINE 18  COLUMN 41 VALUE  "5-CARDIOLOGISTA".
           05  LINE 19  COLUMN 41 VALUE  "6-OFTALMOLOGIA".
           05  LINE 20  COLUMN 41 VALUE  "7-PSIQUIATRIA".

      *-----------------------------------------------------------------

       PROCEDURE DIVISION.
       INICIO.

       R0.
           OPEN I-O CADMED
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADMED
                 CLOSE CADMED
                 MOVE "*** ARQUIVO CADMED FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMED" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE ZEROS TO CRM ESPECIALIDADE DATANASC TELEFONE.
           MOVE SPACES TO NOME SEXO EMAIL.      
           DISPLAY TELA2.

       R2.
           ACCEPT(01, 15) CRM.
           ACCEPT(02, 15) NOME.         
           ACCEPT(03, 15) ESPECIALIDADE.
           ACCEPT(04, 15) SEXO.
           ACCEPT(05, 15) DATANASC. 
           ACCEPT(06, 15) EMAIL. 
           ACCEPT(07, 15) TELEFONE.
           WRITE REGMED.

       ROT-FIM.
           CLOSE CADMED.
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

       