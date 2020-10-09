       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMP004.
      *AUTHOR. LUCAS COELHO DOS SANTOS.
      ********************************************************
      * CADASTRO DE PACIENTES   *
      ********************************************************
      *----------------------------------------------------------------

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADPACI ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS CPF
                      ALTERNATE RECORD KEY IS NOMEP WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODIGO
                    ALTERNATE RECORD KEY IS ENDERECO WITH DUPLICATES
                    FILE STATUS IS ST-ERRO.

           SELECT CADCONV ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS CODIGOC
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                    ALTERNATE RECORD KEY IS PLANO WITH DUPLICATES
                    FILE STATUS IS ST-ERRO.
      *-----------------------------------------------------------------

       DATA DIVISION.
       FILE SECTION.
       FD CADPACI
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPACI.DAT". 
       01 REGPACI.
          03 CPF            PIC 9(04).
          03 NOMEP           PIC X(30).
          03 SEXO           PIC X(01).
          03 DATANASC       PIC 9(08).
          03 GENERO         PIC X(01).
          03 CONVENIO       PIC 9(04).
          03 CEPPACI        PIC 9(08).  
          03 NUMENDE        PIC 9(04).
          03 COMPLEM        PIC X(10).
          03 TELEFONE       PIC 9(11).
          03 EMAIL          PIC X(30).
      *
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT". 
       01 REGCEP.
           03 CODIGO           PIC 9(08).
           03 ENDERECO         PIC X(30).
           03 BAIRRO           PIC X(20).
           03 CIDADE           PIC X(20).
           03 ESTADO           PIC X(02).
           03 CEP-UF            PIC X(02).

       FD CADCONV
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCONV.DAT". 
       01 REGCONV.
           03 CODIGOC           PIC 9(04).
           03 NOME             PIC X(30).
           03 PLANO            PIC 9(02).
      *-----------------------------------------------------------------

        WORKING-STORAGE SECTION.
       77 W-CONT          PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT             PIC 9(02) VALUE ZEROS.
       01 MENS.
          02 MENS1       PIC X(48) VALUE SPACES.
          02 MENS2       PIC ZZZ.
       77 LIMPA              PIC X(50) VALUE SPACES. 
       01 ST-ERRO        PIC X(02) VALUE "00".
       01 W-SEL             PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND                  PIC 9(02) VALUE ZEROS.
       01 TXTSEXO       PIC X(12) VALUE SPACES.

       01 TABAMIGO.
          03 TBAMIGO    PIC X(10) OCCURS 9 TIMES.

       01 TABGENEROX.
          03 FILLER     PIC X(15) VALUE "THETEROSEXUAL".
          03 FILLER     PIC X(15) VALUE "HHOMESEXUAL".
          03 FILLER     PIC X(15) VALUE "BBISSESUXUAL".
          03 FILLER     PIC X(15) VALUE "PPANSEXUAL".
          03 FILLER     PIC X(15) VALUE "AA           ".
          03 FILLER     PIC X(15) VALUE "BB           ".
          03 FILLER     PIC X(15) VALUE "CC           ".
          03 FILLER     PIC X(15) VALUE "DD            ".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 9 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------

       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 10 VALUE  "** CADASTRO DE PACIENTES **".
           05  LINE 02  COLUMN 01 VALUE  "CPF:".
           05  LINE 03  COLUMN 01 VALUE  "NOME:".
           05  LINE 04  COLUMN 01 VALUE  "SEXO:".
           05  LINE 05  COLUMN 01 VALUE  "DATA DE NASCIMENTO:".
           05  LINE 06  COLUMN 01 VALUE  "GENERO:".
           05  LINE 07  COLUMN 01 VALUE  "CONVENIO:".
           05  LINE 09  COLUMN 10 VALUE  "** ENDERECO DO PACIENTE **".
           05  LINE 11  COLUMN 01 VALUE  "CEP:".
           05  LINE 12  COLUMN 01 VALUE  "NUMERO:".
           05  LINE 13  COLUMN 01 VALUE  "COMPLEMENTO:".
           05  LINE 14  COLUMN 01 VALUE  "TELEFONE:".
           05  LINE 15  COLUMN 01 VALUE  "EMAIL:".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

       R0.
           OPEN I-O CADPACI
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPACI
                 CLOSE CADPACI
                 MOVE "*** ARQUIVO CADPACI FOI CRIADO **" TO MENS1
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPACI" TO MENS1
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
             
       R0A.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CADCEP NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0B.
           OPEN I-O CADCONV
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO CADCONV NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCONV " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R1.
           MOVE ZEROS TO CPF DATANASC CONVENIO CEPPACI NUMENDE TELEFONE.
           MOVE SPACES TO NOME SEXO COMPLEM EMAIL.      
           DISPLAY TELA2.

       R2.
           ACCEPT(02, 20) CPF.
           ACCEPT(03, 20) NOME. 
           ACCEPT(04, 20) SEXO.
           ACCEPT(05, 20) DATANASC.
           ACCEPT(06, 20) GENERO.
           ACCEPT(07, 20) CONVENIO.
           ACCEPT(11, 20) CEPPACI.
           ACCEPT(12, 20) NUMENDE.
           ACCEPT(13, 20) COMPLEM.
           ACCEPT(14, 20) TELEFONE.
           ACCEPT(15, 20) EMAIL.
           WRITE REGPACI.

       ROT-FIM.
           CLOSE CADPACI.
           CLOSE CADCONV.
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


