       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2NN.
      *AUTHOR. ANGELO LOTIERZO FILHO.
      ********************************************************
      * MANUTENCAO DO CADASTRO DE AMIGOS   *
      ********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADAMIGO ASSIGN TO DISK
                      ORGANIZATION IS INDEXED
                      ACCESS MODE  IS DYNAMIC
                      RECORD KEY   IS APELIDO
                      ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                      FILE STATUS  IS ST-ERRO.
      *
           SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP-NUMCEP
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS CEP-LOGRA
                                   WITH DUPLICATES.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADAMIGO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CAMIGO.DAT". 
       01 REGAMIGO.
          03 APELIDO        PIC X(12).
          03 NOME           PIC X(30).
          03 EMAIL          PIC X(30).
          03 SEXO           PIC X(01).
          03 GENERO         PIC X(01).
          03 TPAMIGO        PIC 9(01).
          03 CEPAMIGO       PIC 9(08).  
          03 NUMLOGRA       PIC 9(04).
          03 COMPLOGRA      PIC X(12).
      *
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
                03 CEP-NUMCEP        PIC 9(08).
                03 CEP-LOGRA         PIC X(30).
                03 CEP-BAIRRO        PIC X(20).
                03 CEP-CIDADE        PIC X(20).
                03 CEP-UF            PIC X(02).
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
           05  LINE 03  COLUMN 01 
               VALUE  "                            [ CADASTRO D".
           05  LINE 03  COLUMN 41 
               VALUE  "E AMIGOS ]".
           05  LINE 06  COLUMN 01 
               VALUE  "    APELIDO                      NOME".
           05  LINE 08  COLUMN 01 
               VALUE  "    EMAIL".
           05  LINE 10  COLUMN 01 
               VALUE  "    SEXO".
           05  LINE 10  COLUMN 41 
               VALUE  "GENERO".
           05  LINE 12  COLUMN 01 
               VALUE  "    TIPO DE AMIGO".
           05  LINE 14  COLUMN 01 
               VALUE  "    --------------------------[ E N D E".
           05  LINE 14  COLUMN 41 
               VALUE  "R E C O ]-------------------------------".
           05  LINE 15  COLUMN 01 
               VALUE  "    CEP :            ENDERECO :".
           05  LINE 15  COLUMN 41 
               VALUE  "                        NUM :".
           05  LINE 17  COLUMN 01 
               VALUE  "    COMPLEM.:               BAIRRO :".
           05  LINE 19  COLUMN 01 
               VALUE  "    CIDADE :                         UF".
           05  LINE 19  COLUMN 41 
               VALUE  ":".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TAPELIDO
               LINE 06  COLUMN 13  PIC X(12)
               USING  APELIDO
               HIGHLIGHT.
           05  TNOME
               LINE 06  COLUMN 39  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TEMAIL
               LINE 08  COLUMN 11  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TSEXO
               LINE 10  COLUMN 11  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTXTSEXO
               LINE 10  COLUMN 13  PIC X(12)
               USING  TXTSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 10  COLUMN 48  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO
               LINE 10  COLUMN 50  PIC X(15)
               USING  TXTGENERO
               HIGHLIGHT.
           05  TTPAMIGO
               LINE 12  COLUMN 19  PIC 9
               USING  TPAMIGO
               HIGHLIGHT.
           05  TTXTTPAMIGO
               LINE 12  COLUMN 21  PIC X(15)
               USING  TXTTPAMIGO
               REVERSE-VIDEO.
           05  TCEPAMIGO
               LINE 15  COLUMN 11  PIC 99999.999
               USING  CEPAMIGO
               HIGHLIGHT.
           05  TCEP-LOGRA
               LINE 15  COLUMN 33  PIC X(30)
               USING  CEP-LOGRA
               HIGHLIGHT.
           05  TNUMLOGRA
               LINE 15  COLUMN 71  PIC ZZZ9
               USING  NUMLOGRA
               HIGHLIGHT.
           05  TCOMPLOGRA
               LINE 17  COLUMN 15  PIC X(12)
               USING  COMPLOGRA
               HIGHLIGHT.
           05  TCEP-BAIRRO
               LINE 17  COLUMN 38  PIC X(20)
               USING  CEP-BAIRRO
               HIGHLIGHT.
           05  TCEP-CIDADE
               LINE 19  COLUMN 14  PIC X(20)
               USING  CEP-CIDADE
               HIGHLIGHT.
           05  TCEP-UF
               LINE 19  COLUMN 43  PIC X(02)
               USING  CEP-UF
               HIGHLIGHT.

      *
       01  TELATA.
           05  LINE 14  COLUMN 41 VALUE  "1-FACULDADE".
           05  LINE 15  COLUMN 41 VALUE  "2-IGREJA".
           05  LINE 16  COLUMN 41 VALUE  "3-BAIRRO".
           05  LINE 17  COLUMN 41 
               VALUE  "4-COLEGIO".
           05  LINE 18  COLUMN 41 
               VALUE  "5-BALADA".
           05  LINE 19  COLUMN 41 
               VALUE  "6-FUTEBOL".
           05  LINE 20  COLUMN 41 
               VALUE  "7-VIAGEM".
           05  LINE 21  COLUMN 41 
               VALUE  "8-EX (A)".
           05  LINE 22  COLUMN 41 
               VALUE  "9-NAMORADA(0) EX(A)".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "FACULDADE" TO TBAMIGO(1)
           MOVE "IGREJA   " TO TBAMIGO(2)
           MOVE "BAIRRO"    TO TBAMIGO(3)
           MOVE "COLEGIO"   TO TBAMIGO(4)
           MOVE "BALADA"    TO TBAMIGO(5)
           MOVE "FUTEBOL"   TO TBAMIGO(6)
           MOVE "VIAGEM"    TO TBAMIGO(7)
           MOVE "EX"        TO TBAMIGO(8)
           MOVE "NAMOR.EX"  TO TBAMIGO(9).
       
       R0.
           OPEN I-O CADAMIGO
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADAMIGO
                 CLOSE CADAMIGO
                 MOVE "*** ARQUIVO CADAMIGO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO =>" TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *
       R0A.
           OPEN INPUT CADCEP
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
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO APELIDO NOME EMAIL SEXO GENERO
           MOVE SPACES TO TXTGENERO TXTSEXO TXTTPAMIGO
           MOVE ZEROS TO TPAMIGO W-SEL.
           MOVE SPACES TO COMPLOGRA CEP-LOGRA CEP-BAIRRO 
                                       CEP-CIDADE CEP-UF
           MOVE ZEROS TO CEPAMIGO NUMLOGRA
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELA2.
       R2.
           ACCEPT TAPELIDO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       LER-CADMAMIGO.
           READ CADAMIGO
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R5A
                PERFORM R6A
                PERFORM R7A
                PERFORM R8A
                DISPLAY TELA2
                MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADAMIGO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** AMIGO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
 
       R4.
           ACCEPT TEMAIL.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R5.
           ACCEPT TSEXO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       R5A.
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO TXTSEXO
           ELSE
              IF SEXO = "F"
                 MOVE "FEMENINO" TO TXTSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           DISPLAY TTXTSEXO.
       R6.
           MOVE 1 TO IND
           ACCEPT TGENERO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               DISPLAY TTXTGENERO.
       R7.
           DISPLAY TELATA
           ACCEPT TTPAMIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA2
                   GO TO R6.
           IF TPAMIGO = 0 
                 MOVE "*** DIGITE APENAS DE 1 ATE 9 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R7A.
           MOVE TBAMIGO(TPAMIGO) TO TXTTPAMIGO
           DISPLAY TTXTTPAMIGO.
           DISPLAY TELA2.
      *
       R8.
           ACCEPT TCEPAMIGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO R7.
           IF CEPAMIGO = 0 
                 MOVE "*** CEP NAO INFORMADO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
      *
       R8A.
           MOVE CEPAMIGO TO CEP-NUMCEP
           READ CADCEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "23"
                 MOVE "* CEP NAO CADASTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO CEP " TO MENS1
                 MOVE ST-ERRO TO MENS2
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.
           DISPLAY TELA2.
        
       R9.
           ACCEPT TNUMLOGRA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO R8.
           IF CEPAMIGO = 0 
                 MOVE "* NUMERO RESIDIDENCIA NAO INFORMADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R9.
       R10.
           ACCEPT TCOMPLOGRA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO  R10.
           

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R10.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* AMIGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADAMIGO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO AMIGO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R10.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADAMIGO.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
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

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      

