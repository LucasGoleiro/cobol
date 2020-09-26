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
           03 DATANASC
               05 DIA          PIC 9(02).
               05 MES          PIC 9(02).
               05 ANO          PIC 9(04).   
           03 EMAIL            PIC X(30).
           03 TEL              PIC 9(09).

      *----------------------------------------------------------------

       WORKING-STORAGE SECTION.
       77 W-CONT        PIC 9(06) VALUE ZEROS.
       77 W-OPCAO       PIC X(01) VALUE SPACES.
       77 W-ACT         PIC 9(02) VALUE ZEROS.
       77 MENS          PIC X(50) VALUE SPACES.
       77 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 TXTTPMED    PIC X(10) VALUE SPACES.
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
           05  LINE 01  COLUMN 01 
               VALUE  "  CADASTRO DE MEDICOS".
           05  LINE 05  COLUMN 01 
               VALUE  "  CRM :                    NOME".
           05  LINE 05  COLUMN 41 
               VALUE  ":".
           05  LINE 08  COLUMN 01 
               VALUE  "    ESPECIALIDADE  :".
           05  LINE 11  COLUMN 01 
               VALUE  "    SEXO   :         .
           05  LINE 11  COLUMN 41 
               VALUE  "O :".
           05  LINE 14  COLUMN 01 
               VALUE  "    EMAIL :                  TELEFONE".
           05  TAPELIDO
               LINE 05  COLUMN 15  PIC X(12)
               USING  APELIDO
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 43  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TEMAIL
               LINE 08  COLUMN 14  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TSEXO
               LINE 11  COLUMN 14  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 11  COLUMN 16  PIC X(15)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 11  COLUMN 45  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO2
               LINE 11  COLUMN 47  PIC X(14)
               USING  TXTGENERO2
               HIGHLIGHT.
           05  TTPAMIGO
               LINE 14  COLUMN 18  PIC 9
               USING  TPAMIGO
               HIGHLIGHT.
           05  TTXTTPAMIGO
               LINE 14  COLUMN 20  PIC X(10)
               USING  TXTTPAMIGO
               HIGHLIGHT.
      
       01  TELATA.
           05  LINE 14  COLUMN 41 VALUE  "1-CLINICA MEDICA".
           05  LINE 15  COLUMN 41 VALUE  "2-UROLOGIA".
           05  LINE 16  COLUMN 41 VALUE  "3-GINICOLOGISTA".
           05  LINE 17  COLUMN 41 
               VALUE  "4-PEDIATRIA".
           05  LINE 18  COLUMN 41 
               VALUE  "5-CARDIOLOGISTA".
           05  LINE 19  COLUMN 41 
               VALUE  "6-OFTALMOLOGIA".
           05  LINE 20  COLUMN 41 
               VALUE  "7-PSIQUIATRIA".

       PROCEDURE DIVISION.
      *************************
      * Parágrafos Principais *
      *************************
       P-ABERTURA-ARQUIVO-MEDICO.
           OPEN I-O ARQUIVO-MEDICO
           IF WS-STATUS-ARQUIVO NOT = "00"
               IF WS-STATUS-ARQUIVO = "30"
                   OPEN OUTPUT ARQUIVO-MEDICO
                   MOVE "Arquivo de estoque sendo criado..."
                       TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   CLOSE ARQUIVO-MEDICO
                   GO TO P-ABERTURA-ARQUIVO-MEDICO
               ELSE
                   MOVE "Erro na abertura do arquivo de medico..."
                       TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   GO TO P-FIM-EXIT.
       P-ABERTURA-ARQUIVO-MARGEM.
           OPEN I-O ARQUIVO-MARGEM
           IF WS-STATUS-ARQUIVO-MARGEM NOT = "00"
               IF WS-STATUS-ARQUIVO-MARGEM = "30"
                   OPEN OUTPUT ARQUIVO-MARGEM
                   MOVE "Arquivo de margens sendo criado..." 
                       TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   CLOSE ARQUIVO-MARGEM
                   GO TO P-ABERTURA-ARQUIVO-MARGEM
               ELSE
                   MOVE "Erro na abertura do arquivo de margens..."
                       TO WS-MENSAGEM
                   PERFORM P-MSG-ZERA THRU P-MSG-FIM
                   GO TO P-FIM-EXIT.
       
       P-MENU-PRINCIPAL.
           
           DISPLAY TELA-MENU-PRINCIPAL
           ACCEPT TELA-MENU-PRINCIPAL.
           IF WS-CODIGO-ESC = "C" OR "c"
               PERFORM P-CAD-ZERA-VARS THRU P-CAD-DISPLAY
           ELSE IF WS-CODIGO-ESC = "S" OR "s"
               GO TO P-FIM-CONFIRMACAO
           ELSE
               MOVE "Opcao invalida!" TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM.
           GO TO P-MENU-PRINCIPAL.
               
      **************************
      * Parágrafos de cadastro *
      **************************
       P-CAD-ZERA-VARS.
           MOVE SPACES TO NOME
         
       P-CAD-DISPLAY.
           DISPLAY TELA-CADASTRO
           ACCEPT TELA-CADASTRO.
           PERFORM P-VALIDA-FORM
           IF WS-CHAVE-VALIDACAO = "N" GO TO P-CAD-DISPLAY.
       
       P-CAD-SUCESSO.
           MOVE "Registro gravado com sucesso!" TO WS-MENSAGEM
           PERFORM P-MSG-ZERA THRU P-MSG-FIM.
       P-CAD-SAIDA.
           EXIT.
       
      ***************************
      * Parágrafos de Validação *
      ***************************
       P-VALIDA-FORM.
           
           MOVE "S" TO WS-CHAVE-VALIDACAO
           IF NOME EQUAL SPACES
               MOVE "Erro: preencha o nome." TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
           IF DIA EQUAL ZEROS
               MOVE "Erro: preencha o dia." TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
           
           IF MES EQUAL ZEROS
               MOVE "Erro: preencha o mes." TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
           
           IF ANO EQUAL ZEROS
               MOVE "Erro: preencha o ano." TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
           
           IF VALOR-CUSTO EQUAL ZEROS
               MOVE "Erro: preencha o valor de custo." TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
           
           IF VALOR-VENDA EQUAL ZEROS
               MOVE "Erro: preencha o valor de venda." TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
            DIVIDE ANO BY 4 GIVING WS-ANO-DIVISAO REMAINDER WS-ANO-RESTO
            IF MES = 02 AND
               WS-ANO-RESTO = 0 AND
               DIA > 29
               MOVE "Erro: esse mes tem no maximo 29 dias."
                   TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
            IF MES = 02 AND
               WS-ANO-RESTO NOT = 0 AND
               DIA > 28
               MOVE "Erro: esse mes tem no maximo 28 dias."
                   TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               MOVE "N" TO WS-CHAVE-VALIDACAO.
                   
      **************************
      * Parágrafos de mensagem *
      **************************
       P-MSG-ZERA.
           MOVE ZEROS TO WS-TEMPORIZADOR.
       P-MSG-DISPLAY.
           DISPLAY TELA-MENSAGEM.
       P-MSG-TEMPO.
           ADD 1 TO WS-TEMPORIZADOR
           IF WS-TEMPORIZADOR < 2500
               GO TO P-MSG-TEMPO.
       P-MSG-FIM.
           MOVE SPACES TO WS-MENSAGEM
           EXIT.
           
      *****************************
      * Parágrafos de Finalização *
      *****************************
       P-FIM-CONFIRMACAO.
           DISPLAY TELA-SAIDA
           ACCEPT TELA-SAIDA
           ACCEPT WS-CODIGO-ESC FROM ESCAPE KEY
           IF WS-CODIGO-ESC = 00
               GO TO P-FIM-FECHA-ARQUIVOS
           ELSE IF WS-CODIGO-ESC = 01
               GO TO P-MENU-PRINCIPAL
           ELSE
               MOVE "Opcao invalida!" TO WS-MENSAGEM
               PERFORM P-MSG-ZERA THRU P-MSG-FIM
               GO TO P-FIM-CONFIRMACAO.
       P-FIM-FECHA-ARQUIVOS.
           CLOSE ARQUIVO-ESTOQUE
           CLOSE ARQUIVO-MARGEM.
       P-FIM-EXIT.
           EXIT PROGRAM.
       P-FIM-STOP-RUN.
           STOP RUN.