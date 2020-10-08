
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
