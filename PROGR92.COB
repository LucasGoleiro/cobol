       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXERC01.
      *AUTOR Lucas Coelho dos Santos
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
       SELECT CADVEIC ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE IS DYNAMIC
                    RECORD KEY IS PLACA
                    ALTERNATE RECORD KEY IS NOMEPROP WITH DUPLICATES
                    FILE STATUS IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADVEIC
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADVEIC.DAT".
       01 REGVEIC.
          03 PLACA          PIC x(07).
          03 NOMEPROP       PIC x(30).
          03 MARCA          PIC 9(02).
          03 MODELO         PIC x(20).
          03 COR            PIC x(01).
          03 ANOFAB         PIC 9(04).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 TPLACA.
	    03 LETRAP  	    PIC x(03).
 	    03 NUMEROP 	    PIC 9(04).
       01 TNOMEPROP         PIC x(30).
       01 TMARCA            PIC 9(02).
       01 TMODELO 	    PIC x(20).
       01 TCOR	            PIC x(01).
       01 TANOFAB 	    PIC 9(04).
       01 TABMARCA.
	   03 TBMARCA	    PIC x(10) OCCURS 5 TIMES.
       01 TABCORX.
	   03 FILLER	    PIC x(10) VALUE "VVERMELHO".
	   03 FILLER	    PIC x(10) VALUE "AAZUL".
	   03 FILLER	    PIC x(10) VALUE "PPRETO".
	   03 FILLER	    PIC x(10) VALUE "BBRANCO".
	   03 FILLER	    PIC x(10) VALUE "DVERDE".	   
       01 TABCOR REDEFINES TABCORX.
	   03 TBCOR	    PIC x(10) OCCURS 5 TIMES.
       01 TXTCOR.
	   03 TXTCOR1	    PIC x(01) VALUE SPACES.
	   03 TXTCOR2       PIC x(09) VALUE SPACES.
       01 IND               PIC 9(02) VALUE ZEROS.
       01 ST-ERRO           PIC x(02) VALUE "00".
       01 MSGERRO           PIC x(40) VALUE SPACES.
       77 W-ACT             PIC 9(02) VALUE ZEROS.

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01 TELA.
           05 BLANK SCREEN.
           05 LINE 5 COLUMN 7 VALUE "INSIRA A PLACA DO VEICULO:".
           05 TPLACA
		LINE 5 COLUMN 47  PIC x(07)
           	USING PLACA
           	HIGHLIGHT.
	   05 LINE 7 COLUMN 7 VALUE "INSIRA O NOME DO PROPRIETARIO:".
           05 TNOMEPROP
		LINE 7 COLUMN 47  PIC x(30)
           	USING NOMEPROP
           	HIGHLIGHT.
	   05 LINE 9 COLUMN 7 VALUE "INSIRA A MARCA DO VEICULO:".
           05 TMARCA
		LINE 9 COLUMN 46  PIC x(02)
           	USING MARCA
           	HIGHLIGHT.
	   05 LINE 11 COLUMN 7 VALUE "INSIRA O MODELO DO CARRO:".
           05 TMODELO
		LINE 11 COLUMN 47  PIC x(20)
           	USING MODELO
           	HIGHLIGHT.
	   05 LINE 13 COLUMN 7 VALUE "INSIRA A COR DO VEICULO:".
           05 TCOR
		LINE 13 COLUMN 47  PIC x(01)
           	USING COR
           	HIGHLIGHT.
	   05 LINE 15 COLUMN 7 VALUE "INSIRA O ANO DE FABRICACAO:".
           05 TANOFAB
		LINE 15 COLUMN 47  PIC x(04)
           	USING ANOFAB
           	HIGHLIGHT.

       01 TELAM.
           05 LINE 18 COLUMN 7 VALUE "MARCAS DE VEICULOS:".
           05 LINE 19 COLUMN 7 VALUE " 1 - CHEVROLET".
           05 LINE 20 COLUMN 7 VALUE " 2 - VOLKSWAGEN".
           05 LINE 21 COLUMN 7 VALUE " 3 - FIAT".
           05 LINE 22 COLUMN 7 VALUE " 4 - RENAULT".
           05 LINE 23 COLUMN 7 VALUE " 5 - FORD".

       01 TELAC.
           05 LINE 18 COLUMN 30 VALUE "CORES DO VEICULO:".
           05 LINE 19 COLUMN 30 VALUE " V - VERMELHO".
           05 LINE 20 COLUMN 30 VALUE " A - AZUL".
           05 LINE 21 COLUMN 30 VALUE " P - PRETO".
           05 LINE 22 COLUMN 30 VALUE " B - BRANCO".
           05 LINE 23 COLUMN 30 VALUE " D - VERDE".

       PROCEDURE DIVISION.
       INICIO.

       R0.
           OPEN I-O CADVEIC
           IF ST-ERRO NOT = "00"
                IF ST-ERRO = "30"
                   OPEN OUTPUT CADVEIC
                   CLOSE CADVEIC
                   GO TO R0
                ELSE
                   DISPLAY (05, 10) "ERRO AO CRIAR O ARQUIVO " ST-ERRO
                   GO TO FIM
           ELSE
                NEXT SENTENCE.

	   DISPLAY TELA.

 	   MOVE "Chevrolet" TO TBMARCA(1)
	   MOVE "Volkswagen" TO TBMARCA(2)  
	   MOVE "Fiat" TO TBMARCA(3)
	   MOVE "Renault" TO TBMARCA(4)
	   MOVE "Ford" TO TBMARCA(5)	   

       RP.
	       ACCEPT (5, 47) PLACA.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO FIM.
                   
       RN.
	       ACCEPT (7, 47) NOMEPROP.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO FIM.

       RM.
           DISPLAY TELAM.
	       ACCEPT (9, 46) MARCA.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO FIM.

       RMD.
	       ACCEPT (11, 47) MODELO.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO FIM.

       R1.
           DISPLAY TELAC.
	       ACCEPT (13, 47) COR.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO FIM.

	   MOVE 1 TO IND

       R1A.
	   MOVE TBCOR(IND) TO TXTCOR
           IF TXTCOR1 NOT = COR
              ADD 1 TO IND
              IF IND < 6
                 GO TO R1A
              ELSE
              	MOVE "COR OBRIGATORIA" TO MSGERRO
              	DISPLAY (3, 7) MSGERRO
              	GO TO R1
           ELSE
               DISPLAY (13, 49) TXTCOR2.
           
           MOVE SPACES TO MSGERRO
           DISPLAY (3, 7) MSGERRO

       R1B.
	       ACCEPT (15, 47) ANOFAB.
           ACCEPT W-ACT FROM ESCAPE KEY.
           IF W-ACT = 01
              GO TO FIM.

           MOVE SPACES TO MSGERRO
           DISPLAY (3, 7) MSGERRO
           WRITE REGVEIC.     

       FIM.
           CLOSE PROGR92.
.          STOP RUN.
