       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.   CGPRG006.
      *AUTHOR.       MIGUEL MORAIS.
      *INSTALLATION. FATEC SAO CAETANO.
      *DATE-WRITTEN. 10/09/2019.
      *DATE-COMPILED.17/09/2019.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *--------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *====================*
       CONFIGURATION SECTION.
      *---------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           CURRENCY SIGN IS "R$ " WITH PICTURE SYMBOL "$"
           .
       INPUT-OUTPUT SECTION.
      *---------------------*
       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
       WORKING-STORAGE SECTION.
      *-----------------------*
       01  FILLER                 PIC X(35)        VALUE
           '**** INICIO DA WORKING-STORAGE ****'.

      *-----> VARIAVEIS AUXILIARES UTILIZADA NO PROCESSAMENTO
       01  WS-AREA-AUX.
           05  WS-FIM                 PIC X(01).
           05  WS-CTLIDO              PIC 9(02).
           05  WS-CALC-MEDIA          PIC ZZ9,99.
           05  WS-DATA-ATUAL          PIC 9999/99/99.
           05  WS-ACD-SP              PIC ZZ.ZZ9.
           05  WS-CONT-ACD            PIC 9(05).
           05  WS-CID-SP              PIC 99.
           05  WS-QTD-VEI             PIC 9(07).
           05  WS-PORC                PIC 99V99.
           05  WS-PORC-SP             PIC ZZ9,99.
           05  WS-MEDIA-SP            PIC 99V99.
           05  WS-CALC-SP             PIC 99V99.
           05  WS-CONT-SP             PIC 9(04).
           05  WS-TOT-SP              PIC 9(04).
           05  WS-MAIOR               PIC 9(05).
           05  WS-QTD-ACD             PIC 9(04).
           05  WS-ACD-M               PIC Z.ZZ9.
           05  WS-QTD-P               PIC 9(02).
           05  WS-CMENOR              PIC 99V99.
           05  WS-FMENOR              PIC 99V99.
           05  WS-CIDM                PIC 9(05).
           05  WS-PM                  PIC ZZ9,99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(05).
           05 WS-ESTADO           PIC X(2).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).
      *-----> SAIDA - SYSOUT
       01  WS-REG-SYSOUT.
           05 CID                 PIC 99999.
           05 FILLER              PIC X(01)        VALUE '-'.
           05 UF                  PIC XX.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 VEICS               PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 BAFO                PIC X.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 ACIDS               PIC Z.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 OBITOS              PIC Z.ZZ9.
           05 FILLER              PIC X(04)        VALUE SPACES.
           05 PORC-ACIDS          PIC ZZ9,99.
           05 FILLER              PIC X(01)        VALUE '%'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG006.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 040-PROCESSAR-SP
           PERFORM 045-PROCESSAR-MAIOR
           PERFORM 047-PROCESSAR-MENOR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

	       ACCEPT WS-DATA-ATUAL FROM DATE
		   .
           DISPLAY 'ATIVIDADE 6'
           DISPLAY 'MIGUEL MORAIS - JESSICA HOLANDA'
           DISPLAY 'ESTATISTICAS - DATA DO CALCULO:' WS-DATA-ATUAL
           DISPLAY '-------------------------------'
           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    LEITURA DADOS DA SYSIN
      *--------------------------------------------------------------*
       025-LER-SYSIN.

           ACCEPT WS-REG-SYSIN  FROM SYSIN

           IF WS-REG-SYSIN = ALL '9'
              MOVE   'S'     TO  WS-FIM
           ELSE
              ADD 1  TO WS-CTLIDO
           END-IF
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN ATE FIM DOS REGISTROS
      *--------------------------------------------------------------*
       030-PROCESSAR.
           MOVE WS-QTD-VEICULOS          TO WS-QTD-VEI
           COMPUTE WS-PORC = (WS-QTD-ACIDENTES * 100) / WS-QTD-VEI
           MOVE WS-CIDADE                TO CID
           MOVE WS-ESTADO                TO UF
           MOVE WS-QTD-VEICULOS          TO VEICS
           MOVE WS-BAFOMETRO             TO BAFO
           MOVE WS-QTD-ACIDENTES         TO ACIDS
           MOVE WS-QTD-OBITOS            TO OBITOS
           MOVE WS-PORC                  TO PORC-ACIDS
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN DA CIDADE DE SP        *
      *--------------------------------------------------------------*
           IF WS-ESTADO = 'SP'
              ADD 1  TO WS-CID-SP
              ADD 1  TO WS-CONT-SP
              ADD WS-PORC  TO WS-CALC-SP
              ADD WS-QTD-ACIDENTES  TO WS-CONT-ACD
           END-IF

           COMPUTE WS-MEDIA-SP = (WS-CALC-SP / WS-CONT-SP)
           MOVE WS-CONT-ACD  TO WS-ACD-SP
           MOVE WS-MEDIA-SP  TO WS-PORC-SP
      *--------------------------------------------------------------*
      *        PROCESSA CIDADE COM MAIOR QUANTIDADE DE ACIDENTES
      *--------------------------------------------------------------*
           ADD 1  TO WS-QTD-P
           IF WS-QTD-ACIDENTES > WS-QTD-ACD
              MOVE WS-QTD-ACIDENTES  TO WS-QTD-ACD
              MOVE WS-CIDADE  TO WS-MAIOR
              MOVE WS-QTD-ACD  TO WS-ACD-M
           END-IF
      *--------------------------------------------------------------*
      *        PROCESSA CIDADE COM MENOR PORCENTAGEM DE OBITO        *
      *--------------------------------------------------------------*
           COMPUTE WS-CMENOR = (WS-QTD-OBITOS / WS-QTD-ACIDENTES) * 100
           IF WS-QTD-P = 1
              MOVE 99  TO WS-FMENOR
           END-IF
           IF WS-CMENOR < WS-FMENOR
              MOVE WS-CMENOR  TO WS-FMENOR
              MOVE WS-CIDADE  TO WS-CIDM
              MOVE WS-FMENOR  TO WS-PM
           END-IF

           DISPLAY WS-REG-SYSOUT
           PERFORM 025-LER-SYSIN
           .
	  *--------------------------------------------------------------*   
       040-PROCESSAR-SP.
           DISPLAY ' ------------------------------ '
           DISPLAY 'MEDIA DAS PORCENTAGENS DE SP.....: ' WS-PORC-SP '%'
           DISPLAY 'QTDE. DE ACIDENTES TOTAIS EM SP...: ' WS-ACD-SP
           DISPLAY 'QTDE. DE CIDADES DE SP PESQUISADAS: ' WS-CID-SP
           .
	  *--------------------------------------------------------------*	   
       045-PROCESSAR-MAIOR.
           DISPLAY ' ------------------------------ '
           DISPLAY 'CIDADE COM MAIOR QTD DE ACIDENTES.......: ' WS-MAIOR
           DISPLAY 'QTD. DE ACIDENTES DESTA CIDADE..........: ' WS-ACD-M
           DISPLAY 'QTD. DE CIDADES PESQUISADAS.............: ' WS-QTD-P
           .
	  *--------------------------------------------------------------*	   
       047-PROCESSAR-MENOR.
           DISPLAY ' ------------------------------ '
           DISPLAY 'CIDADE COM MENOR PORCENTAGEM DE OBITOS: ' WS-CIDM
           DISPLAY 'PORCENTAGEM DE OBITOS DESTA CIDADE....: ' WS-PM '%'
           .
      *---------------> FIM DO PROGRAMA CGPRG006 <-------------------*