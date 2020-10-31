       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG007.
       AUTHOR.        MIGUEL COSTA DE MORAIS.
       DATE-WRITTEN.  18/12/2018.
       DATE-COMPILED. 24/09/2019.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    DEZ/2018 010001  SISTEMA MOSTRA SYSOUT
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
           05  AS-P-ACIDS             PIC 9(02)V99.
           05  AS-MAIOR               PIC 9(04).
           05  AS-CID-MAIOR           PIC 9(04).
           05  AS-QTDE-MAIOR          PIC Z.ZZ9.
           05  AS-SP-CONT             PIC 9(02).
           05  AS-SP-ACID             PIC 9(08).
           05  AS-SP-TOTAL            PIC 9(02)V99.
           05  AS-SP-MEDIA            PIC ZZ9.99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-CIDADE           PIC 9(04).
           05 WS-ESTADO           PIC X(02).
           05 WS-QTD-VEICULOS     PIC 9(07).
           05 WS-BAFOMETRO        PIC X(01).
           05 WS-QTD-ACIDENTES    PIC 9(04).
           05 WS-QTD-OBITOS       PIC 9(04).
      *-----> SAIDA - DADOS VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 CID                 PIC 9(04).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 UF                  PIC X(02).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 VEICS               PIC Z.ZZZ.ZZ9.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 BAFO                PIC X(01).
           05 FILLER              PIC X(01) VALUE SPACES.
           05 ACIDS               PIC Z.ZZ9.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 OBITOS              PIC Z.ZZ9.
           05 FILLER              PIC X(01) VALUE SPACES.
           05 PORC-ACIDS          PIC ZZ9,99.
           05 FILLER              PIC X(02) VALUE '% '.
       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-X1001PRG.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "1680481721001 - "
           DISPLAY "PROGRAMA 7 - FATEC SCS"
           DISPLAY "MIGUEL COSTA DE MORAIS"
           DISPLAY "JESSICA HOLANDA"
           DISPLAY "----------------------"
           MOVE 0    TO AS-MAIOR
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

           COMPUTE AS-P-ACIDS = (WS-QTD-OBITOS / WS-QTD-VEICULOS) * 100
           MOVE AS-P-ACIDS  TO PORC-ACIDS

           IF WS-QTD-ACIDENTES > AS-MAIOR
              MOVE WS-QTD-ACIDENTES  TO AS-MAIOR
              MOVE WS-CIDADE         TO AS-CID-MAIOR
              MOVE WS-QTD-ACIDENTES  TO AS-QTDE-MAIOR
           END-IF

           IF WS-ESTADO = 'SP'
              ADD 1 TO  AS-SP-CONT
              ADD WS-QTD-ACIDENTES  TO AS-SP-ACID
              ADD AS-P-ACIDS        TO AS-SP-TOTAL
           END-IF

           MOVE WS-CIDADE         TO CID
           MOVE WS-ESTADO         TO UF
           MOVE WS-QTD-VEICULOS   TO VEICS
           MOVE WS-BAFOMETRO      TO BAFO
           MOVE WS-QTD-ACIDENTES  TO ACIDS
           MOVE WS-QTD-OBITOS     TO OBITOS

           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           COMPUTE AS-SP-MEDIA = AS-SP-TOTAL / AS-SP-CONT
           DISPLAY '------------------------------------------*'
           DISPLAY 'MEDIA DAS PORCENTAGENS DE SP.....: ' AS-SP-MEDIA '%'
           DISPLAY 'QTDE. DE ACIDENTES TOTAIS EM SP..: ' AS-SP-ACID
           DISPLAY 'QTDE. DE CIDADES DE SP PESQUISADAS: ' AS-SP-CONT
           DISPLAY '------------------------------------------*'
           DISPLAY 'CIDADE COM MAIOR NUMERO DE ACIDENTES: ' AS-CID-MAIOR
           DISPLAY 'QTDE DE ACIDENTES DA CIDADE ACIMA: ' AS-QTDE-MAIOR
           DISPLAY 'QTDE DE CIDADES PESQUISADAS: ' WS-CTLIDO
           DISPLAY '------------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA RSPRG002 <-------------------*
