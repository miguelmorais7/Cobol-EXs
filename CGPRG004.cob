       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG004.
       AUTHOR.        MIGUEL MORAIS.
       INSTALLATION.  FATEC SAO CAETANO.
       DATE-WRITTEN.  27/08/2019.
       DATE-COMPILED. 27/08/2019.
       SECURITY.      NIVEL BASICO.
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
       FILE-CONTROL.
      *==> LOCAL PARA O SELECT DOS ARQUVOS

       DATA DIVISION.
      *=============*
       FILE SECTION.
      *------------*
      *==> LOCAL PARA A FD (DESCRICAO DOS ARQUIVOS)

       WORKING-STORAGE SECTION.
      *-----------------------*

      *-----> AREA AUXILIAR
       77  WS-FIM                 PIC X(01) VALUE "N".
       77  WS-CTEXIB              PIC 9(02).
       77  AS-CEP                 PIC 9(08).
       77  AS-FRENTE              PIC 9(03)V99.
       77  AS-FUNDO               PIC 9(03)V99.
       77  AS-VAL-METRO           PIC 9(05)V99.
       77  AS-VAL-VENDA           PIC 9(08)V99.
       77  AS-COMISSAO            PIC 9(06)V99.
       77  WS-DATA-ATUAL          PIC 9999/99/99.
       77  WS-HORA-ATUAL          PIC 99.99.9999.

      *-----> DADOS DE SAIDA VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-CEP              PIC 9(08).
           05 FILLER              PIC X(01)       VALUE SPACES.
           05 WS-FRENTE           PIC ZZ9,99.
           05 FILLER              PIC X(02)       VALUE 'M '.
           05 WS-FUNDO            PIC ZZ9,99.
           05 FILLER              PIC X(02)       VALUE 'M '.
           05 WS-VAL-METRO        PIC $$.$$9,99.
           05 FILLER              PIC X(03)       VALUE 'M2 '.
           05 WS-VAL-VENDA        PIC $$.$$$.$$9,99.
           05 FILLER              PIC X(01)       VALUE SPACES.
           05 WS-COMISSAO         PIC $$$.$$9,99.
           05 FILLER              PIC X(01)       VALUE SPACES.
           05 WS-MENSAGEM         PIC X(25).
           05 FILLER              PIC X(02)       VALUE SPACES.

       LINKAGE SECTION.
      *----------------*
       01  LK-PARAMETROS.
           05 LK-NR-DPTO             PIC 9(04).
           05 LK-NOME-DPTO           PIC X(15).
           05 LK-COD-RETORNO         PIC 99.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL                                        *
      *--------------------------------------------------------------*

           ACCEPT WS-HORA-ATUAL FROM TIME
           ACCEPT WS-DATA-ATUAL FROM DATE
           .
           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 050-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS                                    *
      *--------------------------------------------------------------*
       010-INICIAR.

           DISPLAY "** ATIVIDADE 4 **"
           DISPLAY "** MIGUEL MORAIS - JESSICA HOLANDA **"
           DISPLAY "CALCULO DO PRECO DE VENDA DE UM TERRENO RETANGULAR"
           DISPLAY "DATA DO CALCULO: " WS-DATA-ATUAL
           DISPLAY "HORA DO CALCULO: " WS-HORA-ATUAL
           DISPLAY '-----------------------------------'

           MOVE  ZEROS  TO  WS-CTEXIB
           .
      *--------------------------------------------------------------*
      *    PROCESSAR DADOS RECEBIDOS DA SYSIN                        *
      *--------------------------------------------------------------*
       030-PROCESSAR.

           MOVE 09000300           TO   WS-CEP
           MOVE 12,35              TO   AS-FRENTE
           MOVE 52,00              TO   AS-FUNDO
           MOVE 2315,00            TO   AS-VAL-METRO

           COMPUTE AS-VAL-VENDA = AS-FRENTE * AS-FUNDO * AS-VAL-METRO

           IF AS-VAL-VENDA > 1500000,00
              COMPUTE AS-COMISSAO = AS-VAL-VENDA * 0,04
              MOVE "ALTO PADRAO"   TO WS-MENSAGEM
           ELSE
              COMPUTE AS-COMISSAO = AS-VAL-VENDA * 0,06
              MOVE "MEDIO PADRAO"  TO WS-MENSAGEM
           END-IF

           MOVE AS-FRENTE          TO   WS-FRENTE
           MOVE AS-FUNDO           TO   WS-FUNDO
           MOVE AS-VAL-METRO       TO   WS-VAL-METRO
           MOVE AS-VAL-VENDA       TO   WS-VAL-VENDA
           MOVE AS-COMISSAO        TO   WS-COMISSAO

           DISPLAY WS-REG-SYSOUT
           ADD   1               TO   WS-CTEXIB
           MOVE 'S'              TO   WS-FIM
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS                                      *
      *--------------------------------------------------------------*
       050-TERMINAR.

           DISPLAY '-----------------------------------'
           DISPLAY '** FIM DA EXECUCAO **'
           DISPLAY "REGISTROS EXIBIDOS = " WS-CTEXIB
           DISPLAY "TERMINO NORMAL DO PROGRAMA CGPRG004"
           .
      *---------------> FIM DO PROGRAMA CGPRG004 <-------------------*
