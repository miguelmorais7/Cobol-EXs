       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID.    CGPRG005.
       AUTHOR.        MIGUEL MORAIS.
       INSTALLATION.  FATEC SAO CAETANO.
       DATE-WRITTEN.  03/09/2019.
       DATE-COMPILED. 03/09/2019.
       SECURITY.      NIVEL BASICO.
      *--------------------------------------------------------------*
      * DISCIPLINA PROGRAMACAO MAINFRAME
      *--------------------------------------------------------------*
      * OBJETIVO: RECEBER DADOS DA SYSIN(ACCEPT)
      *           CALCULAR A MEDIA ARITMETICA BIMESTRAL
      *--------------------------------------------------------------*
      *------------------> HISTORICO - MANUTENCAO <------------------*
      * VERSAO  MES/ANO  NR.DOC  IDENT.  DESCRICAO
      * ------  -------  ------  ------  -------------------------   *
      *  V01    FEV/2013 010001  SISTEMA MOSTRA SYSOUT
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
           05  WS-MEDIA               PIC 9(02)V99.
           05  WS-NUM-F               PIC 9(02).
           05  WS-NUM-M               PIC 9(02).
           05  WS-MEDIA-TOTAL         PIC 9(02),99.
           05  WS-NUM-REP             PIC 9(02).
           05  WS-NOTA-TOTAL          PIC 9(02)V99.
           05  WS-PCT-REP             PIC 9(02),99.
      *-----> ENTRADA - DADOS VIA SYSIN (NO JCL DE EXECUCAO)
       01  WS-REG-SYSIN.
           05 WS-NUMERO-IN        PIC 9(04).
           05 WS-NOME-IN          PIC X(20).
           05 WS-SEXO-IN          PIC X(01).
           05 WS-IDADE-IN         PIC 9(02).
           05 WS-CURSO-IN         PIC X(12).
           05 WS-NOTA1-IN         PIC 9(02)V99.
           05 WS-NOTA2-IN         PIC 9(02)V99.
      *-----> SAIDA - DADOS VIA SYSOUT
       01  WS-REG-SYSOUT.
           05 WS-NUMERO-OUT       PIC 9(04).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOME-OUT         PIC X(20).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-SEXO-OUT         PIC X(01).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-IDADE-OUT        PIC Z9.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-CURSO-OUT        PIC X(12).
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOTA1-OUT        PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-NOTA2-OUT        PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
           05 WS-MED              PIC Z9,99.
           05 FILLER              PIC X(01)        VALUE SPACES.
       01  FILLER                 PIC X(35)        VALUE
           '****** FIM DA WORKING-STORAGE *****'.
      *
       PROCEDURE DIVISION.
      *==================*
      *--------------------------------------------------------------*
      *    PROCESSO PRINCIPAL
      *--------------------------------------------------------------*
       000-CGPRG005.

           PERFORM 010-INICIAR
           PERFORM 030-PROCESSAR UNTIL WS-FIM = 'S'
           PERFORM 090-TERMINAR
           STOP RUN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS INICIAIS
      *--------------------------------------------------------------*
       010-INICIAR.

           PERFORM 025-LER-SYSIN
           .
           DISPLAY "** ATIVIDADE 5 **"
           DISPLAY "** MIGUEL MORAIS - JESSICA HOLANDA **"

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

           COMPUTE WS-MEDIA = (WS-NOTA1-IN + WS-NOTA2-IN) / 2
           IF WS-SEXO-IN = 'F'
              ADD 1  TO WS-NUM-F
           ELSE
              ADD 1  TO WS-NUM-M
           END-IF

           IF WS-MEDIA < 6,00
              ADD 1  TO WS-NUM-REP
           END-IF

           ADD WS-MEDIA  TO WS-NOTA-TOTAL

           MOVE WS-MEDIA       TO WS-MED
           MOVE WS-NUMERO-IN   TO WS-NUMERO-OUT
           MOVE WS-NOME-IN     TO WS-NOME-OUT
           MOVE WS-SEXO-IN     TO WS-SEXO-OUT
           MOVE WS-IDADE-IN    TO WS-IDADE-OUT
           MOVE WS-CURSO-IN    TO WS-CURSO-OUT
           MOVE WS-NOTA1-IN    TO WS-NOTA1-OUT
           MOVE WS-NOTA2-IN    TO WS-NOTA2-OUT
           MOVE WS-NOME-IN     TO WS-NOME-OUT
           MOVE WS-NOME-IN     TO WS-NOME-OUT
      *--->MOVE WS-REG-SYSIN   TO WS-REG-SYSOUT
           DISPLAY WS-REG-SYSOUT

           PERFORM 025-LER-SYSIN
           .
      *--------------------------------------------------------------*
      *    PROCEDIMENTOS FINAIS
      *--------------------------------------------------------------*
       090-TERMINAR.

           COMPUTE WS-MEDIA-TOTAL = WS-NOTA-TOTAL / WS-CTLIDO
           COMPUTE WS-PCT-REP = (WS-NUM-REP / WS-CTLIDO) * 100

           DISPLAY ' *========================================*'
           DISPLAY ' *   TOTAIS DE CONTROLE - CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' * REGISTROS LIDOS....= ' WS-CTLIDO
           DISPLAY ' * TOTAL DE MULHERES..= ' WS-NUM-F
           DISPLAY ' * TOTAL DE HOMENS....= ' WS-NUM-M
           DISPLAY ' * MEDIA GERAL DOS ALUNOS....... = ' WS-MEDIA-TOTAL
           DISPLAY ' * TOTAL DE ALUNOS COM MEDIA < 6 = ' WS-NUM-REP
           DISPLAY ' * % DE ALUNOS COM MEDIA < 6.... = ' WS-PCT-REP'%'
           DISPLAY ' *========================================*'
           DISPLAY ' *----------------------------------------*'
           DISPLAY ' *      TERMINO NORMAL DO CGPRG005        *'
           DISPLAY ' *----------------------------------------*'
           .
      *---------------> FIM DO PROGRAMA RSPRG002 <-------------------*
