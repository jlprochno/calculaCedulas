       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZJP02.
      ******************************************************************
      * Author: JENYFFER LAURA PROCHNO PEREIRA
      * Date: 17012024
      * Purpose: CALCULAR A QUANTIDADE DE CEDULAS QUE SAIRAO
      *          DO CAIXA ELETRONICO
      * Tectonics: .CBL
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01  WS-SAQUE-VALOR                 PIC 9(013)V99 VALUES ZEROS.
       01  WS-QTD-NOTAS-200               PIC 9(004)    VALUES ZEROS.
       01  WS-QTD-NOTAS-100               PIC 9(004)    VALUES ZEROS.
       01  WS-QTD-NOTAS-50                PIC 9(004)    VALUES ZEROS.
       01  WS-QTD-NOTAS-20                PIC 9(004)    VALUES ZEROS.
       01  WS-QTD-NOTAS-10                PIC 9(004)    VALUES ZEROS.
       01  WS-QTD-NOTAS-5                 PIC 9(004)    VALUES ZEROS.
       01  WS-RESTO                       PIC 9(013)    VALUES ZEROS.
       01  WS-MASCARA                     PIC ZZ.ZZZ.ZZZ,ZZ.


              PROCEDURE DIVISION.
       0000-PRINCIPAL                  SECTION.

           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR
           .
       0000-PRINCIPAL-FIM.
           EXIT.

      *******************************************************************
      *    INICIALIZACAO DO PROGRAMA                                    *
      *******************************************************************
       1000-INICIALIZAR                SECTION.
      *    SOLICITA O VALOR DE SAQUE PARA O CLIENTE
           DISPLAY 'POR FAVOR, DIGITE O VALOR DESEJADO PARA O SAQUE: '
           ACCEPT WS-SAQUE-VALOR
      *    MOVE O VALOR DO SAQUE PARA O RESTO
           MOVE WS-SAQUE-VALOR TO WS-RESTO
      *    MOVE O VALOR DO RESTO PARA UMA MASCARA
           MOVE WS-RESTO       TO WS-MASCARA
           .
       1000-INICIALIZAR-FIM.
           EXIT.

      *******************************************************************
      *    LOGICA CENTRAL DO PROGRAMA                                   *
      *******************************************************************
       2000-PROCESSAR                  SECTION.
      *    CHAMA O CALCULO DA QUANTIDADE DE NOTAS SOLICITADAS
           PERFORM 2100-CALCULO-QTD-NOTAS
      *    CHAMA A IMPRESSAO DE VALORES
           PERFORM 2200-IMPRIME-NOTAS
           .
       2000-PROCESSAR-FIM.
           EXIT.

      *******************************************************************
      *    CALCULO PARA VERIFICAR QUANTAS NOTAS SAIRAO NO SAQUE         *
      *******************************************************************
       2100-CALCULO-QTD-NOTAS          SECTION.
      *    CALCULA QUANTAS NOTAS DE 200 SAIRAO NO SAQUE
               DIVIDE WS-RESTO BY 200
               GIVING WS-QTD-NOTAS-200
               REMAINDER WS-RESTO
      *    CALCULA QUANTAS NOTAS DE 100 SAIRAO NO SAQUE
               DIVIDE WS-RESTO BY 100
               GIVING WS-QTD-NOTAS-100
               REMAINDER WS-RESTO
      *    CALCULA QUANTAS NOTAS DE 50 SAIRAO NO SAQUE
               DIVIDE WS-RESTO BY 50
               GIVING WS-QTD-NOTAS-50
               REMAINDER WS-RESTO
      *    CALCULA QUANTAS NOTAS DE 20 SAIRAO NO SAQUE
               DIVIDE WS-RESTO BY 20
               GIVING WS-QTD-NOTAS-20
               REMAINDER WS-RESTO
      *    CALCULA QUANTAS NOTAS DE 10 SAIRAO NO SAQUE
               DIVIDE WS-RESTO BY 10
               GIVING WS-QTD-NOTAS-10
               REMAINDER WS-RESTO
      *    CALCULA QUANTAS NOTAS DE 5 SAIRAO NO SAQUE
               DIVIDE WS-RESTO BY 5
               GIVING WS-QTD-NOTAS-5
           .
       2100-CALCULO-QTD-NOTAS-FIM.
           EXIT.
      *******************************************************************
      *    CALCULO PARA VERIFICAR QUANTAS NOTAS SAIRAO NO SAQUE         *
      *******************************************************************
       2200-IMPRIME-NOTAS          SECTION.
      *    IMPRIME NA TELA O VALOR DO SAQUE TOTAL
           DISPLAY 'O VALOR SOLICITADO FOI DE R$: ' WS-MASCARA
      *    IMPRIME NO CONSOLE A QUANTIDADE DE NOTAS DE CADA VALOR.
           DISPLAY 'QUANTIDADE DE NOTAS DE R$200,00: ' WS-QTD-NOTAS-200
           DISPLAY 'QUANTIDADE DE NOTAS DE R$100,00: ' WS-QTD-NOTAS-100
           DISPLAY 'QUANTIDADE DE NOTAS DE R$50,00:  '  WS-QTD-NOTAS-50
           DISPLAY 'QUANTIDADE DE NOTAS DE R$20,00:  '  WS-QTD-NOTAS-20
           DISPLAY 'QUANTIDADE DE NOTAS DE R$10,00:  '  WS-QTD-NOTAS-10
           DISPLAY 'QUANTIDADE DE NOTAS DE R$5,00:   '   WS-QTD-NOTAS-5
           .
       2200-IMPRIME-NOTAS-FIM.
           EXIT.
      *******************************************************************
      *    FINALIZAR PROGRAMA                                           *
      *******************************************************************
       3000-FINALIZAR                  SECTION.
           DISPLAY ' '
           DISPLAY 'OBRIGADA POR UTILIZAR OS NOSSOS SERVICOS!'
           DISPLAY 'FINALIZANDO O PROGRAMA!'
           STOP RUN
           .
       3000-FINALIZAR-FIM.
           EXIT.

       END PROGRAM CBLZJP02.
