*&---------------------------------------------------------------------*
*& Include          ZMFCHKFA0
*&---------------------------------------------------------------------*


***INCLUDE MFCHKFA0.



*---------------------------------------------------------------------*
* Füllen der POSTAB/KONTAB und Aufruf der Transaktion FBL1 bzw. FBL5  *
*---------------------------------------------------------------------*
FORM AUFRUF_FBL1_FBL5.

  REFRESH: POSTAB, KONTAB.
  CALL FUNCTION 'GET_INVOICE_DOCUMENT_NUMBERS'
    EXPORTING
      I_XBUKR = PAYR-XBUKR
      I_ZBUKR = PAYR-ZBUKR
      I_VBLNR = PAYR-VBLNR
      I_GJAHR = PAYR-GJAHR
      I_FULL_SEARCH = 'X'
    TABLES
      T_INVOICE = POSTAB.

  KONTAB-NAME1 = SPACE.
  KONTAB-REFKT = SPACE.
  LOOP AT POSTAB.
    KONTAB-KONTO = POSTAB-KONTO.
    KONTAB-BUKRS = POSTAB-BUKRS.
    KONTAB-KOART = POSTAB-KOART.
    COLLECT KONTAB.
  ENDLOOP.

  EPOS-ERRNR = SPACE.
  EPOS-GJAHR = PAYR-GJAHR.
  SELECT SINGLE * FROM T021V
    WHERE TCODE EQ 'FCHK'
    AND   KOART EQ SPACE.
  IF SY-SUBRC EQ 0 AND T021V-VARNR NE SPACE.
    EPOS-VARNR = T021V-VARNR.
  ELSE.
    CASE KONTAB-KOART.
      WHEN 'D'. GET PARAMETER ID 'DAV' FIELD EPOS-VARNR.
      WHEN 'K'. GET PARAMETER ID 'KAV' FIELD EPOS-VARNR.
    ENDCASE.
    IF EPOS-VARNR EQ SPACE.
      MESSAGE E018(F4) WITH 'FCHK'.
    ENDIF.
  ENDIF.
  EPOS-TITLE = TEXT-001.
  EPOS-INFO1 = TEXT-002.
  REPLACE '&CHECK' WITH PAYR-CHECT INTO EPOS-INFO1.

  EXPORT POSTAB KONTAB EPOS TO MEMORY ID 'RFEPOS00/POSTAB'.
*  READ TABLE KONTAB INDEX 1.
  LOOP AT kontab.               " note 01421443
    IF kontab-koart = 'K'.
      PERFORM BERECHTIGUNG_TCODE USING 'FBL1'.
      CALL TRANSACTION 'FBL1' AND SKIP FIRST SCREEN.
      EXIT.
    ELSEIF kontab-koart = 'D'.
      PERFORM BERECHTIGUNG_TCODE USING 'FBL5'.
      CALL TRANSACTION 'FBL5' AND SKIP FIRST SCREEN.
      EXIT.
    ENDIF.
  ENDLOOP.
*  IF KONTAB-KOART EQ 'K'.
*    PERFORM BERECHTIGUNG_TCODE USING 'FBL1'.
*    CALL TRANSACTION 'FBL1' AND SKIP FIRST SCREEN.
*  ELSE.
*    PERFORM BERECHTIGUNG_TCODE USING 'FBL5'.
*    CALL TRANSACTION 'FBL5' AND SKIP FIRST SCREEN.
*  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* Aufruf der Transaktion FB03                                         *
*---------------------------------------------------------------------*
FORM AUFRUF_FB03.

  PERFORM BERECHTIGUNG_TCODE USING 'FB03'.
  SET PARAMETER:
    ID 'BLN' FIELD PAYR-VBLNR,
    ID 'BUK' FIELD PAYR-ZBUKR,
    ID 'GJR' FIELD PAYR-GJAHR.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

ENDFORM.



*---------------------------------------------------------------------*
* Aufruf der Transaktion FCH1                                         *
*---------------------------------------------------------------------*
FORM AUFRUF_FCH1 USING ERSATZ TYPE CLIKE.

  PERFORM BERECHTIGUNG_TCODE USING 'FCH1'.
  IF ERSATZ NE SPACE.                  "Ersatzscheck anzeigen
    SET PARAMETER:
      ID 'HBK' FIELD PAYR-HBKIV,
      ID 'HKT' FIELD PAYR-HKTIV,
      ID 'CHK' FIELD PAYR-CHECV.
    CALL TRANSACTION 'FCH1' AND SKIP FIRST SCREEN.
    SET PARAMETER:
      ID 'HBK' FIELD PAYR-HBKID,
      ID 'HKT' FIELD PAYR-HKTID,
      ID 'CHK' FIELD PAYR-CHECT.
  ELSE.                                "echten Scheck anzeigen
    SET PARAMETER:
      ID 'HBK' FIELD PAYR-HBKID,
      ID 'HKT' FIELD PAYR-HKTID,
      ID 'CHK' FIELD PAYR-CHECT.
    CALL TRANSACTION 'FCH1' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* Aufruf der Transaktion FCH7                                         *
*---------------------------------------------------------------------*
FORM AUFRUF_FCH7.

  REFRESH BDC.
  CLEAR BDC.
  BDC-PROGRAM  = 'ZSAPMFCHK'.
  BDC-DYNPRO   = '700'.
  BDC-DYNBEGIN = 'X'.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'PAYR-ZBUKR'.
  BDC-FVAL     = PAYR-ZBUKR.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'PAYR-HBKID'.
  BDC-FVAL     = PAYR-HBKID.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'PAYR-HKTID'.
  BDC-FVAL     = PAYR-HKTID.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'PAYR-CHECT'.
  BDC-FVAL     = PAYR-CHECT.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'PCEC-STAPL'.
  BDC-FVAL     = OPAYF-PSTAP.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'OPAYF-PPRIZ'.
  BDC-FVAL     = OPAYF-PPRIZ.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'OPAYF-PSOFO'.
  BDC-FVAL     = OPAYF-PSOFO.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'OPAYF-PPRIA'.
  BDC-FVAL     = OPAYF-PPRIA.
  APPEND BDC.
  IF OPAYF-FOTYP NE 'P'.                        "Note 1458657
    CLEAR BDC.
    BDC-FNAM     = 'OPAYF-PZFOR'.
    BDC-FVAL     = OPAYF-PZFOR.
  APPEND BDC.
  CLEAR BDC.
  BDC-FNAM     = 'OPAYF-PESPR'.
  BDC-FVAL     = OPAYF-PESPR.
    APPEND BDC.
    CLEAR BDC.
    BDC-FNAM     = 'OPAYF-XNOVO'.
    BDC-FVAL     = OPAYF-XNOVO.
    APPEND BDC.
    CLEAR BDC.
    BDC-FNAM     = 'OPAYF-PISOC'.
    BDC-FVAL     = OPAYF-PISOC.
    APPEND BDC.
  ENDIF.                                        "Note 1458657
  CLEAR BDC.
  BDC-FNAM     = 'OPAYF-PFILL'.
  BDC-FVAL     = OPAYF-PFILL.
  APPEND BDC.

  PERFORM BERECHTIGUNG_TCODE USING 'FCH7'.
  MESSAGE S591.
  CALL TRANSACTION 'FCH7' USING BDC MODE 'A'.

ENDFORM.



*---------------------------------------------------------------------*
* Aufruf des Abrechnungsergebnisses                                   *
*---------------------------------------------------------------------*
FORM AUFRUF_HR.

  REFRESH PFORM.
  CALL FUNCTION 'RP_IMPORT_PAY_STATEMENT'
       EXPORTING
            LAUFD  = PAYR-LAUFD
            LAUFI  = PAYR-LAUFI
            PERNR  = PAYR-PERNR
            SEQNR  = PAYR-SEQNR
       IMPORTING
            PINFZ  = PINFO
       TABLES
            PFORM  = PFORM
       EXCEPTIONS
            OTHERS = 8.
  READ TABLE PFORM INDEX 1.
  IF SY-SUBRC EQ 0.
    CALL SCREEN 107 STARTING AT 1 1 ENDING AT 82 22.
  ELSE.
    MESSAGE S640.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* Aufruf des Listreports RFCHKL00                                     *
*---------------------------------------------------------------------*
FORM AUFRUF_RFCHKL00.

  SET PARAMETER:
    ID 'BUK' FIELD PAYR-ZBUKR,
    ID 'HBK' FIELD PAYR-HBKID,
    ID 'HKT' FIELD PAYR-HKTID.
  SUBMIT RFCHKL00 VIA SELECTION-SCREEN AND RETURN.
  IF SY-CALLD EQ 'X'.
    LEAVE.
  ELSE.
    CLEAR: T012, T012K, PAYR, OK-CODE.
    LEAVE TO TRANSACTION SY-TCODE.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* F-Tasten-Befehle ausführen                                          *
*---------------------------------------------------------------------*
FORM AUSWAHL_BEARBEITEN.

  DATA:
    CURSOR_FIELD(21) TYPE C,
    CURSOR_LINE      LIKE SY-STEPL.

  READ TABLE EXCL WITH KEY OK-CODE.
  IF SY-SUBRC EQ 0.
    OK-CODE = SPACE.
  ENDIF.

  IF SY-TCODE EQ 'FCHR' AND
   ( OK-CODE EQ 'ALLE' OR OK-CODE EQ 'EMPF' OR OK-CODE EQ 'HRAB' OR
     OK-CODE EQ 'LAUF' OR OK-CODE EQ 'ZBEL' ).
    PERFORM BERECHTIGUNG USING 'E'.
  ENDIF.

  CASE OK-CODE.
    WHEN 'ABBN'.                       "F12=Abbrechen              Popup
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'ABBR'.                       "F12=Abbrechen              Popup
      CLEAR: T012, T012K, PAYR.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'ALLE'.                       "F7=Zugehörige Belege
      IF PAYR-CHECT NE SPACE AND PAYR-VBLNR NE SPACE.
        PERFORM AUFRUF_FBL1_FBL5.
      ENDIF.
    WHEN 'CDEL'.                       "F17=Entwerten               FCH3
      PERFORM ENTWERTEN.
    WHEN 'CREN'.                       "F18=Umnumerieren            FCH4
      PERFORM UMNUMERIEREN.
    WHEN 'DELE'.                       "F14=Löschen                 FCHR
      IF HLP_INDEX GT 0.
        DELETE TAB_PAYR INDEX HLP_INDEX.
        HLP_SAVE = 0.
      ENDIF.
    WHEN 'DRUK'.                       "F13=Drucken                 FBZ5
      PERFORM DRUCKEN.
    WHEN 'ECHK'.                       "F6=Ersatzscheck           FCH1/2
      PERFORM AUFRUF_FCH1 USING 'X'.
    WHEN 'EDEL'.                       "F17=Entwerten               FCH9
      PERFORM ENTWERTEN_ECHT.
    WHEN 'EINL'.                       "F11=Einlösen+Buchen         FCHR
      PERFORM RUECKLAUF_DURCHFUEHREN.
    WHEN 'EMPF'.                       "F5=Scheckempfänger
      IF PAYR-CHECT NE SPACE.
        CALL SCREEN 103 STARTING AT 1 5.
      ENDIF.
    WHEN 'ENTR'.                       "Enter=Weiter               Popup
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EZUR'.                       "F3=Zurück
      CLEAR OK-CODE.
      PERFORM SICHERHEITSABFRAGE.
      IF SY-CALLD EQ SPACE.
        CLEAR: BNKA, T012, T012K, T012T, PAYR.
        PERFORM DYNPRO_SETZEN.
        LEAVE SCREEN.
      ELSE.
        LEAVE.
      ENDIF.
    WHEN 'HRAB'.                       "F20=Abrechnungsergebnis
      IF PAYR-PERNR NE 0.
        PERFORM AUFRUF_HR.
      ENDIF.
    WHEN 'INFO'.                       "F20=Scheckinfo        FCH7/8/9/R
      IF PAYR-CHECT NE SPACE.
        PERFORM AUFRUF_FCH1 USING ' '.
      ENDIF.
    WHEN 'LAUF'.                       "F19=Scheckaussteller
      IF PAYR-CHECT NE SPACE.
        IF PAYR-RZAWE NE SPACE AND PAYR-LAUFI+5(1) NE '*'.
          CALL SCREEN 104 STARTING AT 1 5 ENDING AT 35 13.
        ELSE.
          CALL SCREEN 105 STARTING AT 1 5 ENDING AT 35 11.
        ENDIF.
      ENDIF.
    WHEN 'LIST'.                       "F9=Liste                FCH1/2/6
      PERFORM AUFRUF_RFCHKL00.
    WHEN 'NEUD'.                       "F13=Neu drucken             FCH7
      PERFORM NEU_DRUCKEN.
    WHEN 'PICK'.                       "F2=Auswählen               Popup
      GET CURSOR FIELD CURSOR_FIELD LINE CURSOR_LINE.
      IF CURSOR_LINE NE 0.
        SY-INDEX = HLP_LNFIR + CURSOR_LINE - 1.
        READ TABLE TAB_SCHECKS INDEX SY-INDEX.
        IF SY-SUBRC EQ 0.
          SET SCREEN 0.
          LEAVE SCREEN.
        ENDIF.
      ENDIF.
    WHEN 'RAGL'.                       "F17=Zahlung zurücknehmen    FCH8
      PERFORM RUECKNAHME_ZAHLUNG.
    WHEN 'SAVE'.                       "F16=Daten merken            FCHR
      PERFORM RUECKLAUF_SPEICHERN.
      IF SY-SUBRC EQ 0.
        MESSAGE S528.
      ENDIF.
    WHEN 'UPDA'.                       "F11=Sichern               FCH5/6
      PERFORM SICHERN.
    WHEN 'ZBEL'.                       "F8=Zahlungsbeleg
      IF PAYR-CHECT NE SPACE AND PAYR-VBLNR NE SPACE.
        PERFORM AUFRUF_FB03.
      ENDIF.
    WHEN 'ZUOR'.                       "F18=Zuordnung ändern        FCHT
      PERFORM TAUSCHEN.
  ENDCASE.

  IF OK-CODE CA '+-'.                  "F21-F24=Blättern           Popup
    CALL FUNCTION 'FI_CUST_SCROLL'
      EXPORTING
        I_INPUT = HLP_INPUT
        I_LNFIR = HLP_LNFIR
        I_LNMAX = HLP_LNMAX
        I_LOOPC = HLP_LOOPC
        I_OKCOD = OK-CODE
      IMPORTING
        E_LNFIR = HLP_LNFIR.
    FCHR_CONTROL-TOP_LINE = HLP_LNFIR.
  ELSE.
    HLP_LNFIR = FCHR_CONTROL-TOP_LINE.    "Tablecontrolscrolling
  ENDIF.

  CLEAR OK-CODE.
ENDFORM.
