***INCLUDE MFCHKO00 .


*---------------------------------------------------------------------*
* Abrechnungsergebnis anzeigen                                        *
*---------------------------------------------------------------------*
MODULE abrechnungsergebnis OUTPUT.

  SET TITLEBAR '107'.
  SET PF-STATUS '107'.

  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  NEW-PAGE
    NO-TITLE
    LINE-COUNT pinfo-psize
    LINE-SIZE  pinfo-pcols.
  DETAIL.
  SET BLANK LINES ON.
  LOOP AT pform.
    CASE pform-ltype.
      WHEN f__ltype-cmd.
        IF pform-linda EQ f__cmd-newpage.
          NEW-PAGE.
        ENDIF.
      WHEN f__ltype-txt.
        WRITE / pform-linda.
    ENDCASE.
  ENDLOOP.
  SET BLANK LINES OFF.
  LEAVE SCREEN.

ENDMODULE.


*---------------------------------------------------------------------*
* Berechtigung für FCH1/2/6 nachträglich prüfen                       *
*---------------------------------------------------------------------*
MODULE berechtigung_pbo OUTPUT.

  PERFORM berechtigung USING 'S'.
  IF hlp_subrc NE 0.                   "keine Berechtigung
    IF sy-calld EQ 'X'.
      LEAVE.
    ELSE.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ENDIF.

ENDMODULE.


*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpro 100/600/800 (FCH1/6/8/9)         *
*---------------------------------------------------------------------*
MODULE get_parameter_100 OUTPUT.

  GET PARAMETER:
    ID 'BUK' FIELD payr-zbukr,
    ID 'HBK' FIELD payr-hbkid,
    ID 'HKT' FIELD payr-hktid.
  IF sy-tcode NE 'FCH8' AND sy-tcode NE 'FCH9' AND sy-tcode NE 'FCHR'.
    GET PARAMETER ID 'CHK' FIELD payr-chect.
  ENDIF.
  IF sy-tcode EQ 'FCHR'.

    IF hlp_bancd IS INITIAL.
      *payr-bancd = sy-datum - 1.
    ELSE.
      *payr-bancd = hlp_bancd.
    ENDIF.
    IF hlp_valut IS INITIAL.
      bseg-valut = sy-datum - 1.
    ELSE.
      bseg-valut  = hlp_valut.
    ENDIF.

  ENDIF.
  IF payr-hbkid EQ space AND payr-hktid EQ space.
    GET PARAMETER:
      ID 'BLZ' FIELD t012-bankl,
      ID 'FBK' FIELD t012k-bankn.
  ELSE.
    CLEAR: t012, t012k.
  ENDIF.
  PERFORM zusatzinfo.

ENDMODULE.



*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpro 200 (FCH2)                       *
*---------------------------------------------------------------------*
MODULE get_parameter_200 OUTPUT.

  GET PARAMETER:
    ID 'BUK' FIELD payr-zbukr,
    ID 'GJR' FIELD payr-gjahr,
    ID 'BLN' FIELD payr-vblnr.
  PERFORM zusatzinfo.

ENDMODULE.



*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpros 300/400 (FCH3/4)                *
*---------------------------------------------------------------------*
MODULE get_parameter_300 OUTPUT.

  GET PARAMETER:
    ID 'BUK' FIELD payr-zbukr,
    ID 'HBK' FIELD payr-hbkid,
    ID 'HKT' FIELD payr-hktid.
  PERFORM zusatzinfo.

ENDMODULE.



*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpro 500 (FCH5)                       *
*---------------------------------------------------------------------*
MODULE get_parameter_500 OUTPUT.

  GET PARAMETER:
    ID 'BUK' FIELD payr-zbukr,
    ID 'GJR' FIELD payr-gjahr,
    ID 'BLN' FIELD payr-vblnr,
    ID 'HBK' FIELD payr-hbkid,
    ID 'HKT' FIELD payr-hktid.
  PERFORM zusatzinfo.

ENDMODULE.



*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpro 652 (FCHR)                       *
*---------------------------------------------------------------------*
MODULE get_parameter_652 OUTPUT.

  bkpf-budat = sy-datlo.
  bkpf-bldat = sy-datlo.
  IF bkpf-blart IS INITIAL.
    bkpf-blart = 'ZC'.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpro 700 (FCH7)                       *
*---------------------------------------------------------------------*
MODULE get_parameter_700 OUTPUT.

  GET PARAMETER:
    ID 'BUK' FIELD payr-zbukr,
    ID 'HBK' FIELD payr-hbkid,
    ID 'HKT' FIELD payr-hktid.
  CHECK xinit EQ space.                "XINIT wird im UP Zusatzinfo
  "nach der Initialisierung gesetzt
  CLEAR opayf.
  PERFORM read_tfbuf
    USING
      '700'
    CHANGING
      opayf.

  IF opayf IS INITIAL.
    SELECT SINGLE * FROM usr01
      WHERE bname EQ sy-uname.
    opayf-ppriz   = usr01-spld.
    IF usr01-spdb EQ 'G'.
      opayf-psofo = 'X'.
    ENDIF.
  ENDIF.
  PERFORM zusatzinfo.

ENDMODULE.



*---------------------------------------------------------------------*
* Get/Set-Parameter lesen für Dynpro 750 (FBZ5)                       *
*---------------------------------------------------------------------*
MODULE get_parameter_750 OUTPUT.

  CHECK xinit EQ space.
  xinit = 'X'.
  GET PARAMETER:
    ID 'GJR' FIELD bkpf-gjahr,
    ID 'BLN' FIELD bkpf-belnr.

  CLEAR opayf.
  PERFORM read_tfbuf
    USING
      '750'
    CHANGING
      opayf.

  IF opayf IS INITIAL.
    SELECT SINGLE * FROM usr01
      WHERE bname EQ sy-uname.
    opayf-ppriz   = usr01-spld.
    IF usr01-spdb EQ 'G'.
      opayf-psofo = 'X'.
    ENDIF.
  ENDIF.
  SELECT SINGLE * FROM t001 WHERE bukrs EQ opayf-bukrs.

  SELECT * FROM payr                   "Scheckinformation probelesen
    WHERE zbukr EQ opayf-bukrs
    AND   vblnr EQ bkpf-belnr
    AND   gjahr EQ bkpf-gjahr
    AND   voidr EQ 0.
  ENDSELECT.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 100 (Anforderungsbild FCH1)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_100 OUTPUT.

  SET TITLEBAR '100'.
  PERFORM excl_100_200.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 101 (Scheckinformation)               *
*---------------------------------------------------------------------*
MODULE funktionstasten_101 OUTPUT.

  SET TITLEBAR '100'.
  PERFORM excl_101.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 102 (entwerteter Scheck)              *
*---------------------------------------------------------------------*
MODULE funktionstasten_102 OUTPUT.

  SET TITLEBAR '100'.
  PERFORM excl_102.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 103 (Scheckempfänger)                 *
*---------------------------------------------------------------------*
MODULE funktionstasten_103 OUTPUT.

  SET TITLEBAR '103'.
  SET PF-STATUS '103'.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 104 (Zahlungslauf)                    *
*---------------------------------------------------------------------*
MODULE funktionstasten_104 OUTPUT.

  IF payr-laufi+5(1) NE 'P'.
    SET TITLEBAR '104'.
  ELSE.
    SET TITLEBAR '107'.
  ENDIF.
  SET PF-STATUS '103'.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 105 (Aussteller manueller Schecks)    *
*---------------------------------------------------------------------*
MODULE funktionstasten_105 OUTPUT.

  IF payr-laufi+5(1) NE '*'.
    SET TITLEBAR '105'.
  ELSE.
    SET TITLEBAR '106'.
  ENDIF.
  SET PF-STATUS '103'.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 106 (entwerteter echter Scheck)       *
*---------------------------------------------------------------------*
MODULE funktionstasten_106 OUTPUT.

  SET TITLEBAR '100'.
  IF payr-xragl NE space.
    opayf-sgtxt = TEXT-003.
  ELSE.
    opayf-sgtxt = TEXT-004.
  ENDIF.
  IF payr-checv EQ '*'.
    CLEAR payr-checv.
  ENDIF.
  PERFORM excl_106.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 200 (Anforderungsbild FCH2)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_200 OUTPUT.

  SET TITLEBAR '200'.
  PERFORM excl_100_200.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 201 (Popup Schecknummernauswahl)      *
*---------------------------------------------------------------------*
MODULE funktionstasten_201 OUTPUT.

  SET TITLEBAR '201'.
  SET PF-STATUS '201'.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 300 (Anforderungsbild FCH3)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_300 OUTPUT.

  SET TITLEBAR '300'.
  PERFORM excl_300_400_450.
  SET PF-STATUS '300' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 400 (Anforderungsbild FCH4)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_400 OUTPUT.

  SET TITLEBAR '400'.
  PERFORM excl_300_400_450.
  SET PF-STATUS '400' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 450 (Anforderungsbild FCHT)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_450 OUTPUT.

  SET TITLEBAR '450'.
  PERFORM excl_300_400_450.
  SET PF-STATUS '450' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 500 (Anforderungsbild FCH5)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_500 OUTPUT.

  SET TITLEBAR '500'.
  PERFORM excl_500.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 501 (Scheck hinzufügen)               *
*---------------------------------------------------------------------*
MODULE funktionstasten_501 OUTPUT.

  SET TITLEBAR '500'.
  PERFORM excl_501.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 600 (Anforderungsbild FCH6)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_600 OUTPUT.

  SET TITLEBAR '600'.
  PERFORM excl_600.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 601 (Scheck ändern/einlösen)          *
*---------------------------------------------------------------------*
MODULE funktionstasten_601 OUTPUT.

  SET TITLEBAR '600'.
  PERFORM excl_601.
  SET PF-STATUS '100' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 650 (Anforderungsbild FCHR)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_650 OUTPUT.

  SET TITLEBAR '650'.
  PERFORM excl_650.
  SET PF-STATUS '650' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 651 (Manueller Scheckrücklauf)        *
*---------------------------------------------------------------------*
MODULE funktionstasten_651 OUTPUT.

  SET TITLEBAR '650'.
  PERFORM excl_651.
  SET PF-STATUS '650' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 652 (Belegart)                        *
*---------------------------------------------------------------------*
MODULE funktionstasten_652 OUTPUT.

  SET TITLEBAR '652'.
  SET PF-STATUS '652'.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 700 (Anforderungsbild FCH7)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_700 OUTPUT.

  SET TITLEBAR '700'.
  PERFORM excl_700.
  SET PF-STATUS '700' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 750 (Anforderungsbild FBZ5)           *
*---------------------------------------------------------------------*
MODULE funktionstasten_750 OUTPUT.

  SET TITLEBAR '750'.
  PERFORM excl_750.
  SET PF-STATUS '750' EXCLUDING excl.

ENDMODULE.



*---------------------------------------------------------------------*
* Titel und F-Tasten für Dynpro 800 (Anforderungsbild FCH8/9)         *
*---------------------------------------------------------------------*
MODULE funktionstasten_800 OUTPUT.

  IF sy-tcode EQ 'FCH8'.
    SET TITLEBAR '800'.
    opayf-sgtxt = TEXT-005.
    PERFORM excl_800_900.
    SET PF-STATUS '800' EXCLUDING excl.
  ELSE.
    SET TITLEBAR '900'.
    opayf-sgtxt = TEXT-006.
    PERFORM excl_800_900.
    SET PF-STATUS '900' EXCLUDING excl.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Einträge der TAB_SCHECKS in das Popup 201 bringen                   *
*---------------------------------------------------------------------*
MODULE schecks_anzeigen OUTPUT.

  IF sy-stepl EQ 1.                    "Anzahl Loopzeilen merken
    hlp_loopc = sy-loopc.
    hlp_lnfir = hlp_lnmax.
    hlp_lncur = hlp_lnfir.
  ENDIF.
  READ TABLE tab_schecks INDEX hlp_lncur.
  IF sy-subrc EQ 0.                    "Eintrag in TAB_SCHECKS auf das
    payr-hbkid = tab_schecks-hbkid.    "Popup bringen
    payr-hktid = tab_schecks-hktid.
    payr-chect = tab_schecks-chect.
    hlp_lncur = hlp_lncur + 1.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Einträge der TAB_PAYR auflisten, danach Leerzeilen anbieten         *
*---------------------------------------------------------------------*
MODULE schecks_auflisten OUTPUT.

  IF sy-stepl EQ 1.                    "Anzahl Loopzeilen merken
    hlp_loopc   = sy-loopc.
    hlp_lncur   = hlp_lnfir.
    hlp_zeile   = TEXT-900.            "Zeile &1 / &2
    hlp_zahl    = hlp_lnfir.
    REPLACE '&1' WITH hlp_zahl INTO hlp_zeile.
    DESCRIBE TABLE tab_payr LINES sy-tfill.
    hlp_zahl    = sy-tfill.
    REPLACE '&2' WITH hlp_zahl INTO hlp_zeile.
    CONDENSE hlp_zeile.
    SHIFT hlp_zeile UP TO '  ' CIRCULAR.
    *payr-zbukr = payr-zbukr.
    *payr-hbkid = payr-hbkid.
    *payr-hktid = payr-hktid.
  ENDIF.
  READ TABLE tab_payr INDEX hlp_lncur.
  IF sy-subrc EQ 0.                    "Eintrag auflisten
    *payr-chect = tab_payr-chect.
    *payr-rwbtr = tab_payr-rwbtr_input.
    *payr-waers = tab_payr-waers.
    bseg-valut  = tab_payr-valut_input.
    *payr-bancd = tab_payr-bancd_input.
    bseg-dmbtr = tab_payr-input_amount.
    hlp_lncur   = hlp_lncur + 1.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND *payr-chect NE space.
      screen-input = 0.                "Schecknummer nur eingabebereit
      MODIFY SCREEN.                   "wenn Eintrag neu
    ENDIF.
    IF screen-group1 EQ '2' AND *payr-rwbtr NE space.
      screen-input = 1.                "Schecknummer nur eingabebereit
      MODIFY SCREEN.                   "wenn Eintrag neu
    ENDIF.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Felder bei echten Schecks ausblenden                                *
*---------------------------------------------------------------------*
MODULE screen_modif_101 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND payr-extrd EQ 0.
      screen-input = 0.                "Extraktdatum ausblenden, wenn
      screen-output = 0.               "es nicht gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '2' AND payr-zregi EQ space.
      screen-input = 0.                "Regionalcode ausblenden, wenn
      screen-output = 0.               "er nicht gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '3' AND payr-vblnr EQ space.
      screen-input = 0.                "Zahlungsbeleg ausblenden, wenn
      screen-output = 0.               "er nicht gefüllt ist (HR)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '4' AND payr-pernr EQ 0.
      screen-input = 0.                "Personalnummer ausblenden, wenn
      screen-output = 0.               "sie nicht gefüllt ist (FI)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '5' AND payr-vblnr EQ space AND payr-rwskt EQ 0.
      screen-input = 0.                "Skonto ausblenden, wenn 0 und
      screen-output = 0.               "Personalscheck (HR)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '6' AND payr-checv EQ space.
      screen-input = 0.                "Ersatzscheck nur einblenden,
      screen-output = 0.               "wenn es einen gibt
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Felder bei entwerteten Schecks ausblenden                           *
*---------------------------------------------------------------------*
MODULE screen_modif_102 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND ( payr-checv EQ space
                               OR payr-vblnr NE space ).
      screen-input = 0.                "Ersatzscheck nur einblenden,
      screen-output = 0.               "wenn es einen gibt und der
      screen-invisible = 1.            "entwertete Scheck keine zurück-
      MODIFY SCREEN.                   "genommene Zahlung ist
    ENDIF.
    IF screen-group1 EQ '2' AND payr-checf EQ payr-chect.
      screen-input = 0.                "Bis-Schecks ggf. ausblenden
      screen-output = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '3' AND payr-extrd EQ 0.
      screen-input = 0.                "Extraktdatum ausblenden, wenn
      screen-output = 0.               "es nicht gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Felder bei Scheckempfängerinformation ausblenden                    *
*---------------------------------------------------------------------*
MODULE screen_modif_103 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND payr-kunnr EQ space.
      screen-input = 0.                "Debitornummer nur einblenden,
      screen-output = 0.               "wenn sie gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '2' AND payr-lifnr EQ space.
      screen-input = 0.                "Kreditornummer nur einblenden,
      screen-output = 0.               "wenn sie gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '3' AND payr-pernr EQ 0.
      screen-input = 0.                "Personalnummer nur einblenden,
      screen-output = 0.               "wenn sie gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '4'.
      IF payr-zbnks EQ space.
        screen-input = 0.                "Bankdaten nur einblenden,
        screen-output = 0.               "wenn sie gefüllt ist
        screen-invisible = 1.
        MODIFY SCREEN.
      ELSEIF screen-group2 EQ 'I' AND payr-zswif EQ space AND payr-ziban EQ space.
        screen-input = 0.              "IBAN und SWIFT nur einblenden,
        screen-output = 0.             "wenn eins von beiden gefüllt ist
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA l_xtech TYPE boole_d.
  IF NOT payr-zbnkn IS INITIAL.
    CALL FUNCTION 'FI_TECH_ACCNO_CHECK_TRY'
      EXPORTING
        i_bankn = payr-zbnkn
      IMPORTING
        e_xtech = l_xtech.
    IF NOT l_xtech IS INITIAL.
      CLEAR payr-zbnkn.
    ENDIF.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Scheckaussteller und Erstellungszeit bei Entwerteten ausblenden     *
*---------------------------------------------------------------------*
MODULE screen_modif_104 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND payr-prius EQ space.
      screen-input = 0.                "Benutzer, Datum und Zeit nur
      screen-output = 0.               "einblenden, wenn sie gefüllt ist
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '2' AND payr-uzawe EQ space.
      screen-input = 0.                "Zahlwegzusatz nur einblenden,
      screen-output = 0.               "wenn er gefüllt ist (ab 4.0)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Zahlweg bei manuellen Schecks ausblenden                            *
*---------------------------------------------------------------------*
MODULE screen_modif_105 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND payr-rzawe EQ space.
      screen-input = 0.                "Zahlweg nur bei Onlineschecks
      screen-output = 0.               "einblenden
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '2' AND payr-uzawe EQ space.
      screen-input = 0.                "Zahlwegzusatz nur einblenden,
      screen-output = 0.               "wenn er gefüllt ist (ab 4.0)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Felder nur bei manuellen Schecks eingabebereit                      *
*---------------------------------------------------------------------*
MODULE screen_modif_601 OUTPUT.

  LOOP AT SCREEN.
    IF screen-group1 EQ '1' AND
      ( payr-xmanu EQ space OR payr-pernr NE 0 ).
      CHECK screen-name NE 'PAYR-ZNME2' OR payr-pernr EQ 0.
      screen-input = 0.                "Scheckdaten nur eingabebereit
      MODIFY SCREEN.                   "wenn manueller Scheck
    ENDIF.
    IF screen-group1 EQ '2' AND payr-vblnr EQ space.
      screen-input = 0.                "Zahlungsbeleg ausblenden, wenn
      screen-output = 0.               "er nicht gefüllt ist (HR)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 EQ '3' AND payr-pernr EQ 0.
      screen-input = 0.                "Personalnummer ausblenden, wenn
      screen-output = 0.               "sie nicht gefüllt ist (FI)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group2 EQ '4' AND payr-vblnr EQ space AND payr-rwskt EQ 0.
      screen-input = 0.                "Skonto ausblenden, wenn 0 und
      screen-output = 0.               "Personalscheck (HR)
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  SET CURSOR FIELD 'PAYR-BANCD'.

ENDMODULE.



*---------------------------------------------------------------------*
* Stornogrund bei FCH9 ausblenden                                     *
*---------------------------------------------------------------------*
MODULE screen_modif_800 OUTPUT.

  CHECK sy-tcode EQ 'FCH9'.
  LOOP AT SCREEN.
    CHECK screen-group1 EQ 'STG'.
    screen-active = 0.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Felder nur bei manuellen Schecks eingabebereit                      *
*---------------------------------------------------------------------*
MODULE screen_modif_652 OUTPUT.

  LOOP AT SCREEN.
    IF screen-name EQ 'RFPDO2-FEBHRPOST' AND rfpdo2-chknxper EQ space.
      screen-input = 0.                "Checkbox 'HR-Schecks buchen'
      screen-output = 0.               "ausblenden, wenn kein HR-Scheck
      screen-invisible = 1.            "zu behandeln ist
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.



*---------------------------------------------------------------------*
* Cursor setzen                                                       *
*---------------------------------------------------------------------*
MODULE set_cursor_651 OUTPUT.

  SET CURSOR FIELD hlp_field LINE hlp_line.
  SORT tab_payr BY chect.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  SET_SIZE_201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_size_201 OUTPUT.

  check_selection-lines = hlp_lnmax.

ENDMODULE.                             " SET_SIZE_201  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  SET_SIZE_651  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_size_651 OUTPUT.

  fchr_control-lines = hlp_lnmax + 28.

ENDMODULE.                             " SET_SIZE_651  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCHECK_SPERRE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE scheck_sperre OUTPUT.

  CHECK: hlp_sperr IS INITIAL.

  CALL FUNCTION 'ENQUEUE_EFPAYR'
    EXPORTING
      zbukr        = payr-zbukr
      hbkid        = payr-hbkid
      hktid        = payr-hktid
      x_rzawe      = 'X'
      chect        = 'FCHR'
    EXCEPTIONS
      foreign_lock = 1.
  IF sy-subrc NE 0.
    CLEAR ok-code.
    MESSAGE e631 WITH sy-msgv1.
  ENDIF.

  hlp_sperr = 'X'.

ENDMODULE.                 " SCHECK_SPERRE  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  CALL FUNCTION 'FI_GET_FORMTYPE'
    EXPORTING
      i_method     = opayf-rzawe
      i_comp_code  = opayf-bukrs
    IMPORTING
      e_formtype   = opayf-fotyp
    EXCEPTIONS
      error_method = 1
      error_format = 2.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            DISPLAY LIKE 'E'
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT SCREEN.
    IF opayf-fotyp EQ 'P'.
      IF screen-name = 'OPAYF-PZFOR' OR
         screen-name = 'OPAYF-PSTAP' OR
         screen-name = 'OPAYF-PESPR' OR
         screen-name = 'OPAYF-PISOC' OR
         screen-name = 'OPAYF-XPDRU' OR
         screen-name = 'OPAYF-XNOVO'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF screen-group1 = 'PDF' AND
       opayf-fotyp IS INITIAL.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF sy-msgno = '677' AND sy-msgid EQ 'FS'.
    IF opayf-fotyp IS INITIAL.
      SET CURSOR FIELD 'OPAYF-PZFOR'.
    ELSE.
      SET CURSOR FIELD 'OPAYF-PDFFO'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
