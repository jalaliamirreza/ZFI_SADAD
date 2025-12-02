***INCLUDE MFCHKFR0.



*---------------------------------------------------------------------*
* Ermitteln der Kurzschlüssel aus Bankleitzahl und Kontonummer        *
*---------------------------------------------------------------------*
FORM read_bank_id.

  REFRESH:
    tab_hbkid,
    tab_hktid.
  tab_hbkid-sign   = tab_hktid-sign   = 'I'.
  tab_hbkid-option = tab_hktid-option = 'EQ'.
  tab_hbkid-high   = tab_hktid-high   = space.
  SELECT * FROM t012
    WHERE bukrs EQ payr-zbukr
      AND bankl EQ t012-bankl.
    SELECT * FROM t012k
      WHERE bukrs EQ payr-zbukr
        AND hbkid EQ t012-hbkid
        AND bankn EQ t012k-bankn.
      tab_hbkid-low = t012-hbkid.
      tab_hktid-low = t012k-hktid.
      COLLECT tab_hbkid.
      COLLECT tab_hktid.
    ENDSELECT.
  ENDSELECT.
  IF sy-subrc NE 0.
    MESSAGE e505 WITH t012-bankl.
  ENDIF.
  READ TABLE tab_hktid INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e506 WITH t012k-bankn.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* Ermitteln der Scheckinformationen                                   *
*---------------------------------------------------------------------*
FORM read_payr.

* Zahlwege ermitteln
  IMPORT payr-rzawe FROM MEMORY ID 'PAYR-RZAWE'.
  IF sy-subrc NE 0.                    "alle Zahlwege
    payr-rzawe = '%'.
  ELSE.                                "übers Memory wurde ein Zahlweg
    FREE MEMORY ID 'PAYR-RZAWE'.       "von einem rufenden Programm (zB.
  ENDIF.                               "RFCHKL00) mitgegeben

* ggf. TAB_HBKID und TAB_HKTID füllen
  READ TABLE tab_hbkid INDEX 1.
  IF sy-subrc NE 0.
    IF sy-dynnr NE 200.
      SELECT * FROM payr
        WHERE ichec EQ space
        AND   zbukr EQ payr-zbukr
        AND   hbkid EQ payr-hbkid
        AND   hktid EQ payr-hktid
        AND   chect EQ payr-chect
        AND   rzawe LIKE payr-rzawe.
      ENDSELECT.
      IF sy-dbcnt EQ 1.
        SET PARAMETER:
          ID 'CHK' FIELD payr-chect,
          ID 'BLN' FIELD payr-vblnr,
          ID 'GJR' FIELD payr-gjahr.
        payr-rwbtr = - payr-rwbtr.
        payr-rwskt = - payr-rwskt.
        EXIT.
      ENDIF.
    ENDIF.
    tab_hbkid-sign   = tab_hktid-sign   = 'I'.
    tab_hbkid-option = tab_hktid-option = 'EQ'.
    tab_hbkid-high   = tab_hktid-high   = space.
    tab_hbkid-low    = payr-hbkid.
    tab_hktid-low    = payr-hktid.
    APPEND tab_hbkid.
    APPEND tab_hktid.
  ENDIF.

* Tabelle PAYR entweder mit Scheck- oder mit Zahlungsbelegnummer lesen
  REFRESH tab_schecks.
  IF sy-dynnr EQ 100 OR sy-dynnr EQ 600 OR sy-dynnr EQ 800.
    SELECT * FROM payr
      WHERE ichec EQ space
        AND zbukr EQ payr-zbukr
        AND hbkid IN tab_hbkid
        AND hktid IN tab_hktid
        AND chect GE payr-chect
        AND checf LE payr-chect
        AND rzawe LIKE payr-rzawe.
      MOVE-CORRESPONDING payr TO tab_schecks.
      APPEND tab_schecks.
    ENDSELECT.
  ELSEIF sy-dynnr EQ 200.
    SELECT * FROM payr
      WHERE zbukr EQ payr-zbukr
        AND vblnr EQ payr-vblnr
        AND gjahr EQ payr-gjahr.
      MOVE-CORRESPONDING payr TO tab_schecks.
      APPEND tab_schecks.
    ENDSELECT.
  ENDIF.

  DESCRIBE TABLE tab_schecks LINES sy-tfill.
  IF sy-tfill EQ 0.
    IF payr-hbkid NE space.
      CLEAR: t012, t012k.
    ENDIF.
    MESSAGE e508.
  ELSE.
    IF sy-tfill GT 1.
      hlp_lnfir = 1.
      hlp_lnmax = sy-tfill.
      check_selection-top_line = 1.
      CALL SCREEN 201 STARTING AT 20 5 ENDING AT 60 14.
      CHECK ok-code NE 'ABBR'.
      SELECT * FROM payr
        WHERE zbukr EQ tab_schecks-zbukr
          AND hbkid EQ tab_schecks-hbkid
          AND hktid EQ tab_schecks-hktid
          AND rzawe EQ tab_schecks-rzawe
          AND chect EQ tab_schecks-chect.
      ENDSELECT.
    ENDIF.
    SET PARAMETER:
      ID 'CHK' FIELD payr-chect,
      ID 'BLN' FIELD payr-vblnr,
      ID 'GJR' FIELD payr-gjahr.
    payr-rwbtr = - payr-rwbtr.         "Regulierungsbetrag ist negativ
    payr-rwskt = - payr-rwskt.
    IF sy-dynnr EQ 200.                "bei Einstieg mit FCH2 sind die
      SELECT SINGLE * FROM t012        "Bankdaten in T012 und T012K noch
        WHERE bukrs EQ payr-zbukr      "nicht gelesen
          AND hbkid EQ payr-hbkid.
      SELECT SINGLE * FROM t012k
        WHERE bukrs EQ payr-zbukr
          AND hbkid EQ payr-hbkid
          AND hktid EQ payr-hktid.
    ENDIF.
    SELECT SINGLE * FROM bnka          "Bankadresse lesen
      WHERE banks EQ t012-banks
        AND bankl EQ t012-bankl.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* Scheckrücklauf durchführen                                          *
*---------------------------------------------------------------------*
FORM ruecklauf_durchfuehren.

  PERFORM ruecklauf_speichern.
  CHECK sy-subrc EQ 0.
  CALL SCREEN 652 STARTING AT 10 10 ENDING AT 45 15.
  IF ok-code EQ 'ABB'.
    SET SCREEN 651.
    LEAVE SCREEN.
  ENDIF.
  SUBMIT zrfebck00
    WITH ck_file  = hlp_rfdt_id
    WITH pcupload = 'T'
    WITH post     = 'X'
    WITH hr_post  = rfpdo2-febhrpost
    WITH group    = sy-uname(8)
    WITH i_budat  = bkpf-budat
    WITH i_bldat  = bkpf-bldat
    WITH i_blart  = bkpf-blart
    WITH p_file   = space
    WITH p_prot   = 'X'
    WITH p_post   = 'X'
    WITH testrun  = space
    AND RETURN.
  LOOP AT tab_user WHERE uname EQ sy-uname.
    DELETE tab_user.
  ENDLOOP.
  tab_user-lfdnr    = hlp_rfdt_id-lfdnr.
  hlp_rfdt_id-lfdnr = 0.
  EXPORT tab_user TO DATABASE rfdt(ck) ID hlp_rfdt_id.
  hlp_rfdt_id-lfdnr = tab_user-lfdnr.
  DELETE FROM rfdt WHERE relid = 'CK' AND srtfd = hlp_rfdt_id.


  BREAK omrani.

  DATA: lv_bseg TYPE bseg.
  READ TABLE tab_payr INDEX 1.
  SELECT SINGLE * INTO lv_bseg FROM bseg WHERE bukrs =  tab_payr-zbukr AND
                                               belnr =  tab_payr-vblnr AND
                                               gjahr =  tab_payr-gjahr AND
                                               augbl <> ''.







  CLEAR ok-code.
  SET SCREEN 650.
  LEAVE SCREEN.

ENDFORM.



*---------------------------------------------------------------------*
* Tabellen aus RFDT einlesen, gemerkte Schecks listen, Felder belegen *
*---------------------------------------------------------------------*
FORM ruecklauf_initialisieren.

  hlp_rfdt_id-progr = 'FCHR'.
  hlp_rfdt_id-zbukr = payr-zbukr.
  hlp_rfdt_id-hbkid = payr-hbkid.
  hlp_rfdt_id-hktid = payr-hktid.
  hlp_rfdt_id-lfdnr = 0.
  REFRESH tab_payr.
  REFRESH tab_user.                    "User suchen
  BREAK omrani.
  IMPORT tab_user FROM DATABASE rfdt(ck) ID hlp_rfdt_id.
  LOOP AT tab_user WHERE uname EQ sy-uname.
    hlp_rfdt_id-lfdnr = tab_user-lfdnr.
    IMPORT check_file FROM DATABASE rfdt(ck) ID hlp_rfdt_id.
    IF sy-subrc EQ 0.                  "gemerkte Schecks zum User lesen
      LOOP AT check_file.
        check_rec = check_file.
        CHECK check_rec-rectp EQ '5'.
        SELECT * FROM payr
          WHERE ichec EQ space
            AND zbukr EQ payr-zbukr
            AND hbkid EQ payr-hbkid
            AND hktid EQ payr-hktid
            AND chect EQ check_rec-cknum.
        ENDSELECT.
        IF sy-subrc EQ 0 AND payr-xbanc EQ space AND payr-voidr EQ 0.
          tab_payr = payr.
*         see comment in RUCKLAUF_SPEICHERN
          IF NOT check_rec-amount IS INITIAL.
            tab_payr-rwbtr_input = check_rec-amount.
          ELSE.
            tab_payr-rwbtr_input = check_rec-bankl.
          ENDIF.
          tab_payr-valut_input = check_rec-valut.
          tab_payr-bancd_input = check_rec-pdate.
          APPEND tab_payr.
        ENDIF.
      ENDLOOP.
    ENDIF.
    EXIT.
  ENDLOOP.
  IF hlp_rfdt_id-lfdnr EQ 0.           "keine gemerkten Schecks gefunden
    REFRESH tab_payr.
    SORT tab_user BY lfdnr.            "freie Nummer für User suchen
    LOOP AT tab_user.
      IF tab_user-lfdnr NE sy-tabix.
        EXIT.
      ELSE.
        hlp_rfdt_id-lfdnr = sy-tabix.
      ENDIF.
    ENDLOOP.
    IF hlp_rfdt_id-lfdnr EQ 9999.      "alle Nummern bereits vergeben
      MESSAGE e630.
    ELSE.                              "User unter neuer Nr. hinzufügen
      hlp_rfdt_id-lfdnr = hlp_rfdt_id-lfdnr + 1.
    ENDIF.
  ENDIF.
  tab_user-uname = sy-uname.
  tab_user-lfdnr = hlp_rfdt_id-lfdnr.
  COLLECT tab_user.
  SORT tab_user BY uname.

  hlp_bancd = *payr-bancd.
  hlp_field = '*PAYR-CHECT'.
  hlp_input = 'X'.
  hlp_line  = 1.
  fchr_control-top_line = hlp_lnfir.
  hlp_save  = 1.
  hlp_valut = bseg-valut.

ENDFORM.



*---------------------------------------------------------------------*
* Schecks vor dem manuellen Scheckrücklauf prüfen                     *
*---------------------------------------------------------------------*
FORM ruecklauf_pruefen.

  DATA: ld_blocked(1) TYPE c.                         "DPP
  DATA: ld_ilm_err(1) TYPE c.                         "DPP

  CHECK *payr-chect NE space.          "nur Einträge mit Schecknummer
  hlp_index = hlp_lnfir + sy-stepl - 1.
  READ TABLE tab_payr INDEX hlp_index.
  IF sy-subrc NE 0.                    "neuer Eintrag?
    LOOP AT tab_payr WHERE chect EQ *payr-chect.
      hlp_index = sy-tabix.            "doch alten Eintrag überschreiben
      hlp_next  = 1.
    ENDLOOP.
    IF sy-subrc NE 0.                  "neuen Eintrag prüfen
      IF ok-code EQ 'DELE'.
        CLEAR *payr-chect.
        EXIT.
      ENDIF.
      SELECT * FROM payr
        WHERE ichec EQ space
          AND zbukr EQ payr-zbukr
          AND hbkid EQ payr-hbkid
          AND hktid EQ payr-hktid
          AND chect GE *payr-chect
          AND checf LE *payr-chect.
        IF *payr-chect CO ' 0123456789' AND               "note 01086269
            payr-checf CO ' 0123456789' AND payr-chect CO ' 0123456789'.
          DATA: ln_check_f(13) TYPE n, ln_check_t(13) TYPE n,
                ln_check_c(13) TYPE n.
          ln_check_f = payr-checf.
          ln_check_t = payr-chect.
          ln_check_c = *payr-chect.
          IF ( ln_check_f  <= ln_check_c AND ln_check_t >= ln_check_c ).
            EXIT.
          ENDIF.
        ENDIF.
      ENDSELECT.
      IF sy-subrc NE 0.                "Scheck nicht gefunden
        MESSAGE e508.
      ENDIF.

*     additional authority-check for vendor/customer           "n2022886
      IF payr-laufi+5(1) NE 'P'.
        CALL FUNCTION 'FIBL_CHECK_ACCOUNT_AUTHORITY'
          EXPORTING
            i_zbukr = payr-zbukr
            i_lifnr = payr-lifnr
            i_kunnr = payr-kunnr.
      ENDIF.

      IF payr-laufi+5(1) NE 'P'.                           "DPP
        PERFORM check_partner_block USING payr             "DPP
                                        space              "DPP
                               CHANGING ld_blocked         "DPP
                                        ld_ilm_err.        "DPP
        IF ld_blocked = 'X'.                                 "DPP
          MESSAGE e682(fs).                                  "DPP
        ENDIF.                                             "DPP
      ENDIF.                                               "DPP
      IF ok-code EQ 'INFO'.
        PERFORM aufruf_fch1 USING ' '.
        CLEAR ok-code.
      ENDIF.
      IF payr-xbanc NE space.          "Scheck bereits eingelöst
        CALL FUNCTION 'DEQUEUE_EFPAYR'
          EXPORTING
            zbukr   = payr-zbukr
            hbkid   = payr-hbkid
            hktid   = payr-hktid
            x_rzawe = 'X'
            chect   = 'FCHR'.
        CLEAR hlp_sperr.
        MESSAGE e584.
      ELSEIF payr-voidr NE 0.          "Scheck entwertet
        MESSAGE e585.
      ELSE.
        hlp_next = 1.
        tab_payr = payr.
      ENDIF.
    ENDIF.
  ENDIF.
  IF *payr-rwbtr EQ 0.                 "Vorschlag aus PAYR
    tab_payr-rwbtr_input = - tab_payr-rwbtr.
  ELSE.
    tab_payr-rwbtr_input = *payr-rwbtr.
  ENDIF.
  IF bseg-valut EQ 0.                  "Vorschlag Valutadatum
    tab_payr-valut_input = hlp_valut.
  ELSE.
    tab_payr-valut_input = bseg-valut.
  ENDIF.
  IF *payr-bancd EQ 0.                 "Vorschlag Einlösedatum
    tab_payr-bancd_input = hlp_bancd.
  ELSE.
    tab_payr-bancd_input = *payr-bancd.
  ENDIF.
  IF tab_payr-zaldt GT tab_payr-bancd_input.
    *payr-bancd = tab_payr-bancd_input.
    SET CURSOR FIELD '*PAYR-BANCD' LINE sy-stepl.
    MESSAGE e582.                      "Einlösung vor Ausstellung
  ENDIF.
  IF tab_payr-zaldt GT tab_payr-valut_input.
    bseg-valut = tab_payr-valut_input.
    SET CURSOR FIELD 'BSEG-VALUT' LINE sy-stepl.
    CALL FUNCTION 'CUSTOMIZED_MESSAGE'
      EXPORTING
        i_arbgb = 'FS'
        i_dtype = 'W'
        i_msgnr = '674'.
*   Das Valutadatum liegt nicht nach dem Ausstellungsdatum
  ENDIF.

  tab_payr-input_amount = bseg-dmbtr.
  MODIFY tab_payr INDEX hlp_index.
  IF sy-subrc NE 0.
    APPEND tab_payr.
  ENDIF.
  hlp_save = 0.

ENDFORM.



*---------------------------------------------------------------------*
* Scheckrücklauf abspeichern in RFDT                                  *
*---------------------------------------------------------------------*
FORM ruecklauf_speichern.

  DATA:
    up_amount(13) TYPE n.
BREAK omrani.
  REFRESH check_file.
  rfpdo2-chknxper = space.
  LOOP AT tab_payr.
    AT FIRST.
      CLEAR check_head.
      check_head-rectp = '1'.
      check_head-bankl = bnka-bnklz.
      check_head-accnr = t012k-bankn.
      check_head-crdat = sy-datlo.
      check_file = check_head.
      APPEND check_file.
    ENDAT.
    CLEAR check_rec.
    check_rec-rectp  = '5'.
    check_rec-valut  = tab_payr-valut_input.
    check_rec-cknum  = tab_payr-chect.
    up_amount        = tab_payr-rwbtr_input.
*   fill amount to unused bank number field, because amount field is too
*   short (only 11 bytes, but 13 needed!) See coding in RFEBCK00
*   CHECK_REC-AMOUNT = UP_AMOUNT.
    check_rec-bankl  = up_amount.
    check_rec-pdate  = tab_payr-bancd_input.
    check_file = check_rec.
    APPEND check_file.
    IF tab_payr-laufi+5(1) EQ 'P'.
      rfpdo2-chknxper = 'X'.
    ENDIF.
    AT LAST.
      CLEAR check_trl.
      check_trl-rectp  = '9'.
      check_file = check_trl.
      APPEND check_file.
    ENDAT.
  ENDLOOP.
  IF sy-subrc EQ 0.
    tab_user-lfdnr    = hlp_rfdt_id-lfdnr.
    hlp_rfdt_id-lfdnr = 0.
    EXPORT tab_user TO DATABASE rfdt(ck) ID hlp_rfdt_id.
    hlp_rfdt_id-lfdnr = tab_user-lfdnr.
    EXPORT check_file TO DATABASE rfdt(ck) ID hlp_rfdt_id.
    IF sy-subrc NE 0.
      MESSAGE e226 WITH 'RFDT'.
    ENDIF.
    hlp_save = 1.
  ELSE.
    MESSAGE s629.
  ENDIF.

ENDFORM.



*---------------------------------------------------------------------*
* Scheck und Zahlungsbelege vor der Rücknahme prüfen                  *
*---------------------------------------------------------------------*
FORM ruecknahme_pruefen.

  CHECK payr-zbukr  NE *payr-zbukr
     OR payr-vblnr  NE *payr-vblnr
     OR payr-gjahr  NE *payr-gjahr
     OR rf05r-stgrd NE t041c-stgrd
     OR rf05r-budat NE hlp_stodt
     OR rf05r-monat NE hlp_stomo.
  CLEAR sy-msgty.
  SET PARAMETER ID 'BUK' FIELD payr-zbukr.
  CALL FUNCTION 'CALL_FB08'
    EXPORTING
      i_bukrs      = payr-zbukr
      i_belnr      = payr-vblnr
      i_gjahr      = payr-gjahr
      i_stgrd      = t041c-stgrd
      i_budat      = rf05r-budat
      i_monat      = rf05r-monat
      i_voidr      = payr-voidr
      i_xsimu      = 'X'
    IMPORTING
      e_budat      = hlp_stodt
      e_monat      = hlp_stomo
      e_xsofo      = hlp_xsofo
    EXCEPTIONS
      not_possible = 4.
  IF hlp_xsofo EQ space AND sy-subrc EQ 0.
    CALL FUNCTION 'CALL_FBRA'
      EXPORTING
        i_bukrs      = payr-zbukr
        i_augbl      = payr-vblnr
        i_gjahr      = payr-gjahr
        i_xsimu      = 'X'
      EXCEPTIONS
        not_possible = 4.
  ENDIF.
  IF sy-subrc NE 0.
    CLEAR: t012, t012k.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    *payr-zbukr = payr-zbukr.
    *payr-vblnr = payr-vblnr.
    *payr-gjahr = payr-gjahr.
    rf05r-stgrd = t041c-stgrd.
    rf05r-budat = hlp_stodt.
    rf05r-monat = hlp_stomo.
  ENDIF.
  MESSAGE s603.

ENDFORM.



*---------------------------------------------------------------------*
* Scheckzahlung zurücknehmen: Rücknahme Ausgleich / Storno            *
*---------------------------------------------------------------------*
FORM ruecknahme_zahlung.

  CALL FUNCTION 'ENQUEUE_EFPAYR'
    EXPORTING
      zbukr        = payr-zbukr
      hbkid        = payr-hbkid
      hktid        = payr-hktid
      rzawe        = payr-rzawe
      chect        = payr-chect
    EXCEPTIONS
      foreign_lock = 1.
  IF sy-subrc NE 0.
    CLEAR ok-code.
    MESSAGE e556 WITH sy-msgv1.
  ENDIF.
  hlp_payr = payr.
  hlp_counter = 0.
  SELECT * FROM payr                   "Mehrere Schecks zum Zahlungs-
    WHERE zbukr EQ bkpf-bukrs          "beleg vorhanden?
    AND   vblnr EQ bkpf-belnr
    AND   gjahr EQ bkpf-gjahr.
    IF payr-voidr EQ 0.
      ADD 1 TO hlp_counter.
    ENDIF.
  ENDSELECT.
  payr = hlp_payr.
  IF hlp_counter GT 1.
    CALL FUNCTION 'POPUP_TO_DECIDE'
      EXPORTING
        titel        = TEXT-300
        textline1    = TEXT-301
        textline2    = TEXT-302
        textline3    = TEXT-303
        text_option1 = TEXT-304
        text_option2 = TEXT-305
      IMPORTING
        answer       = hlp_answer.
    CASE hlp_answer.
      WHEN 'A'.                        "Abbrechen
        CALL FUNCTION 'DEQUEUE_EFPAYR'
          EXPORTING
            zbukr = payr-zbukr
            hbkid = payr-hbkid
            hktid = payr-hktid
            rzawe = payr-rzawe
            chect = payr-chect.
      WHEN '2'.                        "nur Scheck entwerten
        UPDATE payr
          SET   voidr = payr-voidr
                voidd = sy-datlo
                voidu = sy-uname
                extrd = 0
                extrt = 0
          WHERE zbukr EQ payr-zbukr
            AND hbkid EQ payr-hbkid
            AND hktid EQ payr-hktid
            AND rzawe EQ payr-rzawe
            AND chect EQ payr-chect.
        MESSAGE s608 WITH payr-chect payr-zbukr payr-vblnr payr-voidr.
        CALL FUNCTION 'DEQUEUE_EFPAYR'
          EXPORTING
            zbukr = payr-zbukr
            hbkid = payr-hbkid
            hktid = payr-hktid
            rzawe = payr-rzawe
            chect = payr-chect.
        COMMIT WORK.
        CLEAR payr-chect.
    ENDCASE.
    CHECK hlp_answer EQ '1'.
  ENDIF.
  IF hlp_xsofo EQ space.
    CALL FUNCTION 'CALL_FBRA'
      EXPORTING
        i_bukrs = payr-zbukr
        i_augbl = payr-vblnr
        i_gjahr = payr-gjahr
        i_stodt = hlp_stodt
        i_stomo = hlp_stomo.
  ENDIF.
  CALL FUNCTION 'CALL_FB08'
    EXPORTING
      i_bukrs = payr-zbukr
      i_belnr = payr-vblnr
      i_gjahr = payr-gjahr
      i_budat = hlp_stodt
      i_monat = hlp_stomo
      i_stgrd = t041c-stgrd
      i_voidr = payr-voidr.
  bkpf-belnr = sy-msgv1.
  MESSAGE s602 WITH payr-chect bkpf-belnr payr-vblnr payr-voidr.
  CALL FUNCTION 'DEQUEUE_EFPAYR'
    EXPORTING
      zbukr = payr-zbukr
      hbkid = payr-hbkid
      hktid = payr-hktid
      rzawe = payr-rzawe
      chect = payr-chect.
  CLEAR payr-chect.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  read_tfbuf
*&---------------------------------------------------------------------*
FORM read_tfbuf
  USING
    i_lfdnr TYPE lfdnr
  CHANGING
    es_opayf TYPE opayf.

  DATA:
    lt_tfbuf TYPE STANDARD TABLE OF tfbuf,
    ls_tfbuf TYPE tfbuf,
    l_second TYPE lfdnr.

  l_second = i_lfdnr + 1.

  SELECT * FROM tfbuf INTO TABLE lt_tfbuf
           WHERE usnam = sy-uname
             AND applk = 'OS'
             AND ( lfdnr = i_lfdnr OR lfdnr = l_second ).

  LOOP AT lt_tfbuf INTO ls_tfbuf.
    IF ls_tfbuf-lfdnr = i_lfdnr.
      es_opayf(100)  = ls_tfbuf-buffr.
    ELSEIF ls_tfbuf-lfdnr = l_second.
      es_opayf-pdffo = ls_tfbuf-buffr(30).
    ENDIF.
  ENDLOOP.

ENDFORM.                    " read_tfbuf
*&---------------------------------------------------------------------*
*&      Form  RESET_OPAYF
*&---------------------------------------------------------------------*
FORM reset_opayf
  CHANGING
    cs_opayf TYPE opayf.

  DATA:
    l_formtype TYPE fpm_formtype,
    l_pzfor    TYPE fordzfor,
    l_pdffo    TYPE fpwbformname.

  l_formtype = cs_opayf-fotyp.
  l_pzfor    = cs_opayf-pzfor.
  l_pdffo    = cs_opayf-pdffo.
  CLEAR opayf.
  cs_opayf-fotyp = l_formtype.
  cs_opayf-pzfor = l_pzfor.
  cs_opayf-pdffo = l_pdffo.

ENDFORM.                    " RESET_OPAYF
