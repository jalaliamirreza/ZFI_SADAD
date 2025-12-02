***INCLUDE MFCHKI00.



*---------------------------------------------------------------------*
* Abbrechen auf Popups                                                *
*---------------------------------------------------------------------*
MODULE abbrechen INPUT.

  LEAVE SCREEN.

ENDMODULE.



*---------------------------------------------------------------------*
* F-Tasten-Befehle ausführen                                          *
*---------------------------------------------------------------------*
MODULE auswahl_bearbeiten.

  DATA: indxkey LIKE indx-srtfd.

  BREAK omrani.
  CLEAR indxkey.
  CONCATENATE 'ZMFCHKI00' sy-uname INTO indxkey.
  EXPORT tab_payr TO DATABASE rfdt(ck) ID indxkey. " Import in ZFI_PROCESS_00001030.
  PERFORM auswahl_bearbeiten.

ENDMODULE.

MODULE warnung_ausgeben.

  IF ok-code = 'CREN'.         ""F18=Umnumerieren            FCH4
*   Sicherheitswarnung
    MESSAGE w641 WITH pcec-checf.
  ENDIF.

ENDMODULE.


*---------------------------------------------------------------------*
* Prüfen, ob Bankdaten eingegeben wurden                              *
*---------------------------------------------------------------------*
MODULE bank_eingabe_pruefen.

  SET CURSOR FIELD 'PAYR-HBKID'.
  REFRESH:
    tab_hbkid,
    tab_hktid.
  IF ( payr-hbkid  EQ space AND        "Bankdaten angegeben?
       payr-hktid  EQ space AND
       t012-bankl  EQ space AND
       t012k-bankn EQ space ).
    MESSAGE e513.
  ENDIF.
  IF sy-tcode EQ 'FCH1' OR sy-tcode EQ 'FCH6' OR sy-tcode EQ 'FCH8'.
    IF ( ( payr-hbkid  NE space OR     "Entweder Kurzschlüssel oder
           payr-hktid  NE space ) AND  "Banknummern!
         ( t012-bankl  NE space OR
           t012k-bankn NE space ) ).
      MESSAGE e501.
    ENDIF.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Kurzschlüssel für Hausbank und Konto prüfen                         *
*---------------------------------------------------------------------*
MODULE bank_id_pruefen.

  IF payr-hbkid NE space.              "Bankkurzschlüssel vorhanden?
    SELECT SINGLE * FROM t012
      WHERE bukrs EQ payr-zbukr
        AND hbkid EQ payr-hbkid.
    IF sy-subrc NE 0.
      CLEAR: bnka, t012, t012k, t012t.
      MESSAGE e514 WITH payr-hbkid.
    ENDIF.
    IF t005-bnkey NE '2'.
      CLEAR bnka.
      SELECT SINGLE * FROM bnka
        WHERE banks EQ t012-banks
          AND bankl EQ t012-bankl.
    ENDIF.
  ENDIF.
  IF ( payr-hbkid NE space AND         "Beide Kurzschlüssel gefüllt?
       payr-hktid EQ space )
  OR ( payr-hbkid EQ space AND
       payr-hktid NE space ).
    CLEAR: bnka, t012, t012k, t012t.
    MESSAGE e502.
  ENDIF.
  IF payr-hktid NE space.              "Kontokurzschlüssel vorhanden?
    SELECT SINGLE * FROM t012k
      WHERE bukrs EQ payr-zbukr
        AND hbkid EQ payr-hbkid
        AND hktid EQ payr-hktid.
    IF sy-subrc NE 0.
      CLEAR: bnka, t012, t012k, t012t.
      MESSAGE e507 WITH payr-hktid.
    ENDIF.
    IF t005-bnkey EQ '2'.
      t012-bankl = t012k-bankn.
      CLEAR bnka.
      SELECT SINGLE * FROM bnka
        WHERE banks EQ t012-banks
          AND bankl EQ t012-bankl.
    ENDIF.
    CLEAR t012t.
    SELECT SINGLE * FROM t012t
      WHERE spras EQ sy-langu
        AND bukrs EQ payr-zbukr
        AND hbkid EQ payr-hbkid
        AND hktid EQ payr-hktid.
  ENDIF.
  IF sy-tcode NE 'FCH1' AND
     sy-tcode NE 'FCH6' AND
     sy-tcode NE 'FCH8' AND
     sy-tcode NE 'FCHT'.
    SELECT COUNT(*) FROM pcec UP TO 1 ROWS.
    IF sy-subrc NE 0.
      CLEAR: bnka, t012, t012k, t012t.
      MESSAGE e607.
    ENDIF.
  ENDIF.
  SET PARAMETER:
    ID 'HBK' FIELD payr-hbkid,
    ID 'HKT' FIELD payr-hktid.

ENDMODULE.



*---------------------------------------------------------------------*
* Bankleitzahl und Kontonummer prüfen                                 *
*---------------------------------------------------------------------*
MODULE bank_nummern_pruefen.

  IF ( t012-bankl  NE space AND        "beide Nummern gefüllt?
       t012k-bankn EQ space )
  OR ( t012-bankl  EQ space AND
       t012k-bankn NE space ).
    MESSAGE e504.
  ENDIF.
  IF payr-hbkid EQ space.              "Ermitteln der Kurzschlüssel aus
    PERFORM read_bank_id.              "den Banknummern --> interne Ta-
    SET PARAMETER:                     "bellen TAB_HBKID und TAB_HKTID
      ID 'BLZ' FIELD t012-bankl,
      ID 'FBK' FIELD t012k-bankn.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Bankschlüssel und Konto der zweiten Bank prüfen                     *
*---------------------------------------------------------------------*
MODULE bank2_pruefen.

  IF pcec-hbkid EQ space.
    pcec-hbkid = payr-hbkid.
  ENDIF.
  IF pcec-hktid EQ space.
    pcec-hktid = payr-hktid.
  ENDIF.
  IF pcec-hbkid NE payr-hbkid.
    SELECT SINGLE * FROM t012
      WHERE bukrs EQ payr-zbukr
        AND hbkid EQ pcec-hbkid.
    IF sy-subrc NE 0.
      CLEAR: *bnka, *t012t.
      MESSAGE e514 WITH pcec-hbkid.
    ENDIF.
  ENDIF.
  IF pcec-hbkid NE payr-hbkid OR pcec-hktid NE payr-hktid.
    SELECT SINGLE * FROM t012k
      WHERE bukrs EQ payr-zbukr
        AND hbkid EQ pcec-hbkid
        AND hktid EQ pcec-hktid.
    IF sy-subrc NE 0.
      CLEAR: *bnka, *t012t.
      MESSAGE e507 WITH pcec-hktid.
    ENDIF.
  ENDIF.
  CLEAR: *bnka, *t012t.
  SELECT SINGLE * FROM bnka INTO *bnka
    WHERE banks EQ t012-banks
      AND bankl EQ t012-bankl.
  SELECT SINGLE * FROM t012t INTO *t012t
    WHERE spras EQ sy-langu
      AND bukrs EQ payr-zbukr
      AND hbkid EQ pcec-hbkid
      AND hktid EQ pcec-hktid.

ENDMODULE.



*---------------------------------------------------------------------*
* F3 und F15 vor den Prüfungen ausführen                              *
*---------------------------------------------------------------------*
MODULE beenden.

  DATA: up_calls(1) TYPE n.            "Anzahl Aufrufe (maximal 8)
  IF sy-ucomm(2) EQ 'CH' AND sy-calld NE space.
    IMPORT up_calls FROM MEMORY ID 'CALLS'.
    ADD 1 TO up_calls.
    EXPORT up_calls TO MEMORY ID 'CALLS'.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'ABB'.                        "F12=Abbrechen
      CLEAR sy-ucomm.
      PERFORM sicherheitsabfrage.
      IF sy-calld EQ space.
        CLEAR: bnka, t012, t012k, t012t, payr.
        PERFORM dynpro_setzen.
        LEAVE SCREEN.
      ELSE.
        LEAVE.
      ENDIF.
    WHEN 'CH1'.                        "Wechsel zum Anzeigen
      CLEAR sy-ucomm.
      PERFORM berechtigung_tcode USING 'FCH1'.
      PERFORM sicherheitsabfrage.
      IF sy-calld EQ space OR up_calls EQ 8.
        LEAVE TO TRANSACTION 'FCH1'.
      ELSE.
        CALL TRANSACTION 'FCH1'.
        LEAVE.
      ENDIF.
    WHEN 'CH5'.                        "Wechsel zum Hinzufügen
      CLEAR sy-ucomm.
      PERFORM berechtigung_tcode USING 'FCH5'.
      PERFORM sicherheitsabfrage.
      IF sy-calld EQ space OR up_calls EQ 8.
        LEAVE TO TRANSACTION 'FCH5'.
      ELSE.
        CALL TRANSACTION 'FCH5'.
        LEAVE.
      ENDIF.
    WHEN 'CH6'.                        "Wechsel zum Ändern/Einlösen
      CLEAR sy-ucomm.
      PERFORM berechtigung_tcode USING 'FCH6'.
      IF sy-dynnr EQ 101.
        CALL TRANSACTION 'FCH6' AND SKIP FIRST SCREEN.
        SELECT SINGLE * FROM payr
          WHERE zbukr EQ payr-zbukr
            AND hbkid EQ payr-hbkid
            AND hktid EQ payr-hktid
            AND rzawe EQ payr-rzawe
            AND chect EQ payr-chect.
        payr-rwbtr = - payr-rwbtr.     "Regulierungsbetrag ist negativ
        payr-rwskt = - payr-rwskt.
      ELSE.
        PERFORM sicherheitsabfrage.
        IF sy-calld EQ space OR up_calls EQ 8.
          LEAVE TO TRANSACTION 'FCH6'.
        ELSE.
          CALL TRANSACTION 'FCH6'.
          LEAVE.
        ENDIF.
      ENDIF.
    WHEN 'END'.                        "F15=Beenden
      CLEAR sy-ucomm.
      PERFORM sicherheitsabfrage.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.



*---------------------------------------------------------------------*
* Eingabe der Zahlungsbelegnummer und des Geschäftsjahres prüfen      *
*---------------------------------------------------------------------*
MODULE beleg_gjahr_pruefen.

  IF payr-vblnr EQ space.
    MESSAGE e055(00).
  ENDIF.
  IF payr-gjahr EQ space.
    MESSAGE e055(00).
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Zahlungsbeleg prüfen                                                *
*---------------------------------------------------------------------*
MODULE beleg_pruefen.

  PERFORM zahlungsbeleg_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Berechtigung prüfen                                                 *
*---------------------------------------------------------------------*
MODULE berechtigung_pai.

* Vorbereitung Entwerten
  IF sy-tcode EQ 'FCH3'.
    IF rfpdo2-chknxper EQ 'X'.
      payr-pernr      = 0.
      payr-laufi+5(1) = 'P'.
    ELSE.
      payr-laufi+5(1) = space.
    ENDIF.
  ENDIF.

* Vorbereitung Umnumerieren
  IF sy-tcode EQ 'FCH4'.
    rfpdo2-chknxper = '*'.
    SELECT * FROM payr INTO *payr
      WHERE ichec EQ space
        AND zbukr EQ payr-zbukr
        AND hbkid EQ payr-hbkid
        AND hktid EQ payr-hktid
        AND checf GE payr-checf
        AND chect LE payr-chect.
      IF rfpdo2-chknxper EQ '*'.
        payr-laufi = *payr-laufi.
        IF *payr-laufi+5(1) EQ 'P'.
          rfpdo2-chknxper = 'X'.
        ELSE.
          rfpdo2-chknxper = space.
        ENDIF.
      ELSEIF ( rfpdo2-chknxper NE 'X' AND *payr-laufi+5(1) EQ 'P' ) OR
             ( rfpdo2-chknxper EQ 'X' AND *payr-laufi+5(1) NE 'P' ).
        MESSAGE e638.
      ENDIF.
    ENDSELECT.
  ENDIF.

  PERFORM berechtigung USING 'E'.

ENDMODULE.



*---------------------------------------------------------------------*
* Eingabe des zahlenden Buchungskreises prüfen                        *
*---------------------------------------------------------------------*
MODULE bukreis_pruefen.

  SELECT SINGLE * FROM t001            "Buchungskreis vorgesehen?
    WHERE bukrs EQ payr-zbukr.
  IF sy-subrc NE 0.
    MESSAGE e511 WITH payr-zbukr.
  ENDIF.
  SELECT SINGLE * FROM t005            "Land zum Buchungskreis nachlesen
    WHERE land1 EQ t001-land1.         "für Länge der BLZ (siehe unten)
  SET PARAMETER ID 'BUK' FIELD payr-zbukr.
  pcec-zbukr = payr-zbukr.

ENDMODULE.



*---------------------------------------------------------------------*
* Buchungskreis beim Druck prüfen (FBZ5 / Dynpro 750)                 *
*---------------------------------------------------------------------*
MODULE druck_bukreis.

  SELECT SINGLE * FROM t001            "Buchungskreis vorgesehen?
    WHERE bukrs EQ opayf-bukrs.
  IF sy-subrc NE 0.
    MESSAGE e511 WITH opayf-bukrs.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'F_REGU_BUK'  "Druckberechtigungsprüfung
    ID 'BUKRS' FIELD opayf-bukrs
    ID 'FBTCH' FIELD '31'.
  IF sy-subrc NE 0.
    MESSAGE e461(f5) WITH opayf-bukrs.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Drucker und Formular prüfen (FBZ5 und FCH7)                         *
*---------------------------------------------------------------------*
MODULE druck_pruefen.

  IF opayf-ppriz NE space.             "Drucker Formular
    SET CURSOR FIELD 'OPAYF-PPRIZ'.
    CALL FUNCTION 'PRINTER_CHECK'
      EXPORTING
        i_printer = opayf-ppriz.
  ENDIF.

  IF opayf-ppria NE space.             "Drucker Avis
    SET CURSOR FIELD 'OPAYF-PPRIA'.
    CALL FUNCTION 'PRINTER_CHECK'
      EXPORTING
        i_printer = opayf-ppria.
  ENDIF.

  IF opayf-pzfor NE space OR opayf-pdffo NE space.
    PERFORM check_form_type_changed
      USING
        opayf-rzawe
        opayf-bukrs
      CHANGING
        opayf-fotyp
        ok-code.
  ENDIF.

  IF opayf-pzfor NE space AND opayf-fotyp IS INITIAL.
    SET CURSOR FIELD 'OPAYF-PZFOR'.
    CALL FUNCTION 'FORM_CHECK'
      EXPORTING
        i_pzfor = opayf-pzfor.
  ELSEIF opayf-pdffo NE space AND NOT opayf-fotyp IS INITIAL.
    CALL FUNCTION 'PDF_FORM_CHECK'
      EXPORTING
        i_formname = opayf-pdffo
      EXCEPTIONS
        not_found  = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  IF sy-dynnr EQ 750.
    SET CURSOR FIELD 'BKPF-BELNR'.
  ELSE.
    SET CURSOR FIELD 'PAYR-ZBUKR'.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Drucken Scheck zum Zahlungsbeleg (FBZ5 / Dynpro 750) vorbereiten    *
*---------------------------------------------------------------------*
MODULE druck_vorbereiten.

  IF bkpf-belnr = space.               "Zahlungsbeleg lesen
    MESSAGE s587.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ENDIF.

  CALL FUNCTION 'READ_DOCUMENT_HEADER'
    EXPORTING
      belnr     = bkpf-belnr
      bukrs     = opayf-bukrs
      gjahr     = bkpf-gjahr
      xarch     = ' '
    IMPORTING
      e_bkpf    = bkpf
    EXCEPTIONS
      exit      = 4
      not_found = 8.
  CASE sy-subrc.
    WHEN 4.
      MESSAGE s587.
      SET SCREEN sy-dynnr.
      LEAVE SCREEN.
    WHEN 8.
      MESSAGE s586.
      SET SCREEN sy-dynnr.
      LEAVE SCREEN.
  ENDCASE.

  IF bkpf-stblg NE space.
    MESSAGE s622 WITH bkpf-bukrs bkpf-belnr bkpf-gjahr.
    SET SCREEN sy-dynnr.
    LEAVE SCREEN.
  ENDIF.

  CALL FUNCTION 'PAYMENT_METHOD_CHECK' "Zahlweg / Bank
    EXPORTING
      i_bukrs = t001-bukrs
      i_hbkid = opayf-hbkid
      i_land1 = t001-land1
      i_rzawe = opayf-rzawe.

  SELECT * FROM payr                   "Taste Scheckinformation ggf.
    WHERE zbukr EQ bkpf-bukrs          "anbieten?
    AND   vblnr EQ bkpf-belnr
    AND   gjahr EQ bkpf-gjahr
    AND   voidr EQ 0.
  ENDSELECT.
  IF sy-subrc NE 0.
    CLEAR payr.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Druckvorgaben sichern (FBZ5 / FCH7)                                 *
*---------------------------------------------------------------------*
MODULE druckvorgaben_save.

  tfbuf-buffr = opayf.
  tfbuf-usnam = sy-uname.
  tfbuf-applk = 'OS'.
  tfbuf-lfdnr = sy-dynnr+1.
  tfbuf-datum = sy-datlo.
  MODIFY tfbuf.

  IF NOT opayf-pdffo IS INITIAL.
    tfbuf-buffr = opayf-pdffo.
    tfbuf-usnam = sy-uname.
    tfbuf-applk = 'OS'.
    tfbuf-lfdnr = tfbuf-lfdnr + 1.
    tfbuf-datum = sy-datlo.
    MODIFY tfbuf.
  ELSE.
    tfbuf-lfdnr = tfbuf-lfdnr + 1.
    DELETE FROM tfbuf WHERE usnam = sy-uname
                      AND   applk = 'OS'
                      AND   lfdnr = tfbuf-lfdnr.
  ENDIF.


ENDMODULE.



*---------------------------------------------------------------------*
* Cursor in der Liste des Scheckrücklaufs holen und PAYR lesen        *
*---------------------------------------------------------------------*
MODULE get_cursor_651.

  GET CURSOR FIELD hlp_field LINE hlp_line.
  IF hlp_line NE 0.
    hlp_index = hlp_lnfir + hlp_line - 1.
    READ TABLE tab_payr INDEX hlp_index.
    IF sy-subrc EQ 0.
      payr = tab_payr.
    ELSE.
      CLEAR payr-chect.
    ENDIF.
  ELSE.
    CLEAR payr-chect.
  ENDIF.
  DESCRIBE TABLE tab_payr LINES hlp_lnmax.
  IF hlp_next NE 0.
    hlp_field = '*PAYR-CHECT'.
    hlp_line  = hlp_lnmax - hlp_lnfir + 2.
    hlp_next  = 0.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Ungültigkeitsgrund muß 'manuell' sein (außer im Callmodus)          *
*---------------------------------------------------------------------*
MODULE grund_pruefen.

  CLEAR tvoit-voidt.
  SELECT SINGLE * FROM tvoid
    WHERE voidr EQ payr-voidr.
  IF tvoid-xsyse NE space AND
    ( sy-calld EQ space OR sy-tcode EQ 'FCH7' ).
    MESSAGE e539.
  ENDIF.
  hlp_voidr = payr-voidr.
  SELECT SINGLE * FROM tvoit
    WHERE langu EQ sy-langu
    AND   voidr EQ hlp_voidr.

ENDMODULE.



*---------------------------------------------------------------------*
* Scheckinformationen ausgeben                                        *
*---------------------------------------------------------------------*
MODULE infos_ausgeben.

  PERFORM read_payr.
  IF ok-code NE 'ABBR'.
    IF payr-voidr EQ 0.
      SET SCREEN 101.
    ELSE.
      SELECT SINGLE * FROM tvoit
        WHERE langu EQ sy-langu
          AND voidr EQ payr-voidr.
      IF payr-vblnr EQ space AND payr-pernr EQ 0.
        SET SCREEN 102.
      ELSE.
        SET SCREEN 106.
      ENDIF.
    ENDIF.
  ELSE.
    SET SCREEN sy-dynnr.
  ENDIF.
  CLEAR ok-code.
  LEAVE SCREEN.

ENDMODULE.



*---------------------------------------------------------------------*
* Struktur OPAYF initialisieren                                       *
*---------------------------------------------------------------------*
MODULE opayf_reset.

  PERFORM reset_opayf
    CHANGING
      opayf.

ENDMODULE.



*---------------------------------------------------------------------*
* Schecknummer vor dem Ändern / Einlösen prüfen                       *
*---------------------------------------------------------------------*
MODULE scheck_aendr_pruefen.

  PERFORM sichern_aendern_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Vor dem Neudruck prüfen, ob Alt-Nummer und Zahllauf vorhanden       *
*---------------------------------------------------------------------*
MODULE scheck_alt_pruefen.

  PERFORM neudruck_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Initialisierung von der Erfassung zum manuellen Scheckrücklauf      *
*---------------------------------------------------------------------*
MODULE scheck_rlauf_init.

  PERFORM ruecklauf_initialisieren.

ENDMODULE.



*---------------------------------------------------------------------*
* Schecknummer vor dem Scheckrücklauf prüfen                          *
*---------------------------------------------------------------------*
MODULE scheck_rlauf_pruefen.
  PERFORM delete_line2.
  PERFORM ruecklauf_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Vor dem Sichern prüfen, ob Bis-Nummer frei ist                      *
*---------------------------------------------------------------------*
MODULE scheck_frei1_pruefen.

  payr-checf = space.
  PERFORM freie_nummer_pruefen.
  PERFORM scheck_sperren.

ENDMODULE.



*---------------------------------------------------------------------*
* Vor dem Entwerten prüfen, ob Von- und Bis-Nummer frei sind          *
*---------------------------------------------------------------------*
MODULE scheck_frei2_pruefen.

  PERFORM freie_nummer_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Schecknummer muß gefüllt sein                                       *
*---------------------------------------------------------------------*
MODULE schecknummer_pruefen.

  IF payr-chect EQ space OR payr-chect(1) EQ '?'.
    payr-chect = '?'.
    MESSAGE e055(00).
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Schecknummer vor der Rücknahme der Zahlung prüfen                   *
*---------------------------------------------------------------------*
MODULE scheck_rueck_pruefen.

  IF sy-tcode EQ 'FCH8'.
    IF payr-laufi+5(1) EQ 'P'.
      CLEAR: t012, t012k.
      MESSAGE e632.
    ENDIF.
    PERFORM ruecknahme_pruefen.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Manuelle Schecknummer vor dem Sichern prüfen                        *
*---------------------------------------------------------------------*
MODULE scheck_sichr_pruefen.

  PERFORM sichern_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Schecknummern vor dem Umnumerieren prüfen                           *
*---------------------------------------------------------------------*
MODULE scheck_umnum_pruefen.

  PERFORM umnumerieren_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Schecknummern vor dem Neu-Zuordnen (Tauschen der Info) prüfen       *
*---------------------------------------------------------------------*
MODULE scheck_zuord_pruefen.

  PERFORM tauschen_pruefen.

ENDMODULE.



*---------------------------------------------------------------------*
* Vor dem Neudruck prüfen, ob Stapel gültig, ggf. Folgestapel         *
*---------------------------------------------------------------------*
MODULE stapel_pruefen.

  IF pcec-stapl EQ 0.
    CLEAR pcec-stapi.
  ELSE.
    SET CURSOR FIELD 'PCEC-STAPL'.
    CALL FUNCTION 'LOT_CHECK'
      EXPORTING
        i_zbukr = payr-zbukr
        i_hbkid = payr-hbkid
        i_hktid = payr-hktid
        i_stapl = pcec-stapl
      IMPORTING
        e_stapl = pcec-stapl
        e_stapi = pcec-stapi
      EXCEPTIONS
        OTHERS  = 4.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'LOT_AND_PAYMENT_METHOD_CHECK'
        EXPORTING
          i_zbukr = payr-zbukr
          i_hbkid = payr-hbkid
          i_hktid = payr-hktid
          i_stapl = pcec-stapl
          i_rzawe = payr-rzawe
        EXCEPTIONS
          OTHERS  = 3.
    ENDIF.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDMODULE.



*---------------------------------------------------------------------*
* Stornogrund prüfen und Langtext besorgen                            *
*---------------------------------------------------------------------*
MODULE stornogrund_pruefen.

  CHECK sy-tcode EQ 'FCH8'.
  t041c-stgrd = uf05a-stgrd.
  CLEAR t041ct.
  CALL FUNCTION 'FI_REVERSE_POSTING_PARAMETERS'
    EXPORTING
      i_stgrd = t041c-stgrd
    IMPORTING
      e_xnegp = t041c-xnegp
      e_xabwd = t041c-xabwd.
  SELECT SINGLE * FROM t041ct
    WHERE spras EQ sy-langu
      AND stgrd EQ t041c-stgrd.

ENDMODULE.



*---------------------------------------------------------------------*
* Werthilfe für den Scheckstapel                                      *
*---------------------------------------------------------------------*
MODULE f4_stapl_700.

  CALL FUNCTION 'F4_CHECK_LOT'
    EXPORTING
      i_xdynp      = 'X'
      i_dynp_progn = 'ZSAPMFCHK'
      i_dynp_dynnr = '0700'
      i_dynp_zbukr = 'PAYR-ZBUKR'
      i_dynp_hbkid = 'PAYR-HBKID'
      i_dynp_hktid = 'PAYR-HKTID'
    IMPORTING
      e_stapl      = pcec-stapl
    EXCEPTIONS
      OTHERS       = 0.

ENDMODULE.



*---------------------------------------------------------------------*
* Werthilfe für den Scheckstapel                                      *
*---------------------------------------------------------------------*
MODULE f4_stapl_750.

  IF payr-chect IS INITIAL.
    CALL FUNCTION 'F4_CHECK_LOT'
      EXPORTING
        i_xdynp      = 'X'
        i_dynp_progn = 'ZSAPMFCHK'
        i_dynp_dynnr = '0750'
        i_dynp_zbukr = 'OPAYF-BUKRS'
      IMPORTING
        e_stapl      = opayf-pstap
      EXCEPTIONS
        OTHERS       = 0.
  ELSE.
    CALL FUNCTION 'F4_CHECK_LOT'
      EXPORTING
        i_zbukr = payr-zbukr
        i_hbkid = payr-hbkid
        i_hktid = payr-hktid
      IMPORTING
        e_stapl = opayf-pstap
      EXCEPTIONS
        OTHERS  = 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  F1_PDFFO  INPUT
*&---------------------------------------------------------------------*
MODULE f1_pdffo INPUT.

  CALL FUNCTION 'FI_DOCUMENTATION_SHOW'
    EXPORTING
      ic_fname    = 'FORDZFOR'
      ic_dokclass = 'DE'.

ENDMODULE.                 " F1_PDFFO  INPUT

*&---------------------------------------------------------------------*
*&      Module  PUT_INFO_TO_OPAYF  INPUT
*&---------------------------------------------------------------------*
MODULE put_info_to_opayf INPUT.
  opayf-pstap = pcec-stapl.
  opayf-bukrs = payr-zbukr.
  opayf-rzawe = payr-rzawe.
  opayf-hbkid = payr-hbkid.
ENDMODULE.                 " PUT_INFO_TO_OPAYF  INPUT
