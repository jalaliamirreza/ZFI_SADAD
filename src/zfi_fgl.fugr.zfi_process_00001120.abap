FUNCTION zfi_process_00001120.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------
  DATA lv_bkpf      TYPE bkpf.
  DATA lv_bkpf_sub  TYPE bkpf_subst.
  DATA lv_bseg      TYPE bseg.
  DATA lv_bseg1     TYPE bseg.
  DATA lv_kunnr     TYPE kunnr.
  DATA lv_assig     TYPE bseg-zuonr.
  DATA lv_bseg_sub  TYPE bseg_subst.
  DATA lv_index     TYPE int4.
  DATA lv_index1    TYPE int4.
  DATA lv_index2    TYPE int4.
  DATA lv_lifnr     TYPE lifnr.
  DATA lv_ebeln     TYPE ebeln.
  DATA lv_ebelp     TYPE ebelp.
  DATA lv_vbeln     TYPE vbeln.
  DATA lv_xblnr     TYPE xblnr.
  DATA lv_lastbudat TYPE budat.
  DATA lv_vbel2     TYPE bseg-vbel2.
  DATA lv_posn2     TYPE posnr_va.
  DATA lv_awkey     TYPE awkey.
  DATA lv_XREF2     TYPE XREF2.



*  BREAK mehrali.
  DATA: BEGIN OF lt_vbrk OCCURS 0,
          vbeln TYPE vbrk-vbeln,
          fkart TYPE vbrk-fkart,
          zterm TYPE vbrk-zterm,
          vgbel TYPE vbrp-vgbel,
          aubel TYPE vbrp-aubel,
          vgpos TYPE vbrp-vgpos,
        END OF lt_vbrk,
        lv_bolnr TYPE likp-bolnr,
        lv_bstkd TYPE vbkd-bstkd.

  FIELD-SYMBOLS: <table> TYPE ANY TABLE,
                 <row>   TYPE any,
                 <fs>    TYPE any.
  FIELD-SYMBOLS:<ls_bkpf> LIKE LINE OF t_bkpf.



  BREAK rafighdoust.

  PERFORM fill_invoice_payment_req TABLES t_bkpf
                                          t_bseg
                                          t_bsegsub.

  LOOP AT t_bkpf INTO lv_bkpf .

    lv_index = sy-tabix.

*    IF lv_bkpf-bukrs <> 1000.
*      CALL FUNCTION 'ZGENERATE_XBLNR_FOR_SD'
*        EXPORTING
*          doctype                    = lv_bkpf-blart
*          budat                      = lv_bkpf-budat
*          bukrs                      = lv_bkpf-bukrs
*        IMPORTING
*          xblnr                      = lv_xblnr
*          lastbudat                  = lv_lastbudat
*        EXCEPTIONS
*          budat_after_today          = 1
*          budat_before_last_document = 2
*          no_docclas_for_doctype     = 3
*          OTHERS                     = 4.
*
*      IF sy-subrc IS INITIAL.
*        READ TABLE t_bkpfsub INDEX lv_index INTO lv_bkpf_sub.
*        SHIFT lv_xblnr LEFT DELETING LEADING '0'.
*        lv_bkpf_sub-xblnr = lv_xblnr.
*        MODIFY t_bkpfsub FROM lv_bkpf_sub INDEX lv_index.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lv_bkpf-awkey
*          IMPORTING
*            output = lv_vbeln.
*
*
*        ASSIGN ('(SAPLV60A)XVBRK') TO <row>.
*        IF <row> IS ASSIGNED.
*          ASSIGN COMPONENT 'XBLNR' OF STRUCTURE <row> TO <fs>.
*          IF <fs> IS ASSIGNED.
*            <fs> = lv_xblnr.
*          ENDIF.
*
*          UNASSIGN <row>.
*          UNASSIGN <fs>.
*
*        ENDIF.
*
*      ENDIF.
*
    IF ( lv_bkpf-blart = 'RV' OR lv_bkpf-blart = 'RR' OR lv_bkpf-blart = 'RC' OR lv_bkpf-blart = 'RD').
      READ TABLE t_bkpfsub INDEX lv_index INTO lv_bkpf_sub.
      lv_bkpf_sub-xblnr = ''.
      MODIFY t_bkpfsub FROM lv_bkpf_sub INDEX lv_index.
    ENDIF.

    CASE lv_bkpf-blart.
      WHEN 'WE'.
        LOOP AT t_bseg INTO lv_bseg WHERE ebeln <> '' AND ( ktosl = 'WRX' OR ( ktosl BETWEEN 'Z01' AND 'Z08' ) ).
          lv_index = sy-tabix.
          SELECT SINGLE lifnr FROM ekko INTO lv_lifnr WHERE ebeln = lv_bseg-ebeln.
          lv_bseg_sub-zuonr = lv_lifnr.
          SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
          lv_bseg-zuonr = lv_bseg_sub-zuonr.
          MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
        ENDLOOP.

      WHEN 'RV' OR 'RR' OR 'RC' OR 'RD'.
*        ----------------------------- کد قبلي تاريخ 1404/04/09
        LOOP AT t_bseg INTO lv_bseg WHERE koart = 'D'.
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          CLEAR lv_vbel2.
          lv_kunnr = lv_bseg-kunnr.
          lv_vbeln = lv_bseg-vbeln.
          SELECT SINGLE aubel aupos FROM vbrp INTO (lv_vbel2 , lv_posn2) WHERE vbeln  = lv_vbeln.
          LOOP AT t_bsegsub INTO lv_bseg_sub.
            lv_index = sy-tabix.
            IF lv_kunnr IS NOT INITIAL.
              lv_bseg_sub-zuonr = lv_kunnr.
              SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
            ENDIF.
            IF lv_vbeln IS NOT INITIAL.
              lv_bseg_sub-vbeln = lv_vbeln.
            ENDIF.
            IF lv_vbel2 IS NOT INITIAL.
              lv_bseg_sub-vbel2 = lv_vbel2.
            ENDIF.
            IF lv_posn2 IS NOT INITIAL.
              lv_bseg_sub-posn2 = lv_posn2.
            ENDIF.
            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
          ENDLOOP.
        ENDIF.
        IF lv_bkpf-bukrs = 1000.
          READ TABLE t_bkpfsub INDEX lv_index INTO lv_bkpf_sub.
          CLEAR lv_bkpf_sub-xblnr.
          MODIFY t_bkpfsub FROM lv_bkpf_sub INDEX lv_index.
        ENDIF.
*----------------------- اين کد براي اينکه همه vbrp-posnr ب

*
*                IF sy-subrc IS INITIAL.
*          DATA: lt_vbrp TYPE TABLE OF vbrp,
*                ls_vbrp TYPE vbrp.
*          BREAK mamardani.
*          CLEAR lv_vbel2.
*          lv_kunnr = lv_bseg-kunnr.
*          lv_vbeln = lv_bseg-vbeln.
*           SELECT * INTO TABLE lt_vbrp FROM vbrp WHERE vbeln = lv_vbeln.
*          LOOP AT t_bsegsub INTO lv_bseg_sub.
*            lv_index = sy-tabix.
*            LOOP AT lt_vbrp INTO ls_vbrp.
*              IF ls_vbrp-POSNR eq lv_posn2.
*                CONTINUE.
*              ENDIF.
*                lv_posn2 = ls_vbrp-POSNR.
*                lv_vbel2 = ls_vbrp-aubel.
*            IF lv_kunnr IS NOT INITIAL.
*              lv_bseg_sub-zuonr = lv_kunnr.
*              SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
*            ENDIF.
*            IF lv_vbeln IS NOT INITIAL.
*              lv_bseg_sub-vbeln = lv_vbeln.
*            ENDIF.
*            IF lv_vbel2 IS NOT INITIAL.
*              lv_bseg_sub-vbel2 = lv_vbel2.
*            ENDIF.
*            IF lv_posn2 IS NOT INITIAL.
*              lv_bseg_sub-posn2 = lv_posn2.
*            ENDIF.
*            exit.
*            ENDLOOP.
*            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
*          ENDLOOP.
*        ENDIF.
*      WHEN 'WS'.
*        LOOP AT t_bseg INTO lv_bseg WHERE koart = 'D'.
*          EXIT.
*        ENDLOOP.
*        IF sy-subrc IS INITIAL.
*          CLEAR lv_vbel2.
*          lv_kunnr = lv_bseg-kunnr.
*          lv_vbeln = lv_bseg-vbeln.
*          SELECT SINGLE aubel aupos FROM vbrp INTO (lv_vbel2 , lv_posn2) WHERE vbeln  = lv_vbeln.
*          LOOP AT t_bsegsub INTO lv_bseg_sub.
*            lv_index = sy-tabix.
*            IF lv_kunnr IS NOT INITIAL.
*              lv_bseg_sub-zuonr = lv_kunnr.
*              SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
*            ENDIF.
*            IF lv_vbeln IS NOT INITIAL.
*              lv_bseg_sub-vbeln = lv_vbeln.
*            ENDIF.
*            IF lv_vbel2 IS NOT INITIAL.
*              lv_bseg_sub-vbel2 = lv_vbel2.
*            ENDIF.
*            IF lv_posn2 IS NOT INITIAL.
*              lv_bseg_sub-posn2 = lv_posn2.
*            ENDIF.
*            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
*          ENDLOOP.
*        ENDIF.


      WHEN 'RE'.
        LOOP AT t_bseg INTO lv_bseg WHERE koart = 'K'.
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          lv_lifnr = lv_bseg-lifnr.
          LOOP AT t_bsegsub INTO lv_bseg_sub.
            lv_index = sy-tabix.
            lv_bseg_sub-zuonr = lv_lifnr.
            SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
            READ TABLE t_bseg INTO lv_bseg INDEX lv_index.
            lv_bseg-zuonr = lv_bseg_sub-zuonr.
            MODIFY t_bseg FROM lv_bseg INDEX lv_index.
            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
          ENDLOOP.
        ENDIF.

        LOOP AT t_bseg INTO lv_bseg WHERE ebeln <> '' AND ( buzid = 'W' OR buzid = 'S'  OR buzid = 'F' ).
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          lv_ebeln = lv_bseg-ebeln.
*          lv_ebelp = lv_bseg-ebelp.
          LOOP AT t_bsegsub INTO lv_bseg_sub.
            lv_index = sy-tabix.
            IF strlen( lv_ebeln ) > 0.
              IF lv_bseg_sub-ebeln IS INITIAL.
                lv_bseg_sub-ebeln = lv_ebeln.
*              lv_bseg_sub-ebelp = lv_ebelp.
              ENDIF.
              lv_bseg_sub-xref1 = lv_bseg_sub-ebeln.
            ENDIF.
            lv_bseg_sub-xref2 = lv_bkpf-awkey(10).
            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
          ENDLOOP.
        ENDIF.

      WHEN 'RA'.
        LOOP AT t_bseg INTO lv_bseg WHERE koart = 'K'.
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          lv_lifnr = lv_bseg-lifnr.
          LOOP AT t_bsegsub INTO lv_bseg_sub.
            lv_index = sy-tabix.
            IF lv_lifnr IS NOT INITIAL.
              lv_bseg_sub-zuonr = lv_lifnr.
              SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
            ENDIF.
            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
          ENDLOOP.
        ENDIF.

        LOOP AT t_bseg INTO lv_bseg WHERE ebeln <> '' AND ( buzid = 'W' OR buzid = 'S'  OR buzid = 'F' ).
          EXIT.
        ENDLOOP.
        IF sy-subrc IS INITIAL.
          lv_ebeln = lv_bseg-ebeln.
*          lv_ebelp = lv_bseg-ebelp.
          LOOP AT t_bsegsub INTO lv_bseg_sub.
            lv_index = sy-tabix.
            IF strlen( lv_ebeln ) > 0.
              IF lv_bseg_sub-ebeln IS INITIAL.
                lv_bseg_sub-ebeln = lv_ebeln.
*              lv_bseg_sub-ebelp = lv_ebelp.
              ENDIF.
              lv_bseg_sub-xref1 = lv_bseg_sub-ebeln.
            ENDIF.
            lv_bseg_sub-xref2 = lv_bkpf-awkey(10).
            MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index.
          ENDLOOP.
        ENDIF.


      WHEN 'AB'.
*        LOOP AT t_bseg INTO lv_bseg WHERE koart = 'S'.
*          DATA ls_header TYPE bapiache09.
*          DATA ls_item TYPE bapiacgl09.
*          DATA lt_item TYPE TABLE OF bapiacgl09.
*          DATA ls_currency TYPE bapiaccr09.
*          DATA lt_currency TYPE TABLE OF bapiaccr09.
*          DATA lt_return TYPE TABLE OF bapiret2.
*
*          CLEAR ls_header.
*
*          ls_header-username = lv_bkpf-usnam.
*          ls_header-comp_code = lv_bseg-bukrs.
*          ls_header-doc_date = lv_bkpf-bldat.
*          ls_header-pstng_date = lv_bkpf-budat.
*          ls_header-fisc_year = lv_bkpf-gjahr.
*          ls_header-doc_type = 'ZB'.
*          ls_header-header_txt = 'وصول چک پرداختني'.
*
*          CLEAR lt_item.
*
*          ls_item-itemno_acc = 1.
*          ls_item-item_text = 'وصول چک پرداختني'.
*          ls_item-acct_type = 'S'.
*          ls_item-doc_type = ls_header-doc_type.
*          ls_item-pstng_date = ls_header-pstng_date.
*          ls_item-fisc_year = ls_header-fisc_year.
*          ls_item-comp_code = ls_header-comp_code.
*          ls_item-value_date = lv_bseg-valut.
*          ls_item-gl_account = lv_bseg-hkont.
*          ls_item-de_cre_ind = 'S'.
*
*          APPEND ls_item TO lt_item.
*
*          ls_item-itemno_acc = 2.
*          ls_item-item_text = 'وصول چک پرداختني'.
*          ls_item-acct_type = 'S'.
*          ls_item-doc_type = ls_header-doc_type.
*          ls_item-pstng_date = ls_header-pstng_date.
*          ls_item-fisc_year = ls_header-fisc_year.
*          ls_item-comp_code = ls_header-comp_code.
*          ls_item-value_date = lv_bseg-valut.
*          SELECT SINGLE bnkn2 FROM t012k INTO ls_item-gl_account WHERE hkont = lv_bseg-hkont AND bukrs = lv_bseg-bukrs.
*          CONCATENATE '000' ls_item-gl_account INTO ls_item-gl_account.
*
*          ls_item-de_cre_ind = 'H'.
*
*          APPEND ls_item TO lt_item.
*
*          CLEAR lt_currency.
*
*          ls_currency-itemno_acc = 1.
*          ls_currency-currency = 'IRR'.
*          ls_currency-amt_doccur = lv_bseg-wrbtr * 100.
*
*          APPEND ls_currency TO lt_currency.
*
*          ls_currency-itemno_acc = 2.
*          ls_currency-currency = 'IRR'.
*          ls_currency-amt_doccur = lv_bseg-wrbtr * -100.
*
*          APPEND ls_currency TO lt_currency.
*
*          CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
*            EXPORTING
*              documentheader = ls_header
**             CUSTOMERCPD    =
**             CONTRACTHEADER =
**              IMPORTING
**             OBJ_TYPE       =
**             OBJ_KEY        =
**             OBJ_SYS        =
*            TABLES
*              accountgl      = lt_item
**             ACCOUNTRECEIVABLE       =
**             ACCOUNTPAYABLE =
**             ACCOUNTTAX     =
*              currencyamount = lt_currency
**             CRITERIA       =
**             VALUEFIELD     =
**             EXTENSION1     =
*              return         = lt_return
**             PAYMENTCARD    =
**             CONTRACTITEM   =
**             EXTENSION2     =
**             REALESTATE     =
**             ACCOUNTWT      =
*            .
*
*        ENDLOOP.

    ENDCASE.
  ENDLOOP.

  LOOP AT t_bseg INTO lv_bseg.
    lv_index1 = sy-tabix.
    IF lv_bseg-umskz IS NOT INITIAL.
      lv_index = sy-tabix.
      IF lv_bseg-umskz = 'W'.
        LOOP AT t_bseg INTO lv_bseg1 WHERE vbel2 <> '' AND posn2 <> '' .
          READ TABLE t_bsegsub INTO lv_bseg_sub INDEX lv_index.
          lv_bseg_sub-vbel2 = lv_bseg1-vbel2.
          lv_bseg_sub-posn2 = lv_bseg1-posn2.

          MODIFY t_bsegsub INDEX lv_index FROM lv_bseg_sub.
        ENDLOOP.

      ENDIF.
      IF lv_bseg-umskz = 'P'.
        READ TABLE t_bsegsub INTO lv_bseg_sub INDEX lv_index.
        SELECT SINGLE ebeln FROM acdoca INTO lv_ebeln  WHERE belnr = lv_bseg-rebzg AND gjahr = lv_bseg-rebzj AND rbukrs = lv_bseg-bukrs AND koart = 'K'.
        IF sy-subrc IS INITIAL.
          IF lv_ebeln IS NOT INITIAL AND lv_bseg_sub-ebeln IS INITIAL.
            lv_bseg_sub-ebeln = lv_ebeln.
            lv_bseg_sub-xref1 = lv_ebeln.
          ENDIF.
        ENDIF.
        SELECT SINGLE awkey FROM bkpf INTO lv_awkey  WHERE belnr = lv_bseg-rebzg AND gjahr = lv_bseg-rebzj AND bukrs = lv_bseg-bukrs AND blart = 'RE'.
        IF sy-subrc IS INITIAL.
          IF lv_awkey IS NOT INITIAL.
            lv_bseg_sub-xref2 = lv_awkey(10).
          ENDIF.
        ENDIF.
        MODIFY t_bsegsub INDEX lv_index FROM lv_bseg_sub.
      ELSE.
        IF lv_bseg-ebeln IS NOT INITIAL.
          lv_bseg_sub-xref1 = lv_bseg-ebeln.
          MODIFY t_bsegsub INDEX lv_index FROM lv_bseg_sub.
        ENDIF.
      ENDIF.
    ENDIF.


    IF ( lv_bseg-hkont(5) = '00010' OR lv_bseg-hkont(5) = '00012' ).
      READ TABLE t_bsegsub INDEX lv_index1 INTO lv_bseg_sub.
      SELECT SINGLE hbkid hktid FROM skb1 INTO (lv_bseg_sub-hbkid , lv_bseg_sub-hktid) WHERE  bukrs = lv_bseg-bukrs AND saknr = lv_bseg-hkont.
      lv_bseg-hbkid = lv_bseg_sub-hbkid.
      lv_bseg-hktid = lv_bseg_sub-hktid.
      MODIFY t_bseg FROM lv_bseg.
      MODIFY t_bsegsub FROM lv_bseg_sub INDEX lv_index1.
    ENDIF.

  ENDLOOP.

*Break Mehrali.
  IF lv_bkpf-blart = 'KZ'.
    LOOP AT t_bseg INTO lv_bseg.
      CLEAR LV_XREF2.
      lv_index2 = sy-tabix.
      READ TABLE t_bsegsub INTO lv_bseg_sub INDEX lv_index2.
      IF lv_bseg-KOART = 'K' and lv_bseg-SHKZG = 'S'.
        SELECT SINGLE ebeln AWREF FROM  acdoca INTO ( lv_ebeln , LV_XREF2 )
          WHERE belnr = lv_bseg-rebzg AND gjahr = lv_bseg-rebzj AND rbukrs = lv_bseg-bukrs AND koart = 'K'.
          IF lv_ebeln IS NOT INITIAL AND lv_bseg_sub-ebeln IS INITIAL.
            lv_bseg_sub-ebeln = lv_ebeln.
            lv_bseg_sub-xref1 = lv_ebeln.
           MODIFY t_bsegsub INDEX lv_index2 FROM lv_bseg_sub TRANSPORTING ebeln xref1.
          ENDIF.
          IF LV_XREF2 IS NOT INITIAL AND lv_bseg_sub-XREF2 IS INITIAL.
            lv_bseg_sub-xref2 = LV_XREF2.
           MODIFY t_bsegsub INDEX lv_index2 FROM lv_bseg_sub TRANSPORTING XREF2.
          ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.



  IF ( sy-tcode = 'FAGL_FCV'  OR sy-repid = 'FAGL_FCV' ).

    ASSIGN ('(FAGL_FCV)GT_SUM_GRBAL') TO <table>.

    IF <table> IS ASSIGNED.

      LOOP AT <table> ASSIGNING <row>.
        ASSIGN COMPONENT 'GROUPING_REFERENCE' OF STRUCTURE <row> TO <fs>.
        lv_assig = <fs>.
      ENDLOOP.

      lv_assig = lv_assig.

      LOOP AT t_bsegsub INTO lv_bseg_sub.

        lv_bseg_sub-zuonr = lv_assig.
        SHIFT lv_bseg_sub-zuonr LEFT DELETING LEADING '0'.
        MODIFY t_bsegsub FROM lv_bseg_sub.

      ENDLOOP.

    ENDIF.
  ENDIF.
*  IF sy-tcode = 'FBA1'.
*    LOOP AT t_bsegsub INTO lv_bseg_sub.
*
*      CLEAR lv_bseg_sub-posn2.
*      MODIFY t_bsegsub FROM lv_bseg_sub.
*
*    ENDLOOP.
*  ENDIF.

************************************************************************
*"----------------------------------------------------------------------
* Develop by:Taban     Date:2017/10/21
* Req by:Aghaei        BRZ-657
* desc:BRZ SD FI fill refrence of factor in fi doc
  BREAK taban.
  BREAK omrani.
  LOOP AT t_bkpf ASSIGNING <ls_bkpf>
                 WHERE awkey IS NOT INITIAL AND
                     ( blart = 'RV' OR blart = 'RR' OR blart = 'RC' OR blart = 'RD' ).

    CLEAR lt_vbrk.
    SELECT vbrk~vbeln fkart zterm vgbel aubel vgpos
      FROM vbrk INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
      INTO CORRESPONDING FIELDS OF TABLE lt_vbrk
                              WHERE vbrk~vbeln = <ls_bkpf>-awkey.

    LOOP AT lt_vbrk." WHERE fkart = 'ZF25'.

      CLEAR : lv_bolnr,lv_bstkd.
      SELECT SINGLE bolnr FROM likp INTO lv_bolnr WHERE vbeln = lt_vbrk-vgbel.
      "SELECT SINGLE bstkd FROM vbkd INTO lv_bstkd WHERE vbeln = lt_vbrk-aubel.
      SELECT SINGLE bstkd
        INTO lv_bstkd
        FROM vbkd
        JOIN vbfa ON vbfa~vbelv = vbkd~vbeln
        WHERE vbfa~vbtyp_n IN ('C','H')              AND
              vbfa~vbtyp_v =   'E'                   AND
              vbfa~vbeln   =   lt_vbrk-aubel.



      LOOP AT t_bseg WHERE koart = 'D'.
        lv_index = sy-tabix.
        t_bseg-xref1    = lv_bolnr.
        t_bseg-xref2    = lv_bstkd.
        t_bseg-xref3    = lt_vbrk-vgbel.
        t_bsegsub-xref1 = t_bseg-xref1.
        t_bsegsub-xref2 = t_bseg-xref2.
        t_bsegsub-xref3 = t_bseg-xref3.
        MODIFY t_bseg    INDEX lv_index TRANSPORTING xref1 xref2 xref3.
        MODIFY t_bsegsub INDEX lv_index TRANSPORTING xref1 xref2 xref3.
      ENDLOOP.

    ENDLOOP.
  ENDLOOP.
*"----------------------------------------------------------------------

  DATA: lv_wa_bsid TYPE bsid,
        lv_wa_bsad TYPE bsad.
  IF sy-tcode <> 'FB08'.
    LOOP AT t_bseg WHERE umskz = 'F' AND bschl = '09' AND koart = 'D'.
      CLEAR lv_wa_bsid.
      SELECT SINGLE * INTO lv_wa_bsid
        FROM bsid
        WHERE bukrs = t_bseg-bukrs AND
              kunnr = t_bseg-kunnr AND
              umskz = t_bseg-umskz AND
              bschl = t_bseg-bschl AND
              vbel2 = t_bseg-vbel2 AND
              wrbtr = t_bseg-wrbtr.
      IF sy-subrc EQ 0.
        MESSAGE w010(zfi) WITH lv_wa_bsid-belnr lv_wa_bsid-vbel2.
      ENDIF.

      CLEAR lv_wa_bsad.
      SELECT SINGLE * INTO lv_wa_bsad
        FROM bsad
        WHERE bukrs = t_bseg-bukrs AND
              kunnr = t_bseg-kunnr AND
              umskz = t_bseg-umskz AND
              bschl = t_bseg-bschl AND
              vbel2 = t_bseg-vbel2 AND
              wrbtr = t_bseg-wrbtr.
      IF sy-subrc EQ 0.
        MESSAGE w010(zfi) WITH lv_wa_bsad-belnr lv_wa_bsad-vbel2.
      ENDIF.

    ENDLOOP.
  ENDIF.


*Break mehrali.
DATA:
      Field_Group TYPE SKB1-FSTAG.

CLEAR Field_Group.
LOOP AT t_bseg INTO lv_bseg WHERE ( hkont(5) = '00071' OR hkont(5) = '00072'
   or hkont(5) = '00073'  or hkont(5) = '00075') AND FKBER = ''.
  SELECT SINGLE FSTAG FROM SKB1 INTO Field_Group WHERE BUKRS = t_bkpf-bukrs AND SAKNR = t_bseg-hkont.
   IF lv_bseg-FKBER = '' and ( Field_Group = 'Z071' or Field_Group = 'Z072' or Field_Group = 'Z073' or Field_Group = 'Z075').
       MESSAGE E017(zfi) WITH lv_bseg-BUZEI.
   ENDIF.
ENDLOOP.



  PERFORM fill_bank_data          TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

  PERFORM fill_dz_item_txt        TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

  PERFORM fill_z2_item_txt        TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

  PERFORM fill_check_no           TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

  PERFORM fill_po_fbcj            TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

  PERFORM fill_person_name        TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

  PERFORM fill_vendor_name        TABLES t_bkpf
                                         t_bseg
                                         t_bsegsub.

ENDFUNCTION.



FORM fill_person_name TABLES it_bkpf    STRUCTURE bkpf
                             it_bseg    STRUCTURE bseg
                             it_bsegsub STRUCTURE bseg_subst.

  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_index   TYPE         sy-tabix.


  LOOP AT it_bseg INTO lv_bseg WHERE pernr <> ''.

    lv_index = sy-tabix.
    READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

    SELECT SINGLE concat_with_space( vnamc, nchmc, 1 ) FROM pa0002 WHERE pernr = @lv_bseg-pernr INTO @lv_bseg-xref3.

    lv_bsegsub-xref3 = lv_bseg-xref3.

    MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
    MODIFY it_bseg FROM lv_bseg.

  ENDLOOP.



ENDFORM.

FORM fill_po_fbcj TABLES it_bkpf    STRUCTURE bkpf
                         it_bseg    STRUCTURE bseg
                         it_bsegsub STRUCTURE bseg_subst.



  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_index   TYPE         sy-tabix,
        lv_ebeln   TYPE         ebeln.



  CLEAR:     lv_bkpf,lv_bseg,lv_bseg1.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.

  IF lv_bkpf-awtyp = 'CAJO'.
    CLEAR lv_ebeln.
    SELECT SINGLE ebeln INTO lv_ebeln FROM tcj_documents WHERE posting_number = lv_bkpf-awkey(10)   AND
                                                               cajo_number    = lv_bkpf-awkey+10(4) AND
                                                               comp_code      = lv_bkpf-awkey+14(4).

    IF lv_ebeln IS NOT INITIAL.

      LOOP AT it_bseg INTO lv_bseg WHERE koart = 'K'.

        lv_index = sy-tabix.
        READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

        lv_bseg-ebeln    = lv_ebeln.
        lv_bsegsub-ebeln = lv_ebeln.

        MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
        MODIFY it_bseg FROM lv_bseg.

      ENDLOOP.
    ENDIF.


  ENDIF.



ENDFORM.

FORM fill_invoice_payment_req TABLES it_bkpf    STRUCTURE bkpf
                                     it_bseg    STRUCTURE bseg
                                     it_bsegsub STRUCTURE bseg_subst.


  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_index   TYPE         sy-tabix.

  BREAK omrani.
  CLEAR:     lv_bkpf,lv_bseg.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.
  IF lv_bkpf-blart = 'ZK' AND lv_bkpf-glvor = 'RFST'.

    LOOP AT it_bseg INTO lv_bseg WHERE umskz = 'P' AND koart = 'K' AND xref3 IS NOT INITIAL .

      lv_index = sy-tabix.
      READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.


      lv_bseg-rebzj = lv_bseg-xref3(4).
      lv_bseg-rebzg = lv_bseg-xref3+4(10).
      lv_bseg-rebzz = lv_bseg-xref3+14(3).
      lv_bseg-rebzt = 'P'.
      lv_bseg-buzid = 'P'.
      lv_bseg-ktosl = '31'.
      lv_bsegsub-rebzj = lv_bseg-xref3(4).
      lv_bsegsub-rebzg = lv_bseg-xref3+4(10).
      lv_bsegsub-rebzz = lv_bseg-xref3+14(3).
      lv_bsegsub-rebzt = 'P'.
      lv_bsegsub-buzid = 'P'.
      lv_bsegsub-ktosl = '31'.

      MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
      MODIFY it_bseg FROM lv_bseg.

    ENDLOOP.

  ENDIF.


ENDFORM.



FORM fill_dz_item_txt TABLES it_bkpf    STRUCTURE bkpf
                             it_bseg    STRUCTURE bseg
                             it_bsegsub STRUCTURE bseg_subst.



  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_index   TYPE         sy-tabix,
        lv_txt(50).



  CLEAR:     lv_bkpf,lv_bseg,lv_bseg1.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.


  IF lv_bkpf-blart = 'DZ'.

    LOOP AT it_bseg INTO lv_bseg WHERE sgtxt IS INITIAL AND vbel2 IS NOT INITIAL AND koart = 'D' AND umskz = 'A' AND bschl = '19'.

      lv_index = sy-tabix.
      READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

      lv_txt = lv_bseg-vbel2.
      SHIFT lv_txt LEFT DELETING LEADING '0'.
      CONCATENATE 'واريز نقدي سفارش' lv_txt INTO lv_txt SEPARATED BY space.

      lv_bseg-sgtxt    = lv_txt .
      lv_bsegsub-sgtxt = lv_txt .

      MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
      MODIFY it_bseg FROM lv_bseg.

    ENDLOOP.

*    LOOP AT it_bseg INTO lv_bseg WHERE koart = 'D' AND umskz = '' AND bschl = '15' AND vbel2 IS NOT INITIAL AND vbeln IS NOT INITIAL.
*
*      lv_index = sy-tabix.
*      READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.
*
*      lv_txt = lv_bseg-vbel2.
*      SHIFT lv_txt LEFT DELETING LEADING '0'.
*      CONCATENATE 'واريز نقدي سفارش' lv_txt INTO lv_txt SEPARATED BY space.
*
*      lv_bseg-sgtxt    = lv_txt .
*      lv_bsegsub-sgtxt = lv_txt .
*
*      MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
*      MODIFY it_bseg FROM lv_bseg.
*
*    ENDLOOP.

  ENDIF.



ENDFORM.


FORM fill_bank_data TABLES it_bkpf    STRUCTURE bkpf
                           it_bseg    STRUCTURE bseg
                           it_bsegsub STRUCTURE bseg_subst.



  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_index   TYPE         sy-tabix.



  CLEAR:     lv_bkpf,lv_bseg,lv_bseg1.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.

  BREAK omrani.
  IF lv_bkpf-blart = 'ZC'.

    LOOP AT it_bseg INTO lv_bseg1 WHERE hbkid IS NOT INITIAL .
      EXIT.
    ENDLOOP.
    IF lv_bseg1-hbkid IS NOT INITIAL.
      LOOP AT it_bseg INTO lv_bseg WHERE hbkid IS INITIAL .

        lv_index = sy-tabix.
        READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

        lv_bseg-hbkid    = lv_bseg1-hbkid.
        lv_bsegsub-hbkid = lv_bseg1-hbkid.

        lv_bseg-hktid    = lv_bseg1-hktid.
        lv_bsegsub-hktid = lv_bseg1-hktid.

        MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
        MODIFY it_bseg FROM lv_bseg.

      ENDLOOP.
    ENDIF.


  ENDIF.



ENDFORM.

FORM fill_check_no TABLES it_bkpf    STRUCTURE bkpf
                          it_bseg    STRUCTURE bseg
                          it_bsegsub STRUCTURE bseg_subst.

  DATA: lv_boeno   TYPE         bsed-boeno,
        lv_vbel2   TYPE         bseg-vbel2,
        lv_posn2   TYPE         bseg-posn2,
        lv_bseg    LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_index   TYPE         sy-tabix,
        lv_bsed    TYPE         bsed,
        lv_ausz3   TYPE         ausz_clr,
        lv_txt(50).


  FIELD-SYMBOLS: <xbsed>  TYPE ANY TABLE,
                 <xausz3> TYPE ANY TABLE.

  CLEAR:     lv_bkpf,lv_bseg.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.

  IF lv_bkpf-blart = 'DA' OR lv_bkpf-blart = 'SA' .

    CLEAR lv_txt.
    IMPORT lv_txt TO lv_txt FROM MEMORY ID  'RFIDTRBOE1'. " Export in RFIDTRBOE1_ALV_SUBROUTINES | RFIDTRBOE2_ALV_SUBROUTINES

    SELECT SINGLE boeno INTO lv_boeno FROM bsed WHERE bukrs =  lv_bkpf-bukrs        AND
                                                      belnr =  lv_bkpf-bktxt(10)    AND
                                                      gjahr =  lv_bkpf-bktxt+13(4)  AND
                                                      buzei =  lv_bkpf-bktxt+10(3).
    SELECT SINGLE vbel2 posn2 INTO (lv_vbel2,lv_posn2) FROM bseg WHERE bukrs =  lv_bkpf-bukrs        AND
                                                                       belnr =  lv_bkpf-bktxt(10)    AND
                                                                       gjahr =  lv_bkpf-bktxt+13(4)  AND
                                                                       vbel2 <> ''.

    IF sy-subrc EQ 0.
      LOOP AT it_bseg INTO lv_bseg.
        lv_index = sy-tabix.
        READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

        lv_bseg-boeno           = lv_boeno.
        lv_bsegsub-boeno        = lv_boeno.

        lv_bseg-check_belnr     = lv_bkpf-bktxt(10).
        lv_bsegsub-check_belnr  = lv_bkpf-bktxt(10).

        lv_bseg-check_gjahr     = lv_bkpf-bktxt+13(4).
        lv_bsegsub-check_gjahr  = lv_bkpf-bktxt+13(4).

        lv_bseg-check_buzei     = lv_bkpf-bktxt+10(3).
        lv_bsegsub-check_buzei  = lv_bkpf-bktxt+10(3).

        lv_bseg-check_status    = lv_txt.
        lv_bsegsub-check_status = lv_txt.

        lv_bseg-vbel2           = lv_vbel2.
        lv_bsegsub-vbel2        = lv_vbel2.

        lv_bseg-posn2           = lv_posn2.
        lv_bsegsub-posn2        = lv_posn2.

        PERFORM update_check_status USING lv_bkpf-bukrs
                                          lv_bseg-check_belnr
                                          lv_bseg-check_gjahr
                                          lv_bseg-check_buzei
                                          lv_bseg-boeno
                                          lv_bseg-check_status
                                          lv_bkpf-budat.

        MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
        MODIFY it_bseg FROM lv_bseg.
      ENDLOOP.
    ENDIF.
  ENDIF.


  IF lv_bkpf-blart = 'Z2'.

    ASSIGN ('(SAPMF05A)XBSED[]')  TO <xbsed>.
    ASSIGN ('(SAPMF05A)XAUSZ3[]') TO <xausz3>.
    IF <xbsed> IS ASSIGNED.
      LOOP AT <xbsed> INTO lv_bsed.
        EXIT.
      ENDLOOP.
      IF lv_bsed-boeno IS NOT INITIAL.

        LOOP AT it_bseg INTO lv_bseg.
          lv_index = sy-tabix.
          READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

          lv_bseg-boeno    = lv_bsed-boeno.
          lv_bsegsub-boeno = lv_bsed-boeno.

          MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
          MODIFY it_bseg FROM lv_bseg.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.
FORM update_check_status USING lv_bukrs
                               lv_check_belnr
                               lv_check_gjahr
                               lv_check_buzei
                               lv_boeno
                               lv_check_status
                               lv_h_budat.

  DATA: lv_it_data TYPE TABLE OF zfi_check_status,
        lv_wa_data LIKE LINE OF  lv_it_data.

  REFRESH lv_it_data.
  CLEAR lv_wa_data.

  lv_wa_data-check_bukrs  = lv_bukrs.
  lv_wa_data-check_belnr  = lv_check_belnr.
  lv_wa_data-check_gjahr  = lv_check_gjahr.
  lv_wa_data-check_buzei  = lv_check_buzei.
  lv_wa_data-boeno        = lv_boeno.
  lv_wa_data-check_status = lv_check_status.
  lv_wa_data-h_budat      = lv_h_budat.

  APPEND lv_wa_data TO lv_it_data.
  MODIFY zfi_check_status FROM TABLE lv_it_data.


ENDFORM.



FORM fill_z2_item_txt TABLES it_bkpf    STRUCTURE bkpf
                             it_bseg    STRUCTURE bseg
                             it_bsegsub STRUCTURE bseg_subst.



  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_index   TYPE         sy-tabix,
        lv_txt(50).



  CLEAR:     lv_bkpf,lv_bseg,lv_bseg1.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.


  IF lv_bkpf-blart = 'Z2'.

    LOOP AT it_bseg INTO lv_bseg1 WHERE bschl = '09'.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      LOOP AT it_bseg INTO lv_bseg WHERE bschl <> '09'.

        lv_index = sy-tabix.
        READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

        lv_bseg-xref1    = lv_bseg1-xref1 .
        lv_bsegsub-xref1 = lv_bseg1-xref1 .

        lv_bseg-xref2    = lv_bseg1-xref2 .
        lv_bsegsub-xref2 = lv_bseg1-xref2 .

        lv_bseg-xref3    = lv_bseg1-xref3 .
        lv_bsegsub-xref3 = lv_bseg1-xref3 .

        lv_txt = lv_bseg-vbel2.
        SHIFT lv_txt LEFT DELETING LEADING '0'.
        CONCATENATE 'دريافت چک سفارش' lv_txt INTO lv_txt SEPARATED BY space.

        lv_bseg-sgtxt    = lv_txt .
        lv_bsegsub-sgtxt = lv_txt .


        MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
        MODIFY it_bseg FROM lv_bseg.

      ENDLOOP.
    ENDIF.
  ENDIF.



ENDFORM.



FORM fill_vendor_name TABLES it_bkpf    STRUCTURE bkpf
                             it_bseg    STRUCTURE bseg
                             it_bsegsub STRUCTURE bseg_subst.

  DATA: lv_bseg    LIKE LINE OF it_bseg,
        lv_bseg1   LIKE LINE OF it_bseg,
        lv_bkpf    LIKE LINE OF it_bkpf,
        lv_bsegsub LIKE LINE OF it_bsegsub,
        lv_index   TYPE         sy-tabix,
        lv_lifnr   TYPE         lifnr.


  LOOP AT it_bseg INTO lv_bseg WHERE ktosl = 'WIT' AND zuonr <> ''.

    lv_index = sy-tabix.
    READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

    lv_lifnr = lv_bseg-zuonr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_lifnr
      IMPORTING
        output = lv_lifnr.

    SELECT SINGLE name1 INTO lv_bseg-sgtxt FROM lfa1 WHERE lifnr = lv_lifnr.



    lv_bsegsub-sgtxt = lv_bseg-sgtxt.

    MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
    MODIFY it_bseg FROM lv_bseg.

  ENDLOOP.


  CLEAR:     lv_bkpf,lv_bseg.
  READ TABLE it_bkpf INTO lv_bkpf INDEX 1.

  IF lv_bkpf-blart = 'RE'.
    CLEAR lv_bseg1.
    READ TABLE it_bseg INTO lv_bseg1 WITH KEY koart = 'K'.
    IF sy-subrc EQ 0 AND lv_bseg1-sgtxt IS NOT INITIAL.
      LOOP AT it_bseg INTO lv_bseg WHERE ktosl <> 'WIT' AND sgtxt = '' AND koart <> 'K'.

        lv_index = sy-tabix.
        READ TABLE it_bsegsub INTO lv_bsegsub INDEX lv_index.

        lv_bseg-sgtxt    = lv_bseg1-sgtxt.
        lv_bsegsub-sgtxt = lv_bseg1-sgtxt.
        MODIFY it_bsegsub FROM lv_bsegsub INDEX lv_index.
        MODIFY it_bseg FROM lv_bseg.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
