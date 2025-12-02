*&---------------------------------------------------------------------*
*& Report  ZMIR7
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zmir7.



TABLES : likp,ekko.

TYPE-POOLS:slis.


DATA: gv_it_likp   TYPE TABLE OF likp,
      gv_wa_likp   LIKE LINE OF  gv_it_likp,
      gv_it_lips   TYPE TABLE OF lips,
      gv_wa_lips   LIKE LINE OF  gv_it_lips,
      gv_it_ekko   TYPE TABLE OF ekko,
      gv_wa_ekko   LIKE LINE OF  gv_it_ekko,
      gv_it_ekpo   TYPE TABLE OF ekpo,
      gv_wa_ekpo   LIKE LINE OF  gv_it_ekpo,
      gv_it_log    TYPE TABLE OF bapiret2,
      gv_wa_log    LIKE LINE OF  gv_it_log,
      gv_subrc(1),
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .

PARAMETERS: p_invod TYPE sy-datum OBLIGATORY,
            p_postd TYPE sy-datum OBLIGATORY,
            p_basef TYPE sy-datum OBLIGATORY,
            p_xblnr TYPE xblnr1   OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-003.
PARAMETERS:      p_lifnr TYPE lifnr    OBLIGATORY.
SELECTION-SCREEN COMMENT 45(60) txtlifnr.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_bukrs TYPE bukrs    OBLIGATORY.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-001.
PARAMETERS: p_value TYPE wrbtr OBLIGATORY.
*SELECTION-SCREEN COMMENT 54(8) TEXT-002.
PARAMETERS: p_waers TYPE waers OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: s_vbeln FOR likp-vbeln,
                s_ebeln FOR ekko-ebeln.

PARAMETERS: p_kschl  TYPE zmir7-kschl  OBLIGATORY,
            p_sgtxt  TYPE sgtxt        OBLIGATORY,
            p_sgtxt1 TYPE sgtxt,
            p_mwskz  TYPE t007a-mwskz  OBLIGATORY,
            p_tamnt  TYPE bapi_incinv_create_tax-tax_amount,
            p_zuonr TYPE invfo-zuonr,
            p_rate  TYPE bapi_incinv_create_header-exch_rate,
            p_xref1 TYPE bseg-xref1,
            p_xref2 TYPE bseg-xref2,
            p_xref3 TYPE bseg-xref3.

SELECTION-SCREEN: END   OF BLOCK blk1.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_sgtxt1.
  PERFORM sgtxt1_f4help.

AT SELECTION-SCREEN OUTPUT.
  PERFORM get_lifnr_txt.

START-OF-SELECTION.

  REFRESH gv_it_log.
  PERFORM check_input_data CHANGING gv_subrc.

  IF gv_subrc IS INITIAL.
    IF gv_it_log[] IS INITIAL.

      IF s_ebeln IS INITIAL.
        PERFORM get_data.
        PERFORM create_invoice.
      ELSE.
        PERFORM get_po_data.
        PERFORM create_invoice_from_po.
      ENDIF.
    ENDIF.

    PERFORM build_fieldcatalog.
    PERFORM display_grid.
  ENDIF.







*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'BAPIRET2'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'MESSAGE_V1'.
        fieldcatalog-seltext_l     = 'Invoice'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN 'MESSAGE_V2'.
        fieldcatalog-seltext_l     = 'Year'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN 'MESSAGE_V3'.
        fieldcatalog-seltext_l     = 'Amount'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN 'MESSAGE_V4'.
        fieldcatalog-seltext_l     = 'Currency'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN OTHERS.
    ENDCASE.



    MODIFY fieldcatalog INDEX sy-tabix.
  ENDLOOP.


ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_grid .

  DATA: is_layout TYPE slis_layout_alv .

  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat             = fieldcatalog[]
      i_callback_user_command = 'USER_COMMAND'
      i_save                  = 'X'
      is_layout               = is_layout
    TABLES
      t_outtab                = gv_it_log
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    " DISPLAY_GRID
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .


  DATA: lv_ebelp TYPE ekpo-ebelp.



  IF s_ebeln IS NOT INITIAL.

    REFRESH gv_it_ekpo.
    SELECT * FROM ekpo INTO TABLE gv_it_ekpo WHERE ebeln IN s_ebeln AND loekz = ''.

    IF gv_it_ekpo IS NOT INITIAL.
      SELECT * INTO TABLE gv_it_lips
        FROM lips
        FOR ALL ENTRIES IN gv_it_ekpo
        WHERE vgbel = gv_it_ekpo-ebeln.
      SORT gv_it_lips BY vbeln.
      DELETE ADJACENT DUPLICATES FROM gv_it_lips COMPARING vbeln.
      LOOP AT gv_it_lips INTO gv_wa_lips.
        s_vbeln-low    = gv_wa_lips-vbeln.
        s_vbeln-option = 'EQ'.
        s_vbeln-sign   = 'I'.
        APPEND s_vbeln.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF s_vbeln IS INITIAL.
    MESSAGE 'Data not found' TYPE 'E'.
  ENDIF.



  REFRESH gv_it_likp.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gv_it_likp
    FROM likp
    WHERE vbeln IN s_vbeln.

  REFRESH gv_it_lips.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gv_it_lips
    FROM lips
    WHERE vbeln IN s_vbeln.

  DELETE gv_it_lips WHERE vgbel IS INITIAL.


  REFRESH gv_it_ekpo.
  IF gv_it_lips[] IS NOT INITIAL.

    LOOP AT gv_it_lips INTO gv_wa_lips.
      lv_ebelp = gv_wa_lips-vgpos.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF gv_wa_ekpo
      FROM ekpo
      WHERE ebeln = gv_wa_lips-vgbel AND
            ebelp = lv_ebelp         AND
            loekz = ''.
      IF sy-subrc EQ 0.
        APPEND gv_wa_ekpo TO gv_it_ekpo.
      ENDIF.
    ENDLOOP.

  ENDIF.


  SORT gv_it_ekpo.
  DELETE ADJACENT DUPLICATES FROM gv_it_ekpo COMPARING ALL FIELDS.


ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_data CHANGING lv_subrc TYPE char1.


  DATA: lv_it_likp TYPE TABLE OF likp,
        lv_wa_likp LIKE LINE OF  lv_it_likp,
        lv_it_rbkp TYPE TABLE OF rbkp,
        lv_wa_rbkp LIKE LINE OF  lv_it_rbkp,
        lv_wa_rseg TYPE          rseg,
        lv_wa_mir7 TYPE          zmir7,
        lv_wa_t685 TYPE          t685,
        lv_rbkp    TYPE          rbkp,
        lv_amount  TYPE          wrbtr.




  CLEAR lv_subrc.


  IF s_vbeln IS INITIAL AND s_ebeln IS INITIAL.
    CONCATENATE 'Plz Fill Delivery or PO.' '' INTO gv_wa_log-message SEPARATED BY space.
    MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

  IF s_vbeln IS NOT INITIAL.
    REFRESH lv_it_likp.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE lv_it_likp
      FROM likp
      WHERE vbeln IN s_vbeln.
  ENDIF.

*  LOOP AT lv_it_likp INTO lv_wa_likp.
*
*    IF ( lv_wa_likp-lfart <> 'Z301' AND lv_wa_likp-lfart <> 'Z305' ).
*
*      CLEAR gv_wa_log.
*      gv_wa_log-type = 'E'.
**      CONCATENATE 'Inbound:' lv_wa_likp-vbeln '---> Wrong Type.' INTO gv_wa_log-message SEPARATED BY space.
*      APPEND gv_wa_log TO gv_it_log.
*
*    ENDIF.
*
*  ENDLOOP.


  IF p_sgtxt1 IS INITIAL.
    CONCATENATE 'Doc Header Text:' 'Please Fill.' INTO gv_wa_log-message SEPARATED BY space.
    MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ELSE.
*    SELECT SINGLE * INTO lv_wa_mir7 FROM zmir7 WHERE kschl = p_kschl AND sgtxt = p_sgtxt1.
*    IF sy-subrc NE 0.
*      CLEAR gv_wa_log.
*      gv_wa_log-type = 'E'.
*      CONCATENATE 'Doc Header Text:' 'Select from Search Help.' INTO gv_wa_log-message SEPARATED BY space.
*      MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
*      lv_subrc = 1.
*    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO lv_wa_t685 FROM t685 WHERE kvewe = 'A' AND kappl = 'M' AND kschl = p_kschl.
  IF sy-subrc NE 0.
    CONCATENATE 'Condition Type:' 'Wrong Condition.' INTO gv_wa_log-message SEPARATED BY space.
    MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

*  IF ( p_kschl = 'ZCF4' OR p_kschl = 'ZCF6' )  AND s_ebeln IS INITIAL.
*    CONCATENATE 'Error: ' 'Fill PO number for ZCF4/ZCF6 Condition.' INTO gv_wa_log-message SEPARATED BY space.
*    MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
*    lv_subrc = 1.
*  ENDIF.

*  IF p_kschl <> 'ZCF4' AND p_kschl <> 'ZCF6' AND s_vbeln IS INITIAL.
*    CONCATENATE 'Error: ' 'Fill Inbound number.' INTO gv_wa_log-message SEPARATED BY space.
*    MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
*    lv_subrc = 1.
*  ENDIF.

  IF s_ebeln IS NOT INITIAL AND s_vbeln IS NOT INITIAL.
    CONCATENATE 'Error: ' 'Fill Inbound or PO.' INTO gv_wa_log-message SEPARATED BY space.
    MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.


  CHECK lv_subrc IS INITIAL.

  PERFORM convert_amount_to_internal USING p_value p_waers CHANGING lv_amount.

  REFRESH lv_it_rbkp.
  SELECT * INTO TABLE lv_it_rbkp
    FROM rbkp
    WHERE budat  =  p_postd   AND
          xblnr  =  p_xblnr   AND
          lifnr  =  p_lifnr   AND
          rbstat <> '2'       AND
          stblg  =  ''        AND
          "rmwwr = lv_amount AND
          waers = p_waers.

  LOOP AT lv_it_rbkp INTO lv_wa_rbkp.
    IF lv_wa_rbkp-rmwwr - lv_wa_rbkp-wmwst1 =  lv_amount.
      CONCATENATE 'document has already been entered under number->' lv_wa_rbkp-belnr ':' lv_wa_rbkp-gjahr INTO gv_wa_log-message SEPARATED BY space.
      MESSAGE gv_wa_log-message TYPE 'S' DISPLAY LIKE 'E'.
      lv_subrc = 1.
      EXIT.
    ENDIF.
  ENDLOOP.






ENDFORM.                    " CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_invoice .

  DATA: lv_header       TYPE          bapi_incinv_create_header,
        lv_it_item      TYPE TABLE OF bapi_incinv_create_item,
        lv_wa_item      LIKE LINE OF  lv_it_item,
        lv_it_tax       TYPE TABLE OF bapi_incinv_create_tax,
        lv_wa_tax       LIKE LINE OF  lv_it_tax,
        lv_it_return    TYPE TABLE OF bapiret2,
        lv_wa_return    LIKE LINE OF  lv_it_return,
        lv_it_ext       TYPE TABLE OF bapiparex,
        lv_wa_ext       LIKE LINE OF  lv_it_ext,
        lv_inv_doc      TYPE          bapi_incinv_fld-inv_doc_no,
        lv_inv_doc_year TYPE          bapi_incinv_fld-fisc_year,
        lv_wa_ekko      TYPE          ekko,
        lv_total_netpr  TYPE          bapiwrbtr,
        lv_netpr        TYPE          ekpo-netpr,
        lv_total_inb    TYPE          bapiwrbtr,
        lv_ebelp        TYPE          ekpo-ebelp,
        lv_index        TYPE          i,
        lv_item         TYPE          i,
        lv_xmwst        TYPE          xmwst,
        lv_rbkp         TYPE          rbkp,
        lv_found(1).






  BREAK omrani.
  CLEAR lv_total_netpr.
  LOOP AT gv_it_lips INTO gv_wa_lips.
    lv_ebelp = gv_wa_lips-vgpos.
    CLEAR: gv_wa_ekpo,lv_wa_ekko.
    READ TABLE gv_it_ekpo INTO gv_wa_ekpo WITH KEY ebeln = gv_wa_lips-vgbel ebelp = lv_ebelp.
    lv_index = sy-tabix.
    SELECT SINGLE * INTO lv_wa_ekko FROM ekko WHERE ebeln = gv_wa_ekpo-ebeln.
    CLEAR: lv_netpr.
    PERFORM convert_currency USING gv_wa_ekpo-netpr lv_wa_ekko-waers p_waers CHANGING lv_netpr.
    lv_total_netpr = lv_total_netpr + ( ( ( lv_netpr / gv_wa_ekpo-peinh ) * gv_wa_lips-lfimg )  ).

  ENDLOOP.



  "LOOP AT gv_it_likp INTO gv_wa_likp.


  CLEAR: lv_header,lv_total_inb,lv_rbkp.

  lv_header-invoice_ind    = 'X'.
  lv_header-doc_type       = 'RE'.
  lv_header-doc_date       = p_invod.
  lv_header-pstng_date     = p_postd.
  lv_header-ref_doc_no     = p_xblnr.
  lv_header-comp_code      = p_bukrs.
  lv_header-diff_inv       = p_lifnr.
  lv_header-currency       = p_waers.
  "lv_header-gross_amount  = p_value.
  lv_header-bline_date     = p_basef.
  lv_header-header_txt     = p_sgtxt1.
  lv_header-item_text      = p_sgtxt.
  lv_header-del_costs_taxc = p_mwskz.
  lv_header-alloc_nmbr     = p_zuonr.
  IF p_rate IS NOT INITIAL.
    lv_header-exch_rate      = p_rate.
  ELSE.
    PERFORM get_exchange_rate USING p_waers CHANGING lv_header-exch_rate.
  ENDIF.

*  lv_rbkp-xref1 = p_xref1.
*  lv_rbkp-xref2 = p_xref2.
*  lv_rbkp-xref3 = p_xref3.
*  EXPORT lv_rbkp FROM lv_rbkp TO MEMORY ID  'ZRBKP'. "" Import in FM: MRM_HEADER_PREPARE

  REFRESH: lv_it_item.
  CLEAR lv_item.
  LOOP  AT gv_it_lips INTO gv_wa_lips." WHERE vbeln = gv_wa_likp-vbeln.


    lv_ebelp = gv_wa_lips-vgpos.
    CLEAR: gv_wa_ekpo,lv_found.
    READ TABLE gv_it_ekpo INTO gv_wa_ekpo WITH KEY ebeln = gv_wa_lips-vgbel ebelp = lv_ebelp.

    CLEAR lv_wa_item.
    READ TABLE lv_it_item INTO lv_wa_item WITH KEY po_number = gv_wa_lips-vgbel po_item = gv_wa_lips-vgpos.
    IF sy-subrc EQ 0.
      lv_index = sy-tabix.
      lv_found = 'X'.
      lv_item = lv_wa_item-invoice_doc_item.
    ELSE.
      CLEAR lv_found.
      lv_item = lv_item + 1.
    ENDIF.

    CLEAR lv_wa_ekko.
    SELECT SINGLE * INTO lv_wa_ekko FROM ekko WHERE ebeln = gv_wa_ekpo-ebeln.
    PERFORM convert_currency USING gv_wa_ekpo-netpr lv_wa_ekko-waers p_waers CHANGING gv_wa_ekpo-netpr.





    lv_wa_item-invoice_doc_item = lv_item.
    lv_wa_item-po_number        = gv_wa_lips-vgbel.
    lv_wa_item-po_item          = gv_wa_lips-vgpos.
    lv_wa_item-item_amount      = lv_wa_item-item_amount + ( ( p_value / lv_total_netpr ) * ( ( ( gv_wa_ekpo-netpr / gv_wa_ekpo-peinh ) * gv_wa_lips-lfimg ) ) ).
    lv_wa_item-quantity         = lv_wa_item-quantity + gv_wa_lips-lfimg.
    lv_wa_item-po_unit          = gv_wa_lips-meins.
    PERFORM convert_unit USING gv_wa_ekpo-matnr lv_wa_item-quantity gv_wa_lips-meins gv_wa_ekpo-bprme CHANGING lv_wa_item-po_pr_qnt.
    lv_wa_item-po_pr_uom        = gv_wa_ekpo-bprme.
    lv_wa_item-cond_type        = p_kschl.
    "lv_wa_item-freight_ven      = p_lifnr.
    IF lv_wa_item-item_text IS INITIAL.
      lv_wa_item-item_text        = gv_wa_lips-vbeln.
    ELSE.
      CONCATENATE lv_wa_item-item_text gv_wa_lips-vbeln INTO lv_wa_item-item_text SEPARATED BY '-'.
    ENDIF.

    IF p_tamnt > 0 .
      lv_wa_item-tax_code         = p_mwskz.
    ELSE.
      lv_wa_item-tax_code         = p_mwskz.
      lv_xmwst = 'X'.
    ENDIF.
    lv_total_inb = lv_total_inb + ( ( p_value / lv_total_netpr ) * ( ( ( gv_wa_ekpo-netpr / gv_wa_ekpo-peinh ) * gv_wa_lips-lfimg ) ) ).

    IF lv_found = 'X'.
      MODIFY lv_it_item FROM lv_wa_item INDEX lv_index.
    ELSE.
      APPEND lv_wa_item TO lv_it_item.
    ENDIF.
  ENDLOOP.



  REFRESH lv_it_tax.
  IF p_tamnt IS NOT INITIAL.
    CLEAR lv_wa_tax.
    lv_wa_tax-tax_amount = p_tamnt.
    lv_wa_tax-tax_code   = p_mwskz.
    APPEND lv_wa_tax TO lv_it_tax.
  ELSE.
    CLEAR lv_wa_tax.
    lv_wa_tax-tax_code   = p_mwskz.
    APPEND lv_wa_tax TO lv_it_tax.
  ENDIF.
  lv_header-calc_tax_ind = lv_xmwst.
  lv_header-gross_amount = lv_total_inb + p_tamnt.

  IF lv_header-calc_tax_ind = 'X'.
    CLEAR lv_it_tax[].
  ENDIF.


  IF lv_it_item[] IS NOT INITIAL.

    REFRESH lv_it_return.
    CLEAR: lv_inv_doc, lv_inv_doc_year.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
      EXPORTING
        headerdata       = lv_header
      IMPORTING
        invoicedocnumber = lv_inv_doc
        fiscalyear       = lv_inv_doc_year
      TABLES
        itemdata         = lv_it_item
        taxdata          = lv_it_tax
        return           = lv_it_return.

  ENDIF.

  LOOP AT lv_it_return INTO lv_wa_return WHERE type = 'E'.
    "lv_wa_return-message_v2 = gv_wa_likp-vbeln.
    lv_wa_return-message_v3 = lv_total_inb.
    lv_wa_return-message_v4 = p_waers.
    APPEND lv_wa_return TO gv_it_log.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR gv_wa_log.
    gv_wa_log-type = 'S'.
    "gv_wa_log-message_v2 = gv_wa_likp-vbeln.
    gv_wa_log-message_v3 = lv_total_inb.
    gv_wa_log-message_v4 = p_waers.
    gv_wa_log-message_v1 = lv_inv_doc.
    gv_wa_log-message_v2 = lv_inv_doc_year.
    CONCATENATE 'Document Number:' lv_inv_doc '-' lv_inv_doc_year 'Created' INTO gv_wa_log-message SEPARATED BY space.
    APPEND gv_wa_log TO gv_it_log.

  ENDIF.



  "ENDLOOP.

ENDFORM.                    " CREATE_INVOICE
*&---------------------------------------------------------------------*
*&      Form  GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_WAERS  text
*      <--P_LV_HEADER_EXCH_RATE  text
*----------------------------------------------------------------------*
FORM get_exchange_rate  USING    lv_waers TYPE waers
                        CHANGING lv_rate  TYPE ukursp.

  DATA: lv_bapi        TYPE bapi1093_0.


  CLEAR lv_bapi.
  CALL FUNCTION 'BAPI_EXCHANGERATE_GETDETAIL'
    EXPORTING
      rate_type  = 'M'
      from_curr  = lv_waers
      to_currncy = 'IRR'
      date       = sy-datum
    IMPORTING
      exch_rate  = lv_bapi.

  lv_rate = lv_bapi-exch_rate.

ENDFORM.                    " GET_EXCHANGE_RATE
*&---------------------------------------------------------------------*
*&      Form  SGTXT1_F4HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sgtxt1_f4help .


  DATA: lv_it_return TYPE STANDARD TABLE OF ddshretval WITH HEADER LINE,
        lv_it_f4     TYPE TABLE OF zmir7.




  REFRESH lv_it_f4.
  IF p_kschl IS INITIAL.
    SELECT * INTO TABLE lv_it_f4 FROM zmir7.
  ELSE.
    SELECT * INTO TABLE lv_it_f4 FROM zmir7 WHERE kschl = p_kschl .
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'SGTXT'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_SGTXT1'
      value_org   = 'S'
      display     = 'F'
    TABLES
      value_tab   = lv_it_f4
      return_tab  = lv_it_return.


ENDFORM.                    " SGTXT1_F4HELP



*&---------------------------------------------------------------------*
*&      Form  convert_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->lv_kwert  text
*      -->lv_waers  text
*      -->lv_changed  text
*----------------------------------------------------------------------*
FORM convert_currency  USING lv_kwert    TYPE ekpo-netpr
                             lv_waers    TYPE waers
                             lv_waers_to TYPE waers
                    CHANGING lv_changed  TYPE ekpo-netpr.


  IF lv_waers <> p_waers.


    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
        date             = sy-datum
        foreign_amount   = lv_kwert
        foreign_currency = lv_waers
        local_currency   = lv_waers_to
      IMPORTING
        local_amount     = lv_changed
      EXCEPTIONS
        no_rate_found    = 1
        overflow         = 2
        no_factors_found = 3
        no_spread_found  = 4
        derived_2_times  = 5
        OTHERS           = 6.
  ELSE.
    lv_changed = lv_kwert.
  ENDIF.

ENDFORM.                    "convert_CURRENCY
*&---------------------------------------------------------------------*
*&      Form  GET_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_po_data .


  REFRESH gv_it_ekko.
  SELECT * FROM ekko INTO TABLE gv_it_ekko WHERE ebeln IN s_ebeln AND loekz = ''.

  REFRESH gv_it_ekpo.
  SELECT * FROM ekpo INTO TABLE gv_it_ekpo WHERE ebeln IN s_ebeln AND loekz = ''.


ENDFORM.                    " GET_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_INVOICE_FROM_PO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_invoice_from_po .


  DATA: lv_header       TYPE          bapi_incinv_create_header,
        lv_it_item      TYPE TABLE OF bapi_incinv_create_item,
        lv_wa_item      LIKE LINE OF  lv_it_item,
        lv_it_tax       TYPE TABLE OF bapi_incinv_create_tax,
        lv_wa_tax       LIKE LINE OF  lv_it_tax,
        lv_it_return    TYPE TABLE OF bapiret2,
        lv_wa_return    LIKE LINE OF  lv_it_return,
        lv_inv_doc      TYPE          bapi_incinv_fld-inv_doc_no,
        lv_inv_doc_year TYPE          bapi_incinv_fld-fisc_year,
        lv_wa_ekko      TYPE          ekko,
        lv_total_netpr  TYPE          bapiwrbtr,
        lv_netpr        TYPE          ekpo-netpr,
        lv_total_inb    TYPE          bapiwrbtr,
        lv_ebelp        TYPE          ekpo-ebelp,
        lv_index        TYPE          i,
        lv_item         TYPE          i,
        lv_xmwst        TYPE          xmwst,
        lv_rbkp         TYPE          rbkp,
        lv_found(1).






  BREAK omrani.
  CLEAR lv_total_netpr.

  LOOP AT gv_it_ekpo INTO gv_wa_ekpo.
    CLEAR gv_wa_ekko.
    READ TABLE gv_it_ekko INTO gv_wa_ekko WITH KEY ebeln = gv_wa_ekpo-ebeln.
    CLEAR: lv_netpr.
    PERFORM convert_currency USING gv_wa_ekpo-netpr gv_wa_ekko-waers p_waers CHANGING lv_netpr.
    lv_total_netpr = lv_total_netpr + ( ( ( lv_netpr / gv_wa_ekpo-peinh ) * gv_wa_ekpo-menge )  ).
  ENDLOOP.



  "LOOP AT gv_it_ekko INTO gv_wa_ekko.


  CLEAR: lv_header,lv_total_inb.

  lv_header-invoice_ind    = 'X'.
  lv_header-doc_type       = 'RE'.
  lv_header-doc_date       = p_invod.
  lv_header-pstng_date     = p_postd.
  lv_header-ref_doc_no     = p_xblnr.
  lv_header-comp_code      = p_bukrs.
  lv_header-diff_inv       = p_lifnr.
  lv_header-currency       = p_waers.
  "lv_header-gross_amount  = p_value.
  lv_header-bline_date     = p_basef.
  lv_header-header_txt     = p_sgtxt1.
  lv_header-item_text      = p_sgtxt.
  lv_header-del_costs_taxc = p_mwskz.
  lv_header-alloc_nmbr     = p_zuonr.

  IF p_rate IS NOT INITIAL.
    lv_header-exch_rate      = p_rate.
  ELSE.
    PERFORM get_exchange_rate USING p_waers CHANGING lv_header-exch_rate.
  ENDIF.

*  lv_rbkp-xref1 = p_xref1.
*  lv_rbkp-xref2 = p_xref2.
*  lv_rbkp-xref3 = p_xref3.
*  EXPORT lv_rbkp FROM lv_rbkp TO MEMORY ID  'ZRBKP'. "" Import in FM: MRM_HEADER_PREPARE

  REFRESH: lv_it_item.
  CLEAR lv_item.
  LOOP AT gv_it_ekpo INTO gv_wa_ekpo ."WHERE ebeln = gv_wa_ekko-ebeln.
    CLEAR gv_wa_ekko.
    READ TABLE gv_it_ekko INTO gv_wa_ekko WITH KEY ebeln = gv_wa_ekpo-ebeln.

    CLEAR: lv_found.
    CLEAR lv_wa_item.
    READ TABLE lv_it_item INTO lv_wa_item WITH KEY po_number = gv_wa_ekpo-ebeln po_item = gv_wa_ekpo-ebelp.
    IF sy-subrc EQ 0.
      lv_index = sy-tabix.
      lv_found = 'X'.
      lv_item = lv_wa_item-invoice_doc_item.
    ELSE.
      CLEAR lv_found.
      lv_item = lv_item + 1.
    ENDIF.

    PERFORM convert_currency USING gv_wa_ekpo-netpr gv_wa_ekko-waers p_waers CHANGING gv_wa_ekpo-netpr.




    lv_wa_item-invoice_doc_item = lv_item.
    lv_wa_item-po_number        = gv_wa_ekpo-ebeln.
    lv_wa_item-po_item          = gv_wa_ekpo-ebelp.
    lv_wa_item-item_amount      = lv_wa_item-item_amount + ( ( p_value / lv_total_netpr ) * ( ( ( gv_wa_ekpo-netpr / gv_wa_ekpo-peinh ) * gv_wa_ekpo-menge ) ) ).
    lv_wa_item-quantity         = lv_wa_item-quantity + gv_wa_ekpo-menge.
    lv_wa_item-po_unit          = gv_wa_ekpo-meins.
    PERFORM convert_unit USING gv_wa_ekpo-matnr lv_wa_item-quantity gv_wa_ekpo-meins gv_wa_ekpo-bprme CHANGING lv_wa_item-po_pr_qnt.
    lv_wa_item-po_pr_uom        = gv_wa_ekpo-bprme.
    lv_wa_item-cond_type        = p_kschl.
    "lv_wa_item-freight_ven      = p_lifnr.
    lv_wa_item-item_text        = ''.

    IF p_tamnt > 0 .
      lv_wa_item-tax_code         = p_mwskz.
    ELSE.
      lv_wa_item-tax_code         = p_mwskz.
      lv_xmwst = 'X'.
    ENDIF.

    lv_total_inb = lv_total_inb + ( ( p_value / lv_total_netpr ) * ( ( ( gv_wa_ekpo-netpr / gv_wa_ekpo-peinh ) * gv_wa_ekpo-menge ) ) ).

    IF lv_found = 'X'.
      MODIFY lv_it_item FROM lv_wa_item INDEX lv_index.
    ELSE.
      APPEND lv_wa_item TO lv_it_item.
    ENDIF.
  ENDLOOP.

  lv_header-gross_amount = lv_total_inb + p_tamnt.
  lv_header-calc_tax_ind   = lv_xmwst.




  REFRESH lv_it_tax.
  IF p_tamnt IS NOT INITIAL.
    CLEAR lv_wa_tax.
    lv_wa_tax-tax_amount = p_tamnt.
    lv_wa_tax-tax_code   = p_mwskz.
    APPEND lv_wa_tax TO lv_it_tax.
  ELSE.
    CLEAR lv_wa_tax.
    lv_wa_tax-tax_code   = p_mwskz.
    APPEND lv_wa_tax TO lv_it_tax.
  ENDIF.

  IF lv_header-calc_tax_ind = 'X'.
    CLEAR lv_it_tax[].
  ENDIF.

  IF lv_it_item[] IS NOT INITIAL.

    REFRESH lv_it_return.
    CLEAR: lv_inv_doc, lv_inv_doc_year.

    CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
      EXPORTING
        headerdata       = lv_header
      IMPORTING
        invoicedocnumber = lv_inv_doc
        fiscalyear       = lv_inv_doc_year
      TABLES
        itemdata         = lv_it_item
        taxdata          = lv_it_tax
        return           = lv_it_return.

  ENDIF.

  LOOP AT lv_it_return INTO lv_wa_return WHERE type = 'E'.
    "lv_wa_return-message_v2 = gv_wa_ekko-ebeln.
    lv_wa_return-message_v3 = lv_total_inb.
    lv_wa_return-message_v4 = p_waers.
    APPEND lv_wa_return TO gv_it_log.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR gv_wa_log.
    gv_wa_log-type = 'S'.
    "gv_wa_log-message_v2 = gv_wa_ekko-ebeln.
    gv_wa_log-message_v3 = lv_total_inb.
    gv_wa_log-message_v4 = p_waers.
    gv_wa_log-message_v1 = lv_inv_doc.
    gv_wa_log-message_v2 = lv_inv_doc_year.
    CONCATENATE 'Document Number:' lv_inv_doc '-' lv_inv_doc_year 'Created' INTO gv_wa_log-message SEPARATED BY space.
    APPEND gv_wa_log TO gv_it_log.

  ENDIF.



  "ENDLOOP.

ENDFORM.                    " CREATE_INVOICE_FROM_PO


*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM user_command  USING    r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.


  IF r_ucomm = '&IC1' AND rs_selfield-fieldname = 'MESSAGE_V1'.
    PERFORM call_mir7 USING rs_selfield-tabindex.
  ENDIF.


  rs_selfield-refresh = 'X'.

ENDFORM.                    "user_command


*&---------------------------------------------------------------------*
*&      Form  call_mir7
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LV_INDEX   text
*----------------------------------------------------------------------*
FORM call_mir7   USING lv_index TYPE sy-tabix.

  DATA: lv_rbkp TYPE rbkp.


  IF lv_index IS NOT INITIAL.
    READ TABLE gv_it_log INTO gv_wa_log INDEX lv_index.
    IF sy-subrc EQ 0.
      SELECT SINGLE * INTO lv_rbkp FROM rbkp WHERE belnr = gv_wa_log-message_v1 AND gjahr = gv_wa_log-message_v2.
      IF sy-subrc EQ 0.
        SET PARAMETER ID 'RBN'  FIELD lv_rbkp-belnr.
        SET PARAMETER ID 'GJR'  FIELD lv_rbkp-gjahr.
        CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDIF.


ENDFORM.                    "call_mir7
*&---------------------------------------------------------------------*
*&      Form  GET_LIFNR_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM get_lifnr_txt .

  CHECK p_lifnr IS NOT INITIAL.
  SELECT SINGLE name1 INTO txtlifnr FROM lfa1 WHERE lifnr = p_lifnr.


ENDFORM.                    " GET_LIFNR_TXT
*&---------------------------------------------------------------------*
*&      Form  CONVERT_AMOUNT_TO_INTERNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VALUE  text
*      -->P_P_WAERS  text
*      <--P_LV_AMOUNT  text
*----------------------------------------------------------------------*
FORM convert_amount_to_internal  USING    lv_value  TYPE wrbtr
                                          lv_waers  TYPE waers
                                 CHANGING lv_amount TYPE wrbtr.

  DATA: lv_amn TYPE bapicurr_d.

  lv_amn = lv_value.
  CLEAR lv_amount.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = lv_waers
      amount_external      = lv_amn
      max_number_of_digits = 13
    IMPORTING
      amount_internal      = lv_amount.



ENDFORM.                    " CONVERT_AMOUNT_TO_INTERNAL

FORM convert_unit  USING     lv_matnr  TYPE zmaterial_weight-matnr
                             lv_menge1 TYPE bpmng
                             lv_from   TYPE zmaterial_weight-meins
                             lv_to     TYPE zmaterial_weight-meins
                    CHANGING lv_menge2 TYPE bpmng.

  CLEAR lv_menge2.


  IF lv_from = lv_to .
    lv_menge2 = lv_menge1.
  ELSE.

    CALL FUNCTION 'MATERIAL_CONVERT_QUANTITY'
      EXPORTING
        pi_material_src    = lv_matnr
        pi_meinh_src       = lv_from
        pi_quantity_src    = lv_menge1
        pi_material_dst    = lv_matnr
        pi_meinh_dst       = lv_to
      IMPORTING
        pe_quantity_dst    = lv_menge2
      EXCEPTIONS
        wrong_call         = 1
        material_not_found = 2
        no_conversion      = 3
        OTHERS             = 4.


  ENDIF.


ENDFORM.
