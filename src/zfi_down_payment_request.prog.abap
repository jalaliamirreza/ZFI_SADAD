*&---------------------------------------------------------------------*
*& Report ZFI_DOWN_PAYMENT_REQUEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_down_payment_request.



TABLES : zfidpr,bkpf,bseg.

DATA: ok_code      LIKE          sy-ucomm,
      gv_grid_item TYPE REF TO   cl_gui_alv_grid,
      gv_cc_item   TYPE REF TO   cl_gui_custom_container,
      gv_it_item   TYPE TABLE OF zfidpr_i,
      gv_wa_item   LIKE LINE OF  gv_it_item,
      gv_subrc(1),
      txt_taxcode  TYPE mwskz.



PARAMETERS: p_bldat TYPE bkpf-bldat OBLIGATORY DEFAULT sy-datum,
            p_budat TYPE bkpf-budat OBLIGATORY DEFAULT sy-datum,
            p_blart TYPE bkpf-blart OBLIGATORY DEFAULT 'DZ',
            p_bukrs TYPE bkpf-bukrs OBLIGATORY,
            p_waers TYPE bkpf-waers OBLIGATORY DEFAULT 'IRR',
*            p_bktxt TYPE bkpf-bktxt OBLIGATORY DEFAULT 'پيش دريافت',
            p_kunnr TYPE bseg-kunnr OBLIGATORY,
            p_zumsk TYPE bseg-zumsk OBLIGATORY DEFAULT 'A',
            p_zlsch TYPE bseg-zlsch OBLIGATORY.

START-OF-SELECTION.

  PERFORM clear.
  PERFORM check_input_data CHANGING gv_subrc.
  IF gv_subrc IS INITIAL.
    PERFORM fill_data.
    PERFORM call_screen.
  ENDIF.




*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM fill_screen_data.
  PERFORM display_item.
  PERFORM set_status.
  PERFORM loop_screen.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_commands INPUT.

  PERFORM exit_commands_form CHANGING ok_code .

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_commands INPUT.

  PERFORM exit_commands_form CHANGING ok_code .

ENDMODULE.

FORM exit_commands_form CHANGING ok_code.

  DATA : lv_subrc(1).

  CASE ok_code.
    WHEN 'ENDE' OR 'ECAN' OR 'EXIT'.
      PERFORM exit.
    WHEN 'ADD_LINE'.
      PERFORM check_add_line CHANGING lv_subrc.
      IF lv_subrc IS INITIAL.
        PERFORM add_line.
        PERFORM clear_add_line.
      ENDIF.
    WHEN 'DEL_LINE'.
      PERFORM delete_line.
    WHEN 'POST_DOC'.
      PERFORM post_fi_check CHANGING lv_subrc.
      IF lv_subrc IS INITIAL.
        PERFORM post_fi_doc CHANGING lv_subrc.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.
ENDFORM.

FORM exit .
  SET SCREEN 0.
ENDFORM.

FORM set_status .


  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR '001'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear .

  CLEAR zfidpr.
  REFRESH gv_it_item.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM call_screen .

  CALL SCREEN '0100'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_item .

  DATA: lv_it_fieldcatalog TYPE         lvc_t_fcat,
        lv_it_exclude      TYPE         ui_functions,
        lv_layout          TYPE         lvc_s_layo.


  IF gv_cc_item IS INITIAL.
    CREATE OBJECT gv_cc_item
      EXPORTING
        container_name = 'ITEM_CC'.


    CREATE OBJECT gv_grid_item
      EXPORTING
        i_parent = gv_cc_item.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFIDPR_I'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.

    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog.

    lv_layout-zebra       = 'X'.
    lv_layout-no_toolbar  = 'X'.
    lv_layout-no_rowmark  = 'X'.

    PERFORM fill_exclude_alv CHANGING  lv_it_exclude.


    CALL METHOD gv_grid_item->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFIDPR_I'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
      CHANGING
        it_outtab            = gv_it_item[]
        it_fieldcatalog      = lv_it_fieldcatalog[].

  ELSE.

    PERFORM refresh_alv.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_SCREEN_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_screen_data .

  CLEAR: zfidpr-kunnr_desc,zfidpr-wrbtr_remain.
  SELECT SINGLE name1 INTO zfidpr-kunnr_desc FROM kna1 WHERE kunnr = zfidpr-kunnr.

  zfidpr-wrbtr_remain = zfidpr-wrbtr.
  LOOP AT gv_it_item INTO gv_wa_item.
    zfidpr-wrbtr_remain = zfidpr-wrbtr_remain - gv_wa_item-wrbtr.
  ENDLOOP.
  zfidpr-wrbtr_remain = zfidpr-wrbtr_remain * -1.
  if TXT_TAXCODE is INITIAL.
  TXT_TAXCODE = 'S0'.
  ENDIF.
*  IF zfidpr-zfbdt IS INITIAL.
*    zfidpr-zfbdt = sy-datum.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_data .

  zfidpr-bldat = p_bldat.
  zfidpr-budat = p_budat.
  zfidpr-blart = p_blart.
  zfidpr-bukrs = p_bukrs.
  zfidpr-waers = p_waers.
*  zfidpr-bktxt = p_bktxt.
  zfidpr-kunnr = p_kunnr.
  zfidpr-zumsk = p_zumsk.
  zfidpr-zlsch = p_zlsch.

  zfidpr-bktxt = 'پيش دريافت'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_alv .


  DATA: lv_stable     TYPE lvc_s_stbl.

  IF gv_cc_item IS NOT INITIAL.

    lv_stable-row = 'X'.
    lv_stable-col = 'X'.

    CALL METHOD gv_grid_item->check_changed_data.
    CALL METHOD gv_grid_item->refresh_table_display
      EXPORTING
        is_stable = lv_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.


  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_EXCLUDE_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_IT_EXCLUDE  text
*&---------------------------------------------------------------------*
FORM fill_exclude_alv CHANGING lv_it_exclude  TYPE ui_functions.

  DATA : lv_wa_exclude      TYPE ui_func.

  REFRESH lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_save_variant .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_separator .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_detail.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_views.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND lv_wa_exclude TO lv_it_exclude.

ENDFORM.

FORM modify_fieldcatalog TABLES lv_it_fieldcatalog TYPE lvc_t_fcat.

  DATA : lv_wa_fieldcatalog LIKE LINE OF lv_it_fieldcatalog.


  LOOP AT lv_it_fieldcatalog INTO lv_wa_fieldcatalog.

    CASE lv_wa_fieldcatalog-fieldname.

      WHEN 'SELECT'.
        PERFORM set_catalog_text USING 'Select'           CHANGING lv_wa_fieldcatalog.
        lv_wa_fieldcatalog-checkbox  = 'X'.
        lv_wa_fieldcatalog-edit      = 'X'.
        lv_wa_fieldcatalog-outputlen = 7.
      WHEN 'WRBTR'.
        PERFORM set_catalog_text USING 'Amount'           CHANGING lv_wa_fieldcatalog.
      WHEN 'WRBTR_OLD'.
        PERFORM set_catalog_text USING 'Prev.Downpayment' CHANGING lv_wa_fieldcatalog.
      WHEN 'WAERS'.
        PERFORM set_catalog_text USING 'Currency'         CHANGING lv_wa_fieldcatalog.

    ENDCASE.

    MODIFY lv_it_fieldcatalog FROM lv_wa_fieldcatalog.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CATALOG_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      <--P_LV_WA_FIELDCATALOG  text
*&---------------------------------------------------------------------*
FORM set_catalog_text  USING    lv_text            TYPE char100
                       CHANGING lv_wa_fieldcatalog TYPE lvc_s_fcat.


  lv_wa_fieldcatalog-scrtext_l     = lv_text.
  lv_wa_fieldcatalog-scrtext_m     = lv_wa_fieldcatalog-scrtext_l.
  lv_wa_fieldcatalog-scrtext_s     = lv_wa_fieldcatalog-scrtext_l.
  lv_wa_fieldcatalog-reptext       = lv_wa_fieldcatalog-scrtext_l.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_LINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_line .

  CALL METHOD gv_grid_item->check_changed_data.
  DELETE gv_it_item WHERE select = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_LINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM add_line.


  CLEAR gv_wa_item.

  gv_wa_item-vbeln = zfidpr-vbeln.

  SELECT SINGLE SUM( netwr + mwsbp ) INTO @gv_wa_item-wrbtr FROM vbap WHERE vbeln = @zfidpr-vbeln.

  SELECT posnr INTO gv_wa_item-posnr UP TO 1 ROWS FROM vbap WHERE vbeln = zfidpr-vbeln ORDER BY posnr ASCENDING.
  ENDSELECT.

  gv_wa_item-waers = zfidpr-waers.
  gv_wa_item-zfbdt = zfidpr-zfbdt.
  gv_wa_item-zlsch = zfidpr-zlsch.
  gv_wa_item-sgtxt = zfidpr-sgtxt.

  SELECT SINGLE SUM( dmbtr ) INTO @gv_wa_item-wrbtr_old
    FROM bsid
    WHERE bukrs = @zfidpr-bukrs         AND
          vbel2 = @zfidpr-vbeln         AND
      ( ( umskz = 'A' AND shkzg = 'H' ) OR
        ( umskz = 'F' AND shkzg = 'S' ) ).

  APPEND gv_wa_item TO gv_it_item.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_FI_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*&---------------------------------------------------------------------*
FORM post_fi_check  CHANGING lv_subrc TYPE char1.

  CLEAR lv_subrc.

  IF  zfidpr-kunnr IS INITIAL OR
      zfidpr-bldat IS INITIAL OR
      zfidpr-budat IS INITIAL OR
      zfidpr-blart IS INITIAL OR
      zfidpr-bukrs IS INITIAL OR
      zfidpr-waers IS INITIAL OR
      zfidpr-bktxt IS INITIAL OR
      zfidpr-wrbtr IS INITIAL.

    PERFORM call_msg USING '' '' '020' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  IF gv_it_item[] IS INITIAL.
    PERFORM call_msg USING '' '' '022' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.
  IF zfidpr-zlsch = 'N'.
    IF    zfidpr-xref1 IS INITIAL OR
          zfidpr-xref3 IS INITIAL.

      PERFORM call_msg USING '' '' '025' '' '' '' ''.
      lv_subrc = 1.
    ENDIF.
  ELSE.
    IF    zfidpr-hbkid IS INITIAL OR
          zfidpr-hktid IS INITIAL.

      PERFORM call_msg USING '' '' '026' '' '' '' ''.
      lv_subrc = 1.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_FI_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*&---------------------------------------------------------------------*
FORM post_fi_doc  CHANGING lv_subrc TYPE char1.



  DATA: lv_header      TYPE          bapiache09,
        lv_it_ret      TYPE TABLE OF bapiret2,
        lv_wa_ret      LIKE LINE OF  lv_it_ret,
        lv_it_customer TYPE TABLE OF bapiacar09,
        lv_wa_customer LIKE LINE OF  lv_it_customer,
        lv_it_ext      TYPE TABLE OF bapiparex,
        lv_wa_ext      LIKE LINE OF  lv_it_ext,
        lv_it_curr     TYPE TABLE OF bapiaccr09,
        lv_wa_curr     LIKE LINE OF  lv_it_curr,
        lv_objkey      TYPE          bapiache02-obj_key,
        lv_index       TYPE          i.


  CLEAR: lv_header,lv_objkey,lv_subrc.
  REFRESH: lv_it_ret,lv_it_customer,lv_it_curr,lv_it_ext.



  lv_header-pstng_date          = zfidpr-budat.
  lv_header-comp_code           = zfidpr-bukrs.
  lv_header-doc_type            = zfidpr-blart.
  lv_header-doc_date            = zfidpr-bldat.
  lv_header-username            = sy-uname.
  lv_header-header_txt          = zfidpr-bktxt.
  lv_header-bus_act             = 'RFST'.

  lv_index = 0.

  LOOP AT gv_it_item INTO gv_wa_item.
    lv_index = lv_index + 1.
    CLEAR lv_wa_customer.
    lv_wa_customer-itemno_acc       = lv_index.
    lv_wa_customer-customer         = zfidpr-kunnr.
    lv_wa_customer-sp_gl_ind        = 'F'.
    lv_wa_customer-item_text        = gv_wa_item-sgtxt.
    lv_wa_customer-pymt_meth        = gv_wa_item-zlsch.
    lv_wa_customer-bank_id          = zfidpr-hbkid.
    lv_wa_customer-housebankacctid  = zfidpr-hktid.
    lv_wa_customer-bline_date       = gv_wa_item-zfbdt.
    lv_wa_customer-ref_key_1        = zfidpr-xref1.
    lv_wa_customer-ref_key_3        = zfidpr-xref3.
    lv_wa_customer-tax_code         = txt_taxcode.
    APPEND lv_wa_customer TO lv_it_customer.

    CLEAR lv_wa_curr.
    lv_wa_curr-itemno_acc   = lv_index.
    lv_wa_curr-currency     = zfidpr-waers.
    lv_wa_curr-amt_doccur   = gv_wa_item-wrbtr * 100.
    lv_wa_curr-disc_base    = zfidpr-wrbtr     * 100.
    AT LAST.
      lv_wa_curr-amt_doccur = lv_wa_curr-amt_doccur - ( zfidpr-wrbtr_remain * 100 ).
    ENDAT.
    APPEND lv_wa_curr TO lv_it_curr.

    CLEAR lv_wa_ext.
    lv_wa_ext-structure  = 'ACCOUNTRECEIVABLE'.
    lv_wa_ext-valuepart1 = lv_index.
    lv_wa_ext-valuepart2 = 'VBEL2'.
    lv_wa_ext-valuepart3 = gv_wa_item-vbeln.
    lv_wa_ext-valuepart4 = gv_wa_item-posnr.
    APPEND lv_wa_ext TO lv_it_ext.

  ENDLOOP.


  BREAK omrani.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = lv_header
    IMPORTING
      obj_key           = lv_objkey
    TABLES
      accountreceivable = lv_it_customer
      currencyamount    = lv_it_curr
      extension2        = lv_it_ext
      return            = lv_it_ret.


  READ TABLE lv_it_ret INTO lv_wa_ret WITH KEY 'E'.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

call function 'FINB_BAPIRET2_DISPLAY'
  exporting
    it_message       = lv_it_ret
          .

*    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
*      TABLES
*        ss_bapiret2 = lv_it_ret.

    PERFORM call_msg USING '' '' '023' '' '' '' ''.
    lv_subrc = 1.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    PERFORM call_msg USING 'S' '' '011' lv_objkey(10) lv_objkey+10(4) '' ''.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_ADD_LINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*&---------------------------------------------------------------------*
FORM check_add_line  CHANGING lv_subrc TYPE char1.

  DATA: lv_kunnr TYPE zfidpr-kunnr,
        lv_bukrs TYPE zfidpr-bukrs.

  CLEAR lv_subrc.

  SELECT SINGLE kna1~kunnr vbak~bukrs_vf INTO (lv_kunnr,lv_bukrs)
  FROM kna1
  JOIN vbak ON vbak~kunnr = kna1~kunnr
  WHERE vbeln = zfidpr-vbeln.

  IF lv_bukrs <> zfidpr-bukrs.
    PERFORM call_msg USING '' '' '013' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  IF zfidpr-kunnr <> lv_kunnr.
    PERFORM call_msg USING '' '' '015' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  IF zfidpr-zfbdt IS INITIAL.
    PERFORM call_msg USING '' '' '018' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  IF zfidpr-zlsch IS INITIAL.
    PERFORM call_msg USING '' '' '019' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  READ TABLE gv_it_item INTO gv_wa_item WITH KEY vbeln = zfidpr-vbeln.
  IF sy-subrc EQ 0.
    PERFORM call_msg USING '' '' '024' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.


ENDFORM.

FORM call_msg USING lv_msg_type TYPE char1
                    lv_msg_txt  TYPE char255
                    lv_msg_no   TYPE msgnr
                    lv_v1
                    lv_v2
                    lv_v3
                    lv_v4.


  DATA lv_type TYPE char1.

  IF lv_msg_type IS INITIAL.
    lv_type = 'E'.
  ELSE.
    lv_type = lv_msg_type.
  ENDIF.

  IF lv_msg_no IS NOT INITIAL.
    MESSAGE ID 'ZFI' TYPE 'S' NUMBER lv_msg_no DISPLAY LIKE lv_type WITH lv_v1 lv_v2 lv_v3 lv_v4.
  ELSE.
    MESSAGE ID 'ZFI' TYPE 'S' NUMBER '001'     DISPLAY LIKE lv_type WITH lv_msg_txt.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_ADD_LINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_add_line .

  CLEAR: zfidpr-vbeln, zfidpr-zfbdt, zfidpr-sgtxt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*&---------------------------------------------------------------------*
FORM check_input_data  CHANGING lv_subrc TYPE char1.

  DATA: lv_kunnr TYPE kna1-kunnr.

  CLEAR lv_subrc.

  SELECT SINGLE kunnr INTO lv_kunnr FROM kna1 WHERE kunnr = p_kunnr.
  IF sy-subrc NE 0.
    PERFORM call_msg USING '' '' '021' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LOOP_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM loop_screen .



  LOOP AT SCREEN.
    IF screen-name = 'ZFIDPR-XREF1' OR screen-name = 'ZFIDPR-XREF3' OR screen-name = 'ZFIDPR-HBKID' OR screen-name = 'ZFIDPR-HKTID'.
      screen-input = '0'.
    ENDIF.
    IF zfidpr-zlsch = 'N'.
      IF screen-name = 'ZFIDPR-XREF1' OR screen-name = 'ZFIDPR-XREF3'.
        screen-input = '1'.
      ENDIF.
    ELSE.
      IF screen-name = 'ZFIDPR-HBKID' OR screen-name = 'ZFIDPR-HKTID'.
        screen-input = '1'.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.



ENDFORM.
