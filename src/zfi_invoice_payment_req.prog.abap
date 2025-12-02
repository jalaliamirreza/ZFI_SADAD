*&---------------------------------------------------------------------*
*& Report ZFI_INVOICE_PAYMENT_REQ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_invoice_payment_req.




TABLES : rbkp.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_invoice_payment_req,
      gv_wa_output LIKE LINE OF  gv_it_output,
      fieldcatalog TYPE          lvc_t_fcat WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME.
PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY,
            p_gjahr TYPE bkpf-gjahr OBLIGATORY.
SELECT-OPTIONS : s_belnr    FOR  rbkp-belnr OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.


  PERFORM get_data.
  PERFORM calc_data.
  PERFORM build_fieldcatalog.
  PERFORM display_grid.








*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_INVOICE_PAYMENT_REQ'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'SUM'.
        PERFORM set_catalog_text USING 'Payment/Request' CHANGING fieldcatalog.

      WHEN 'REMAIN'.
        PERFORM set_catalog_text USING 'Remaining Amnt'  CHANGING fieldcatalog.
        fieldcatalog-edit          = 'X'.

      WHEN 'NETDT'.
        fieldcatalog-edit          = 'X'.

      WHEN 'ZTERM'.
        fieldcatalog-edit          = 'X'.

      WHEN 'SELECT'.
        fieldcatalog-edit          = 'X'.
        fieldcatalog-checkbox      = 'X'.

      WHEN 'BVTYP'.
        fieldcatalog-edit          = 'X'.

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



  DATA: is_layout TYPE lvc_s_layo .

  is_layout-zebra      = 'X'.
  is_layout-cwidth_opt = 'X'.
  is_layout-stylefname = 'FIELD_STYLE'.
  is_layout-no_rowmark = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat_lvc          = fieldcatalog[]
      i_callback_pf_status_set = 'PF_STAT'
      i_save                   = 'X'
      is_layout_lvc            = is_layout
    TABLES
      t_outtab                 = gv_it_output
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.


ENDFORM.                    " DISPLAY_GRID
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
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .


  REFRESH gv_it_output.



  SELECT rbkp~gjahr , rbkp~belnr AS belnr_inv , rbkp~bukrs , bkpf~belnr , bseg1~buzei, bseg1~lifnr,lfa1~name1 , bseg1~dmbtr , bkpf~waers,
         bseg1~NETDT , bseg1~zterm , bseg1~zlspr, bseg1~bvtyp
  FROM       rbkp
  CROSS JOIN bkpf
  JOIN       bseg AS bseg1 ON bkpf~bukrs  = bseg1~bukrs AND
                              bkpf~gjahr  = bseg1~gjahr AND
                              bkpf~belnr  = bseg1~belnr
  LEFT JOIN  lfa1 ON bseg1~lifnr = lfa1~lifnr
  WHERE  rbkp~bukrs  =  @p_bukrs AND
         rbkp~gjahr  =  @p_gjahr AND
         rbkp~belnr  IN @s_belnr AND
         rbkp~stblg  =  ''       AND
         rbkp~rbstat = '5'       AND
         rbkp~xrech  = 'X'       AND
         bkpf~blart  = 'RE'      AND
         bseg1~koart = 'K'       AND
         bkpf~stblg  = ''        AND
         concat( rbkp~belnr , rbkp~gjahr ) = bkpf~awkey
 INTO CORRESPONDING FIELDS OF TABLE @gv_it_output.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM calc_data .

  LOOP AT gv_it_output INTO gv_wa_output.



    SELECT SUM( dmbtr )  INTO gv_wa_output-sum FROM bsik WHERE  bukrs = gv_wa_output-bukrs AND
                                                                lifnr = gv_wa_output-lifnr AND
                                                                rebzj = gv_wa_output-gjahr AND
                                                                rebzg = gv_wa_output-belnr AND
                                                                rebzz = gv_wa_output-buzei.


    gv_wa_output-remain = gv_wa_output-dmbtr - gv_wa_output-sum.

    IF NOT ( gv_wa_output-remain > 0 ) OR gv_wa_output-zlspr IS NOT INITIAL.
      REFRESH gv_wa_output-field_style.
      PERFORM editable_cell USING '' 'REMAIN' CHANGING gv_wa_output-field_style.
      PERFORM editable_cell USING '' 'ZTERM'  CHANGING gv_wa_output-field_style.
      PERFORM editable_cell USING '' 'NETDT'  CHANGING gv_wa_output-field_style.
      PERFORM editable_cell USING '' 'SELECT' CHANGING gv_wa_output-field_style.
    ENDIF.

    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.



ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EDITABLE_CELL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0379   text
*      -->P_0380   text
*      <--P_GV_WA_OUTPUT_FIELD_STYLE  text
*----------------------------------------------------------------------*
FORM editable_cell  USING    lv_enable TYPE char1
                             lv_field  TYPE lvc_s_styl-fieldname
                    CHANGING lv_style  TYPE zfi_bank_statement_bank-field_style.

  DATA: lv_stylerow TYPE lvc_s_styl.


  CLEAR lv_stylerow.
  lv_stylerow-fieldname = lv_field.
  IF lv_enable = 'X'.
    lv_stylerow-style     = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    lv_stylerow-style     = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.
  INSERT lv_stylerow INTO TABLE lv_style.

ENDFORM.

FORM user_command  USING p_ucomm    LIKE sy-ucomm
                         p_selfield TYPE slis_selfield.

  DATA: lv_subrc(1),
        ref1        TYPE REF TO cl_gui_alv_grid.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.

  CALL METHOD ref1->check_changed_data.


  CASE p_ucomm.
    WHEN 'POST'.
      PERFORM check_data CHANGING lv_subrc.
      IF lv_subrc IS INITIAL.
        PERFORM post.
      ENDIF.
    WHEN 'ALLS'.

      PERFORM select_all.

    WHEN 'SALLS'.

      PERFORM deselect_all.

  ENDCASE.

  CALL METHOD ref1->check_changed_data.
  CALL METHOD ref1->refresh_table_display.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_all .


  LOOP AT gv_it_output INTO gv_wa_output WHERE remain > 0 AND zlspr IS INITIAL.
    gv_wa_output-select = 'X'.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.


ENDFORM.                    " SELECT_ALL

*&---------------------------------------------------------------------*
*&      Form  deselect_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM deselect_all .


  LOOP AT gv_it_output INTO gv_wa_output.
    gv_wa_output-select = ''.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM post .

  DATA: lv_it_curr   TYPE TABLE OF bapiaccr09,
        lv_it_vendor TYPE TABLE OF bapiacap09,
        lv_wa_vendor LIKE LINE OF  lv_it_vendor,
        lv_wa_curr   LIKE LINE OF  lv_it_curr,
        lv_it_return TYPE TABLE OF bapiret2,
        lv_index     TYPE          i,
        lv_zarib     TYPE          menge_d.


  lv_zarib = - 100.

  CLEAR lv_index.
  LOOP AT gv_it_output INTO gv_wa_output WHERE select = 'X' AND remain > 0.

    lv_index = lv_index + 1.
    CLEAR lv_wa_vendor.
    lv_wa_vendor-itemno_acc     = lv_index.
    lv_wa_vendor-vendor_no      = gv_wa_output-lifnr.
    lv_wa_vendor-sp_gl_ind      = 'P'.
    lv_wa_vendor-bline_date     = gv_wa_output-NETDT.
    lv_wa_vendor-pmnttrms       = gv_wa_output-zterm.
    lv_wa_vendor-partner_bk     = gv_wa_output-bvtyp.
    CONCATENATE gv_wa_output-gjahr gv_wa_output-belnr gv_wa_output-buzei INTO lv_wa_vendor-ref_key_3.
    APPEND lv_wa_vendor TO lv_it_vendor.

    CLEAR lv_wa_curr.
    lv_wa_curr-itemno_acc   = lv_index.
    lv_wa_curr-currency     = gv_wa_output-waers.
    lv_wa_curr-amt_doccur   = gv_wa_output-remain * lv_zarib.
    APPEND lv_wa_curr TO lv_it_curr.

  ENDLOOP.

  PERFORM post_fi_doc TABLES lv_it_curr lv_it_vendor lv_it_return USING p_bukrs sy-datum.

  IF lv_it_return[] IS NOT INITIAL.

*    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
*      TABLES
*        ss_bapiret2 = lv_it_return.
call function 'FINB_BAPIRET2_DISPLAY'
  exporting
    it_message       = lv_it_return
          .



    READ TABLE lv_it_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
    IF sy-subrc EQ 0.
      PERFORM get_data.
      PERFORM calc_data.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  pf_stat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RT_EXTAB   text
*----------------------------------------------------------------------*
FORM pf_stat USING rt_extab TYPE slis_t_extab .
  SET PF-STATUS 'STANDARD'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_FI_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_CURR  text
*      -->P_LV_IT_VENDOR  text
*      -->P_LV_IT_RETURN  text
*      -->P_P_BUKRS  text
*      -->P_SY_DATUM  text
*&---------------------------------------------------------------------*
FORM post_fi_doc  TABLES   lv_it_curr   STRUCTURE bapiaccr09
                           lv_it_vendor STRUCTURE bapiacap09
                           lv_it_return STRUCTURE bapiret2
                  USING    lv_bukrs     TYPE      bkpf-bukrs
                           lv_date      TYPE      bkpf-budat.

  DATA: lv_header TYPE          bapiache09,
        lv_it_ret TYPE TABLE OF bapiret2,
        lv_wa_ret LIKE LINE OF  lv_it_ret,
        lv_objkey TYPE          bapiache02-obj_key.

  lv_header-comp_code   = lv_bukrs.
  lv_header-doc_type    = 'ZK'.
  lv_header-doc_date    = sy-datum.
  lv_header-pstng_date  = lv_date.
  lv_header-username    = sy-uname.
  lv_header-bus_act     = 'RFST'.


  REFRESH lv_it_ret.
  BREAK omrani.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = lv_header
    IMPORTING
      obj_key        = lv_objkey
    TABLES
      accountpayable = lv_it_vendor
      currencyamount = lv_it_curr
      return         = lv_it_ret.

  READ TABLE lv_it_ret INTO lv_wa_ret WITH KEY 'E'.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    APPEND LINES OF lv_it_ret TO lv_it_return.
  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

    CLEAR lv_wa_ret.
    lv_wa_ret-type       = 'S'.
    lv_wa_ret-id         = 'ZFI'.
    lv_wa_ret-number     = '011'.
    lv_wa_ret-message_v1 = lv_objkey(10).
    lv_wa_ret-message_v2 = '|'.
    lv_wa_ret-message_v3 = lv_objkey+14.

    APPEND lv_wa_ret TO lv_it_return.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*&---------------------------------------------------------------------*
FORM check_data  CHANGING lv_subrc TYPE char1.


  CLEAR lv_subrc.


  LOOP AT gv_it_output INTO gv_wa_output WHERE select = 'X' AND remain > 0.
    IF gv_wa_output-remain + gv_wa_output-sum > gv_wa_output-dmbtr.
      MESSAGE e012(zfi) WITH gv_wa_output-gjahr gv_wa_output-belnr gv_wa_output-buzei.
      lv_subrc = 1.
      EXIT.
    ENDIF.
  ENDLOOP.


ENDFORM.
