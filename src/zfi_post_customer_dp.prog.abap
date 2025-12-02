*&---------------------------------------------------------------------*
*& Report ZFI_POST_CUSTOMER_DP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_post_customer_dp.




TABLES : mara,payr,bkpf.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_post_customer_dp,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_log    TYPE TABLE OF zfi_post_customer_dp_log,
      gv_wa_log    LIKE LINE OF  gv_it_log,
      gv_subrc     TYPE          sy-subrc,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
      bdcdata      LIKE          bdcdata             OCCURS 0 WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk2 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE payr-zbukr OBLIGATORY,
            p_gjahr TYPE payr-gjahr OBLIGATORY DEFAULT sy-datum(4),
            p_blart TYPE bkpf-blart OBLIGATORY DEFAULT 'DZ',
            p_waers TYPE bkpf-waers OBLIGATORY DEFAULT 'IRR',
            p_xblnr TYPE bkpf-xblnr,
            p_bktxt TYPE bkpf-bktxt.
SELECTION-SCREEN: END   OF BLOCK blk2.


SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: s_belnr FOR payr-vblnr,
                s_budat FOR bkpf-budat.
SELECTION-SCREEN: END   OF BLOCK blk1.

SELECTION-SCREEN: BEGIN     OF BLOCK blk3 WITH FRAME TITLE TEXT-003.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) TEXT-004.
PARAMETERS: p_hbkid TYPE payr-hbkid OBLIGATORY .
SELECTION-SCREEN COMMENT 45(40) t_hbkid FOR FIELD p_hbkid.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) TEXT-005.
PARAMETERS: p_hktid TYPE payr-hktid OBLIGATORY .
SELECTION-SCREEN COMMENT 45(40) t_hktid FOR FIELD p_hktid.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_hkont TYPE bseg-hkont NO-DISPLAY,
            p_valut TYPE bseg-valut OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN: END   OF BLOCK blk3.



INITIALIZATION.
  PERFORM initialization.


AT SELECTION-SCREEN OUTPUT.
  PERFORM fill_screen_desc.

START-OF-SELECTION.

  PERFORM check_input_data CHANGING gv_subrc.
  CHECK gv_subrc IS INITIAL.
  PERFORM get_data.
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

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_POST_CUSTOMER_DP'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'AMOUNT'.
        PERFORM set_catalog_text USING 'Bank Amount' CHANGING fieldcatalog.
        "fieldcatalog-edit          = 'X'.

      WHEN 'NETWR1'.
        PERFORM set_catalog_text USING 'مبلغ سفارش' CHANGING fieldcatalog.

      WHEN 'NETWR2'.
        PERFORM set_catalog_text USING 'مبلغ پيش دريافت' CHANGING fieldcatalog.


      WHEN 'SELECT'.
        fieldcatalog-edit          = 'X'.
        fieldcatalog-checkbox      = 'X'.

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
      i_callback_program       = sy-repid
      it_fieldcat              = fieldcatalog[]
      i_save                   = 'A'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_pf_status_set = 'PF_STAT'
      is_layout                = is_layout
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
                       CHANGING lv_wa_fieldcatalog TYPE slis_fieldcat_alv.


  lv_wa_fieldcatalog-seltext_l     = lv_text.
  lv_wa_fieldcatalog-seltext_m     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-seltext_s     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-reptext_ddic  = lv_wa_fieldcatalog-seltext_l.

ENDFORM.

FORM user_command  USING p_ucomm    LIKE sy-ucomm
                         p_selfield TYPE slis_selfield.

  DATA: lv_subrc TYPE        sy-subrc,
        lv_check TYPE        payr-chect,
        lv_belnr TYPE        bkpf-belnr,
        lv_gjahr TYPE        bkpf-gjahr,
        ref1     TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.

  CALL METHOD ref1->check_changed_data.

  CASE p_ucomm.
    WHEN 'POST'.
      PERFORM call_post CHANGING lv_subrc.
      IF lv_subrc IS INITIAL.
        PERFORM display_log.
        PERFORM get_data.
      ENDIF.
    WHEN 'ALLS'.

      PERFORM select_all.

    WHEN 'SALLS'.

      PERFORM deselect_all.

  ENDCASE.

  CALL METHOD ref1->check_changed_data.
  CALL METHOD ref1->refresh_table_display.

ENDFORM.

FORM select_all .


  LOOP AT gv_it_output INTO gv_wa_output.
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

FORM post CHANGING lv_subrc     TYPE      char1
                   lv_belnr     TYPE      bkpf-belnr
                   lv_gjahr     TYPE      bkpf-gjahr.


  DATA: lv_it_curr   TYPE TABLE OF bapiaccr09,
        lv_it_vendor TYPE TABLE OF bapiacap09,
        lv_wa_vendor LIKE LINE OF  lv_it_vendor,
        lv_wa_curr   LIKE LINE OF  lv_it_curr,
        lv_it_return TYPE TABLE OF bapiret2,
        lv_index     TYPE          i,
        lv_zarib     TYPE          menge_d,
        lv_total     TYPE          bseg-dmbtr.


  lv_zarib = - 100.

  CLEAR lv_index.
  CLEAR lv_total.
  LOOP AT gv_it_output INTO gv_wa_output WHERE select = 'X' AND amount > 0.

    lv_index = lv_index + 1.
    CLEAR lv_wa_vendor.
    lv_wa_vendor-itemno_acc = lv_index.
    lv_wa_vendor-vendor_no  = gv_wa_output-kunnr.
    lv_wa_vendor-sp_gl_ind  = 'W'.
    CONCATENATE gv_wa_output-gjahr gv_wa_output-belnr gv_wa_output-buzei INTO lv_wa_vendor-ref_key_3.
    APPEND lv_wa_vendor TO lv_it_vendor.

    CLEAR lv_wa_curr.
    lv_wa_curr-itemno_acc   = lv_index.
    lv_wa_curr-currency     = gv_wa_output-waers.
    lv_wa_curr-amt_doccur   = gv_wa_output-amount * lv_zarib.
    lv_total                = lv_wa_curr-amt_doccur + lv_total.
    APPEND lv_wa_curr TO lv_it_curr.

  ENDLOOP.

  lv_index = lv_index + 1.
  CLEAR lv_wa_vendor.
  lv_wa_vendor-itemno_acc     = lv_index.
  lv_wa_vendor-vendor_no      = gv_wa_output-kunnr.
  lv_wa_vendor-sp_gl_ind      = ''.
  APPEND lv_wa_vendor TO lv_it_vendor.

  CLEAR lv_wa_curr.
  lv_wa_curr-itemno_acc   = lv_index.
  lv_wa_curr-currency     = gv_wa_output-waers.
  lv_wa_curr-amt_doccur   = lv_total * -1.
  APPEND lv_wa_curr TO lv_it_curr.

  PERFORM post_fi_doc TABLES lv_it_curr lv_it_vendor lv_it_return USING p_bukrs p_valut CHANGING lv_belnr lv_gjahr.

  IF lv_it_return[] IS NOT INITIAL.

    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
      TABLES
        ss_bapiret2 = lv_it_return.

  ENDIF.

  READ TABLE lv_it_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
  IF sy-subrc EQ 0.
    CLEAR lv_subrc.
  ELSE.
    lv_subrc = 1.
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
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  DATA: lv_hkont TYPE hkont.

  REFRESH gv_it_output.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE @gv_it_output
    FROM bsid
    JOIN bkpf ON bkpf~bukrs  = bsid~bukrs AND
                 bkpf~gjahr  = bsid~gjahr AND
                 bkpf~belnr  = bsid~belnr
    JOIN bseg ON bkpf~bukrs  = bseg~bukrs AND
                 bkpf~gjahr  = bseg~gjahr AND
                 bkpf~belnr  = bseg~belnr AND
                 bsid~buzei  = bseg~buzei
    LEFT JOIN kna1 ON bsid~kunnr = kna1~kunnr
    LEFT JOIN t074t ON t074t~shbkz = bsid~zumsk AND t074t~koart = 'D' AND t074t~spras = 'E'
    WHERE bkpf~belnr IN @s_belnr  AND
          bkpf~bukrs =  @p_bukrs  AND
          bkpf~budat IN @s_budat  AND
          bsid~umskz IN ('F')     AND
          bsid~zlsch =  'M'       AND
        ( bsid~hbkid =  @p_hbkid  OR
          bsid~hbkid = '' ).


  IF p_hkont IS INITIAL.
    SELECT SINGLE hkont INTO p_hkont FROM t012k WHERE bukrs = p_bukrs AND hbkid = p_hbkid AND hktid = p_hktid.
  ENDIF.

  LOOP AT gv_it_output INTO gv_wa_output.
    gv_wa_output-amount = gv_wa_output-dmbtr.
    gv_wa_output-hkont  = p_hkont.


    SELECT SINGLE SUM( netwr + mwsbp ) INTO @gv_wa_output-netwr1 FROM vbap WHERE vbeln = @gv_wa_output-vbel2.

    SELECT SINGLE SUM( dmbtr ) INTO @gv_wa_output-netwr2 FROM bsid
      WHERE bukrs = @gv_wa_output-bukrs   AND
            vbel2 = @gv_wa_output-vbel2   AND
        ( ( umskz = 'A' AND shkzg = 'H' ) OR
          ( umskz = 'F' AND shkzg = 'S' ) ).

    PERFORM get_bank_desc USING gv_wa_output-bukrs gv_wa_output-hbkid gv_wa_output-hktid CHANGING gv_wa_output-hbkid_t gv_wa_output-hktid_t.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.


ENDFORM.

FORM post_fi_doc  TABLES   lv_it_curr   STRUCTURE bapiaccr09
                           lv_it_vendor STRUCTURE bapiacap09
                           lv_it_return STRUCTURE bapiret2
                  USING    lv_bukrs     TYPE      bkpf-bukrs
                           lv_date      TYPE      bkpf-budat
                  CHANGING lv_belnr     TYPE      bkpf-belnr
                           lv_gjahr     TYPE      bkpf-gjahr.

  DATA: lv_header TYPE          bapiache09,
        lv_it_ret TYPE TABLE OF bapiret2,
        lv_wa_ret LIKE LINE OF  lv_it_ret,
        lv_objkey TYPE          bapiache02-obj_key.

  lv_header-comp_code   = lv_bukrs.
  lv_header-doc_type    = p_blart.
  lv_header-doc_date    = sy-datum.
  lv_header-pstng_date  = lv_date.
  lv_header-username    = sy-uname.
  lv_header-bus_act     = 'RFST'.
  lv_header-ref_doc_no  = p_xblnr.
  lv_header-header_txt  = p_bktxt.

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

    lv_belnr = lv_objkey(10).
    lv_gjahr = lv_objkey+14.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_NO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_CHECK  text
*&---------------------------------------------------------------------*
FORM get_check_no  CHANGING lv_check TYPE payr-chect.

  DATA: lv_pcec TYPE pcec.
  CLEAR lv_check.
  SELECT * UP TO 1 ROWS
    INTO CORRESPONDING FIELDS OF lv_pcec
    FROM pcec WHERE zbukr = p_bukrs AND
                    hbkid = p_hbkid AND
                    hktid = p_hktid
    ORDER BY checl DESCENDING.
  ENDSELECT.


  IF lv_pcec-checl IS INITIAL.
    lv_pcec-checl = lv_pcec-checf.
  ELSE.
    CALL FUNCTION 'ADD_N_TO_CHECK_NUMBER'
      EXPORTING
        i_pcec      = lv_pcec
      IMPORTING
        e_pcec      = lv_pcec
      EXCEPTIONS
        not_filled  = 1
        not_found   = 2
        not_numeric = 3
        not_valid   = 4
        OTHERS      = 5.
  ENDIF.


  lv_check = lv_pcec-checl.


  IF lv_check IS INITIAL.
    CLEAR gv_wa_log.
    MOVE-CORRESPONDING gv_wa_output TO gv_wa_log.
    gv_wa_log-type       = 'E'.
    gv_wa_log-message    = 'شماره چک پيدا نشد.'.
    APPEND gv_wa_log TO gv_it_log.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_FCH5
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM call_fch5 USING    lv_belnr TYPE bkpf-belnr
                        lv_gjahr TYPE bkpf-gjahr
                        lv_check TYPE payr-chect
               CHANGING lv_subrc TYPE sy-subrc.


  DATA: lv_message    TYPE  string.

  REFRESH bdcdata.

  PERFORM bdc_dynpro      USING 'SAPMFCHK'   '0500'.
  PERFORM bdc_field       USING 'PAYR-VBLNR' lv_belnr.
  PERFORM bdc_field       USING 'PAYR-ZBUKR' p_bukrs.
  PERFORM bdc_field       USING 'PAYR-GJAHR' lv_gjahr.
  PERFORM bdc_field       USING 'PAYR-HBKID' p_hbkid.
  PERFORM bdc_field       USING 'PAYR-HKTID' p_hktid.
  PERFORM bdc_field       USING 'PAYR-CHECT' lv_check.

  PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.

  PERFORM bdc_dynpro      USING 'SAPMFCHK'   '0501'.
  PERFORM bdc_field       USING 'BDC_OKCODE' '=UPDA'.

  PERFORM bdc_transaction USING 'FCH5' CHANGING lv_subrc.

  IF lv_subrc IS NOT INITIAL.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = sy-msgid
        msgnr               = sy-msgno
        msgv1               = sy-msgv1
        msgv2               = sy-msgv2
        msgv3               = sy-msgv3
        msgv4               = sy-msgv4
      IMPORTING
        message_text_output = lv_message.

    CLEAR gv_wa_log.
    MOVE-CORRESPONDING gv_wa_output TO gv_wa_log.
    gv_wa_log-type       = sy-msgty.
    gv_wa_log-id         = sy-msgid.
    gv_wa_log-number     = sy-msgno.
    gv_wa_log-message_v1 = sy-msgv1.
    gv_wa_log-message_v2 = sy-msgv2.
    gv_wa_log-message_v3 = sy-msgv3.
    gv_wa_log-message_v4 = lv_check.
    gv_wa_log-message    = lv_message.
    APPEND gv_wa_log TO gv_it_log.

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1141   text
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode CHANGING lv_subrc TYPE sy-subrc.

  DATA : lv_mode(1),
         lv_update(1),
         lv_it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.

  lv_mode = 'N'.
  CALL TRANSACTION tcode USING         bdcdata
                         MODE          lv_mode
                         MESSAGES INTO lv_it_messtab.

  LOOP AT lv_it_messtab WHERE msgtyp = 'E'.
    lv_subrc = 1.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_WITH_CLEAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_CHECK  text
*      <--P_LV_SUBRC  text
*      <--P_LV_BELNR  text
*      <--P_LV_GJAHR  text
*&---------------------------------------------------------------------*
FORM post_with_clear CHANGING lv_subrc     TYPE      sy-subrc
                              lv_belnr     TYPE      bkpf-belnr
                              lv_gjahr     TYPE      bkpf-gjahr.

  DATA: lv_it_blntab  TYPE TABLE OF blntab,
        lv_it_ftclear TYPE TABLE OF ftclear,
        lv_wa_ftclear LIKE LINE OF  lv_it_ftclear,
        lv_it_ftpost  TYPE TABLE OF ftpost,
        lv_it_fttax   TYPE TABLE OF fttax,
        lv_it_tmp     TYPE TABLE OF zfi_post_customer_dp,
        lv_wa_tmp     LIKE LINE OF  lv_it_tmp,
        lv_datet(10),
        lv_boeno      TYPE bsed-boeno,
        lv_amount(13),
        lv_msgid      LIKE  sy-msgid,
        lv_msgno      LIKE  sy-msgno,
        lv_msgty      LIKE  sy-msgty,
        lv_msgv1      LIKE  sy-msgv1,
        lv_msgv2      LIKE  sy-msgv2,
        lv_msgv3      LIKE  sy-msgv3,
        lv_msgv4      LIKE  sy-msgv4,
        lv_message    TYPE  string,
        lv_periv      TYPE  t001-periv,
        lv_count      TYPE  i,
        lv_total      TYPE  bseg-wrbtr,
        lv_txt(50).


  REFRESH: lv_it_blntab, lv_it_ftclear, lv_it_ftpost, lv_it_fttax, lv_it_tmp.



  lv_it_tmp[] = gv_it_output[].
  DELETE lv_it_tmp WHERE select <> 'X' OR NOT amount > 0.
  SORT lv_it_tmp BY bukrs gjahr belnr.
  DELETE ADJACENT DUPLICATES FROM lv_it_tmp COMPARING bukrs gjahr belnr.


  BREAK omrani.
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function         = 'C'
      i_mode             = 'E'
      i_update           = 'S'
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      user_invalid       = 6
      OTHERS             = 7.


  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BUKRS' p_bukrs.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BLART' p_blart.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-WAERS' p_waers.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BKTXT' 'واريز نقدي'.
  PERFORM convert_date_to_ext USING p_valut CHANGING lv_datet.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BUDAT' lv_datet.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BLDAT' lv_datet.

  CLEAR lv_count.
  LOOP AT lv_it_tmp INTO lv_wa_tmp.
    lv_count =  lv_count + 1.
    CLEAR lv_total.
    LOOP AT gv_it_output INTO gv_wa_output WHERE bukrs  = lv_wa_tmp-bukrs AND
                                                 gjahr  = lv_wa_tmp-gjahr AND
                                                 belnr  = lv_wa_tmp-belnr AND
                                                 select = 'X'             AND
                                                 amount > 0.
      lv_total = lv_total + gv_wa_output-amount.
    ENDLOOP.

    PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' lv_count 'RF05A-NEWKO' gv_wa_output-hkont.
    PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' lv_count 'RF05A-NEWBS' '40'.
    PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' lv_count 'RF05A-NEWUM' ''.

    WRITE lv_total TO lv_amount CURRENCY gv_wa_output-waers.
    PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' lv_count 'BSEG-WRBTR'  lv_amount.
    PERFORM convert_date_to_ext USING p_valut CHANGING lv_datet.
    PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' lv_count 'BSEG-VALUT'  lv_datet.

    lv_txt = gv_wa_output-belnr.
    SHIFT lv_txt LEFT DELETING LEADING '0'.
    SHIFT gv_wa_output-kunnr LEFT DELETING LEADING '0'.
    CONCATENATE 'واريز نقدي سند:' lv_txt '-' gv_wa_output-KUNNR INTO lv_txt SEPARATED BY space.
    "CONCATENATE 'واريز نقدي سند:' lv_txt INTO lv_txt SEPARATED BY space.
    PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' lv_count 'BSEG-SGTXT'  lv_txt.

  ENDLOOP.


  LOOP AT gv_it_output INTO gv_wa_output WHERE select = 'X' AND amount > 0.

    lv_wa_ftclear-agkoa  = 'D'.
    lv_wa_ftclear-agkon  = gv_wa_output-kunnr.
    lv_wa_ftclear-agbuk  = p_bukrs.
    lv_wa_ftclear-xnops  = 'X'.
    lv_wa_ftclear-xfifo  = ''.
    lv_wa_ftclear-agums  = gv_wa_output-umskz.
    lv_wa_ftclear-avsid  = ''.
    lv_wa_ftclear-selfd  = 'BELNR'.
    CONCATENATE gv_wa_output-belnr gv_wa_output-gjahr gv_wa_output-buzei INTO lv_wa_ftclear-selvon .
    APPEND lv_wa_ftclear TO lv_it_ftclear.

  ENDLOOP.



  BREAK omrani.


  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = 'EINGZAHL'
      i_tcode                    = 'FB05'
      i_sgfunct                  = 'C'
    IMPORTING
      e_msgid                    = lv_msgid
      e_msgno                    = lv_msgno
      e_msgty                    = lv_msgty
      e_msgv1                    = lv_msgv1
      e_msgv2                    = lv_msgv2
      e_msgv3                    = lv_msgv3
      e_msgv4                    = lv_msgv4
      e_subrc                    = lv_subrc
    TABLES
      t_blntab                   = lv_it_blntab
      t_ftclear                  = lv_it_ftclear
      t_ftpost                   = lv_it_ftpost
      t_fttax                    = lv_it_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.


  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
    EXPORTING
      msgid               = lv_msgid
      msgnr               = lv_msgno
      msgv1               = lv_msgv1
      msgv2               = lv_msgv2
      msgv3               = lv_msgv3
      msgv4               = lv_msgv4
    IMPORTING
      message_text_output = lv_message.

  CLEAR gv_wa_log.
  MOVE-CORRESPONDING gv_wa_output TO gv_wa_log.
  gv_wa_log-type       = lv_msgty.
  gv_wa_log-id         = lv_msgid.
  gv_wa_log-number     = lv_msgno.
  gv_wa_log-message_v1 = lv_msgv1.
  gv_wa_log-message_v2 = lv_msgv2.
  gv_wa_log-message_v3 = lv_msgv3.
  gv_wa_log-message_v4 = lv_msgv4.
  gv_wa_log-message    = lv_message.
  APPEND gv_wa_log TO gv_it_log.


  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.


  IF lv_msgty = 'E' OR lv_msgv1 IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM call_post CHANGING lv_subrc TYPE sy-subrc.

  DATA: lv_belnr     TYPE         bkpf-belnr,
        lv_gjahr     TYPE         bkpf-gjahr,
        lv_wa_output LIKE LINE OF gv_it_output.

  REFRESH gv_it_log.
  CLEAR lv_subrc.

  LOOP AT gv_it_output INTO gv_wa_output WHERE select = 'X' AND amount > 0.
    READ TABLE gv_it_output INTO lv_wa_output WITH KEY select = ''
                                                       bukrs  = gv_wa_output-bukrs
                                                       gjahr  = gv_wa_output-gjahr
                                                       belnr  = gv_wa_output-belnr.
    IF sy-subrc EQ 0.
      PERFORM call_msg USING '' 'تمام لاين هاي يک ثبت را انتخاب کنيد' '' ':' lv_wa_output-belnr '' ''.
      lv_subrc = 1.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc NE 0.
    PERFORM call_msg USING '' 'لايني انتخاب نشده است' '' '' '' '' ''.
    lv_subrc = 1.
  ENDIF.

  IF lv_subrc IS INITIAL.
    PERFORM post_with_clear CHANGING lv_subrc lv_belnr lv_gjahr.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FTPOST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_FTPOST  text
*      -->P_       text
*      -->P_       text
*      -->P_       text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM fill_ftpost  TABLES   lv_it_ftpost  STRUCTURE ftpost
                  USING    lv_stype
                           lv_count
                           lv_fnam
                           lv_fval.

  DATA: lv_wa_ftpost LIKE LINE OF lv_it_ftpost.

  CLEAR lv_wa_ftpost.
  lv_wa_ftpost-stype = lv_stype.
  lv_wa_ftpost-count = lv_count.
  lv_wa_ftpost-fnam  = lv_fnam.
  lv_wa_ftpost-fval  = lv_fval.

  COLLECT lv_wa_ftpost INTO lv_it_ftpost.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_DATE_TO_EXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_P_BUDAT  text
*      <--P_LV_DATET  text
*&---------------------------------------------------------------------*
FORM convert_date_to_ext  USING    lv_date  TYPE datum
                          CHANGING lv_datet TYPE char10.

  CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
    EXPORTING
      im_datint   = lv_date
      im_datfmdes = 'C'
    IMPORTING
      ex_datext   = lv_datet.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_log .



  DATA: is_layout       TYPE slis_layout_alv,
        lv_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  CHECK gv_it_log[] IS NOT INITIAL.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_POST_CUSTOMER_DP_LOG'
    CHANGING
      ct_fieldcat      = lv_fieldcatalog[].


  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = lv_fieldcatalog[]
      i_save                = 'X'
      is_layout             = is_layout
      i_screen_start_column = 10
      i_screen_start_line   = 20
      i_screen_end_column   = 100
      i_screen_end_line     = 40
    TABLES
      t_outtab              = gv_it_log
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.


ENDFORM.
.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_input_data CHANGING lv_subrc TYPE sy-subrc.

  CLEAR lv_subrc.
  IF ( p_hbkid IS INITIAL AND     p_hktid IS INITIAL )      AND
     ( p_hbkid IS NOT INITIAL AND p_hktid IS INITIAL )      AND
     ( p_hbkid IS INITIAL AND     p_hktid IS NOT INITIAL )  AND
       p_hkont IS INITIAL.
    MESSAGE '' TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
      ID 'ACTVT' FIELD '03'
      ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE s002(zfi) DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .
  "p_bukrs = '1000'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_SCREEN_DESC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_screen_desc .

  CLEAR: t_hbkid,t_hktid.


  PERFORM get_bank_desc USING p_bukrs p_hbkid p_hktid CHANGING t_hbkid t_hktid.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BANK_DESC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_P_HBKID  text
*      -->P_P_HKTID  text
*      <--P_T_HBKID  text
*      <--P_T_HKTID  text
*&---------------------------------------------------------------------*
FORM get_bank_desc  USING    lv_bukrs TYPE payr-zbukr
                             lv_hbkid TYPE payr-hbkid
                             lv_hktid TYPE payr-hktid
                    CHANGING lt_hbkid
                             lt_hktid.

  CLEAR: lt_hbkid, lt_hktid.

  SELECT SINGLE bankl INTO lt_hbkid FROM t012  WHERE bukrs = lv_bukrs AND hbkid = lv_hbkid AND banks = 'IR'.
  SELECT SINGLE text1 INTO lt_hktid FROM t012t WHERE bukrs = lv_bukrs AND hbkid = lv_hbkid AND hktid = lv_hktid AND spras = 'EN'.



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
    MESSAGE ID 'ZFI' TYPE 'S' NUMBER '001'     DISPLAY LIKE lv_type WITH lv_msg_txt lv_v1 lv_v2 lv_v3.
  ENDIF.

ENDFORM.
