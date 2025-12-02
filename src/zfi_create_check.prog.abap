*&---------------------------------------------------------------------*
*& Report ZFI_CREATE_CHECK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_create_check.




TABLES : mara,payr,bkpf.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_create_check,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_tmp    TYPE TABLE OF zfi_create_check,
      gv_wa_tmp    LIKE LINE OF  gv_it_tmp,
      gv_it_log    TYPE TABLE OF zfi_create_check_log,
      gv_wa_log    LIKE LINE OF  gv_it_log,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
      bdcdata      LIKE          bdcdata             OCCURS 0 WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk2 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_bukrs TYPE payr-zbukr NO-DISPLAY,
            p_gjahr TYPE payr-gjahr OBLIGATORY DEFAULT '2019',
            p_budat TYPE bkpf-budat OBLIGATORY DEFAULT sy-datum,
            p_blart TYPE bkpf-blart OBLIGATORY DEFAULT 'Z3',
            p_waers TYPE bkpf-waers OBLIGATORY DEFAULT 'IRR',
            p_xblnr TYPE bkpf-xblnr,
            p_bktxt TYPE bkpf-bktxt.
SELECTION-SCREEN: END   OF BLOCK blk2.


SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: s_belnr FOR payr-vblnr,
                s_budat FOR bkpf-budat.
SELECTION-SCREEN: END   OF BLOCK blk1.

SELECTION-SCREEN: BEGIN     OF BLOCK blk3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_hbkid TYPE payr-hbkid OBLIGATORY,
            p_hktid TYPE payr-hktid OBLIGATORY,
            p_stapl TYPE pcec-stapl OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK blk3.


INITIALIZATION.
  PERFORM initialization.


START-OF-SELECTION.

  PERFORM check_input.
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
      i_structure_name = 'ZFI_CREATE_CHECK'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'AMOUNT'.
        PERFORM set_catalog_text USING 'Payment Request' CHANGING fieldcatalog.
        "fieldcatalog-edit          = 'X'.

      WHEN 'SELECT'.
        fieldcatalog-edit          = 'X'.
        fieldcatalog-checkbox      = 'X'.

      WHEN 'DUDAT'.
        PERFORM set_catalog_text USING 'Due Date' CHANGING fieldcatalog.
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


  DATA: is_layout TYPE slis_layout_alv .

  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      it_fieldcat              = fieldcatalog[]
      i_save                   = 'X'
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

  DATA: lv_subrc(1),
        lv_check    TYPE        payr-chect,
        lv_belnr    TYPE        bkpf-belnr,
        lv_gjahr    TYPE        bkpf-gjahr,
        ref1        TYPE REF TO cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.

  CALL METHOD ref1->check_changed_data.

  CASE p_ucomm.
    WHEN 'POST'.
      PERFORM call_post.
      PERFORM display_log.
      PERFORM get_data.

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
    lv_wa_vendor-vendor_no  = gv_wa_output-lifnr.
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
  lv_wa_vendor-vendor_no      = gv_wa_output-lifnr.
  lv_wa_vendor-sp_gl_ind      = ''.
  APPEND lv_wa_vendor TO lv_it_vendor.

  CLEAR lv_wa_curr.
  lv_wa_curr-itemno_acc   = lv_index.
  lv_wa_curr-currency     = gv_wa_output-waers.
  lv_wa_curr-amt_doccur   = lv_total * -1.
  APPEND lv_wa_curr TO lv_it_curr.

  PERFORM post_fi_doc TABLES lv_it_curr lv_it_vendor lv_it_return USING p_bukrs p_budat CHANGING lv_belnr lv_gjahr.

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

  REFRESH gv_it_output.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE @gv_it_output
    FROM bsik
    JOIN bkpf ON bkpf~bukrs  = bsik~bukrs AND
                 bkpf~gjahr  = bsik~gjahr AND
                 bkpf~belnr  = bsik~belnr
    JOIN bseg ON bkpf~bukrs  = bseg~bukrs AND
                 bkpf~gjahr  = bseg~gjahr AND
                 bkpf~belnr  = bseg~belnr AND
                 bsik~buzei  = bseg~buzei
    LEFT JOIN lfa1 ON bsik~lifnr = lfa1~lifnr
    WHERE bkpf~belnr IN @s_belnr AND
          bkpf~gjahr =  @p_gjahr AND
          bkpf~bukrs =  @p_bukrs AND
          bkpf~budat IN @s_budat AND
          bsik~umskz IN ('F','P').



  LOOP AT gv_it_output INTO gv_wa_output.
    gv_wa_output-dudat  = gv_wa_output-zfbdt.
    gv_wa_output-amount = gv_wa_output-dmbtr.
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
    MOVE-CORRESPONDING gv_wa_tmp TO gv_wa_log.
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
FORM post_with_clear USING    lv_check     TYPE      payr-chect
                     CHANGING lv_subrc     TYPE      sy-subrc
                              lv_belnr     TYPE      bkpf-belnr
                              lv_gjahr     TYPE      bkpf-gjahr.

  DATA: lv_it_blntab  TYPE TABLE OF blntab,
        lv_it_ftclear TYPE TABLE OF ftclear,
        lv_wa_ftclear LIKE LINE OF  lv_it_ftclear,
        lv_it_ftpost  TYPE TABLE OF ftpost,
        lv_it_fttax   TYPE TABLE OF fttax,
        lv_total      TYPE          zfi_create_check-amount,
        lv_datet(10),
        lv_boeno      TYPE bsed-boeno,
        lv_amount(13),
        lv_count      TYPE  i,
        lv_msgid      LIKE  sy-msgid,
        lv_msgno      LIKE  sy-msgno,
        lv_msgty      LIKE  sy-msgty,
        lv_msgv1      LIKE  sy-msgv1,
        lv_msgv2      LIKE  sy-msgv2,
        lv_msgv3      LIKE  sy-msgv3,
        lv_msgv4      LIKE  sy-msgv4,
        lv_message    TYPE  string,
        lv_periv      TYPE  t001-periv,
        lv_gj(4).


  REFRESH:      lv_it_blntab, lv_it_ftclear, lv_it_ftpost, lv_it_fttax.

  SHIFT lv_check LEFT DELETING LEADING space.
  lv_boeno = lv_check.

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
  PERFORM convert_date_to_ext USING p_budat CHANGING lv_datet.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BUDAT' lv_datet.
  PERFORM convert_date_to_ext USING sy-datum CHANGING lv_datet.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'K' 1 'BKPF-BLDAT' lv_datet.

  CLEAR lv_total.
  LOOP AT gv_it_output INTO gv_wa_output WHERE   lifnr  = gv_wa_tmp-lifnr AND
                                                 ebeln  = gv_wa_tmp-ebeln AND
                                                 rebzg  = gv_wa_tmp-rebzg AND
                                                 dudat  = gv_wa_tmp-dudat AND
                                                 select = 'X'.
    lv_total = lv_total + gv_wa_output-amount.
  ENDLOOP.


  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' 1 'RF05A-NEWKO' gv_wa_output-lifnr.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' 1 'RF05A-NEWBS' '39'.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' 1 'RF05A-NEWUM' 'W'.


  WRITE lv_total TO lv_amount CURRENCY gv_wa_output-waers.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' 1 'BSEG-WRBTR'  lv_amount.
  PERFORM convert_date_to_ext USING gv_wa_output-dudat CHANGING lv_datet.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' 1 'BSEG-ZFBDT'  lv_datet.
  PERFORM fill_ftpost TABLES lv_it_ftpost USING 'P' 1 'BSED-BOENO'  lv_boeno.

  CLEAR lv_count.
  LOOP AT gv_it_output INTO gv_wa_output WHERE lifnr  = gv_wa_tmp-lifnr AND
                                               ebeln  = gv_wa_tmp-ebeln AND
                                               rebzg  = gv_wa_tmp-rebzg AND
                                               dudat  = gv_wa_tmp-dudat AND
                                               select = 'X'.

    lv_count = lv_count + 1.
    lv_wa_ftclear-agkoa  = 'K'.
    lv_wa_ftclear-agkon  = gv_wa_output-lifnr.
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
      i_auglv                    = 'AUSGZAHL'
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
  gv_wa_log-message_v4 = lv_check.
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


    lv_belnr = lv_msgv1.


    SELECT SINGLE periv INTO lv_periv FROM t001 WHERE bukrs = p_bukrs.
    CALL FUNCTION 'GM_GET_FISCAL_YEAR'
      EXPORTING
        i_date                     = p_budat
        i_fyv                      = lv_periv
      IMPORTING
        e_fy                       = lv_gj
      EXCEPTIONS
        fiscal_year_does_not_exist = 1
        not_defined_for_date       = 2
        OTHERS                     = 3.


    lv_gjahr = lv_gj.

    CLEAR lv_subrc.
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
FORM call_post .

  DATA: lv_subrc TYPE          sy-subrc,
        lv_check TYPE          payr-chect,
        lv_belnr TYPE          bkpf-belnr,
        lv_gjahr TYPE          bkpf-gjahr.

  REFRESH: gv_it_log,gv_it_tmp.

  gv_it_tmp[] = gv_it_output[].
  DELETE gv_it_tmp WHERE select <> 'X'.

  SORT gv_it_tmp BY lifnr ebeln rebzg dudat.
  DELETE ADJACENT DUPLICATES FROM gv_it_tmp COMPARING lifnr ebeln rebzg dudat.

  LOOP AT gv_it_tmp INTO gv_wa_tmp.
    PERFORM get_check_no    CHANGING lv_check.
    IF lv_check IS NOT INITIAL.
      PERFORM post_with_clear USING lv_check CHANGING lv_subrc lv_belnr lv_gjahr.
      BREAK omrani.
      IF lv_subrc IS INITIAL.
        PERFORM call_fch5    USING lv_belnr lv_gjahr lv_check CHANGING lv_subrc.
      ENDIF.
    ENDIF.
  ENDLOOP.



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

  APPEND lv_wa_ftpost TO lv_it_ftpost.

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
      i_structure_name = 'ZFI_CREATE_CHECK_LOG'
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
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

  p_bukrs = '1000'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_input .


  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
      ID 'ACTVT' FIELD '03'
      ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e002(zfi).
  ENDIF.

ENDFORM.
