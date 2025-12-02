*&---------------------------------------------------------------------*
*& Report ZFI_AUTO_CLEAR_VENDOR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_auto_clear_vendor.




TABLES : rbkp,bsik,t100,bkpf.

TYPE-POOLS:slis.

DATA  BEGIN OF ybsik OCCURS 1.
INCLUDE STRUCTURE bsik.
DATA  END OF ybsik.

DATA: gv_it_output TYPE TABLE OF zfi_auto_clear_vendor,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_log    TYPE TABLE OF zfi_auto_clear_vendor,
      gv_wa_log    LIKE LINE OF  gv_it_log,
      fieldcatalog TYPE          lvc_t_fcat          WITH HEADER LINE,
      bdcdata      LIKE          bdcdata    OCCURS 0 WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS : s_bukrs  FOR  bkpf-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY DEFAULT '1000',
                 s_lifnr  FOR  bsik-lifnr,
                 s_belnr  FOR  rbkp-belnr,
                 s_gjahr  FOR  rbkp-gjahr,
                 s_budat  FOR  rbkp-budat.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM check_company_code_auth.
  PERFORM get_data_step1.
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
      i_structure_name = 'ZFI_AUTO_CLEAR_VENDOR'
    CHANGING
      ct_fieldcat      = fieldcatalog[].



  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'MSG'.
        PERFORM set_catalog_text USING 'Txt' CHANGING fieldcatalog.
        fieldcatalog-edit   = 'X'.
        fieldcatalog-no_out = 'X'.
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

  DATA: is_layout     TYPE lvc_s_layo,
        ls_event_exit TYPE slis_event_exit,
        lt_event_exit TYPE slis_t_event_exit.



  is_layout-zebra      = 'X'.
  is_layout-cwidth_opt = 'X'.

  ls_event_exit-ucomm = '&REFRESH'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

  ls_event_exit-ucomm = '&NTE'.
  ls_event_exit-after = 'X'.
  APPEND ls_event_exit TO lt_event_exit.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program      = sy-repid
      it_fieldcat_lvc         = fieldcatalog[]
      i_save                  = 'X'
      is_layout_lvc           = is_layout
      i_callback_user_command = 'USER_COMMAND'
      it_event_exit           = lt_event_exit
    TABLES
      t_outtab                = gv_it_output
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

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
*& Form GET_DATA_STEP1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_step1 .

  REFRESH gv_it_output.


  SELECT
      rbkp~belnr AS inv_belnr,
      rbkp~gjahr AS inv_gjahr,
      rbkp~budat AS inv_budat,
      bsik1~bukrs,
      bsik1~lifnr,
      bsik1~gjahr,
      bsik1~belnr,
      bsik1~buzei,
      bsik1~wrbtr,
      bsik1~waers,
      bsik2~gjahr AS clr_gjahr,
      bsik2~belnr AS clr_belnr,
      bsik2~buzei AS clr_buzei,
      bsik2~wrbtr AS clr_wrbtr,
      bsik2~waers AS clr_waers,
      SUM( bsik3~wrbtr ) AS clr_wrbtr_sum
  FROM rbkp
  JOIN bkpf ON substring( bkpf~awkey,1,10 ) = rbkp~belnr AND
               substring( bkpf~awkey,11,4 ) = rbkp~gjahr
  JOIN bsik_view AS bsik1 ON bsik1~bukrs = bkpf~bukrs    AND
                             bsik1~belnr = bkpf~belnr    AND
                             bsik1~gjahr = bkpf~gjahr
  JOIN bsik_view AS bsik2 ON bsik1~bukrs = bsik2~bukrs   AND
                             bsik1~belnr = bsik2~rebzg   AND
                             bsik1~gjahr = bsik2~rebzj   AND
                             bsik1~buzei = bsik2~rebzz   AND
                             bsik2~umskz = ''
  JOIN bsik_view AS bsik3 ON bsik1~bukrs = bsik3~bukrs   AND
                             bsik1~belnr = bsik3~rebzg   AND
                             bsik1~gjahr = bsik3~rebzj   AND
                             bsik1~buzei = bsik3~rebzz   AND
                             bsik3~umskz = ''
  WHERE  bkpf~bukrs  IN @s_bukrs AND
         rbkp~belnr  IN @s_belnr AND
         rbkp~budat  IN @s_budat AND
         rbkp~gjahr  IN @s_gjahr AND
         bsik1~lifnr IN @s_lifnr
  GROUP BY
      rbkp~belnr,
      rbkp~gjahr,
      rbkp~budat,
      bsik1~bukrs,
      bsik1~lifnr,
      bsik1~gjahr,
      bsik1~belnr,
      bsik1~buzei,
      bsik1~wrbtr,
      bsik1~waers,
      bsik2~gjahr,
      bsik2~belnr,
      bsik2~buzei,
      bsik2~wrbtr,
      bsik2~waers
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_output.


  LOOP AT gv_it_output INTO gv_wa_output.
    IF gv_wa_output-wrbtr <> gv_wa_output-clr_wrbtr_sum.
      DELETE gv_it_output INDEX sy-tabix.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLEAR_FI_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear_fi_doc .

  DATA: lv_it_tmp    TYPE TABLE OF zfi_auto_clear_vendor,
        lv_wa_tmp    LIKE LINE OF  lv_it_tmp,
        lv_it_header TYPE TABLE OF zfi_auto_clear_vendor,
        lv_wa_header LIKE LINE OF  lv_it_header.

  SORT gv_it_output.
  REFRESH lv_it_tmp.
  lv_it_tmp[]    = gv_it_output[].
  lv_it_header[] = gv_it_output[].
  DELETE ADJACENT DUPLICATES FROM lv_it_tmp    COMPARING inv_belnr inv_gjahr.
  DELETE ADJACENT DUPLICATES FROM lv_it_header COMPARING inv_belnr inv_gjahr gjahr belnr buzei.



  LOOP AT lv_it_tmp INTO lv_wa_tmp.

    REFRESH ybsik.


    LOOP AT lv_it_header INTO lv_wa_header WHERE inv_belnr = lv_wa_tmp-inv_belnr AND
                                                 inv_gjahr = lv_wa_tmp-inv_gjahr.
      PERFORM fill_ybsik USING lv_wa_tmp-bukrs lv_wa_header-gjahr lv_wa_header-belnr lv_wa_header-buzei.
    ENDLOOP.

    LOOP AT gv_it_output INTO gv_wa_output WHERE inv_belnr = lv_wa_tmp-inv_belnr AND
                                                 inv_gjahr = lv_wa_tmp-inv_gjahr.

      PERFORM fill_ybsik USING lv_wa_tmp-bukrs gv_wa_output-clr_gjahr gv_wa_output-clr_belnr gv_wa_output-clr_buzei.
    ENDLOOP.

    REFRESH bdcdata.
    PERFORM bdc_dynpro      USING 'SAPMF05A'       '0131'.
    PERFORM bdc_field       USING 'BKPF-BUKRS'     lv_wa_tmp-bukrs.
    PERFORM bdc_field       USING 'RF05A-AGKON'    lv_wa_tmp-lifnr.
    PERFORM bdc_field       USING 'BDC_OKCODE'      '/11'.
    PERFORM bdc_dynpro      USING 'SAPMF05A'       '0700'.
    PERFORM bdc_field       USING 'BDC_OKCODE'      '/11'.

    BREAK omrani.
    EXPORT ybsik TO MEMORY ID '%F124%'.
    PERFORM bdc_transaction USING 'FB1K' lv_wa_tmp.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_YBSIK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_TMP_BUKRS  text
*      -->P_LV_IT_TMP_GJAHR  text
*      -->P_LV_IT_TMP_BELNR  text
*      -->P_LV_IT_TMP_BUZEI  text
*&---------------------------------------------------------------------*
FORM fill_ybsik  USING    lv_bukrs TYPE bsik-bukrs
                          lv_gjahr TYPE bsik-gjahr
                          lv_belnr TYPE bsik-belnr
                          lv_buzei TYPE bsik-buzei.

  CLEAR ybsik.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ybsik
  FROM bsik
  WHERE bukrs = lv_bukrs AND
        gjahr = lv_gjahr AND
        belnr = lv_belnr AND
        buzei = lv_buzei.
  IF sy-subrc EQ 0.
    APPEND ybsik.
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
FORM bdc_transaction USING tcode
                           lv_data TYPE zfi_auto_clear_vendor.

  DATA : lv_mode(1),
         lv_update(1),
         lv_it_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.
  DATA: l_mstring(480).
  DATA: l_subrc LIKE sy-subrc.

  lv_mode = 'N'.
  CALL TRANSACTION tcode USING         bdcdata
                         MODE          lv_mode
                         MESSAGES INTO lv_it_messtab.

  LOOP AT lv_it_messtab.

    CLEAR l_mstring.
    SELECT SINGLE * FROM t100 WHERE sprsl = lv_it_messtab-msgspra
                              AND   arbgb = lv_it_messtab-msgid
                              AND   msgnr = lv_it_messtab-msgnr.
    IF sy-subrc = 0.
      l_mstring = t100-text.
      IF l_mstring CS '&1'.
        REPLACE '&1' WITH lv_it_messtab-msgv1 INTO l_mstring.
        REPLACE '&2' WITH lv_it_messtab-msgv2 INTO l_mstring.
        REPLACE '&3' WITH lv_it_messtab-msgv3 INTO l_mstring.
        REPLACE '&4' WITH lv_it_messtab-msgv4 INTO l_mstring.
      ELSE.
        REPLACE '&' WITH lv_it_messtab-msgv1 INTO l_mstring.
        REPLACE '&' WITH lv_it_messtab-msgv2 INTO l_mstring.
        REPLACE '&' WITH lv_it_messtab-msgv3 INTO l_mstring.
        REPLACE '&' WITH lv_it_messtab-msgv4 INTO l_mstring.
      ENDIF.
      CONDENSE l_mstring.
    ELSE.
    ENDIF.
    CLEAR gv_wa_log.
    MOVE-CORRESPONDING lv_data TO gv_wa_log.
    gv_wa_log-msg  = l_mstring.
    APPEND gv_wa_log TO gv_it_log.
  ENDLOOP.

ENDFORM.


FORM user_command USING i_ucomm     TYPE syucomm
                        is_selfield TYPE slis_selfield.


  DATA: ref1 TYPE REF TO cl_gui_alv_grid.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.
  CALL METHOD ref1->check_changed_data.


  CASE i_ucomm.
    WHEN '&REFRESH'.
      PERFORM get_data_step1.
    WHEN '&DATA_SAVE'.
      REFRESH gv_it_log.
      PERFORM clear_fi_doc.
      PERFORM display_log.
      PERFORM get_data_step1.
    WHEN OTHERS.
  ENDCASE.

  CALL METHOD ref1->refresh_table_display.
  SET USER-COMMAND '&OPT'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_log.

  DATA: lv_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        is_layout       TYPE slis_layout_alv.

  CHECK gv_it_log[] IS NOT INITIAL.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_AUTO_CLEAR_VENDOR'
    CHANGING
      ct_fieldcat      = lv_fieldcatalog[].



  LOOP AT lv_fieldcatalog WHERE fieldname(3) = 'CLR'.
    lv_fieldcatalog-no_out = 'X'.
    MODIFY lv_fieldcatalog.
  ENDLOOP.


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
*&---------------------------------------------------------------------*
*& Form CHECK_COMPANY_CODE_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_company_code_auth .


  DATA: lv_t001 TYPE t001.
  RANGES: s_bukrs1 FOR bkpf-bukrs.

  REFRESH s_bukrs1.
  s_bukrs1[] = s_bukrs[].

  REFRESH s_bukrs.

  SELECT * INTO lv_t001 FROM t001 WHERE bukrs IN s_bukrs1.

    AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
        ID 'ACTVT' FIELD '03'
        ID 'BUKRS' FIELD lv_t001-bukrs.
    IF sy-subrc EQ 0.
      CLEAR s_bukrs.
      s_bukrs-low    = lv_t001-bukrs.
      s_bukrs-sign   = 'I'.
      s_bukrs-option = 'EQ'.
      APPEND s_bukrs.
    ENDIF.
  ENDSELECT.

  IF s_bukrs[] IS INITIAL.
    MESSAGE e002(zfi).
    EXIT.
  ENDIF.

ENDFORM.
