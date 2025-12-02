*&---------------------------------------------------------------------*
*& Report ZFI_UPDATE_CHECK_ENCASHMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_update_check_encashment.



TABLES : payr.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF payr,
      gv_wa_output LIKE LINE OF  gv_it_output,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
      gv_it_log    TYPE TABLE OF zfi_update_check_encashment,
      gv_wa_log    LIKE LINE OF  gv_it_log,
      bdcdata      LIKE          bdcdata             OCCURS 0 WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS : s_zbukr    FOR  payr-zbukr OBLIGATORY NO INTERVALS NO-EXTENSION DEFAULT '1100' ,
                 s_hbkid    FOR  payr-hbkid,
                 s_hktid    FOR  payr-hktid,
                 s_chect    FOR  payr-chect,
                 s_zaldt    FOR  payr-zaldt.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM get_data.
  PERFORM update_encashment.
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
      i_structure_name = 'ZFI_UPDATE_CHECK_ENCASHMENT'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'F1'.
        PERFORM set_catalog_text USING 'Txt' CHANGING fieldcatalog.

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
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gv_it_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

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
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .


  REFRESH: gv_it_output,gv_it_log.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gv_it_output
    FROM payr
    WHERE zbukr IN s_zbukr    AND
          hbkid IN s_hbkid    AND
          hktid IN s_hktid    AND
          chect IN s_chect    AND
          zaldt IN s_zaldt    AND
          bancd =  '00000000' AND
          zaldt <> '00000000' AND
          zaldt <> ''.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_ENCASHMENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_encashment .

  LOOP AT gv_it_output INTO gv_wa_output.
    PERFORM call_fch6 USING gv_wa_output-zbukr gv_wa_output-hbkid gv_wa_output-hktid gv_wa_output-chect gv_wa_output-zaldt.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_FCH6
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_GV_WA_OUTPUT_ZBUKR  text
*      -->P_GV_WA_OUTPUT_HBKID  text
*      -->P_GV_WA_OUTPUT_HKTID  text
*      -->P_GV_WA_OUTPUT_CHECT  text
*      -->P_GV_WA_OUTPUT_ZALDT  text
*&---------------------------------------------------------------------*
FORM call_fch6  USING    lv_zbukr TYPE payr-zbukr
                         lv_hbkid TYPE payr-hbkid
                         lv_hktid TYPE payr-hktid
                         lv_chect TYPE payr-chect
                         lv_zaldt TYPE payr-zaldt.


  DATA: lv_message   TYPE string,
        lv_datet(10),
        lv_subrc     TYPE sy-subrc.

  REFRESH bdcdata.

  IF p_test IS INITIAL.
    PERFORM convert_date_to_ext USING lv_zaldt CHANGING lv_datet.

    PERFORM bdc_dynpro      USING 'SAPMFCHK'   '0600'.
    PERFORM bdc_field       USING 'PAYR-ZBUKR' lv_zbukr.
    PERFORM bdc_field       USING 'PAYR-HBKID' lv_hbkid.
    PERFORM bdc_field       USING 'PAYR-HKTID' lv_hktid.
    PERFORM bdc_field       USING 'PAYR-CHECT' lv_chect.
    PERFORM bdc_field       USING 'BDC_OKCODE' '/00'.

    PERFORM bdc_dynpro      USING 'SAPMFCHK'   '0601'.
    PERFORM bdc_field       USING 'PAYR-BANCD' lv_datet.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=UPDA'.

    PERFORM bdc_transaction USING 'FCH6' CHANGING lv_subrc.
  ENDIF.

  CLEAR gv_wa_log.
  gv_wa_log-zbukr = lv_zbukr.
  gv_wa_log-hbkid = lv_hbkid.
  gv_wa_log-hktid = lv_hktid.
  gv_wa_log-chect = lv_chect.
  gv_wa_log-bancd = lv_zaldt.


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

    gv_wa_log-type       = sy-msgty.
    gv_wa_log-id         = sy-msgid.
    gv_wa_log-number     = sy-msgno.
    gv_wa_log-message_v1 = sy-msgv1.
    gv_wa_log-message_v2 = sy-msgv2.
    gv_wa_log-message_v3 = sy-msgv3.
    gv_wa_log-message_v4 = sy-msgv4.
    gv_wa_log-message    = lv_message.
  ELSE.
    IF p_test IS INITIAL.
      gv_wa_log-type       = 'S'.
      gv_wa_log-message    = 'Update...'.
    ENDIF.
  ENDIF.


  APPEND gv_wa_log TO gv_it_log.


ENDFORM.



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
ENDFORM.

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
*& Form CONVERT_DATE_TO_EXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_P_BUDAT  text
*      <--P_LV_DATET  text
*&---------------------------------------------------------------------*
FORM convert_date_to_ext  USING    lv_date  TYPE datum
                          CHANGING lv_datet TYPE char10.


  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = lv_date
    IMPORTING
      date_external            = lv_datet
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.


ENDFORM.
