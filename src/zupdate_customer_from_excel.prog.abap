*&---------------------------------------------------------------------*
*& Report ZUPDATE_CUSTOMER_FROM_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zupdate_customer_from_excel.


TYPE-POOLS: slis,truxs.




TYPES: BEGIN OF gt_excel_file,
         kunnr  TYPE kna1-kunnr,
         phone1 TYPE ad_telnrlg,
         typep1 TYPE ad_flgmob,
         phone2 TYPE ad_telnrlg,
         typep2 TYPE ad_flgmob,
         phone3 TYPE ad_telnrlg,
         typep3 TYPE ad_flgmob,
         phone4 TYPE ad_telnrlg,
         typep4 TYPE ad_flgmob,
         phone5 TYPE ad_telnrlg,
         typep5 TYPE ad_flgmob,
       END OF gt_excel_file.

DATA: fieldcatalog TYPE                   slis_t_fieldcat_alv WITH HEADER LINE,
      gv_it_excel  TYPE STANDARD TABLE OF gt_excel_file,
      gv_wa_excel  LIKE LINE OF           gv_it_excel,
      gv_it_log    TYPE STANDARD TABLE OF zupdate_customer_from_excel,
      gv_wa_log    TYPE                   zupdate_customer_from_excel,
      gv_it_raw    TYPE                   truxs_t_text_data.


SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME .
PARAMETERS: p_file   TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK blk1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.



START-OF-SELECTION.

  PERFORM read_excel.
  PERFORM update_customer.
  PERFORM build_fieldcatalog.
  PERFORM display_grid.









*&---------------------------------------------------------------------*
*&      Form  f4_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_file .

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.


ENDFORM.                                                    "f4_file


*&---------------------------------------------------------------------*
*&      Form  read_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_excel .

  REFRESH gv_it_raw.
  REFRESH gv_it_excel.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = gv_it_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = gv_it_excel[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error in read Excel file!' TYPE 'E'.
  ENDIF.





ENDFORM.
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
      i_structure_name = 'ZUPDATE_CUSTOMER_FROM_EXCEL'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'F1'.
        fieldcatalog-seltext_l     = 'text'.
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


  CHECK gv_it_log[] IS NOT INITIAL.

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
*& Form UPDATE_CUSTOMER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_customer .

  DATA: lv_it_kunnr  TYPE TABLE OF gt_excel_file,
        lv_wa_kunnr  LIKE LINE OF  lv_it_kunnr,
        lv_it_phone  TYPE TABLE OF bapiadtel,
        lv_wa_phone  LIKE LINE OF  lv_it_phone,
        lv_it_phonex TYPE TABLE OF bapiadtelx,
        lv_wa_phonex LIKE LINE OF  lv_it_phonex,
        lv_kunnr     TYPE          bu_partner,
        lv_it_ret    TYPE TABLE OF bapiret2,
        lv_wa_ret    LIKE LINE OF  lv_it_ret.

  REFRESH lv_it_kunnr.

  lv_it_kunnr[] = gv_it_excel[].

  SORT lv_it_kunnr BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lv_it_kunnr COMPARING kunnr.



  LOOP AT lv_it_kunnr INTO lv_wa_kunnr.

    CLEAR lv_kunnr.
    PERFORM convert_customer USING lv_wa_kunnr-kunnr CHANGING lv_kunnr.

    REFRESH: lv_it_phonex,lv_it_phone,lv_it_ret.

    LOOP AT gv_it_excel INTO gv_wa_excel WHERE kunnr = lv_wa_kunnr-kunnr.
      CLEAR: lv_wa_phonex,lv_wa_phone.

      IF gv_wa_excel-phone1 IS NOT INITIAL.
        lv_wa_phone-r_3_user  = gv_wa_excel-typep1 .
        lv_wa_phone-country   = 'IR' .
        lv_wa_phone-telephone = gv_wa_excel-phone1 .
        APPEND lv_wa_phone TO lv_it_phone .

        lv_wa_phonex-r_3_user   = 'X' .
        lv_wa_phonex-telephone  = 'X' .
        lv_wa_phonex-updateflag = 'I' .
        lv_wa_phonex-country    = 'X' .
        APPEND lv_wa_phonex TO lv_it_phonex .
      ENDIF.

      IF gv_wa_excel-phone2 IS NOT INITIAL.
        lv_wa_phone-r_3_user  = gv_wa_excel-typep2 .
        lv_wa_phone-country   = 'IR' .
        lv_wa_phone-telephone = gv_wa_excel-phone2 .
        APPEND lv_wa_phone TO lv_it_phone .

        lv_wa_phonex-r_3_user   = 'X' .
        lv_wa_phonex-telephone  = 'X' .
        lv_wa_phonex-updateflag = 'I' .
        lv_wa_phonex-country    = 'X' .
        APPEND lv_wa_phonex TO lv_it_phonex .
      ENDIF.

      IF gv_wa_excel-phone3 IS NOT INITIAL.
        lv_wa_phone-r_3_user  = gv_wa_excel-typep3 .
        lv_wa_phone-country   = 'IR' .
        lv_wa_phone-telephone = gv_wa_excel-phone3 .
        APPEND lv_wa_phone TO lv_it_phone .

        lv_wa_phonex-r_3_user   = 'X' .
        lv_wa_phonex-telephone  = 'X' .
        lv_wa_phonex-updateflag = 'I' .
        lv_wa_phonex-country    = 'X' .
        APPEND lv_wa_phonex TO lv_it_phonex .
      ENDIF.

      IF gv_wa_excel-phone4 IS NOT INITIAL.
        lv_wa_phone-r_3_user  = gv_wa_excel-typep4 .
        lv_wa_phone-country   = 'IR' .
        lv_wa_phone-telephone = gv_wa_excel-phone4 .
        APPEND lv_wa_phone TO lv_it_phone .

        lv_wa_phonex-r_3_user   = 'X' .
        lv_wa_phonex-telephone  = 'X' .
        lv_wa_phonex-updateflag = 'I' .
        lv_wa_phonex-country    = 'X' .
        APPEND lv_wa_phonex TO lv_it_phonex .
      ENDIF.

      IF gv_wa_excel-phone5 IS NOT INITIAL.
        lv_wa_phone-r_3_user  = gv_wa_excel-typep5 .
        lv_wa_phone-country   = 'IR' .
        lv_wa_phone-telephone = gv_wa_excel-phone5 .
        APPEND lv_wa_phone TO lv_it_phone .

        lv_wa_phonex-r_3_user   = 'X' .
        lv_wa_phonex-telephone  = 'X' .
        lv_wa_phonex-updateflag = 'I' .
        lv_wa_phonex-country    = 'X' .
        APPEND lv_wa_phonex TO lv_it_phonex .
      ENDIF.
    ENDLOOP.


    CALL FUNCTION 'BAPI_BUPA_ADDRESS_CHANGE'
      EXPORTING
        businesspartner        = lv_kunnr
        duplicate_message_type = '-'
      TABLES
        bapiadtel              = lv_it_phone
        bapiadtel_x            = lv_it_phonex
        return                 = lv_it_ret.


    READ TABLE lv_it_ret INTO lv_wa_ret WITH KEY 'E'.
    IF sy-subrc EQ 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.


    LOOP AT lv_it_ret INTO lv_wa_ret.

      CLEAR gv_wa_log.
      MOVE-CORRESPONDING lv_wa_kunnr TO gv_wa_log.
      MOVE-CORRESPONDING lv_wa_ret TO gv_wa_log.
      APPEND gv_wa_log TO gv_it_log.

    ENDLOOP.


  ENDLOOP.



  MESSAGE 'Data Updated...' TYPE 'S'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_CUSTOMER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_KUNNR_KUNNR  text
*      <--P_LV_KUNNR  text
*&---------------------------------------------------------------------*
FORM convert_customer  USING    lv_input  TYPE kunnr
                       CHANGING lv_output TYPE bu_partner.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_input
    IMPORTING
      output = lv_output.




ENDFORM.
