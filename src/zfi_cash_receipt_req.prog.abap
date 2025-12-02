*&---------------------------------------------------------------------*
*& Report ZFI_CASH_RECEIPT_REQ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_cash_receipt_req.

TABLES: zficrr.


DATA: ok_code     LIKE          sy-ucomm,
      gv_error(1).




SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
PARAMETERS: p_bukrs TYPE bkpf-bukrs  NO-DISPLAY,
            p_crrid TYPE zfi_cash_receipt-crrid.
SELECTION-SCREEN: END   OF BLOCK blk1.


INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM check_input_data CHANGING gv_error.
  CHECK gv_error IS INITIAL.
  PERFORM fill_data.
  PERFORM call_screen.




*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

  CLEAR: zficrr.
  p_bukrs = '1000'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_GV_ERROR  text
*&---------------------------------------------------------------------*
FORM check_input_data  CHANGING lv_error TYPE char1.

  DATA: lv_crrid TYPE zfi_cash_receipt-crrid.

  CHECK p_crrid IS NOT INITIAL.
  SELECT SINGLE crrid INTO lv_crrid FROM zfi_cash_receipt WHERE crrid = p_crrid AND bukrs = p_bukrs.
  IF sy-subrc NE 0.
    lv_error = '1'.
    PERFORM msg USING 'E' 'کد وارد شده وجود ندارد' '' '' ''.
  ENDIF.



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

  IF p_crrid IS NOT INITIAL.

    SELECT SINGLE * INTO CORRESPONDING FIELDS OF zficrr
      FROM zfi_cash_receipt WHERE crrid = p_crrid.

  ELSE.

    zficrr-bukrs     = p_bukrs.
    zficrr-erdat     = sy-datum.
    zficrr-ernam     = sy-uname.
    zficrr-erzet     = sy-uzeit.

  ENDIF.



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

FORM msg  USING    lv_msg
                   lv_1
                   lv_2
                   lv_3
                   lv_4.

  MESSAGE ID 'ZFI' TYPE 'S' NUMBER '001'  DISPLAY LIKE lv_msg WITH lv_1 lv_2 lv_3 lv_4.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM loop_screen_0100.
  PERFORM set_status.
  PERFORM fill_screen_data.

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
*&---------------------------------------------------------------------*
*& Form EXIT_COMMANDS_FORM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_OK_CODE  text
*&---------------------------------------------------------------------*
FORM exit_commands_form  CHANGING ok_code.



  DATA : lv_subrc(1).

  CASE ok_code.
    WHEN 'ENDE' OR 'ECAN' OR 'EXIT'.
      PERFORM exit.
    WHEN 'DELETE'.
      PERFORM delete.
      PERFORM exit.
    WHEN 'SAVE'.
      PERFORM final_check CHANGING lv_subrc.
      IF lv_subrc IS INITIAL.
        PERFORM save.
      ENDIF.
    WHEN 'PRINT'.
      PERFORM print.
    WHEN OTHERS.
  ENDCASE.

  CLEAR ok_code.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exit .
  SET SCREEN 0.
ENDFORM.
FORM delete .

  CHECK zficrr-crrid IS NOT INITIAL.

  UPDATE zfi_cash_receipt SET loekz = 'X'
                              ldate = sy-datum
                              ltime = sy-uzeit
                              luser = sy-uname
  WHERE crrid = zficrr-crrid.
  COMMIT WORK AND WAIT.

  PERFORM msg USING 'S' 'سند حذف شد:' zficrr-crrid '' ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LOOP_SCREEN_0100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM loop_screen_0100 .


  LOOP AT SCREEN.

    IF zficrr-loekz IS NOT INITIAL.
      IF screen-name <> 'ZFICRR-LOEKZ'.
        screen-input = 0.
      ENDIF.
    ENDIF.


    MODIFY SCREEN.
  ENDLOOP.



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


  IF zficrr-currency IS INITIAL.
    zficrr-currency  = 'IRR'.
  ENDIF.

  IF zficrr-bukrs IS NOT INITIAL.
    SELECT SINGLE butxt INTO zficrr-butxt FROM t001 WHERE bukrs = zficrr-bukrs.
  ELSE.
    CLEAR zficrr-butxt.
  ENDIF.

  IF zficrr-cajo_number IS NOT INITIAL.
    SELECT SINGLE cajo_name INTO zficrr-cajo_name
      FROM tcj_cj_names
      WHERE comp_code   = zficrr-bukrs       AND
            cajo_number = zficrr-cajo_number AND
            langu       = 'EN'.
  ELSE.
    CLEAR: zficrr-cajo_name.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_status .

  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR '001'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FINAL_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_LV_SUBRC  text
*&---------------------------------------------------------------------*
FORM final_check  CHANGING lv_subrc TYPE char1.


  IF zficrr-iban IS NOT INITIAL.

    CALL FUNCTION 'CHECK_IBAN'
      EXPORTING
        i_iban    = zficrr-iban
      EXCEPTIONS
        not_valid = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      PERFORM msg USING 'E' 'شماره شبا اشتباه مي باشد' '' '' ''.
      lv_subrc = 1.
    ENDIF.


  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save .

  DATA: lv_it_header TYPE TABLE OF zfi_cash_receipt,
        lv_wa_header LIKE LINE OF  lv_it_header.

  CLEAR lv_wa_header.

  MOVE-CORRESPONDING zficrr TO lv_wa_header.
  IF zficrr-crrid IS INITIAL.
    PERFORM get_crrid_id   CHANGING zficrr-crrid.
    lv_wa_header-erdat   = sy-datum.
    lv_wa_header-erzet   = sy-uzeit.
    lv_wa_header-ernam   = sy-uname.
  ELSE.
    lv_wa_header-ldate   = sy-datum.
    lv_wa_header-ltime   = sy-uzeit.
    lv_wa_header-luser   = sy-uname.
  ENDIF.

  CHECK zficrr-crrid IS NOT INITIAL.
  lv_wa_header-crrid = zficrr-crrid.

  APPEND lv_wa_header TO lv_it_header.



  DELETE FROM zfi_cash_receipt WHERE crrid = lv_wa_header-crrid.
  MODIFY zfi_cash_receipt FROM TABLE lv_it_header.


  COMMIT WORK AND WAIT.

  PERFORM msg USING 'S' 'سند ذخيره شد:' lv_wa_header-crrid '' ''.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_crrid_ID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_ZMM_AD_crrid  text
*&---------------------------------------------------------------------*
FORM get_crrid_id  CHANGING lv_crrid TYPE zficrr-crrid.

  DATA: lv_number      TYPE          nrquan.

  CLEAR lv_number.
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCRRID'
    IMPORTING
      number                  = lv_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF lv_number IS NOT INITIAL.
    lv_crrid = lv_number+10(10).
  ELSE.
    PERFORM msg USING 'E' 'خطا در ايجاد نامبر رنج:' 'ZCRRID' '' ''.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PRINT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM print .

  DATA: lv_it_data         TYPE TABLE OF zficrr,
        lv_it_fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
        is_layout          TYPE          slis_layout_alv.




  CHECK zficrr-crrid IS NOT INITIAL.

  REFRESH lv_it_data.

  APPEND zficrr TO lv_it_data.


  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFICRR'
    CHANGING
      ct_fieldcat      = lv_it_fieldcatalog[].



  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = lv_it_fieldcatalog[]
      i_save             = 'A'
      is_layout          = is_layout
    TABLES
      t_outtab           = lv_it_data
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  F4_TRANSACT_NAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_transact_name INPUT.

  DATA: loc_sh           LIKE ddshdescr-shlpname,
        loc_shlp         TYPE shlp_descr_t,
        loc_call_control LIKE ddshf4ctrl,
        loc_flds_out_tab LIKE ddshretval OCCURS 1 WITH HEADER LINE.

  loc_sh = 'H_CJ_TRANSACTION'.

  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
    EXPORTING
      shlpname = loc_sh
    IMPORTING
      shlp     = loc_shlp
    EXCEPTIONS
      OTHERS   = 1.

  CALL FUNCTION 'F4_SEARCH_HELP'
    EXPORTING
      shlp                = loc_shlp
      call_control        = loc_call_control
    TABLES
      flds_out_tab        = loc_flds_out_tab
    EXCEPTIONS
      user_cancel         = 1
      no_data_found       = 2
      internal_error      = 3
      not_yet_implemented = 4
      OTHERS              = 5.

ENDMODULE.
