*&---------------------------------------------------------------------*
*& Report ZFI_BANK_STATEMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_bank_statement.



TABLES : bsis.

TYPE-POOLS:truxs.


CONSTANTS: icon_g TYPE char4 VALUE '@08@',
           icon_y TYPE char4 VALUE '@09@',
           icon_r TYPE char4 VALUE '@0A@'.

TYPES: BEGIN OF gt_excel_file,
         budat  TYPE  bsis-budat,
         bankl  TYPE  bnka-bankl,
         desc   TYPE  char40,
         text   TYPE  char40,
         name   TYPE  char40,
         code   TYPE  char40,
         check  TYPE  bsis-xref3,
         shkzg  TYPE  bsis-shkzg,
         amount TYPE  bsis-wrbtr,
         bal    TYPE  bsis-wrbtr,
       END OF gt_excel_file.



DATA: gv_it_gl    TYPE TABLE OF          zfi_bank_statement_gl,
      gv_wa_gl    LIKE LINE OF           gv_it_gl,
      gv_it_bank  TYPE TABLE OF          zfi_bank_statement_bank,
      gv_wa_bank  LIKE LINE OF           gv_it_bank,
      gv_it_excel TYPE STANDARD TABLE OF gt_excel_file,
      gv_wa_excel LIKE LINE OF           gv_it_excel,
      gv_it_raw   TYPE                   truxs_t_text_data,
      ok_code     LIKE                   sy-ucomm,
      gv_subrc(1).

DATA: gv_grid_gl   TYPE REF TO   cl_gui_alv_grid,
      gv_cc_gl     TYPE REF TO   cl_gui_custom_container,
      gv_grid_bank TYPE REF TO   cl_gui_alv_grid,
      gv_cc_bank   TYPE REF TO   cl_gui_custom_container.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : s_bukrs    FOR  bsis-bukrs OBLIGATORY NO-EXTENSION NO INTERVALS,
                 s_hkont    FOR  bsis-hkont OBLIGATORY NO-EXTENSION NO INTERVALS,
                 s_budat    FOR  bsis-budat,
                 s_gjahr    FOR  bsis-gjahr OBLIGATORY NO-EXTENSION NO INTERVALS,
                 s_monat    FOR  bsis-monat OBLIGATORY NO-EXTENSION NO INTERVALS.


SELECTION-SCREEN: END   OF BLOCK blk1.

SELECTION-SCREEN: BEGIN     OF BLOCK blk2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(70) TEXT-004.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END   OF BLOCK blk2.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

START-OF-SELECTION.

  PERFORM check_input_data CHANGING gv_subrc.
  CHECK gv_subrc IS INITIAL.
  PERFORM read_excel.
  PERFORM convert_excel_file.
  PERFORM get_last_period_data.
  PERFORM get_gl_data.
  PERFORM create_output.
  PERFORM update_table.
  PERFORM call_screen.






*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f4_file .


  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.


ENDFORM.
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
*& Form CALL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM call_screen .
  CALL SCREEN 100.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_GL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_gl_data .



  SELECT bsis~bukrs bsis~hkont bsis~zuonr bsis~gjahr bsis~belnr bsis~buzei
         bsis~budat bsis~waers bsis~xblnr bsis~blart bsis~bschl bsis~shkzg
         bsis~wrbtr bsis~dmbtr bsis~sgtxt bsis~valut bsis~xref3 bsis~monat
  APPENDING CORRESPONDING FIELDS OF TABLE gv_it_gl
  FROM bsis
  JOIN bkpf ON bsis~bukrs = bkpf~bukrs AND
               bsis~gjahr = bkpf~gjahr AND
               bsis~belnr = bkpf~belnr
  WHERE bsis~bukrs  IN s_bukrs AND
        bsis~hkont  IN s_hkont AND
        bsis~budat  IN s_budat AND
        bsis~gjahr  IN s_gjahr AND
        bsis~monat  IN s_monat AND
        bkpf~stblg  =  ''.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form CREATE_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_output .


  DATA: lv_counter TYPE pc44_counter,
        lv_begin   TYPE zfi_bank_statement_gl-bal_begin,
        lv_end     TYPE zfi_bank_statement_gl-bal_end.

  lv_counter = 1.

  PERFORM get_begin_balance USING s_bukrs-low s_gjahr-low s_monat-low s_hkont-low CHANGING lv_begin lv_end.

  LOOP AT gv_it_gl INTO gv_wa_gl.
    gv_wa_gl-counter   = lv_counter.
    gv_wa_gl-waers_irr = 'IRR'.

    READ TABLE gv_it_bank INTO gv_wa_bank WITH KEY check = gv_wa_gl-xref3.
    IF sy-subrc EQ 0.
      gv_wa_gl-ref_counter = gv_wa_bank-counter.
      IF gv_wa_bank-dmbtr = gv_wa_gl-dmbtr AND gv_wa_bank-shkzg = gv_wa_gl-shkzg.
        gv_wa_gl-icon        = icon_g.
      ELSE.
        gv_wa_gl-icon        = icon_y.
      ENDIF.
    ELSE.
      gv_wa_gl-icon          = icon_r.
    ENDIF.

    gv_wa_gl-bal_begin = lv_begin.
    gv_wa_gl-bal_end   = lv_end.

    MODIFY gv_it_gl FROM gv_wa_gl.
    lv_counter = lv_counter + 1.
  ENDLOOP.

  LOOP AT gv_it_bank INTO gv_wa_bank.
    READ TABLE gv_it_gl INTO gv_wa_gl WITH KEY xref3 = gv_wa_bank-check.
    IF sy-subrc EQ 0.
      gv_wa_bank-ref_counter = gv_wa_gl-counter.
      IF gv_wa_gl-dmbtr = gv_wa_bank-dmbtr AND gv_wa_bank-shkzg = gv_wa_gl-shkzg.
        gv_wa_bank-icon        = icon_g.
      ELSE.
        gv_wa_bank-icon        = icon_y.
      ENDIF.
    ELSE.
      gv_wa_bank-icon          = icon_r.
    ENDIF.

    IF gv_wa_bank-icon = icon_g.
      REFRESH gv_wa_bank-field_style.
      PERFORM editable_cell USING '' 'HKONT' CHANGING gv_wa_bank-field_style.
      PERFORM editable_cell USING '' 'SGTXT' CHANGING gv_wa_bank-field_style.
    ENDIF.

    MODIFY gv_it_bank FROM gv_wa_bank.
  ENDLOOP.




ENDFORM.
*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM set_status.
  PERFORM display_gl.
  PERFORM display_bank.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form SET_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_status .

  SET TITLEBAR  '01'.
  SET PF-STATUS 'STATUS'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_GL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_gl .


  DATA: lv_it_fieldcatalog TYPE lvc_t_fcat,
        lv_it_exclude      TYPE ui_functions,
        lv_layout          TYPE lvc_s_layo.


  IF gv_cc_gl IS INITIAL.


    CREATE OBJECT gv_cc_gl
      EXPORTING
        container_name = 'CC_GL'.

    CREATE OBJECT gv_grid_gl
      EXPORTING
        i_parent = gv_cc_gl.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_BANK_STATEMENT_GL'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.

    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog USING ''.

    lv_layout-zebra            = 'X'.
    lv_layout-cwidth_opt       = 'X'.

    CALL METHOD gv_grid_gl->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFI_BANK_STATEMENT_GL'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
      CHANGING
        it_outtab            = gv_it_gl[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_BANK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_bank .

  DATA: lv_it_fieldcatalog TYPE lvc_t_fcat,
        lv_it_exclude      TYPE ui_functions,
        lv_layout          TYPE lvc_s_layo.


  IF gv_cc_bank IS INITIAL.


    CREATE OBJECT gv_cc_bank
      EXPORTING
        container_name = 'CC_BANK'.

    CREATE OBJECT gv_grid_bank
      EXPORTING
        i_parent = gv_cc_bank.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_BANK_STATEMENT_BANK'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.

    PERFORM fill_exclude_alv    USING 'X' CHANGING  lv_it_exclude.
    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog USING 'X'.


    lv_layout-zebra            = 'X'.
    "lv_layout-cwidth_opt       = 'X'.
    lv_layout-stylefname       = 'FIELD_STYLE'.
    lv_layout-no_rowmark       = 'X'.

    CALL METHOD gv_grid_bank->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFI_BANK_STATEMENT_BANK'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
      CHANGING
        it_outtab            = gv_it_bank[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_FIELDCATALOG  text
*&---------------------------------------------------------------------*
FORM modify_fieldcatalog  TABLES lv_it_fieldcatalog TYPE lvc_t_fcat
                          USING  lv_edit            TYPE char1.


  DATA : lv_wa_fieldcatalog LIKE LINE OF lv_it_fieldcatalog.


  LOOP AT lv_it_fieldcatalog INTO lv_wa_fieldcatalog.
    CASE lv_wa_fieldcatalog-fieldname.
      WHEN 'ICON'.
        lv_wa_fieldcatalog-scrtext_l = 'Status'.
        lv_wa_fieldcatalog-scrtext_m = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-scrtext_s = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-reptext   = lv_wa_fieldcatalog-scrtext_l.

      WHEN 'REF_COUNTER'.
        lv_wa_fieldcatalog-scrtext_l = 'Ref. Counter'.
        lv_wa_fieldcatalog-scrtext_m = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-scrtext_s = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-reptext   = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-outputlen = 5.

      WHEN 'BAL_BEGIN'.
        lv_wa_fieldcatalog-scrtext_l = 'Begin'.
        lv_wa_fieldcatalog-scrtext_m = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-scrtext_s = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-reptext   = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-outputlen = 12.

      WHEN 'BAL_END'.
        lv_wa_fieldcatalog-scrtext_l = 'End'.
        lv_wa_fieldcatalog-scrtext_m = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-scrtext_s = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-reptext   = lv_wa_fieldcatalog-scrtext_l.
        lv_wa_fieldcatalog-outputlen = 12.

      WHEN 'COUNTER'.
        lv_wa_fieldcatalog-outputlen = 5.

      WHEN 'DMBTR' OR 'BANKL'.
        lv_wa_fieldcatalog-outputlen = 12.

      WHEN 'CHECK'.
        lv_wa_fieldcatalog-outputlen = 10.

      WHEN 'BUDAT'.
        lv_wa_fieldcatalog-outputlen = 8.

      WHEN 'HKONT'.
        lv_wa_fieldcatalog-edit      = lv_edit.
        lv_wa_fieldcatalog-outputlen = 10.

      WHEN 'SGTXT'.
        lv_wa_fieldcatalog-edit      = lv_edit.
        lv_wa_fieldcatalog-outputlen = 40.

      WHEN OTHERS.
    ENDCASE.


    MODIFY lv_it_fieldcatalog FROM lv_wa_fieldcatalog.
  ENDLOOP.


ENDFORM.

MODULE exit_commands INPUT.

  PERFORM exit_commands_form CHANGING ok_code .

ENDMODULE.                 " EXIT_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_commands INPUT.

  PERFORM exit_commands_form CHANGING ok_code .

ENDMODULE.


FORM exit_commands_form CHANGING ok_code.
  CASE ok_code.
    WHEN 'ENDE' OR 'ECAN' OR 'EXIT'.
      PERFORM exit.
    WHEN 'CREATE'.
      PERFORM post.
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
*&---------------------------------------------------------------------*
*& Form CONVERT_EXCEL_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM convert_excel_file .

  DATA: lv_counter TYPE pc44_counter,
        lv_line    TYPE i,
        lv_begin   TYPE bsis-wrbtr,
        lv_end     TYPE bsis-wrbtr.

  REFRESH gv_it_bank.


  CHECK gv_it_excel[] IS NOT INITIAL.
  DESCRIBE TABLE gv_it_excel LINES lv_line.

  READ TABLE gv_it_excel INTO gv_wa_excel INDEX 1.
  lv_begin = gv_wa_excel-bal.

  READ TABLE gv_it_excel INTO gv_wa_excel INDEX lv_line.
  lv_end   = gv_wa_excel-bal.


  lv_counter = 1.
  LOOP AT gv_it_excel INTO gv_wa_excel.

    CLEAR gv_wa_bank.
    MOVE-CORRESPONDING gv_wa_excel TO gv_wa_bank.
    gv_wa_bank-counter   = lv_counter.
    gv_wa_bank-waers_irr = 'IRR'.
    PERFORM convert_currency_to_internal USING gv_wa_bank-waers_irr gv_wa_excel-amount CHANGING gv_wa_bank-dmbtr.
    PERFORM convert_currency_to_internal USING gv_wa_bank-waers_irr lv_begin           CHANGING gv_wa_bank-bal_begin.
    PERFORM convert_currency_to_internal USING gv_wa_bank-waers_irr lv_end             CHANGING gv_wa_bank-bal_end.

    APPEND gv_wa_bank TO gv_it_bank.
    lv_counter = lv_counter + 1.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_CURRENCY_TO_INTERNAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_GV_WA_BANK_WAERS_IRR  text
*      -->P_GV_WA_EXCEL_AMOUNT  text
*      <--P_GV_WA_BANK_DMBTR  text
*&---------------------------------------------------------------------*
FORM convert_currency_to_internal  USING    lv_waers  TYPE bsis-waers
                                            lv_amount TYPE bsis-dmbtr
                                   CHANGING lv_dmbtr  TYPE bsis-dmbtr.

  DATA: lv_input  TYPE wmto_s-amount,
        lv_output TYPE wmto_s-amount.


  lv_input = lv_amount.

  CALL FUNCTION 'CURRENCY_AMOUNT_DISPLAY_TO_SAP'
    EXPORTING
      currency        = lv_waers
      amount_display  = lv_input
    IMPORTING
      amount_internal = lv_output
    EXCEPTIONS
      internal_error  = 1
      OTHERS          = 2.

  lv_dmbtr = lv_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_GV_SUBRC  text
*&---------------------------------------------------------------------*
FORM check_input_data  CHANGING lv_subrc TYPE char1.

  CLEAR lv_subrc.


  IF s_budat IS INITIAL AND s_gjahr IS INITIAL AND s_monat IS INITIAL.
    MESSAGE 'بازه زماني گزارش را وارد نماييد' TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BEGIN_BALANCE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_S_BUKRS_LOW  text
*      -->P_S_GJAHR_LOW  text
*      -->P_S_MONAT_LOW  text
*      -->P_S_HKONT_LOW  text
*      <--P_LV_BEGIN  text
*&---------------------------------------------------------------------*
FORM get_begin_balance  USING    lv_bukrs
                                 lv_gjahr
                                 lv_monat
                                 lv_hkont
                        CHANGING lv_begin TYPE zfi_bank_statement_gl-bal_begin
                                 lv_end   TYPE zfi_bank_statement_gl-bal_begin.

  DATA: companycode	 LIKE	         bapi1028_0-comp_code,
        glacct     	 LIKE	         bapi1028_0-gl_account,
        fiscalyear   LIKE          bapi1028_4-fisc_year,
        currencytype LIKE	         bapi1028_5-curr_type,
        period       LIKE          bapi1028_4-fis_period,
        lv_it_bal    TYPE TABLE OF bapi1028_4,
        lv_wa_bal    LIKE LINE OF  lv_it_bal,
        lv_tmp       TYPE          zfi_bank_statement_gl-bal_begin.


  CLEAR: lv_begin,lv_end.

  companycode  = lv_bukrs.
  glacct       = lv_hkont.
  fiscalyear   = lv_gjahr.
  period       = lv_monat.
  currencytype = '10'.

  CALL FUNCTION 'BAPI_GL_GETGLACCPERIODBALANCES'
    EXPORTING
      companycode      = companycode
      glacct           = glacct
      fiscalyear       = fiscalyear
      currencytype     = currencytype
    TABLES
      account_balances = lv_it_bal.


  READ TABLE lv_it_bal INTO lv_wa_bal WITH KEY fisc_year = fiscalyear fis_period = period.
  IF sy-subrc EQ 0.
    PERFORM convert_to_irr USING lv_wa_bal-balance CHANGING lv_end.
  ENDIF.

  IF period > 1.

    period = period - 1.
    READ TABLE lv_it_bal INTO lv_wa_bal WITH KEY fisc_year = fiscalyear fis_period = period.
    IF sy-subrc EQ 0.
      PERFORM convert_to_irr USING lv_wa_bal-balance CHANGING lv_begin.
    ENDIF.

  ELSE.
    period     = 16.
    fiscalyear = fiscalyear - 1.
    PERFORM get_begin_balance USING s_bukrs-low fiscalyear period s_hkont-low CHANGING lv_tmp lv_begin.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_TO_IRR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_WA_BAL_BALANCE  text
*      <--P_LV_END  text
*&---------------------------------------------------------------------*
FORM convert_to_irr  USING    lv_input  TYPE bapisaldo
                     CHANGING lv_output TYPE zfi_bank_statement_gl-bal_begin.


  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = 'IRR'
      amount_external      = lv_input
      max_number_of_digits = 23
    IMPORTING
      amount_internal      = lv_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_table .

  DATA: lv_it_data TYPE TABLE OF zfi_bs_gl,
        lv_wa_data LIKE LINE OF  lv_it_data.

  REFRESH lv_it_data.
  LOOP AT gv_it_gl INTO gv_wa_gl WHERE icon <> icon_g.
    CLEAR lv_wa_data.
    MOVE-CORRESPONDING gv_wa_gl TO lv_wa_data.
    APPEND lv_wa_data TO lv_it_data.
  ENDLOOP.

  DELETE FROM zfi_bs_gl WHERE gjahr = s_gjahr-low AND monat = s_monat-low AND bukrs = s_bukrs-low AND hkont = s_hkont-low.
  COMMIT WORK AND WAIT.
  CHECK lv_it_data[] IS NOT INITIAL.
  MODIFY zfi_bs_gl FROM TABLE lv_it_data.
  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LAST_PERIOD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_last_period_data .

  DATA: fiscalyear LIKE          bapi1028_4-fisc_year,
        period     LIKE          bapi1028_4-fis_period.


  IF s_monat-low > 1.
    period     = s_monat-low - 1.
    fiscalyear = s_gjahr-low.
  ELSE.
    period     = 12.
    fiscalyear = s_gjahr-low - 1.
  ENDIF.


  REFRESH gv_it_gl.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gv_it_gl
    FROM zfi_bs_gl
    WHERE bukrs  IN s_bukrs    AND
          hkont  IN s_hkont    AND
          budat  IN s_budat    AND
          gjahr  =  fiscalyear AND
          monat  =  period.

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
  APPEND lv_stylerow TO lv_style.

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

  DATA: lv_it_date   TYPE TABLE OF zfi_bank_statement_bank,
        lv_wa_date   LIKE LINE OF  lv_it_date,
        lv_it_curr   TYPE TABLE OF bapiaccr09,
        lv_it_gl     TYPE TABLE OF bapiacgl09,
        lv_it_return TYPE TABLE OF bapiret2,
        lv_index     TYPE          i.

  CALL METHOD gv_grid_bank->check_changed_data.

  REFRESH: lv_it_date,lv_it_return.
  lv_it_date[] = gv_it_bank[].
  DELETE lv_it_date WHERE icon  = icon_g.
  DELETE lv_it_date WHERE hkont = ''.
  SORT lv_it_date BY budat.
  DELETE ADJACENT DUPLICATES FROM lv_it_date COMPARING budat.


  LOOP AT lv_it_date INTO lv_wa_date.

    REFRESH: lv_it_curr,lv_it_gl.
    CLEAR lv_index.

    LOOP AT gv_it_bank INTO gv_wa_bank WHERE hkont <> '' AND icon <> icon_g AND budat = lv_wa_date-budat.
      PERFORM fi_add_item TABLES lv_it_curr lv_it_gl USING s_hkont-low gv_wa_bank-hkont gv_wa_bank-sgtxt gv_wa_bank-check gv_wa_bank-dmbtr gv_wa_bank-shkzg CHANGING lv_index.
    ENDLOOP.

    IF lv_it_gl[] IS NOT INITIAL.
      PERFORM post_fi_doc TABLES lv_it_curr lv_it_gl lv_it_return USING s_bukrs-low lv_wa_date-budat.
    ENDIF.

  ENDLOOP.


  IF lv_it_return[] IS NOT INITIAL.

    CALL FUNCTION 'ISH_BAPIRET2_DISPLAY'
      TABLES
        ss_bapiret2 = lv_it_return.

    READ TABLE lv_it_return TRANSPORTING NO FIELDS WITH KEY type = 'S'.
    IF sy-subrc EQ 0.
      PERFORM refresh.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FI_ADD_ITEM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_CURR  text
*      -->P_LV_IT_GL  text
*      -->P_S_HKONT_LOW  text
*      -->P_GV_WA_BANK_HKONT  text
*      -->P_GV_WA_BANK_SGTXT  text
*      -->P_GV_WA_BANK_DMBTR  text
*&---------------------------------------------------------------------*
FORM fi_add_item  TABLES   lv_it_curr STRUCTURE bapiaccr09
                           lv_it_gl   STRUCTURE bapiacgl09
                  USING    lv_hkont1  TYPE      zfi_bank_statement_bank-hkont
                           lv_hkont2  TYPE      zfi_bank_statement_bank-hkont
                           lv_sgtxt   TYPE      zfi_bank_statement_bank-sgtxt
                           lv_ref     TYPE      zfi_bank_statement_bank-check
                           lv_dmbtr   TYPE      zfi_bank_statement_bank-dmbtr
                           lv_shkzg   TYPE      zfi_bank_statement_bank-shkzg
                  CHANGING lv_index   TYPE      i.

  DATA: lv_wa_gl   LIKE LINE OF lv_it_gl,
        lv_wa_curr LIKE LINE OF lv_it_curr,
        lv_zarib   TYPE         menge_d.


  CASE lv_shkzg.
    WHEN 'H'.
      lv_zarib = - 100.
    WHEN 'S'.
      lv_zarib =   100.

  ENDCASE.


  lv_index = lv_index + 1.
  CLEAR lv_wa_gl.
  lv_wa_gl-itemno_acc     = lv_index.
  lv_wa_gl-gl_account     = lv_hkont1.
  lv_wa_gl-acct_type      = 'S'.
  lv_wa_gl-item_text      = lv_sgtxt.
  lv_wa_gl-ref_key_3      = lv_ref.
  APPEND lv_wa_gl TO lv_it_gl.

  CLEAR lv_wa_curr.
  lv_wa_curr-itemno_acc   = lv_index.
  lv_wa_curr-currency     = 'IRR'.
  lv_wa_curr-amt_doccur   = lv_dmbtr * lv_zarib.
  APPEND lv_wa_curr TO lv_it_curr.

  lv_index = lv_index + 1.
  CLEAR lv_wa_gl.
  lv_wa_gl-itemno_acc     = lv_index.
  lv_wa_gl-gl_account     = lv_hkont2.
  lv_wa_gl-acct_type      = 'S'.
  lv_wa_gl-item_text      = lv_sgtxt.
  lv_wa_gl-ref_key_3      = lv_ref.
  APPEND lv_wa_gl TO lv_it_gl.

  CLEAR lv_wa_curr.
  lv_wa_curr-itemno_acc   = lv_index.
  lv_wa_curr-currency     = 'IRR'.
  lv_wa_curr-amt_doccur   = lv_dmbtr * lv_zarib * -1.
  APPEND lv_wa_curr TO lv_it_curr.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form POST_FI_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_CURR  text
*      -->P_LV_IT_GL  text
*      -->P_LV_WA_DATE_BUDAT  text
*&---------------------------------------------------------------------*
FORM post_fi_doc  TABLES   lv_it_curr   STRUCTURE bapiaccr09
                           lv_it_gl     STRUCTURE bapiacgl09
                           lv_it_return STRUCTURE bapiret2
                  USING    lv_bukrs     TYPE      bkpf-bukrs
                           lv_date      TYPE      bkpf-budat.

  DATA: lv_header TYPE          bapiache09,
        lv_it_ret TYPE TABLE OF bapiret2,
        lv_wa_ret LIKE LINE OF  lv_it_ret,
        lv_objkey TYPE          bapiache02-obj_key.

  lv_header-comp_code   = lv_bukrs.
  lv_header-doc_type    = 'ZC'.
  lv_header-doc_date    = sy-datum.
  lv_header-pstng_date  = lv_date.
  lv_header-username    = sy-uname.

  REFRESH lv_it_ret.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = lv_header
    IMPORTING
      obj_key        = lv_objkey
    TABLES
      accountgl      = lv_it_gl
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

FORM fill_exclude_alv USING    no_edit        TYPE char1
                      CHANGING lv_it_exclude  TYPE ui_functions.

  DATA : lv_wa_exclude      TYPE ui_func.

  REFRESH lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_print .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_print_back .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_print_prev .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_refresh .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_save_variant .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_select_all .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_send .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_separator .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_sort .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_sort_asc .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_subtot .
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_detail.
  APPEND lv_wa_exclude TO lv_it_exclude.
  lv_wa_exclude = cl_gui_alv_grid=>mc_fc_views.
  APPEND lv_wa_exclude TO lv_it_exclude.

  IF no_edit = 'X'.
    lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND lv_wa_exclude TO lv_it_exclude.
    lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND lv_wa_exclude TO lv_it_exclude.
    lv_wa_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND lv_wa_exclude TO lv_it_exclude.
  ENDIF.






ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh .

  PERFORM convert_excel_file.
  PERFORM get_last_period_data.
  PERFORM get_gl_data.
  PERFORM create_output.
  PERFORM update_table.

  CALL METHOD gv_grid_gl->check_changed_data.
  CALL METHOD gv_grid_bank->check_changed_data.

  CALL METHOD gv_grid_gl->refresh_table_display.
  CALL METHOD gv_grid_bank->refresh_table_display.

ENDFORM.
