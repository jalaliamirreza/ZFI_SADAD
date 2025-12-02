*----------------------------------------------------------------------*
***INCLUDE ZMF05A_INCLUDE.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_GUARANTY_TYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
DATA lv_guaran_reason_txt TYPE c LENGTH 40.
DATA lv_guaran_type_txt   TYPE c LENGTH 40.
DATA lt_dyn_val           TYPE STANDARD TABLE OF dynpread.
DATA lw_dyn_val           TYPE dynpread.
MODULE f4_guaranty_type INPUT.

  DATA: t_set         TYPE TABLE OF rgsbv,
        wa_set        TYPE rgsbv,
        lt_return_tab TYPE TABLE OF ddshretval,
        lw_return_tab TYPE ddshretval.

  DATA: BEGIN OF wa_srch,
          val TYPE zguaranty_reason,
          txt TYPE stext,
        END OF wa_srch,
        it_srch LIKE STANDARD TABLE OF wa_srch.


  CLEAR t_set.

  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      class           = '0000'
*     LANGU           =
*     NO_AUTHORITY_CHECK        = ' '
      setnr           = 'GUARANTEE_TYPE'
*     SOURCE_CLIENT   =
*     TABLE           = ' '
*     NO_TITLES       = ' '
*     NO_SETID_CONVERSION       = 'X'
*   IMPORTING
*     SET_HEADER      =
    TABLES
*     FORMULA_LINES   =
      set_lines_basic = t_set
*     SET_LINES_DATA  =
*     SET_LINES_MULTI =
*     SET_LINES_SINGLE          =
    EXCEPTIONS
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR it_srch.

  LOOP AT t_set INTO wa_set.
    wa_srch-val = wa_set-from.
    wa_srch-txt = wa_set-title.
    APPEND wa_srch TO it_srch.
  ENDLOOP.

  CLEAR lt_return_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield   = 'VAL'
*     PVALKEY    = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD            = ' '
*     STEPL      = 0
*     WINDOW_TITLE           =
*     VALUE      = ' '
      value_org  = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY    = ' '
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB   =
*   IMPORTING
*     USER_RESET =
    TABLES
      value_tab  = it_srch
*     FIELD_TAB  =
      return_tab = lt_return_tab
*     DYNPFLD_MAPPING        =
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS     = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


  LOOP AT lt_return_tab INTO lw_return_tab.
    bseg-zguarantype = lw_return_tab-fieldval.

    LOOP AT it_srch INTO wa_srch WHERE val = lw_return_tab-fieldval.

      lv_guaran_type_txt = wa_srch-txt.

      CLEAR lt_dyn_val.

      lw_dyn_val-fieldname = 'LV_GUARAN_TYPE_TXT'.
      lw_dyn_val-fieldvalue = wa_srch-txt.
      APPEND lw_dyn_val TO lt_dyn_val.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = lt_dyn_val
*       EXCEPTIONS
*         INVALID_ABAPWORKAREA       = 1
*         INVALID_DYNPROFIELD        = 2
*         INVALID_DYNPRONAME         = 3
*         INVALID_DYNPRONUMMER       = 4
*         INVALID_REQUEST            = 5
*         NO_FIELDDESCRIPTION        = 6
*         UNDEFIND_ERROR             = 7
*         OTHERS     = 8
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDLOOP.

  ENDLOOP.




ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_GUARANTY_REASON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_guaranty_reason INPUT.

  CLEAR t_set.

  CALL FUNCTION 'G_SET_FETCH'
    EXPORTING
      class           = '0000'
*     LANGU           =
*     NO_AUTHORITY_CHECK        = ' '
      setnr           = 'GUARANTEE_REASON'
*     SOURCE_CLIENT   =
*     TABLE           = ' '
*     NO_TITLES       = ' '
*     NO_SETID_CONVERSION       = 'X'
*   IMPORTING
*     SET_HEADER      =
    TABLES
*     FORMULA_LINES   =
      set_lines_basic = t_set
*     SET_LINES_DATA  =
*     SET_LINES_MULTI =
*     SET_LINES_SINGLE          =
    EXCEPTIONS
      no_authority    = 1
      set_is_broken   = 2
      set_not_found   = 3
      OTHERS          = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CLEAR it_srch.

  LOOP AT t_set INTO wa_set.
    wa_srch-val = wa_set-from.
    wa_srch-txt = wa_set-title.
    APPEND wa_srch TO it_srch.
  ENDLOOP.

  CLEAR lt_return_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield   = 'VAL'
*     PVALKEY    = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD            = ' '
*     STEPL      = 0
*     WINDOW_TITLE           =
*     VALUE      = ' '
      value_org  = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY    = 'X'
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB   =
*   IMPORTING
*     USER_RESET =
    TABLES
      value_tab  = it_srch
*     FIELD_TAB  =
      return_tab = lt_return_tab
*     DYNPFLD_MAPPING        =
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS     = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_return_tab INTO lw_return_tab.
    bseg-zguaranreason = lw_return_tab-fieldval.
    LOOP AT it_srch INTO wa_srch WHERE val = lw_return_tab-fieldval.
      lv_guaran_reason_txt = wa_srch-txt.

      CLEAR lt_dyn_val.

      lw_dyn_val-fieldname = 'LV_GUARAN_REASON_TXT'.
      lw_dyn_val-fieldvalue = wa_srch-txt.
      APPEND lw_dyn_val TO lt_dyn_val.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = lt_dyn_val
*       EXCEPTIONS
*         INVALID_ABAPWORKAREA       = 1
*         INVALID_DYNPROFIELD        = 2
*         INVALID_DYNPRONAME         = 3
*         INVALID_DYNPRONUMMER       = 4
*         INVALID_REQUEST            = 5
*         NO_FIELDDESCRIPTION        = 6
*         UNDEFIND_ERROR             = 7
*         OTHERS     = 8
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module PBO_9109 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_9109 OUTPUT.
  SET PF-STATUS 'K005'.
* SET TITLEBAR 'xxx'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_REASON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_reason INPUT.
  DATA: it_pso13 TYPE STANDARD TABLE OF pso13.
  DATA: wa_pso12 TYPE pso13.

  CLEAR it_pso13.

  SELECT psoak psot2 FROM pso13 INTO CORRESPONDING FIELDS OF TABLE it_pso13 WHERE spras = 'E'.
  CLEAR t_set.

  CLEAR lt_return_tab.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield   = 'PSOAK'
*     PVALKEY    = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD            = ' '
*     STEPL      = 0
*     WINDOW_TITLE           =
*     VALUE      = ' '
      value_org  = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY    = 'X'
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB   =
*   IMPORTING
*     USER_RESET =
    TABLES
      value_tab  = it_pso13
*     FIELD_TAB  =
      return_tab = lt_return_tab
*     DYNPFLD_MAPPING        =
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS     = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_return_tab INTO lw_return_tab.
    bkpf-psoak = lw_return_tab-fieldval.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  F4_EBELN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_ebeln INPUT.
  DATA: fval TYPE dynfieldvalue.


  CALL FUNCTION 'FM_FYC_DYNPRO_VALUE_READ'
    EXPORTING
      i_repid      = sy-repid
      i_dynnr      = sy-dynnr
      i_fieldname  = 'BSEG-ZDOCCATEGORY'
    IMPORTING
      e_fieldvalue = fval.

  SET PARAMETER ID 'ZBSTYPE' FIELD fval.

  CLEAR lt_return_tab.


  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'BSEG'
      fieldname  = 'EBLEN'
      searchhelp = 'ZSRCH_EBELN'
*     SHLPPARAM  = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD               = ' '
*     STEPL      = 0
*     VALUE      = ' '
*     MULTIPLE_CHOICE           = ' '
*     DISPLAY    = ' '
*     SUPPRESS_RECORDLIST       = ' '
*     CALLBACK_PROGRAM          = ' '
*     CALLBACK_FORM             = ' '
*     CALLBACK_METHOD           =
*     SELECTION_SCREEN          = ' '
*   IMPORTING
*     USER_RESET =
    TABLES
      return_tab = lt_return_tab
*   EXCEPTIONS
*     FIELD_NOT_FOUND           = 1
*     NO_HELP_FOR_FIELD         = 2
*     INCONSISTENT_HELP         = 3
*     NO_VALUES_FOUND           = 4
*     OTHERS     = 5
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_return_tab INTO lw_return_tab.
    bseg-ebeln = lw_return_tab-fieldval.
  ENDLOOP.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module GET_DEF_VAL OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE get_def_val OUTPUT.
  DATA: cursorfield(20).
  IF sy-ucomm = ''.
    GET CURSOR FIELD cursorfield.
    IF cursorfield = 'BSEG-ZGUARANTYPE'.
      CLEAR t_set.

      CALL FUNCTION 'G_SET_FETCH'
        EXPORTING
          class           = '0000'
          setnr           = 'GUARANTEE_TYPE'
*         SOURCE_CLIENT   =
*         TABLE           = ' '
*         NO_TITLES       = ' '
*         NO_SETID_CONVERSION       = 'X'
*   IMPORTING
*         SET_HEADER      =
        TABLES
*         FORMULA_LINES   =
          set_lines_basic = t_set
*         SET_LINES_DATA  =
*         SET_LINES_MULTI =
*         SET_LINES_SINGLE          =
        EXCEPTIONS
          no_authority    = 1
          set_is_broken   = 2
          set_not_found   = 3
          OTHERS          = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CLEAR it_srch.

      LOOP AT t_set INTO wa_set WHERE from = bseg-zguarantype.
        CLEAR lt_dyn_val.

        lw_dyn_val-fieldname = 'LV_GUARAN_TYPE_TXT'.
        lw_dyn_val-fieldvalue = wa_set-title.
        APPEND lw_dyn_val TO lt_dyn_val.

      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.
        CLEAR lt_dyn_val.

        lw_dyn_val-fieldname = 'LV_GUARAN_TYPE_TXT'.
        lw_dyn_val-fieldvalue = ''.
        APPEND lw_dyn_val TO lt_dyn_val.

      ENDIF.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = lt_dyn_val
*       EXCEPTIONS
*         INVALID_ABAPWORKAREA       = 1
*         INVALID_DYNPROFIELD        = 2
*         INVALID_DYNPRONAME         = 3
*         INVALID_DYNPRONUMMER       = 4
*         INVALID_REQUEST            = 5
*         NO_FIELDDESCRIPTION        = 6
*         UNDEFIND_ERROR             = 7
*         OTHERS     = 8
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_VBEL2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_vbel2 INPUT.



  CALL FUNCTION 'FM_FYC_DYNPRO_VALUE_READ'
    EXPORTING
      i_repid      = sy-repid
      i_dynnr      = sy-dynnr
      i_fieldname  = 'BSEG-ZDOCCATEGORY'
    IMPORTING
      e_fieldvalue = fval.

  SET PARAMETER ID 'ZVTYPE' FIELD fval.

  CLEAR lt_return_tab.


  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'BSEG'
      fieldname  = 'VBEL2'
      searchhelp = 'ZSRCH_VBELN'
*     SHLPPARAM  = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD               = ' '
*     STEPL      = 0
*     VALUE      = ' '
*     MULTIPLE_CHOICE           = ' '
*     DISPLAY    = ' '
*     SUPPRESS_RECORDLIST       = ' '
*     CALLBACK_PROGRAM          = ' '
*     CALLBACK_FORM             = ' '
*     CALLBACK_METHOD           =
*     SELECTION_SCREEN          = ' '
*   IMPORTING
*     USER_RESET =
    TABLES
      return_tab = lt_return_tab
*   EXCEPTIONS
*     FIELD_NOT_FOUND           = 1
*     NO_HELP_FOR_FIELD         = 2
*     INCONSISTENT_HELP         = 3
*     NO_VALUES_FOUND           = 4
*     OTHERS     = 5
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_return_tab INTO lw_return_tab.
    bseg-vbel2 = lw_return_tab-fieldval.
  ENDLOOP.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BSEG-ZDOCCATEGORY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bseg-zdoccategory INPUT.
  DATA: BEGIN OF ls_domval,
          DOMVALUE_L TYPE DOMVALUE_L,
          ddtext TYPE ddtext,
        END OF ls_domval.

  DATA  it_domval TYPE STANDARD TABLE OF dd07v.
  DATA  wa_domval TYPE dd07v.
  DATA  lt_domval LIKE STANDARD TABLE OF ls_domval.

  CLEAR lt_return_tab.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'VBTYPL'
      TEXT      = 'X'
      LANGU     = sy-langu
*     BYPASS_BUFFER        = ' '
*   IMPORTING
*     RC        =
    TABLES
      dd07v_tab = it_domval
*   EXCEPTIONS
*     WRONG_TEXTFLAG       = 1
*     OTHERS    = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT it_domval INTO wa_domval.
    MOVE-CORRESPONDING wa_domval TO ls_domval.
    APPEND ls_domval TO lt_domval.
  ENDLOOP.



  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield   = 'DOMVALUE_L'
*     PVALKEY    = ' '
*     DYNPPROG   = ' '
*     DYNPNR     = ' '
*     DYNPROFIELD            = ' '
*     STEPL      = 0
*     WINDOW_TITLE           =
*     VALUE      = ' '
      value_org  = 'S'
*     MULTIPLE_CHOICE        = ' '
*     DISPLAY    = 'X'
*     CALLBACK_PROGRAM       = ' '
*     CALLBACK_FORM          = ' '
*     CALLBACK_METHOD        =
*     MARK_TAB   =
*   IMPORTING
*     USER_RESET =
    TABLES
      value_tab  = lt_domval
*     FIELD_TAB  =
      return_tab = lt_return_tab
*     DYNPFLD_MAPPING        =
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
*     OTHERS     = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  LOOP AT lt_return_tab INTO lw_return_tab.
    bseg-zdoccategory = lw_return_tab-fieldval.
  ENDLOOP.

ENDMODULE.
