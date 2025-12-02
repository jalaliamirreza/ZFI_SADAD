*&---------------------------------------------------------------------*
*& Report ZFI_CUSTOMER_DEBT_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_customer_debt_report.




TABLES : bsad,but000,knvv.

TYPE-POOLS:slis.

TYPES: BEGIN OF ty_date,
         from TYPE datum,
         to   TYPE datum,
       END OF ty_date.

DATA: gv_it_output TYPE TABLE OF zfi_customer_debt_report,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_date   TYPE TABLE OF ty_date,
      gv_wa_date   LIKE LINE OF  gv_it_date,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME.
SELECT-OPTIONS : s_bukrs    FOR  bsad-bukrs OBLIGATORY DEFAULT '1000',
                 s_kunnr    FOR  bsad-kunnr,
                 s_bugrp    FOR  but000-bu_group DEFAULT 'C006' OBLIGATORY,
                 s_kdgrp    FOR  knvv-kdgrp,
                 s_budat    FOR  bsad-budat NO-EXTENSION OBLIGATORY.
PARAMETERS: p_month AS CHECKBOX.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.


  PERFORM check_company_code_auth.
  PERFORM calc_period.

  LOOP AT gv_it_date INTO gv_wa_date.
    PERFORM get_data_amount1_b.
    PERFORM get_data_amount2_b.
    PERFORM get_data_amount3_b.
    PERFORM get_data_amount1.
    PERFORM get_data_amount2.
    PERFORM get_data_amount3.
    PERFORM get_data_amount5.
  ENDLOOP.
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

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_CUSTOMER_DEBT_REPORT'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'AMOUNT0'.
        PERFORM set_catalog_text USING 'بدهي ابتداي دوره' CHANGING fieldcatalog.
      WHEN 'AMOUNT1'.
        PERFORM set_catalog_text USING 'فروش با احتساب ارزش افزوده' CHANGING fieldcatalog.
      WHEN 'AMOUNT2'.
        PERFORM set_catalog_text USING 'چک های برگشتی' CHANGING fieldcatalog.
      WHEN 'AMOUNT3'.
        PERFORM set_catalog_text USING 'چک های دریافتی' CHANGING fieldcatalog.
      WHEN 'AMOUNT4'.
        PERFORM set_catalog_text USING 'بدهی واقعی' CHANGING fieldcatalog.
      WHEN 'AMOUNT5'.
        PERFORM set_catalog_text USING 'چک های سررسید نشده' CHANGING fieldcatalog.
      WHEN 'AMOUNT6'.
        PERFORM set_catalog_text USING 'بدهی نهایی پس از اضافه کردن چک های سررسید نشده' CHANGING fieldcatalog.
      WHEN 'AMOUNT7'.
        PERFORM set_catalog_text USING 'سقف اعتبار مصوب' CHANGING fieldcatalog.
      WHEN 'AMOUNT9'.
        PERFORM set_catalog_text USING 'مازاد بدهی پس از کسر اعتبار مصوب' CHANGING fieldcatalog.
      WHEN 'AMOUNT8'.
        PERFORM set_catalog_text USING 'اعتبار تخصيص يافته' CHANGING fieldcatalog.
      WHEN 'DATE_FROM1'.
        PERFORM set_catalog_text USING 'Period From Date' CHANGING fieldcatalog.
      WHEN 'DATE_TO1'.
        PERFORM set_catalog_text USING 'Period to Date' CHANGING fieldcatalog.
      WHEN 'DATE_FROM2'.
        PERFORM set_catalog_text USING 'Report From Date' CHANGING fieldcatalog.
      WHEN 'DATE_TO2'.
        PERFORM set_catalog_text USING 'Report to Date' CHANGING fieldcatalog.
      WHEN 'DATE_REPORT'.
        PERFORM set_catalog_text USING 'Today' CHANGING fieldcatalog.

      WHEN OTHERS.
    ENDCASE.

    IF fieldcatalog-fieldname+7(1) = '_'.
      fieldcatalog-no_out = 'X'.
    ENDIF.


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
      t_outtab           = gv_it_output
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
FORM get_data_amount1 .


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount1_s,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      >= @gv_wa_date-from      AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~blart      IN ('RD','RC','RR','RV') AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount1_h,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      >= @gv_wa_date-from      AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~blart      IN ('RD','RC','RR','RV') AND
        bsid~shkzg      =  'H'                   AND
        bsid~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount1_s,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      >= @gv_wa_date-from      AND
        bsad~budat      <= @gv_wa_date-to        AND
        bsad~blart      IN ('RD','RC','RR','RV') AND
        bsad~shkzg      =  'S'                   AND
        bsad~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount1_h,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      >= @gv_wa_date-from      AND
        bsad~budat      <= @gv_wa_date-to        AND
        bsad~blart      IN ('RD','RC','RR','RV') AND
        bsad~shkzg      =  'H'                   AND
        bsad~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALC_PERIOD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM calc_period .

  DATA: lv_date       TYPE datum,
        lv_date_t(10).

  REFRESH gv_it_date.
  CLEAR gv_wa_date.
  IF p_month IS INITIAL.
    gv_wa_date-from = s_budat-low.
    gv_wa_date-to   = s_budat-high.
    APPEND gv_wa_date TO gv_it_date.
  ELSE.

    lv_date = s_budat-low.
    WHILE lv_date < s_budat-high.
      gv_wa_date-from = lv_date.
      PERFORM convert_date_to_external USING lv_date CHANGING lv_date_t.

      IF lv_date_t+5(2) = '12'.
        lv_date_t+5(2)  = '01'.
        lv_date_t(4)    = lv_date_t(4)   + 1.
      ELSE.
        lv_date_t+5(2)  = lv_date_t+5(2) + 1.
        IF strlen( lv_date_t+5(2) ) = 1.
          CONCATENATE '0' lv_date_t+5(2) INTO lv_date_t+5(2).
        ENDIF.
      ENDIF.
      lv_date_t+8(2) = '01'.

      PERFORM convert_date_to_internal USING lv_date_t CHANGING lv_date.
      lv_date = lv_date - 1.

      IF lv_date > s_budat-high.
        gv_wa_date-to   = s_budat-high.
      ELSE.
        gv_wa_date-to   = lv_date.
        lv_date         = lv_date + 1.
      ENDIF.

      APPEND gv_wa_date TO gv_it_date.
    ENDWHILE.

  ENDIF.



ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE_TO_INTERNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_DATE_FROM_T  text
*      <--P_LV_DATE_FROM  text
*----------------------------------------------------------------------*
FORM convert_date_to_internal  USING    lv_date_from_t TYPE char10
                               CHANGING lv_date_from   TYPE datum.

  CLEAR lv_date_from.

  TRY.

      CALL METHOD cl_abap_datfm=>conv_date_ext_to_int
        EXPORTING
          im_datext   = lv_date_from_t
          im_datfmdes = 'C'
        IMPORTING
          ex_datint   = lv_date_from.

    CATCH cx_root .

  ENDTRY.


ENDFORM.                    " CONVERT_DATE_TO_INTERNAL
*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATE_TO_EXTERNAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_DATE_FROM  text
*      <--P_LV_DATE_FROM_T  text
*----------------------------------------------------------------------*
FORM convert_date_to_external  USING    lv_date_from   TYPE datum
                               CHANGING lv_date_from_t TYPE char10.


  CLEAR lv_date_from_t.

  TRY.

      CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
        EXPORTING
          im_datint   = lv_date_from
          im_datfmdes = 'C'
        IMPORTING
          ex_datext   = lv_date_from_t.

    CATCH cx_root .

  ENDTRY.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM calc_data.

  DATA: lv_it_output TYPE TABLE OF zfi_customer_debt_report,
        lv_wa_output LIKE LINE OF  lv_it_output.

  REFRESH lv_it_output.
  lv_it_output[] = gv_it_output[].
  REFRESH gv_it_output.


  SORT lv_it_output BY kunnr.

  LOOP AT lv_it_output INTO lv_wa_output.

    lv_wa_output-amount1   = lv_wa_output-amount1_s   - lv_wa_output-amount1_h.
    lv_wa_output-amount1_b = lv_wa_output-amount1_s_b - lv_wa_output-amount1_h_b.
    lv_wa_output-amount2   = lv_wa_output-amount2_s   - lv_wa_output-amount2_h.
    lv_wa_output-amount2_b = lv_wa_output-amount2_s_b - lv_wa_output-amount2_h_b.
    CLEAR: lv_wa_output-amount1_s  ,lv_wa_output-amount1_h.
    CLEAR: lv_wa_output-amount1_s_b,lv_wa_output-amount1_h_b.
    CLEAR: lv_wa_output-amount2_s  ,lv_wa_output-amount2_h.
    CLEAR: lv_wa_output-amount2_s_b,lv_wa_output-amount2_h_b.

    lv_wa_output-amount0 = lv_wa_output-amount1_b - lv_wa_output-amount2_b + lv_wa_output-amount3_b.

    COLLECT lv_wa_output INTO gv_it_output.
  ENDLOOP.



  SORT gv_it_output BY bukrs kunnr date_from1 date_to1.
  DELETE ADJACENT DUPLICATES FROM gv_it_output COMPARING bukrs kunnr date_from1 date_to1.



  LOOP AT gv_it_output INTO gv_wa_output.
    SELECT SINGLE credit_limit INTO gv_wa_output-amount7 FROM ukmbp_cms_sgm WHERE partner = gv_wa_output-kunnr AND credit_sgmnt = '1000'.

    IF gv_wa_output-amount2 < 0.
      gv_wa_output-amount2 = gv_wa_output-amount2 * -1.
    ENDIF.

    gv_wa_output-amount4 = gv_wa_output-amount0 + gv_wa_output-amount1 + gv_wa_output-amount2 - gv_wa_output-amount3.
    gv_wa_output-amount6 = gv_wa_output-amount4 - gv_wa_output-amount5.
    gv_wa_output-amount8 = gv_wa_output-amount6.
    IF gv_wa_output-amount8 > gv_wa_output-amount7.
      gv_wa_output-amount8 = gv_wa_output-amount7.
    ENDIF.
    gv_wa_output-amount9 = gv_wa_output-amount6 - gv_wa_output-amount8.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_AMOUNT2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_amount2 .


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount2_s,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      >= @gv_wa_date-from      AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount2_h,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      >= @gv_wa_date-from      AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~shkzg      =  'H'                   AND
        bsid~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount2_s,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      >= @gv_wa_date-from      AND
        bsad~budat      <= @gv_wa_date-to        AND
        bsad~shkzg      =  'S'                   AND
        bsad~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount2_h,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      >= @gv_wa_date-from      AND
        bsad~budat      <= @gv_wa_date-to        AND
        bsad~shkzg      =  'H'                   AND
        bsad~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_AMOUNT3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_amount3 .

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount3,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsid~belnr     = bkpf~belnr AND
                 bsid~gjahr     = bkpf~gjahr AND
                 bsid~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      >= @gv_wa_date-from      AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  'W'                   AND
        bkpf~stblg      =  ''                    AND
        bkpf~blart      IN ('Z2','OO')
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount3,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsad~belnr     = bkpf~belnr AND
                 bsad~gjahr     = bkpf~gjahr AND
                 bsad~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      >= @gv_wa_date-from      AND
        bsad~budat      <= @gv_wa_date-to        AND
        bsad~shkzg      =  'S'                   AND
        bsad~umskz      =  'W'                   AND
        bkpf~stblg      =  ''                    AND
        bkpf~blart      IN ('Z2','OO')
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount3,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsid~belnr     = bkpf~belnr AND
                 bsid~gjahr     = bkpf~gjahr AND
                 bsid~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      >= @gv_wa_date-from      AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  'F'                   AND
        bsid~zlsch      =  'N'                   AND
        bkpf~stblg      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_AMOUNT5
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_amount5 .

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount5,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsid~belnr     = bkpf~belnr AND
                 bsid~gjahr     = bkpf~gjahr AND
                 bsid~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  'W'                   AND
        bkpf~stblg      =  ''                    AND
        bkpf~blart      IN ('Z2','OO')           AND
        bsid~zfbdt      >  @sy-datum
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount5,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsid~belnr     = bkpf~belnr AND
                 bsid~gjahr     = bkpf~gjahr AND
                 bsid~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      <= @gv_wa_date-to        AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  'F'                   AND
        bsid~zlsch      =  'N'                   AND
        bkpf~stblg      =  ''                    AND
        bsid~zfbdt      >  @sy-datum
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_AMOUNT0
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_amount1_b .



  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount1_s_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      <  @gv_wa_date-from      AND
        bsid~blart      IN ('RD','RC','RR','RV') AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount1_h_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      <  @gv_wa_date-from      AND
        bsid~blart      IN ('RD','RC','RR','RV') AND
        bsid~shkzg      =  'H'                   AND
        bsid~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount1_s_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      <  @gv_wa_date-from      AND
        bsad~blart      IN ('RD','RC','RR','RV') AND
        bsad~shkzg      =  'S'                   AND
        bsad~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount1_h_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      <  @gv_wa_date-from      AND
        bsad~blart      IN ('RD','RC','RR','RV') AND
        bsad~shkzg      =  'H'                   AND
        bsad~umskz      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_AMOUNT2_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_amount2_b .

  SELECT
     kna1~kunnr,
     kna1~name1,
     knvv~kdgrp,
     t151t~ktext AS kdgrp_txt,
     but000~bu_group,
     tb002~txt15 AS bu_group_txt,
     @gv_wa_date-from  AS date_from1,
     @gv_wa_date-to    AS date_to1,
     @s_budat-low      AS date_from2,
     @s_budat-high     AS date_to2,
     @sy-datum         AS date_report,
     bsid~bukrs,
     knvv~vkorg,
     knvv~vtweg,
     knvv~spart,
     SUM( bsid~dmbtr ) AS amount2_s_b,
     'IRR'             AS waers
   FROM kna1
   JOIN but000 ON but000~partner = kna1~kunnr
   JOIN bsid   ON bsid~kunnr     = kna1~kunnr
   JOIN tvko   ON tvko~bukrs     = bsid~bukrs
   JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                  knvv~vkorg     = tvko~vkorg
   LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
   LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
   WHERE kna1~kunnr      IN @s_kunnr              AND
         knvv~kdgrp      IN @s_kdgrp              AND
         but000~bu_group IN @s_bugrp              AND
         bsid~bukrs      IN @s_bukrs              AND
         bsid~budat      <  @gv_wa_date-from      AND
         bsid~shkzg      =  'S'                   AND
         bsid~umskz      =  'S'
   GROUP BY
     kna1~kunnr,
     kna1~name1,
     knvv~kdgrp,
     t151t~ktext,
     but000~bu_group,
     tb002~txt15,
     bsid~bukrs,
     knvv~vkorg,
     knvv~vtweg,
     knvv~spart
   APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount2_h_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      <  @gv_wa_date-from      AND
        bsid~shkzg      =  'H'                   AND
        bsid~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount2_s_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      <  @gv_wa_date-from      AND
        bsad~shkzg      =  'S'                   AND
        bsad~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount2_h_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      <  @gv_wa_date-from      AND
        bsad~shkzg      =  'H'                   AND
        bsad~umskz      =  'S'
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_AMOUNT3_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_amount3_b .


  SELECT
     kna1~kunnr,
     kna1~name1,
     knvv~kdgrp,
     t151t~ktext AS kdgrp_txt,
     but000~bu_group,
     tb002~txt15 AS bu_group_txt,
     @gv_wa_date-from  AS date_from1,
     @gv_wa_date-to    AS date_to1,
     @s_budat-low      AS date_from2,
     @s_budat-high     AS date_to2,
     @sy-datum         AS date_report,
     bsid~bukrs,
     knvv~vkorg,
     knvv~vtweg,
     knvv~spart,
     SUM( bsid~dmbtr ) AS amount3_b,
     'IRR'             AS waers
   FROM kna1
   JOIN but000 ON but000~partner = kna1~kunnr
   JOIN bsid   ON bsid~kunnr     = kna1~kunnr
   JOIN bkpf   ON bsid~belnr     = bkpf~belnr AND
                  bsid~gjahr     = bkpf~gjahr AND
                  bsid~bukrs     = bkpf~bukrs
   JOIN tvko   ON tvko~bukrs     = bsid~bukrs
   JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                  knvv~vkorg     = tvko~vkorg
   LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
   LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
   WHERE kna1~kunnr      IN @s_kunnr              AND
         knvv~kdgrp      IN @s_kdgrp              AND
         but000~bu_group IN @s_bugrp              AND
         bsid~bukrs      IN @s_bukrs              AND
         bsid~budat      <  @gv_wa_date-from      AND
         bsid~shkzg      =  'S'                   AND
         bsid~umskz      =  'W'                   AND
         bkpf~stblg      =  ''                    AND
         bkpf~blart      IN ('Z2','OO')
   GROUP BY
     kna1~kunnr,
     kna1~name1,
     knvv~kdgrp,
     t151t~ktext,
     but000~bu_group,
     tb002~txt15,
     bsid~bukrs,
     knvv~vkorg,
     knvv~vtweg,
     knvv~spart
   APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsad~dmbtr ) AS amount3_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsad   ON bsad~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsad~belnr     = bkpf~belnr AND
                 bsad~gjahr     = bkpf~gjahr AND
                 bsad~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsad~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsad~bukrs      IN @s_bukrs              AND
        bsad~budat      <  @gv_wa_date-from      AND
        bsad~shkzg      =  'S'                   AND
        bsad~umskz      =  'W'                   AND
        bkpf~stblg      =  ''                    AND
        bkpf~blart      IN ('Z2','OO')
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsad~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext AS kdgrp_txt,
    but000~bu_group,
    tb002~txt15 AS bu_group_txt,
    @gv_wa_date-from  AS date_from1,
    @gv_wa_date-to    AS date_to1,
    @s_budat-low      AS date_from2,
    @s_budat-high     AS date_to2,
    @sy-datum         AS date_report,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart,
    SUM( bsid~dmbtr ) AS amount3_b,
    'IRR'             AS waers
  FROM kna1
  JOIN but000 ON but000~partner = kna1~kunnr
  JOIN bsid   ON bsid~kunnr     = kna1~kunnr
  JOIN bkpf   ON bsid~belnr     = bkpf~belnr AND
                 bsid~gjahr     = bkpf~gjahr AND
                 bsid~bukrs     = bkpf~bukrs
  JOIN tvko   ON tvko~bukrs     = bsid~bukrs
  JOIN knvv   ON knvv~kunnr     = kna1~kunnr AND
                 knvv~vkorg     = tvko~vkorg
  LEFT JOIN tb002 ON but000~bu_group = tb002~bu_group AND tb002~spras = 'E'
  LEFT JOIN t151t ON knvv~kdgrp      = t151t~kdgrp    AND t151t~spras = 'E'
  WHERE kna1~kunnr      IN @s_kunnr              AND
        knvv~kdgrp      IN @s_kdgrp              AND
        but000~bu_group IN @s_bugrp              AND
        bsid~bukrs      IN @s_bukrs              AND
        bsid~budat      <  @gv_wa_date-from      AND
        bsid~shkzg      =  'S'                   AND
        bsid~umskz      =  'F'                   AND
        bsid~zlsch      =  'N'                   AND
        bkpf~stblg      =  ''
  GROUP BY
    kna1~kunnr,
    kna1~name1,
    knvv~kdgrp,
    t151t~ktext,
    but000~bu_group,
    tb002~txt15,
    bsid~bukrs,
    knvv~vkorg,
    knvv~vtweg,
    knvv~spart
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.

FORM check_company_code_auth.

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
