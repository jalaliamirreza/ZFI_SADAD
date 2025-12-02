*&---------------------------------------------------------------------*
*& Report ZSD_SALES_DAILY_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsd_sales_daily_report.



TABLES : vbak.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zsd_sales_daily_report,
      gv_wa_output LIKE LINE OF  gv_it_output,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME.
PARAMETERS: p_date TYPE datum OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS : s_vkorg    FOR  vbak-vkorg,
                 s_vbeln    FOR  vbak-vbeln,
                 s_date     FOR  vbak-erdat NO-DISPLAY,
                 s_date_m   FOR  vbak-erdat NO-DISPLAY.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM create_month_date.
  PERFORM get_sales_outb.
  PERFORM get_sales_fi_1.
  PERFORM get_sales_fi_2.
  PERFORM get_sales_fi_3.
  PERFORM merge_data.
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
      i_structure_name = 'ZSD_SALES_DAILY_REPORT'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'BRGEW1'.
        PERFORM set_catalog_text USING 'دستور تحويل'                       CHANGING fieldcatalog.
      WHEN 'BRGEW2'.
        PERFORM set_catalog_text USING 'حمل روز'                           CHANGING fieldcatalog.
      WHEN 'WRBTR1'.
        PERFORM set_catalog_text USING 'مبلغ وصولي روز'                    CHANGING fieldcatalog.
      WHEN 'WRBTR2'.
        PERFORM set_catalog_text USING 'جمع کل دريافتي از ابتداي ماه'             CHANGING fieldcatalog.
      WHEN 'WRBTR3'.
        PERFORM set_catalog_text USING 'جمع کل چک هاي قابل وصول تا پايان ماه'     CHANGING fieldcatalog.
      WHEN 'WRBTR4'.
        PERFORM set_catalog_text USING 'چک هاي واخواستي'                   CHANGING fieldcatalog.
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

  DATA: is_layout TYPE slis_layout_alv .

  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog[]
      i_save             = 'A'
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
*& Form CREATE_MONTH_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_month_date .


  DATA: lv_date_t(10),
        lv_date     TYPE datum.

  REFRESH: s_date,s_date_m.
  CLEAR  s_date.


  PERFORM convert_date_to_external USING p_date    CHANGING lv_date_t.
  lv_date_t+8(2) = '01'.
  PERFORM convert_date_to_internal USING lv_date_t CHANGING lv_date.

  s_date-low    = lv_date.

  lv_date = lv_date + 35.
  PERFORM convert_date_to_external USING lv_date   CHANGING lv_date_t.
  lv_date_t+8(2) = '01'.
  PERFORM convert_date_to_internal USING lv_date_t CHANGING lv_date.
  lv_date = lv_date - 1.

  s_date-high   = sy-datum.
  s_date-sign   = 'I'.
  s_date-option = 'BT'.

  APPEND s_date.

  MOVE-CORRESPONDING s_date TO s_date_m.
  s_date_m-low    = p_date + 1.
  s_date_m-high   = lv_date.
  APPEND s_date_m.

ENDFORM.


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


ENDFORM.

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
*&-------
*&---------------------------------------------------------------------*
*& Form GET_SALES_OUTB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_sales_outb .

  DATA: lv_qty TYPE menge_d.


  lv_qty = 1 .


  REFRESH gv_it_output.

  SELECT vbak~vbeln,
         vbak~auart,
         tvakt~bezei,
         vbak~vkorg,
         vbak~vtweg,
         vbak~spart,
         vbak~vkgrp,
         vbak~kvgr1,
         vbak~kvgr2,
         vbak~kvgr3,
         vbak~kvgr4,
         vbak~kvgr5,
         " Nahrevar - 1399/04/23
         " CASE WHEN lips~wbsta <> 'C' AND likp~bldat      =  @p_date THEN ( lips~brgew * @lv_qty ) END AS brgew1,

         CASE WHEN likp~bldat =  @p_date THEN ( lips~brgew * @lv_qty ) END AS brgew1,
         CASE WHEN lips~wbsta =  'C' AND likp~wadat_ist  =  @p_date THEN ( lips~brgew * @lv_qty ) END AS brgew2,

         'KG' AS meins_ton,
         'IRR' AS waers,
         @p_date AS report_date,
         @sy-datum AS current_date
  FROM   vbak
  JOIN   vbap ON vbak~vbeln = vbap~vbeln
  JOIN   vbfa ON vbap~vbeln = vbfa~vbelv AND
                 vbap~posnr = vbfa~posnv
  JOIN   lips ON vbfa~vbeln = lips~vbeln AND
                 vbfa~posnn = lips~posnr
  JOIN   likp ON vbfa~vbeln = likp~vbeln
  LEFT JOIN tvakt ON vbak~auart = tvakt~auart AND tvakt~spras = 'E'
  WHERE vbfa~vbtyp_v =  'C'      AND
        vbfa~vbtyp_n =  'J'      AND
        vbak~vbeln   IN @s_vbeln AND
        vbak~vkorg   IN @s_vkorg AND
        vbak~vbtyp   =  'C'       AND
        vbak~auart   IN ('ZO10','ZO11','ZO22','ZO23','ZO24','ZO25','ZO26','ZO27','ZO40','Z042','ZO41','ZO60') AND
        vbap~spart   IN ('04','05','06','07') AND
      ( likp~bldat      =  @p_date OR
        likp~wadat_ist  =  @p_date )
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_output.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SALES_FI_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_sales_fi_1 .


  SELECT vbak~vbeln,
         vbak~auart,
         tvakt~bezei,
         vbak~vkorg,
         vbak~vtweg,
         vbak~spart,
         vbak~vkgrp,
         vbak~kvgr1,
         vbak~kvgr2,
         vbak~kvgr3,
         vbak~kvgr4,
         vbak~kvgr5,
         'KG' AS meins_ton,
         'IRR' AS waers,
         @p_date AS report_date,
         @sy-datum AS current_date,
         SUM( dmbtr ) AS wrbtr1
  FROM   vbak
  JOIN   bseg ON vbak~vbeln = bseg~vbel2
  LEFT JOIN tvakt ON vbak~auart = tvakt~auart AND tvakt~spras = 'E'
  WHERE  vbak~vbeln   IN @s_vbeln  AND
         vbak~vkorg   IN @s_vkorg  AND
         vbak~vbtyp   =  'C'       AND
         vbak~auart   IN ('ZO10','ZO11','ZO22','ZO23','ZO24','ZO25','ZO26','ZO27','ZO40','Z042','ZO41','ZO60') AND
         bseg~koart   = 'D'        AND
      (
       ( bseg~h_budat =  @p_date AND
     ( ( bseg~umskz   = 'F' AND bseg~augbl = '' )                                                     OR
       ( bseg~umskz   = 'A' AND NOT EXISTS ( SELECT * FROM bsed WHERE bsed~bukrs = bseg~bukrs AND
                                                                      bsed~belnr = bseg~belnr AND
                                                                      bsed~gjahr = bseg~gjahr     ) ) ) ) OR
       ( bseg~umskz   = 'W' AND bseg~bschl = '09' AND bseg~zfbdt = @p_date )
      )

  GROUP BY vbak~vbeln,
           vbak~auart,
           tvakt~bezei,
           vbak~vkorg,
           vbak~vtweg,
           vbak~spart,
           vbak~vkgrp,
           vbak~kvgr1,
           vbak~kvgr2,
           vbak~kvgr3,
           vbak~kvgr4,
           vbak~kvgr5
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SALES_FI_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_sales_fi_2 .


  SELECT vbak~vbeln,
         vbak~auart,
         tvakt~bezei,
         vbak~vkorg,
         vbak~vtweg,
         vbak~spart,
         vbak~vkgrp,
         vbak~kvgr1,
         vbak~kvgr2,
         vbak~kvgr3,
         vbak~kvgr4,
         vbak~kvgr5,
         'KG' AS meins_ton,
         'IRR' AS waers,
         @p_date AS report_date,
         @sy-datum AS current_date,
         SUM( dmbtr ) AS wrbtr2
  FROM   vbak
  JOIN   bseg ON vbak~vbeln = bseg~vbel2
  LEFT JOIN tvakt ON vbak~auart = tvakt~auart AND tvakt~spras = 'E'
  WHERE  vbak~vbeln   IN @s_vbeln AND
         vbak~vkorg   IN @s_vkorg AND
         vbak~vbtyp   =  'C'       AND
         vbak~auart   IN ('ZO10','ZO11','ZO22','ZO23','ZO24','ZO25','ZO26','ZO27','ZO40','Z042','ZO41','ZO60') AND
         bseg~koart   = 'D'       AND
      (
       ( bseg~h_budat IN @s_date  AND
     ( ( bseg~umskz   = 'F' AND bseg~augbl = '' )                                                     OR
       ( bseg~umskz   = 'A' AND NOT EXISTS ( SELECT * FROM bsed WHERE bsed~bukrs = bseg~bukrs AND
                                                                      bsed~belnr = bseg~belnr AND
                                                                      bsed~gjahr = bseg~gjahr     ) ) ) ) OR
       ( bseg~umskz   = 'W' AND bseg~bschl = '09' AND bseg~zfbdt IN @s_date )
      )

  GROUP BY vbak~vbeln,
           vbak~auart,
           tvakt~bezei,
           vbak~vkorg,
           vbak~vtweg,
           vbak~spart,
           vbak~vkgrp,
           vbak~kvgr1,
           vbak~kvgr2,
           vbak~kvgr3,
           vbak~kvgr4,
           vbak~kvgr5
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_SALES_FI_3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_sales_fi_3 .



  SELECT vbak~vbeln,
         vbak~auart,
         tvakt~bezei,
         vbak~vkorg,
         vbak~vtweg,
         vbak~spart,
         vbak~vkgrp,
         vbak~kvgr1,
         vbak~kvgr2,
         vbak~kvgr3,
         vbak~kvgr4,
         vbak~kvgr5,
         'KG' AS meins_ton,
         'IRR' AS waers,
         @p_date AS report_date,
         @sy-datum AS current_date,
         SUM( dmbtr ) AS wrbtr3
  FROM   vbak
  JOIN   bseg ON vbak~vbeln = bseg~vbel2
  JOIN   bkpf ON bseg~gjahr = bkpf~gjahr AND
                 bseg~bukrs = bkpf~bukrs AND
                 bseg~belnr = bkpf~belnr
  LEFT JOIN tvakt ON vbak~auart = tvakt~auart AND tvakt~spras = 'E'
  WHERE  vbak~vbeln   IN @s_vbeln  AND
         vbak~vkorg   IN @s_vkorg  AND
         bseg~zfbdt   IN @s_date_m AND
         vbak~vbtyp   =  'C'       AND
         vbak~auart   IN ('ZO10','ZO11','ZO22','ZO23','ZO24','ZO25','ZO26','ZO27','ZO40','Z042','ZO41','ZO60') AND
         bseg~koart   = 'D'        AND
         bseg~umskz   = 'W' AND bseg~bschl = '09' AND
         NOT EXISTS ( SELECT * FROM zfi_check_status  WHERE check_bukrs = bseg~bukrs            AND
                                                            check_belnr = bseg~check_belnr      AND
                                                            check_gjahr = bseg~check_gjahr      AND
                                                            check_buzei = bseg~check_buzei      AND
                                                            check_status <> 'Cleared at Bank' )
  GROUP BY vbak~vbeln,
           vbak~auart,
           tvakt~bezei,
           vbak~vkorg,
           vbak~vtweg,
           vbak~spart,
           vbak~vkgrp,
           vbak~kvgr1,
           vbak~kvgr2,
           vbak~kvgr3,
           vbak~kvgr4,
           vbak~kvgr5
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MERGE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM merge_data .


  DATA: lv_it_output TYPE TABLE OF zsd_sales_daily_report,
        lv_wa_output LIKE LINE OF  lv_it_output.


  REFRESH lv_it_output.
  lv_it_output[] = gv_it_output[].
  REFRESH gv_it_output.

  LOOP AT lv_it_output INTO lv_wa_output.
    COLLECT lv_wa_output INTO gv_it_output.
  ENDLOOP.

ENDFORM.
