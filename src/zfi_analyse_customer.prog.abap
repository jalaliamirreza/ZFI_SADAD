*&---------------------------------------------------------------------*
*& Report ZFI_ANALYSE_CUSTOMER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_analyse_customer.




TABLES : vbak.

TYPE-POOLS:slis.


DATA: BEGIN OF gv_it_bseg OCCURS 0,
        vbeln TYPE vbak-vbeln,
        amout TYPE vbrk-netwr,
      END OF gv_it_bseg.

DATA: BEGIN OF gv_it_vbrk OCCURS 0,
        vbeln   TYPE vbak-vbeln,
        billing TYPE vbrk-vbeln,
        rfbsk   TYPE vbrk-rfbsk,
        posnr   TYPE vbap-posnr,
        amout   TYPE vbrk-netwr,
      END OF gv_it_vbrk.

DATA: BEGIN OF gv_it_lips OCCURS 0,
        vbeln TYPE vbak-vbeln,
        outb  TYPE likp-vbeln,
        menge TYPE vbfa-rfmng,
      END OF gv_it_lips.

DATA: BEGIN OF gv_it_vbap OCCURS 0,
        vbeln TYPE vbak-vbeln,
        menge TYPE vbap-kwmeng,
      END OF gv_it_vbap.



DATA: gv_it_output TYPE TABLE OF zfi_analyse_customer,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_wa_bseg   LIKE LINE OF  gv_it_bseg,
      gv_wa_vbrk   LIKE LINE OF  gv_it_vbrk,
      gv_wa_lips   LIKE LINE OF  gv_it_lips,
      gv_wa_vbap   LIKE LINE OF  gv_it_vbap,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS : s_bukrs    FOR  vbak-bukrs_vf           ,
                 s_kunnr    FOR  vbak-kunnr              ,
                 s_audat    FOR  vbak-audat NO-EXTENSION ,
                 s_auart    FOR  vbak-auart,
                 s_vkgrp    FOR  vbak-vkgrp,
                 s_vbeln    FOR  vbak-vbeln.

SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM check_company_code_auth.
  PERFORM get_data.
  PERFORM get_vbap.
  PERFORM get_bsid_bsad.
  PERFORM get_vbrk.
  PERFORM get_lips.
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
      i_structure_name = 'ZFI_ANALYSE_CUSTOMER'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'F1'.
        PERFORM set_catalog_text USING 'Txt' CHANGING fieldcatalog.
      WHEN 'AMOUNT1'.
        PERFORM set_catalog_text USING 'مبلغ نت فروش' CHANGING fieldcatalog.
        fieldcatalog-no_out = 'X'.
      WHEN 'AMOUNT2'.
        PERFORM set_catalog_text USING 'ماليات فروش' CHANGING fieldcatalog.
        fieldcatalog-no_out = 'X'.
      WHEN 'AMOUNT3'.
        PERFORM set_catalog_text USING 'مبلغ سفارش' CHANGING fieldcatalog.
      WHEN 'AMOUNT4'.
        PERFORM set_catalog_text USING 'واريزي' CHANGING fieldcatalog.
      WHEN 'AMOUNT5'.
        PERFORM set_catalog_text USING 'جمع فاکتورهاي صادره' CHANGING fieldcatalog.
      WHEN 'AMOUNT6'.
        PERFORM set_catalog_text USING 'جمع فاکتورهاي تاييد شده' CHANGING fieldcatalog.
      WHEN 'AMOUNT7'.
        PERFORM set_catalog_text USING 'مغايرت پيش فاکتور/فاکتور' CHANGING fieldcatalog.
      WHEN 'AMOUNT8'.
        PERFORM set_catalog_text USING 'مغایرت پیش فاکتور/ واریزی' CHANGING fieldcatalog.
      WHEN 'AMOUNT9'.
        PERFORM set_catalog_text USING 'مغایرت فاکتور و واریزی' CHANGING fieldcatalog.
      WHEN 'AMOUNT10'.
        PERFORM set_catalog_text USING 'مغایرت فاکتور و دفتر داری' CHANGING fieldcatalog.
      WHEN 'MENGE'.
        PERFORM set_catalog_text USING 'دستور تحویل حمل نشده (تعداد)' CHANGING fieldcatalog.
      WHEN 'AMOUNT11'.
        PERFORM set_catalog_text USING 'دستور تحویل حمل نشده' CHANGING fieldcatalog.
        fieldcatalog-no_out = 'X'.
      WHEN 'MEINS'.
        fieldcatalog-no_out = 'X'.
      WHEN 'MVGR5' OR 'MVGR5_TXT'.
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
FORM get_data .

  REFRESH gv_it_output.

  SELECT
         kna1~kunnr,
         kna1~name1,
         vbkd~kdgrp,
         t151t~ktext        AS kdgrp_txt,
         @s_audat-low       AS from_date,
         @s_audat-high      AS to_date,
         @sy-datum          AS report_date,
*         vbap~mvgr5,
*         tvm5t~bezei        AS mvgr5_txt,
         vbak~vbeln,
         vbak~auart,
         tvakt~bezei        AS auart_txt,
         vbak~audat,
         vbak~waerk,
         vbak~netwr         AS amount1,
         SUM( a~kwert )     AS amount2
  FROM      vbak
  JOIN      kna1  ON vbak~kunnr = kna1~kunnr
  JOIN      vbkd  ON vbak~vbeln = vbkd~vbeln  AND vbkd~posnr  = ''
  LEFT JOIN t151t ON vbkd~kdgrp = t151t~kdgrp AND t151t~spras = 'E'
  JOIN      vbap  ON vbak~vbeln = vbap~vbeln
*  LEFT JOIN tvm5t ON vbap~mvgr5 = tvm5t~mvgr5 AND tvm5t~spras = 'E'
  LEFT JOIN tvakt ON vbak~auart = tvakt~auart AND tvakt~spras = 'E'
  JOIN  prcd_elements AS a ON a~knumv = vbak~knumv AND a~kposn = vbap~posnr AND a~kstat = '' AND a~kinak = '' AND ( a~kschl = 'ZVAT' OR a~kschl = 'ZVAD' )
  WHERE vbak~bukrs_vf IN @s_bukrs AND
        vbak~kunnr    IN @s_kunnr AND
        vbak~audat    IN @s_audat AND
        vbak~auart    IN @s_auart AND
        vbak~vkgrp    IN @s_vkgrp AND
        vbak~vbeln    IN @s_vbeln
  GROUP BY kna1~kunnr,
           kna1~name1,
           vbkd~kdgrp,
           t151t~ktext,
*           vbap~mvgr5,
*           tvm5t~bezei,
           vbak~vbeln,
           vbak~auart,
           tvakt~bezei,
           vbak~audat,
           vbak~waerk,
           vbak~netwr
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_output.


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



  LOOP AT gv_it_output INTO gv_wa_output.

    gv_wa_output-amount3 = gv_wa_output-amount1 + gv_wa_output-amount2.

    LOOP AT gv_it_bseg INTO gv_wa_bseg WHERE vbeln = gv_wa_output-vbeln.
      gv_wa_output-amount4 = gv_wa_output-amount4 + gv_wa_bseg-amout.
    ENDLOOP.

    LOOP AT gv_it_vbrk INTO gv_wa_vbrk WHERE vbeln = gv_wa_output-vbeln.
      gv_wa_output-amount5 = gv_wa_output-amount5 + gv_wa_vbrk-amout.
      IF gv_wa_vbrk-rfbsk = 'C'.
        gv_wa_output-amount6 = gv_wa_output-amount6 + gv_wa_vbrk-amout.
      ENDIF.
    ENDLOOP.


    gv_wa_output-amount7  = gv_wa_output-amount3 - gv_wa_output-amount5.
    gv_wa_output-amount8  = gv_wa_output-amount3 - gv_wa_output-amount4.
    gv_wa_output-amount9  = gv_wa_output-amount5 - gv_wa_output-amount4.
    gv_wa_output-amount10 = gv_wa_output-amount5 - gv_wa_output-amount6.

    LOOP AT gv_it_vbap INTO gv_wa_vbap WHERE vbeln = gv_wa_output-vbeln.
      gv_wa_output-menge = gv_wa_vbap-menge.
    ENDLOOP.
    LOOP AT gv_it_lips INTO gv_wa_lips WHERE vbeln = gv_wa_output-vbeln.
      gv_wa_output-menge = gv_wa_output-menge - gv_wa_lips-menge.
    ENDLOOP.
    gv_wa_output-meins = 'M'.

    MODIFY gv_it_output FROM gv_wa_output.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BSID_BSAD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_bsid_bsad .

  REFRESH gv_it_bseg.

  CHECK gv_it_output[] IS NOT INITIAL.


  SELECT bsid~vbel2 AS vbeln , SUM( bsid~wrbtr ) AS amout
  FROM   vbak
  JOIN   bsid ON vbak~vbeln = bsid~vbel2
  JOIN   bkpf ON bsid~gjahr = bkpf~gjahr AND
                 bsid~belnr = bkpf~belnr AND
                 bsid~bukrs = bkpf~bukrs
 WHERE  vbak~bukrs_vf IN @s_bukrs AND
        vbak~kunnr    IN @s_kunnr AND
        vbak~audat    IN @s_audat AND
        vbak~auart    IN @s_auart AND
        vbak~vkgrp    IN @s_vkgrp AND
        vbak~vbeln    IN @s_vbeln AND
        bsid~umskz    = 'F'       AND
        bkpf~stblg    =  ''
  GROUP BY bsid~vbel2
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_bseg.

  SELECT bsad~vbel2 AS vbeln , SUM( bsad~wrbtr ) AS amout
  FROM   vbak
  JOIN   bsad ON vbak~vbeln = bsad~vbel2
  JOIN   bkpf ON bsad~gjahr = bkpf~gjahr AND
                 bsad~belnr = bkpf~belnr AND
                 bsad~bukrs = bkpf~bukrs
 WHERE  vbak~bukrs_vf IN @s_bukrs AND
        vbak~kunnr    IN @s_kunnr AND
        vbak~audat    IN @s_audat AND
        vbak~auart    IN @s_auart AND
        vbak~vkgrp    IN @s_vkgrp AND
        vbak~vbeln    IN @s_vbeln AND
        bsad~umskz    = 'F'       AND
        bkpf~stblg    =  ''
  GROUP BY bsad~vbel2
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_bseg.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_VBRK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_vbrk .



  REFRESH gv_it_vbrk.

  CHECK gv_it_output[] IS NOT INITIAL.


  SELECT DISTINCT vbak~vbeln ,vbrk~vbeln AS billing ,vbrk~rfbsk, vbrk~netwr AS amout
  FROM   vbak
  JOIN   vbfa ON vbak~vbeln = vbfa~vbelv
  JOIN   vbrk ON vbfa~vbeln = vbrk~vbeln
  WHERE  vbak~bukrs_vf IN @s_bukrs AND
         vbak~kunnr    IN @s_kunnr AND
         vbak~audat    IN @s_audat AND
         vbak~auart    IN @s_auart AND
         vbak~vkgrp    IN @s_vkgrp AND
         vbak~vbeln    IN @s_vbeln AND
         vbfa~vbtyp_n  =  'M'      AND
         vbfa~vbtyp_v  =  'C'      AND
         vbrk~FKSTO    <> 'X'      AND
         vbrk~rfbsk    <> 'E'
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_vbrk.


  SELECT DISTINCT vbak~vbeln, vbrk~vbeln AS vb ,vbrp~posnr , vbrk~rfbsk, SUM( DISTINCT a~kwert )  AS amout
  FROM   vbak
  JOIN   vbfa ON vbak~vbeln = vbfa~vbelv
  JOIN   vbrk ON vbfa~vbeln = vbrk~vbeln
  JOIN   vbrp ON vbrk~vbeln = vbrp~vbeln
  JOIN  prcd_elements AS a ON a~knumv = vbrk~knumv AND vbrp~posnr = a~kposn AND a~kstat = '' AND a~kinak = '' AND ( a~kschl = 'ZVAT' OR a~kschl = 'ZVAD' )
  WHERE  vbak~bukrs_vf IN @s_bukrs AND
         vbak~kunnr    IN @s_kunnr AND
         vbak~audat    IN @s_audat AND
         vbak~auart    IN @s_auart AND
         vbak~vkgrp    IN @s_vkgrp AND
         vbak~vbeln    IN @s_vbeln AND
         vbfa~vbtyp_n  =  'M'      AND
         vbfa~vbtyp_v  =  'C'      AND
         vbrk~FKSTO    <> 'X'      AND
         vbrk~rfbsk    <> 'E'
  GROUP BY vbak~vbeln,vbrk~vbeln,vbrp~posnr, vbrk~rfbsk
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_vbrk.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LIPS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_lips .

  REFRESH gv_it_lips.

  CHECK gv_it_output[] IS NOT INITIAL.


  SELECT DISTINCT vbak~vbeln ,likp~vbeln AS outb , vbfa~rfmng AS menge
  FROM   vbak
  JOIN   vbfa ON vbak~vbeln = vbfa~vbelv
  JOIN   likp ON vbfa~vbeln = likp~vbeln
  WHERE  vbak~bukrs_vf IN @s_bukrs AND
         vbak~kunnr    IN @s_kunnr AND
         vbak~audat    IN @s_audat AND
         vbak~auart    IN @s_auart AND
         vbak~vkgrp    IN @s_vkgrp AND
         vbak~vbeln    IN @s_vbeln AND
         vbfa~vbtyp_n  =  'J'      AND
         vbfa~vbtyp_v  =  'C'      AND
         likp~wbstk    =  'C'
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_lips.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_VBAP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_vbap .



  REFRESH gv_it_vbap.

  CHECK gv_it_output[] IS NOT INITIAL.


  SELECT vbak~vbeln ,SUM( vbap~kwmeng ) AS menge
  FROM   vbak
  JOIN   vbap ON vbak~vbeln = vbap~vbeln
  WHERE  vbak~bukrs_vf IN @s_bukrs AND
         vbak~kunnr    IN @s_kunnr AND
         vbak~audat    IN @s_audat AND
         vbak~auart    IN @s_auart AND
         vbak~vkgrp    IN @s_vkgrp AND
         vbak~vbeln    IN @s_vbeln
  GROUP BY vbak~vbeln
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_vbap.


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
