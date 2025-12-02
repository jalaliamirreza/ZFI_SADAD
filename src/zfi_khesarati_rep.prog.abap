*&---------------------------------------------------------------------*
*& Report ZFI48
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFI_KHESARATI_REP.



TABLES : mara,vbrk,vbap,vbak,likp.

TYPE-POOLS:slis.


DATA: gv_it_output  TYPE TABLE OF ZFI48,
      gv_wa_output  LIKE LINE OF  gv_it_output,
      gv_it_billing TYPE TABLE OF zfi48,
      gv_wa_billing LIKE LINE OF  gv_it_billing,
      fieldcatalog  TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS :  s_bukrs1   FOR  vbrk-bukrs OBLIGATORY,
                  s_bukrs    FOR  vbrk-bukrs NO-DISPLAY,
                  s_buchk    FOR  vbrk-buchk,
                  s_kunrg    FOR  vbrk-kunrg,
                  s_kdgrp    FOR  vbrk-kdgrp,
                  s_fkart    FOR  vbrk-fkart,
                  s_spart    FOR  vbrk-spart,
                  s_vkgrp    FOR  vbak-vkgrp,
                  s_vkorg    FOR  vbrk-vkorg,
                  s_mvgr5    FOR  vbap-mvgr5,
                  s_regio    FOR  vbrk-regio,
                  s_vbel_s   FOR  vbak-vbeln,
                  s_vbel_b   FOR  vbrk-vbeln,
                  s_fkdat    FOR  vbrk-fkdat,
                  s_werks    FOR  vbap-werks,
                  s_matkl    FOR  mara-matkl,
                  s_rfbsk    FOR  vbrk-rfbsk,
                  s_auart    FOR  vbak-auart.
SELECTION-SCREEN: END   OF BLOCK blk1.





INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.

  PERFORM check_company_code_auth.
  PERFORM get_data.
  PERFORM check_auth.
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
      i_structure_name = 'zfi48'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.


    CASE fieldcatalog-fieldname(3).
      WHEN 'BU_'.
        PERFORM add_catalog_text USING 'Co.' CHANGING fieldcatalog.
      WHEN 'AG_'.
        PERFORM add_catalog_text USING 'SO.' CHANGING fieldcatalog.
      WHEN 'WE_'.
        PERFORM add_catalog_text USING 'SH.' CHANGING fieldcatalog.
      WHEN 'Z7_'.
        PERFORM add_catalog_text USING 'Z7.' CHANGING fieldcatalog.
    ENDCASE.

    CASE fieldcatalog-fieldname.
      WHEN 'DEGREE'.
        PERFORM set_catalog_text USING 'درجه' CHANGING fieldcatalog.

      WHEN 'BU_SORT2'.
        PERFORM set_catalog_text USING 'Co. National ID' CHANGING fieldcatalog.

      WHEN 'KUNRG'.
        PERFORM set_catalog_text USING 'Sold to Party' CHANGING fieldcatalog.

      WHEN 'WE_KUNRG'.
        PERFORM set_catalog_text USING 'Ship to Party' CHANGING fieldcatalog.

      WHEN 'IRANCODE'.
        PERFORM set_catalog_text USING 'ايران کد' CHANGING fieldcatalog.

      WHEN 'SHENASEH'.
        PERFORM set_catalog_text USING 'شناسه کالا' CHANGING fieldcatalog.

      WHEN 'EAN11'.
        PERFORM set_catalog_text USING 'بارکد' CHANGING fieldcatalog.

      WHEN 'EXTI2'.
        PERFORM set_catalog_text USING 'شماره بارنامه' CHANGING fieldcatalog.

      WHEN 'AG_IR0'.
        PERFORM set_catalog_text USING 'کد اقتصادي' CHANGING fieldcatalog.

      WHEN 'AG_IR1'.
        PERFORM set_catalog_text USING 'شماره ثبت' CHANGING fieldcatalog.

      WHEN 'AG_IR2'.
        PERFORM set_catalog_text USING 'شماره شناسنامه' CHANGING fieldcatalog.

      WHEN 'AG_IR3'.
        PERFORM set_catalog_text USING 'شناسه ملي' CHANGING fieldcatalog.

      WHEN 'AG_IR4'.
        PERFORM set_catalog_text USING 'کد ملي' CHANGING fieldcatalog.

      WHEN 'AG_IR5'.
        PERFORM set_catalog_text USING 'شماره پاسپورت' CHANGING fieldcatalog.

      WHEN 'AMOUNT1'.
        PERFORM set_catalog_text USING 'Amount' CHANGING fieldcatalog.

      WHEN 'AMOUNT2'.
        PERFORM set_catalog_text USING 'Condition Value' CHANGING fieldcatalog.

      WHEN 'AMOUNT3'.
        PERFORM set_catalog_text USING 'Cleared Downpayment' CHANGING fieldcatalog.

      WHEN 'AMOUNT4'.
        PERFORM set_catalog_text USING 'Tax' CHANGING fieldcatalog.

      WHEN 'AMOUNT5'.
        PERFORM set_catalog_text USING 'Discount' CHANGING fieldcatalog.

      WHEN 'AMOUNT6'.
        PERFORM set_catalog_text USING 'Delivery Cost' CHANGING fieldcatalog.

      WHEN 'AMOUNT7'.
        PERFORM set_catalog_text USING 'Packing Cost' CHANGING fieldcatalog.

      WHEN 'AMOUNT8'.
        PERFORM set_catalog_text USING 'Total Delivery Cost' CHANGING fieldcatalog.

      WHEN 'AMOUNT9'.
        PERFORM set_catalog_text USING 'Total Packing Cost' CHANGING fieldcatalog.

      WHEN 'AMOUNT10'.
        PERFORM set_catalog_text USING 'Total Net Amount' CHANGING fieldcatalog.

      WHEN 'AMOUNT11'.
        PERFORM set_catalog_text USING 'Total Cleared Downpayment' CHANGING fieldcatalog.

      WHEN 'AMOUNT12'.
        PERFORM set_catalog_text USING 'Payable Amount' CHANGING fieldcatalog.

      WHEN 'AMOUNT13'.
        PERFORM set_catalog_text USING 'Net Amount' CHANGING fieldcatalog.

      WHEN 'AMOUNT14'.
        PERFORM set_catalog_text USING 'Net Value VF05' CHANGING fieldcatalog.

      WHEN 'AMOUNT15'.
        PERFORM set_catalog_text USING 'Net' CHANGING fieldcatalog.

      WHEN 'AMOUNT16'.
        PERFORM set_catalog_text USING 'هزينه کارشناسي خسارت' CHANGING fieldcatalog.

      WHEN 'AMOUNT17'.
        PERFORM set_catalog_text USING 'هزينه خسارت فروش' CHANGING fieldcatalog.

      WHEN 'FROM_DATE'.
        PERFORM set_catalog_text USING 'BillingDate From' CHANGING fieldcatalog.

      WHEN 'TO_DATE'.
        PERFORM set_catalog_text USING 'BillingDate to' CHANGING fieldcatalog.

      WHEN 'CURRENT_DATE'.
        PERFORM set_catalog_text USING 'Current Date' CHANGING fieldcatalog.

      WHEN 'VBELN_Q'.
        PERFORM set_catalog_text USING 'Quotation' CHANGING fieldcatalog.

      WHEN 'AUDAT_Q'.
        PERFORM set_catalog_text USING 'Quotation Date' CHANGING fieldcatalog.

      WHEN 'KURRF_DAT'.
        PERFORM set_catalog_text USING 'Reference Date' CHANGING fieldcatalog.

     WHEN 'ZW01_PCT'.
       PERFORM set_catalog_text USING 'ZW01_PCT' CHANGING fieldcatalog.

     WHEN 'ZW01_AMOUNT'.
       PERFORM set_catalog_text USING 'ZW01_AMOUNT' CHANGING fieldcatalog.

     WHEN 'ZW10_PCT'.
       PERFORM set_catalog_text USING 'ZW10_PCT' CHANGING fieldcatalog.

     WHEN 'ZW10_AMOUNT'.
       PERFORM set_catalog_text USING 'ZW10_AMOUNT' CHANGING fieldcatalog.

     WHEN 'KOEIN'.
       PERFORM set_catalog_text USING 'Percentage' CHANGING fieldcatalog.

     WHEN 'ZP11'.
       PERFORM set_catalog_text USING 'قيمت محصول' CHANGING fieldcatalog.

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
*& Form CHECK_COMPANY_CODE_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_company_code_auth.

  DATA: lv_t001 TYPE t001.

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
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  REFRESH: gv_it_output,gv_it_billing.


  SELECT

    vbrk~bukrs,
    vbrk~vkorg,
    vbrk~vtweg,
    vbrk~spart,
    vbak~vbeln   AS vbeln_s,
    vbap~posnr   AS posnr_s,
    likp~vbeln   AS vbeln_d,
    lips~posnr   AS posnr_d,
    likp~vsart,
    likp~asn_num,
    likp~asn_dat,
    vbrk~vbeln   AS vbeln_b,
    vbrp~posnr   AS posnr_b,
    vbrk~sfakn   AS sfakn1,
    vbfa~vbeln   AS sfakn2,
    vbrp~werks,
    vbrk~kdgrp,
    vbrk~buchk,
    vbrk~fkart,
    tvfkt~vtext  AS fkart_t,
    vbrk~vbtyp,
    vbrk~fkdat,
    vbrk~cmwae,
    vbrk~zterm,
    vbrk~regio,
    t005u~bezei  AS regio_t,
    vbrk~kunrg,
    vbrp~matnr,
    vbrp~fkimg,
    vbrp~vrkme,
    vbrp~ntgew,
    vbrp~brgew,
    vbrp~gewei,
    vbrp~spart   AS spart_b_i,
    vbrp~batch_ntgew,
    vbrp~batch_gewei,
    vbrp~netwr   AS amount15,
    bkpf~xblnr,
    bkpf~belnr,
    lips~charg,
    mara~matkl,
    t023t~wgbez  AS matkl_t,
    mara~groes,
    vbak~erdat,
    vbak~kvgr1,
    vbak~kvgr2,
    vbak~kvgr3,
    vbak~kvgr4,
    vbak~kvgr5,
    tvv1t~bezei  AS kvgr1_t,
    vbap~mvgr5,
    tvm5t~bezei  AS mvgr5_t,
    adr_bu~butxt,
    adr_bu~stceg,
    adr_bu~country    AS bu_country,
    adr_bu~country_t  AS bu_country_t,
    adr_bu~region     AS bu_region,
    adr_bu~region_t   AS bu_region_t,
    adr_bu~city       AS bu_city,
    adr_bu~post_code  AS bu_post_code,
    adr_bu~street     AS bu_street,
    adr_bu~house_num  AS bu_house_num,
    adr_bu~tel        AS bu_tel,
    adr_bu~fax        AS bu_fax,
    adr_bu~sort2      AS bu_sort2,
    adr_ag~org        AS ag_org,
    adr_ag~name_last  AS ag_last,
    adr_ag~name_first AS ag_first,
    adr_ag~ktokd      AS ag_ktokd,
    adr_ag~ktokd_t    AS ag_ktokd_t,
    adr_ag~rpmkr      AS ag_rpmkr,
    adr_ag~country    AS ag_country,
    adr_ag~country_t  AS ag_country_t,
    adr_ag~region     AS ag_region,
    adr_ag~region_t   AS ag_region_t,
    adr_ag~city       AS ag_city,
    adr_ag~post_code  AS ag_post_code,
    adr_ag~street     AS ag_street,
    adr_ag~house_num  AS ag_house_num,
    ad2_ag~tel_number AS ag_tel,
    ad3_ag~fax_number AS ag_fax,
    ir0~taxnum        AS ag_ir0,
    ir1~taxnum        AS ag_ir1,
    ir2~taxnum        AS ag_ir2,
    ir3~taxnum        AS ag_ir3,
    ir4~taxnum        AS ag_ir4,
    ir5~taxnum        AS ag_ir5,
    vbak~auart,


    CASE WHEN vbrk~fkart = 'ZB01' THEN vbrk~kurrf_dat ELSE vbrk~fkdat END AS kurrf_dat,
    CASE WHEN substring( vbrp~matnr,13,1 ) = '1' AND ( vbrk~kdgrp =  'CM' OR  vbrk~kdgrp =  'CA' OR  vbrk~kdgrp =  'CT' ) THEN 'OE'
         WHEN substring( vbrp~matnr,13,1 ) = '1' AND ( vbrk~kdgrp <> 'CM' AND vbrk~kdgrp <> 'CA' AND vbrk~kdgrp <> 'CT' ) THEN 'درجه1'
         WHEN substring( vbrp~matnr,13,1 ) = '2'                                                                          THEN 'درجه2' END AS degree,

    CASE WHEN ( adr_ag~ktokd = 'C006' AND mara~mtart = 'ZFER' ) THEN concat( 'OE-',vbrp~arktx ) ELSE vbrp~arktx END AS arktx,

    prc1~waerk        AS waers,
    CASE WHEN vbrp~fkimg > 0 THEN division( prc1~kwert , vbrp~fkimg , 2 )  ELSE 0 END AS amount1,
    prc1~kwert        AS amount2,
    prc3~kwert        AS amount3,
    SUM( DISTINCT prc2~kwert ) AS amount4,
    SUM( DISTINCT prc4~kwert ) AS amount5,
    SUM( DISTINCT prc5~kwert ) AS amount6,
    SUM( DISTINCT prc6~kwert ) AS amount7,
    SUM( DISTINCT prc7~kwert ) AS amount16,
    SUM( DISTINCT prc8~kwert ) AS amount17,
    SUM( DISTINCT prc9~kbetr )       AS ZW01_PCT,
    SUM( DISTINCT prc9~KWERT )       AS ZW01_AMOUNT,
    SUM( DISTINCT prc10~kbetr )      AS ZW10_PCT,
    SUM( DISTINCT prc10~KWERT )      AS ZW10_AMOUNT,
    SUM( DISTINCT prc11~kwert )      AS zp11,

    @s_fkdat-low      AS from_date,
    @s_fkdat-high     AS to_date,
    @sy-datum         AS current_date


  FROM vbrk
  JOIN vbrp ON           vbrk~vbeln   = vbrp~vbeln
  JOIN vbap ON           vbap~vbeln   = vbrp~aubel AND
                         vbap~posnr   = vbrp~aupos
  JOIN vbak           ON vbap~vbeln   = vbak~vbeln
  JOIN mara           ON vbrp~matnr   = mara~matnr
  JOIN zvadrc AS adr_bu ON vbrk~bukrs   = adr_bu~bukrs
  LEFT JOIN lips      ON lips~vbeln   = vbrp~vgbel AND
                         lips~posnr   = vbrp~vgpos
  LEFT JOIN likp      ON lips~vbeln   = likp~vbeln
  LEFT JOIN bkpf      ON vbrk~vbeln   = bkpf~awkey
  LEFT JOIN tvfkt     ON vbrk~fkart   = tvfkt~fkart  AND
                         tvfkt~spras  = 'E'
  LEFT JOIN t005u     ON vbrk~regio   = t005u~bland  AND
                         vbrk~land1   = t005u~land1  AND
                         t005u~spras  = 'E'
  LEFT JOIN t023t     ON mara~matkl   = t023t~matkl  AND
                         t023t~spras  = 'E'
  LEFT JOIN tvv1t     ON vbak~kvgr1   = tvv1t~kvgr1  AND
                         tvv1t~spras  = 'E'
  LEFT JOIN tvm5t     ON vbap~mvgr5   = tvm5t~mvgr5  AND
                         tvm5t~spras  = 'E'
  LEFT JOIN zvadrc_bp    AS adr_ag ON vbak~vbeln        = adr_ag~vbeln      AND
                                      adr_ag~parvw      = 'AG'
  LEFT JOIN adr2         AS ad2_ag ON adr_ag~addrnumber = ad2_ag~addrnumber AND
                                      ad2_ag~home_flag  = 'X'               AND
                                      ad2_ag~flgdefault = 'X'
  LEFT JOIN adr3         AS ad3_ag ON adr_ag~addrnumber = ad3_ag~addrnumber AND
                                      ad3_ag~home_flag  = 'X'               AND
                                      ad3_ag~flgdefault = 'X'
  LEFT JOIN dfkkbptaxnum AS ir0    ON vbrk~kunrg        = ir0~partner       AND
                                      ir0~taxtype       = 'IR0'
  LEFT JOIN dfkkbptaxnum AS ir1    ON vbrk~kunrg        = ir1~partner       AND
                                      ir1~taxtype       = 'IR1'
  LEFT JOIN dfkkbptaxnum AS ir2    ON vbrk~kunrg        = ir2~partner       AND
                                      ir2~taxtype       = 'IR2'
  LEFT JOIN dfkkbptaxnum AS ir3    ON vbrk~kunrg        = ir3~partner       AND
                                      ir3~taxtype       = 'IR3'
  LEFT JOIN dfkkbptaxnum AS ir4    ON vbrk~kunrg        = ir4~partner       AND
                                      ir4~taxtype       = 'IR4'
  LEFT JOIN dfkkbptaxnum AS ir5    ON vbrk~kunrg        = ir5~partner       AND
                                      ir5~taxtype       = 'IR5'
  JOIN      prcd_elements     AS prc1   ON prc1~knumv = vbrk~knumv AND
                                           prc1~kposn = vbrp~posnr
  JOIN      prcd_elements     AS prc2   ON prc2~knumv = vbrk~knumv AND
                                           prc2~kposn = vbrp~posnr
  LEFT JOIN prcd_elements     AS prc3   ON prc3~knumv = vbrk~knumv AND
                                           prc3~kposn = vbrp~posnr AND
                                           prc3~kschl = 'AZWB'
  LEFT JOIN prcd_elements     AS prc4   ON prc4~knumv = vbrk~knumv AND
                                           prc4~kposn = vbrp~posnr AND
                                           prc4~kstat = ''         AND
                                           prc4~koaid = 'A'        AND
                                         ( prc4~kschl LIKE 'ZD%'   OR
                                           prc4~kschl =    'R100')
  LEFT JOIN prcd_elements     AS prc5   ON prc5~knumv = vbrk~knumv AND
                                           prc5~kposn = vbrp~posnr AND
                                           prc5~kstat = ''         AND
                                           prc5~kinak = ''         AND
                                           prc5~kschl LIKE 'ZH%'
  LEFT JOIN prcd_elements     AS prc6   ON prc6~knumv = vbrk~knumv AND
                                           prc6~kposn = vbrp~posnr AND
                                           prc6~kstat = ''         AND
                                           prc6~kinak = ''         AND
                                           prc6~kschl LIKE 'ZK%'
  LEFT JOIN prcd_elements     AS prc7   ON prc7~knumv = vbrk~knumv AND
                                           prc7~kposn = vbrp~posnr AND
                                           prc7~kinak = ''         AND
                                           prc7~kschl = 'ZP04'
  LEFT JOIN prcd_elements     AS prc8   ON prc8~knumv = vbrk~knumv AND
                                           prc8~kposn = vbrp~posnr AND
                                           prc8~kinak = ''         AND
                                           prc8~kstat = ''         AND
                                           prc8~kschl = 'ZP03'



  LEFT JOIN prcd_elements    AS prc9   ON  prc9~knumv = vbrk~knumv AND
                                           prc9~kposn = vbrp~posnr AND
                                          prc9~kschl LIKE 'ZW01'


 LEFT JOIN prcd_elements    AS prc10  ON  prc10~knumv = vbrk~knumv AND
                                          prc10~kposn = vbrp~posnr AND
                                          prc10~kschl = 'ZW10'

 LEFT JOIN prcd_elements    AS prc11  ON  prc11~knumv = vbrk~knumv AND
                                          prc11~kposn = vbrp~posnr AND
                                          prc11~kschl = 'ZP11'


 LEFT JOIN vbfa              ON vbfa~vbelv   = vbrk~vbeln AND
                                 vbfa~posnv   = vbrp~posnr AND
                             ( ( vbfa~vbtyp_n = 'N'        AND
                                 vbfa~vbtyp_v = 'M' )      OR
                               ( vbfa~vbtyp_n = 'S'        AND
                                 vbfa~vbtyp_v = 'O' ) )

  WHERE vbrk~fkdat     IN @s_fkdat   AND
        vbrk~kunrg     IN @s_kunrg   AND
        vbak~vbeln     IN @s_vbel_s  AND
        vbrk~vbeln     IN @s_vbel_b  AND
        vbrk~buchk     IN @s_buchk   AND
        vbrk~fkart     IN @s_fkart   AND
        vbrk~spart     IN @s_spart   AND
        vbrk~vkorg     IN @s_vkorg   AND
        vbrk~regio     IN @s_regio   AND
        vbak~vkgrp     IN @s_vkgrp   AND
        vbrk~kdgrp     IN @s_kdgrp   AND
        vbrp~werks     IN @s_werks   AND
        vbrk~bukrs     IN @s_bukrs   AND
        mara~matkl     IN @s_matkl   AND
        vbap~mvgr5     IN @s_mvgr5   AND
        vbrk~rfbsk     IN @s_rfbsk   AND
        vbak~auart     IN @s_auart   AND
        prc1~kstat = ''  AND   prc1~koaid = 'B' AND prc1~kinak = '' AND
        prc2~kstat = ''  AND ( prc2~kschl = 'ZVAT' OR prc2~kschl = 'ZVAD' )

  GROUP BY

    vbrk~bukrs,
    vbrk~vkorg,
    vbrk~vtweg,
    vbrk~spart,
    vbak~vbeln,
    vbap~posnr,
    likp~vbeln,
    lips~posnr,
    likp~vsart,
    likp~asn_num,
    likp~asn_dat,
    vbrk~vbeln,
    vbrp~posnr,
    vbrk~sfakn,
    vbfa~vbeln,
    vbrp~werks,
    vbrk~kdgrp,
    vbrk~buchk,
    vbrk~fkart,
    tvfkt~vtext,
    vbrk~vbtyp,
    vbrk~fkdat,
    vbrk~cmwae,
    vbrk~zterm,
    vbrk~regio,
    t005u~bezei,
    vbrk~kunrg,
    vbrp~matnr,
    vbrp~fkimg,
    vbrp~vrkme,
    vbrp~ntgew,
    vbrp~brgew,
    vbrp~gewei,
    vbrp~spart,
    vbrp~batch_ntgew,
    vbrp~batch_gewei,
    vbrp~netwr,
    bkpf~xblnr,
    bkpf~belnr,
    lips~charg,
    mara~matkl,
    t023t~wgbez,
    mara~groes,
    vbak~erdat,
    vbak~kvgr1,
    vbak~kvgr2,
    vbak~kvgr3,
    vbak~kvgr4,
    vbak~kvgr5,
    tvv1t~bezei,
    vbap~mvgr5,
    tvm5t~bezei,
    adr_bu~butxt,
    adr_bu~stceg,
    adr_bu~country,
    adr_bu~country_t,
    adr_bu~region,
    adr_bu~region_t,
    adr_bu~city,
    adr_bu~post_code,
    adr_bu~street,
    adr_bu~house_num,
    adr_bu~tel,
    adr_bu~fax,
    adr_bu~sort2,
    adr_ag~org,
    adr_ag~name_last,
    adr_ag~name_first,
    adr_ag~ktokd,
    adr_ag~ktokd_t,
    adr_ag~rpmkr,
    adr_ag~country,
    adr_ag~country_t,
    adr_ag~region,
    adr_ag~region_t,
    adr_ag~city,
    adr_ag~post_code,
    adr_ag~street,
    adr_ag~house_num,
    ad2_ag~tel_number,
    ad3_ag~fax_number,
    ir0~taxnum,
    ir1~taxnum,
    ir3~taxnum,
    ir2~taxnum,
    ir4~taxnum,
    ir5~taxnum,
    mara~mtart,
    vbrp~arktx,
    prc1~waerk,
    prc1~kbetr,
    prc1~kwert,
    prc3~kwert,
    vbrk~kurrf_dat,
    vbak~auart


  INTO CORRESPONDING FIELDS of table @gv_it_output.

  CHECK sy-subrc is INITIAL.

  SORT gv_it_output.
  DELETE ADJACENT DUPLICATES FROM gv_it_output COMPARING ALL FIELDS.

  SELECT
    vbrk~vbeln as vbeln_b,
    sum( DISTINCT prc1~kwert ) AS amount2,
    sum( DISTINCT prc3~kwert ) AS amount3,
    SUM( DISTINCT prc2~kwert ) AS amount4,
    SUM( DISTINCT prc4~kwert ) AS amount5,
    SUM( DISTINCT prc5~kwert ) AS amount6,
    SUM( DISTINCT prc6~kwert ) AS amount7
  from vbrk
  JOIN vbrp on           vbrk~vbeln   = vbrp~vbeln
  JOIN vbap ON           vbap~vbeln   = vbrp~aubel AND
                         vbap~posnr   = vbrp~aupos
  JOIN vbak           on vbap~vbeln   = vbak~vbeln
  JOIN mara           ON vbrp~matnr   = mara~matnr
  JOIN t001           ON vbrk~bukrs   = t001~bukrs
  JOIN      prcd_elements     AS prc1   ON prc1~knumv = vbrk~knumv AND
                                           prc1~kposn = vbrp~posnr
  JOIN      prcd_elements     AS prc2   ON prc2~knumv = vbrk~knumv AND
                                           prc2~kposn = vbrp~posnr
  LEFT JOIN prcd_elements     AS prc3   ON prc3~knumv = vbrk~knumv AND
                                           prc3~kposn = vbrp~posnr AND
                                           prc3~kschl = 'AZWB'
  LEFT JOIN prcd_elements     AS prc4   ON prc4~knumv = vbrk~knumv AND
                                           prc4~kposn = vbrp~posnr AND
                                           prc4~kstat = ''         AND
                                           prc4~koaid = 'A'        AND
                                         ( prc4~kschl LIKE 'ZD%'   OR
                                           prc4~kschl =    'R100')
  LEFT JOIN prcd_elements     AS prc5   ON prc5~knumv = vbrk~knumv AND
                                           prc5~kposn = vbrp~posnr AND
                                           prc5~kstat = ''         AND
                                           prc5~kinak = ''         AND
                                           prc5~kschl LIKE 'ZH%'
  LEFT JOIN prcd_elements     AS prc6   ON prc6~knumv = vbrk~knumv AND
                                           prc6~kposn = vbrp~posnr AND
                                           prc6~kstat = ''         AND
                                           prc6~kinak = ''         AND
                                           prc6~kschl LIKE 'ZK%'







  WHERE vbrk~fkdat     IN @s_fkdat   AND
        vbrk~kunrg     IN @s_kunrg   AND
        vbak~vbeln     IN @s_vbel_s  AND
        vbrk~vbeln     IN @s_vbel_b  AND
        vbrk~buchk     IN @s_buchk   AND
        vbrk~fkart     IN @s_fkart   AND
        vbrk~spart     IN @s_spart   AND
        vbrk~vkorg     IN @s_vkorg   AND
        vbrk~regio     IN @s_regio   AND
        vbak~vkgrp     IN @s_vkgrp   AND
        vbrk~kdgrp     IN @s_kdgrp   AND
        vbrp~werks     IN @s_werks   AND
        vbrk~bukrs     IN @s_bukrs   AND
        mara~matkl     IN @s_matkl   AND
        vbap~mvgr5     IN @s_mvgr5   AND
        vbak~auart     IN @s_auart   AND
        prc1~kstat = ''  AND   prc1~koaid = 'B' AND prc1~kinak = '' AND
        prc2~kstat = ''  AND ( prc2~kschl = 'ZVAT' OR prc2~kschl = 'ZVAD' )

  GROUP BY
    vbrk~vbeln
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_billing.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_CATALOG_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      <--P_FIELDCATALOG  text
*&---------------------------------------------------------------------*
FORM add_catalog_text  USING    lv_text            TYPE char100
                       CHANGING lv_wa_fieldcatalog TYPE slis_fieldcat_alv.

  CONCATENATE lv_text lv_wa_fieldcatalog-seltext_l    INTO lv_wa_fieldcatalog-seltext_l    SEPARATED BY space.
  CONCATENATE lv_text lv_wa_fieldcatalog-seltext_m    INTO lv_wa_fieldcatalog-seltext_m    SEPARATED BY space.
  CONCATENATE lv_text lv_wa_fieldcatalog-seltext_s    INTO lv_wa_fieldcatalog-seltext_s    SEPARATED BY space.
  CONCATENATE lv_text lv_wa_fieldcatalog-reptext_ddic INTO lv_wa_fieldcatalog-reptext_ddic SEPARATED BY space.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM calc_data .


  LOOP AT gv_it_output INTO gv_wa_output.

    CLEAR gv_wa_billing.
    READ TABLE gv_it_billing INTO gv_wa_billing WITH KEY vbeln_b = gv_wa_output-vbeln_b.

    gv_wa_output-amount8  = gv_wa_billing-amount6.
    gv_wa_output-amount9  = gv_wa_billing-amount7.
    gv_wa_output-amount10 = gv_wa_billing-amount2 +
                            gv_wa_billing-amount5.
    gv_wa_output-amount11 = gv_wa_billing-amount3.
    gv_wa_output-amount12 = gv_wa_output-amount8  +
                            gv_wa_output-amount9  +
                            gv_wa_output-amount10 -
                            gv_wa_output-amount11 +
                            gv_wa_billing-amount4.
    gv_wa_output-amount13 = gv_wa_output-amount2  +
                            gv_wa_output-amount5.

    IF gv_wa_output-fkart = 'ZT01' OR
       gv_wa_output-fkart = 'ZT00' OR
       gv_wa_output-fkart = 'ZB01' OR
       gv_wa_output-fkart = 'ZB00' OR
       gv_wa_output-fkart = 'YT00' OR
       gv_wa_output-fkart = 'YT01'.

      gv_wa_output-fkimg       = 0.
      gv_wa_output-batch_ntgew = 0.
      gv_wa_output-ntgew       = 0.
      gv_wa_output-brgew       = 0.

    ENDIF.


    IF ( gv_wa_output-vbtyp = 'O' AND gv_wa_output-fkart <> 'ZT01' AND gv_wa_output-fkart <> 'ZT00') OR
       ( gv_wa_output-vbtyp = 'N' AND gv_wa_output-fkart <> 'ZT01' AND gv_wa_output-fkart <> 'ZT00').

      gv_wa_output-fkimg       = gv_wa_output-fkimg       * -1.
      gv_wa_output-batch_ntgew = gv_wa_output-batch_ntgew * -1.
      gv_wa_output-ntgew       = gv_wa_output-ntgew       * -1.
      gv_wa_output-brgew       = gv_wa_output-brgew       * -1.

      gv_wa_output-amount1     = gv_wa_output-amount1     * -1.
      gv_wa_output-amount2     = gv_wa_output-amount2     * -1.
      gv_wa_output-amount3     = gv_wa_output-amount3     * -1.
      gv_wa_output-amount4     = gv_wa_output-amount4     * -1.
      gv_wa_output-amount5     = gv_wa_output-amount5     * -1.
      gv_wa_output-amount6     = gv_wa_output-amount6     * -1.
      gv_wa_output-amount7     = gv_wa_output-amount7     * -1.
      gv_wa_output-amount8     = gv_wa_output-amount8     * -1.
      gv_wa_output-amount9     = gv_wa_output-amount9     * -1.
      gv_wa_output-amount10    = gv_wa_output-amount10    * -1.
      gv_wa_output-amount11    = gv_wa_output-amount11    * -1.
      gv_wa_output-amount12    = gv_wa_output-amount12    * -1.
      gv_wa_output-amount13    = gv_wa_output-amount13    * -1.
      gv_wa_output-amount17    = gv_wa_output-amount17    * -1.
      gv_wa_output-ZP11        = gv_wa_output-ZP11        * -1.

    ELSEIF  gv_wa_output-fkart = 'ZT01' OR gv_wa_output-fkart = 'ZT00'.

      gv_wa_output-amount1     = gv_wa_output-amount1     * -1.
      gv_wa_output-amount2     = gv_wa_output-amount2     * -1.
      gv_wa_output-amount3     = gv_wa_output-amount3     * -1.
      gv_wa_output-amount4     = gv_wa_output-amount4     * -1.
      gv_wa_output-amount5     = gv_wa_output-amount5     * -1.
      gv_wa_output-amount6     = gv_wa_output-amount6     * -1.
      gv_wa_output-amount7     = gv_wa_output-amount7     * -1.
      gv_wa_output-amount8     = gv_wa_output-amount8     * -1.
      gv_wa_output-amount9     = gv_wa_output-amount9     * -1.
      gv_wa_output-amount10    = gv_wa_output-amount10    * -1.
      gv_wa_output-amount11    = gv_wa_output-amount11    * -1.
      gv_wa_output-amount12    = gv_wa_output-amount12    * -1.
      gv_wa_output-amount13    = gv_wa_output-amount13    * -1.
      gv_wa_output-amount17    = gv_wa_output-amount17    * -1.
      gv_wa_output-ZP11        = gv_wa_output-ZP11        * -1.

    ENDIF.

    gv_wa_output-amount14 = gv_wa_output-amount2  + gv_wa_output-amount5 + gv_wa_output-amount6.
    gv_wa_output-zw01_pct = gv_wa_output-zw01_pct / 100.
    gv_wa_output-zw10_pct = gv_wa_output-zw10_pct / 100.

    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_auth .

  DATA: lv_it_tmp TYPE TABLE OF zfi48.

  REFRESH lv_it_tmp.
  lv_it_tmp[] = gv_it_output[].

  LOOP AT gv_it_output INTO gv_wa_output GROUP BY gv_wa_output-spart.
    AUTHORITY-CHECK OBJECT  'V_VBAK_VKO'
      ID 'ACTVT' FIELD '03'
      ID 'VKORG' FIELD gv_wa_output-vkorg
      ID 'SPART' FIELD gv_wa_output-spart.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE i008(zfi) WITH gv_wa_output-spart.
      DELETE lv_it_tmp WHERE vkorg = gv_wa_output-vkorg AND spart = gv_wa_output-spart.
    ENDIF.
  ENDLOOP.

  LOOP AT gv_it_output INTO gv_wa_output GROUP BY gv_wa_output-kdgrp.
    AUTHORITY-CHECK OBJECT  'Z_CUST_GRP'
       ID 'ACTVT' FIELD '03'
       ID 'KDGRP' FIELD gv_wa_output-kdgrp.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE i009(zfi) WITH gv_wa_output-kdgrp.
      DELETE lv_it_tmp WHERE kdgrp = gv_wa_output-kdgrp.
    ENDIF.
  ENDLOOP.

  LOOP AT gv_it_output INTO gv_wa_output GROUP BY gv_wa_output-fkart.
    AUTHORITY-CHECK OBJECT  'V_VBRK_FKA'
      ID 'ACTVT' FIELD '03'
      ID 'FKART' FIELD gv_wa_output-fkart.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE i006(zfi) WITH gv_wa_output-fkart.
      DELETE lv_it_tmp WHERE fkart = gv_wa_output-fkart.
    ENDIF.
  ENDLOOP.

  REFRESH gv_it_output.
  gv_it_output[] = lv_it_tmp[].

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

  CLEAR s_rfbsk.

  s_rfbsk-SIGN   = 'I'.
  s_rfbsk-OPTION = 'NE'.
  s_rfbsk-LOW    = 'E'.

  append s_rfbsk.

ENDFORM.
