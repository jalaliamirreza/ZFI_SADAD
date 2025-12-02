*&---------------------------------------------------------------------*
*& Report ZFI_BRZ_BILLING_PRINTOUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_brz_sales_report.

TABLES:vbrk,vbak,vbrp,mara,bkpf,vbap.

TYPES: BEGIN OF t_output,
         bukrs                     TYPE bkpf-bukrs,
         name_org1                 TYPE but000-name_org1,
         name_first                TYPE but000-name_first,
         name_last                 TYPE but000-name_last,
         ecco_no                   TYPE dfkkbptaxnum-taxnum,
         register_no               TYPE dfkkbptaxnum-taxnum,
         birth_no                  TYPE dfkkbptaxnum-taxnum,
         national_id               TYPE dfkkbptaxnum-taxnum,
         nationalcode              TYPE dfkkbptaxnum-taxnum,
         passport_no               TYPE dfkkbptaxnum-taxnum,
         region                    TYPE adrc-region,
         city1                     TYPE adrc-city1,
         post_code1                TYPE adrc-post_code1,
         street                    TYPE adrc-street,
         house_num1                TYPE adrc-house_num1,
         tel_number                TYPE adrc-tel_number,
         butxt                     TYPE t001-butxt,
         stceg                     TYPE t001-stceg,
         partner_region            TYPE adrc-region,
         partner_bezei             TYPE t005u-bezei,
         partner_city1             TYPE adrc-city1,
         partner_post_code1        TYPE adrc-post_code1,
         partner_street            TYPE adrc-street,
         partner_house_num1        TYPE adrc-house_num1,
         partner_tel_number        TYPE adr2-tel_number,
         partner_fax_number        TYPE adr3-fax_number,
         fax_number                TYPE adrc-fax_number,
         payable_amount2           TYPE string,
         vbeln                     TYPE vbrk-vbeln,
         fkart                     TYPE vbrk-fkart,
         werks                     TYPE vbrp-werks,
         vtext                     TYPE tvfkt-vtext,
         fkdat                     TYPE vbrk-fkdat,
         ffkdat                    TYPE string,
         xblnr                     TYPE vbrk-xblnr,
         cmwae                     TYPE vbrk-cmwae,
         zterm                     TYPE vbrk-zterm,
         kunrg                     TYPE vbrk-kunrg,
         spart                     TYPE vbrk-spart,
         kdgrp                     TYPE vbrk-kdgrp,
         buchk                     TYPE vbrk-buchk,
         posnr                     TYPE vbrp-posnr,
         matnr                     TYPE vbrp-matnr,
         arktx                     TYPE vbrp-arktx,
         aubel                     TYPE vbrp-aubel,
         aupos                     TYPE vbrp-aupos,
         vgbel                     TYPE vbrp-vgbel,
         vgpos                     TYPE vbrp-vgpos,
         fkimg                     TYPE vbrp-fkimg,
         vrkme                     TYPE vbrp-vrkme,
         sparti                    TYPE vbrp-spart,
         charg                     TYPE lips-charg,
         prdha                     TYPE mara-prdha,
         regiontxt                 TYPE t005u-bezei,
         matkl                     TYPE mara-matkl,
         wgbez                     TYPE t023t-wgbez,
         groes                     TYPE mara-groes,
         vbtyp                     TYPE vbak-vbtyp,
         erdat                     TYPE vbak-erdat,
         vkorg                     TYPE vbrk-vkorg,
         vtweg                     TYPE vbrk-vtweg,
         mblnr                     TYPE mkpf-mblnr,
         kbetr                     TYPE prcd_elements-kbetr,
         kwert                     TYPE prcd_elements-kwert,
         discount                  TYPE prcd_elements-kwert,
         tax                       TYPE prcd_elements-kwert,
         cleared_downpayment       TYPE prcd_elements-kwert,
         delivery_cost             TYPE prcd_elements-kwert,
         packaging_cost            TYPE prcd_elements-kwert,
         netamount                 TYPE prcd_elements-kwert,
         total_cleared_downpayment TYPE prcd_elements-kwert,
         total_delivery_cost       TYPE prcd_elements-kwert,
         total_packaging_cost      TYPE prcd_elements-kwert,
         total_netamount           TYPE prcd_elements-kwert,
         payable_amount            TYPE prcd_elements-kwert,
         comp_nationalid           TYPE adrc-sort2,
         ausp1                     TYPE p LENGTH 6 DECIMALS 3,
         meins                     TYPE c LENGTH 3,
         ntgew                     TYPE vbrp-ntgew,
         brgew                     TYPE vbrp-brgew,
         irancode                  TYPE c LENGTH 40,
         shenaseh                  TYPE c LENGTH 40,
         belnr                     TYPE belnr_d,
         billingdate_from          TYPE begda,
         billingdate_to            TYPE begda,
         currentdate               TYPE begda,
         kvgr1                     TYPE vbak-kvgr1,
         bezei1                    TYPE tvv1t-bezei,
         kvgr2                     TYPE vbak-kvgr2,
         bezei2                    TYPE tvv1t-bezei,
         kvgr3                     TYPE vbak-kvgr3,
         bezei3                    TYPE tvv1t-bezei,
         kvgr4                     TYPE vbak-kvgr4,
         bezei4                    TYPE tvv1t-bezei,
         kvgr5                     TYPE vbak-kvgr5,
         bezei5                    TYPE tvv1t-bezei,
         kdkg1                     TYPE vbkd-kdkg1,
         kdkg2                     TYPE vbkd-kdkg2,
         kdkg3                     TYPE vbkd-kdkg3,
         kdkg4                     TYPE vbkd-kdkg4,
         kdkg5                     TYPE vbkd-kdkg5,
         mvgr1                     TYPE vbap-mvgr1,
         mvgr1t                    TYPE tvm1t-bezei,
         mvgr2                     TYPE vbap-mvgr2,
         mvgr2t                    TYPE tvm1t-bezei,
         mvgr3                     TYPE vbap-mvgr3,
         mvgr3t                    TYPE tvm1t-bezei,
         mvgr4                     TYPE vbap-mvgr4,
         mvgr4t                    TYPE tvm1t-bezei,
         mvgr5                     TYPE vbap-mvgr5,
         mvgr5t                    TYPE tvm1t-bezei,
         ktokd                     TYPE kna1-ktokd,
         ktokdt                    TYPE t077x-txt30,
         rpmkr                     TYPE kna1-rpmkr,
         t1                        TYPE dfkkbptaxnum-taxnum,
         t2                        TYPE dfkkbptaxnum-taxnum,
         t3                        TYPE dfkkbptaxnum-taxnum,
         t4                        TYPE dfkkbptaxnum-taxnum,
         t5                        TYPE dfkkbptaxnum-taxnum,
         t6                        TYPE dfkkbptaxnum-taxnum,
         ean11                     TYPE mara-ean11,
         tknum                     TYPE vttk-tknum,
         exti2                     TYPE vttk-exti2,
         degree                    TYPE char10,
         batch_ntgew               TYPE vbrp-batch_ntgew,
         batch_gewei               TYPE vbrp-batch_gewei,
         vbpa_kunnr                TYPE vbpa-kunnr,
         vbpa_post_code1           TYPE adrc-post_code1,
         netvf05                   TYPE prcd_elements-kwert,
         nettotal                  TYPE prcd_elements-kwert,
       END OF t_output.


DATA   ls_output    TYPE t_output.
DATA   ls_output1    TYPE t_output.
DATA   lt_output    TYPE TABLE OF t_output.
DATA   lt_output1    TYPE TABLE OF t_output.
DATA   lt_vbeln     TYPE TABLE OF t_output WITH KEY vbeln.
DATA   ct_fieldcat  TYPE slis_t_fieldcat_alv.
DATA   cs_fieldcat  TYPE slis_fieldcat_alv.
DATA   wa_fieldcat  TYPE LINE OF lvc_t_fcat.
DATA   it_fieldcat  TYPE lvc_t_fcat.
DATA   amounttsr    TYPE string.
DATA   currdec      TYPE bapi1090_1.
DATA   return       TYPE bapireturn.
DATA   lt_tvfk      TYPE STANDARD TABLE OF tvfk.
DATA   ls_tvfk      TYPE tvfk.
DATA   ls_error     TYPE string.
DATA   lt_object    TYPE STANDARD TABLE OF clobjdat.
DATA   ls_object    TYPE clobjdat.
DATA   lv_object    TYPE ausp-objek.
DATA   lv_space     TYPE c LENGTH 22 VALUE '                      '.
DATA   lt_class     TYPE STANDARD TABLE OF sclass.
DATA   lv_index     TYPE i.
DATA lv_mat     TYPE mara-matnr.

DATA: BEGIN OF ls_customer ,
        kunrg        TYPE vbrk-kunrg,
        ecco_no      TYPE dfkkbptaxnum-taxnum,
        register_no  TYPE dfkkbptaxnum-taxnum,
        birth_no     TYPE dfkkbptaxnum-taxnum,
        national_id  TYPE dfkkbptaxnum-taxnum,
        nationalcode TYPE dfkkbptaxnum-taxnum,
        passport_no  TYPE dfkkbptaxnum-taxnum,
        region       TYPE adrc-region,
        city1        TYPE  adrc-city1,
        post_code1   TYPE adrc-post_code1,
        street       TYPE adrc-street,
        house_num1   TYPE adrc-house_num1,
        tel_number   TYPE adr2-tel_number,
        fax_number   TYPE adr3-fax_number,
        name_org1    TYPE but000-name_org1,
        name_first   TYPE but000-name_first,
        name_last    TYPE but000-name_last,
        ktokd        TYPE kna1-ktokd,
        rpmkr        TYPE kna1-rpmkr,
      END OF ls_customer.

DATA: lt_customer LIKE STANDARD TABLE OF ls_customer WITH KEY kunrg,
      lv_t001     TYPE t001.
DATA: BEGIN OF ls_taxnum ,
        taxnum TYPE  dfkkbptaxnum-taxnum,
        taxtype TYPE  dfkkbptaxnum-taxtype,
      END OF ls_taxnum,
      lt_taxnum LIKE TABLE OF ls_taxnum WITH HEADER LINE.

SELECT-OPTIONS p_bukrs FOR bkpf-bukrs NO-DISPLAY.
SELECT-OPTIONS p_bukrs1 FOR bkpf-bukrs OBLIGATORY.
SELECT-OPTIONS p_buchk FOR vbrk-buchk .
SELECT-OPTIONS p_kunrg FOR vbrk-kunrg.
SELECT-OPTIONS p_kdgrp FOR vbrk-kdgrp.
SELECT-OPTIONS p_fkart FOR vbrk-fkart.
SELECT-OPTIONS p_spart FOR vbrk-spart.
SELECT-OPTIONS p_vkgrp FOR vbak-vkgrp.
SELECT-OPTIONS p_vkorg FOR vbrk-vkorg.
SELECT-OPTIONS s_mvgr5 FOR vbap-mvgr5.
SELECT-OPTIONS p_regio FOR vbrk-regio.
SELECT-OPTIONS p_vbeln FOR vbrk-vbeln.
SELECT-OPTIONS p_fkdat FOR vbrk-fkdat.
SELECT-OPTIONS: p_werks FOR vbrp-werks,
                s_matkl FOR mara-matkl.
PARAMETERS p_lang TYPE adrc-langu OBLIGATORY DEFAULT 'EN'.


START-OF-SELECTION.



  SELECT * INTO lv_t001 FROM t001 WHERE bukrs IN p_bukrs1.
    AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
        ID 'ACTVT' FIELD '03'
        ID 'BUKRS' FIELD lv_t001-bukrs.
    IF sy-subrc EQ 0.
      CLEAR p_bukrs.
      p_bukrs-low    = lv_t001-bukrs.
      p_bukrs-sign   = 'I'.
      p_bukrs-option = 'EQ'.
      APPEND p_bukrs.
    ENDIF.
  ENDSELECT.

  IF p_bukrs[] IS INITIAL.
    MESSAGE e002(zfi).
    EXIT.
  ENDIF.





  SELECT DISTINCT
     vbrp~ntgew , vbrp~brgew ,vbrk~buchk, vbrk~vbeln , vbrk~fkart,
     vbrp~werks , tvfkt~vtext , vbrk~fkdat , bkpf~xblnr , bkpf~belnr ,
     vbrk~cmwae , vbrk~zterm , vbrk~kunrg , vbrk~spart , vbrk~kdgrp  ,
     vbrp~posnr , vbrp~matnr , vbrp~vrkme , vbrp~arktx ,
     vbrp~aubel , vbrp~aupos , vbrp~vgbel , vbrp~vgpos , vbrp~fkimg ,
     vbrp~spart AS sparti , a~kbetr , a~kwert , g~charg , t005u~bezei AS
     regiontxt, mara~matkl ,t023t~wgbez, mara~groes , vbrk~vbtyp,
     vbrk~fkart , vbrk~vkorg , vbrk~vtweg, vbak~erdat,
    vbak~kvgr1,vbak~kvgr2,vbak~kvgr3,vbak~kvgr4,vbak~kvgr5,
    vbrp~batch_ntgew,vbrp~batch_gewei,
    tvv1t1~bezei AS bezei1,
    tvv2t2~bezei AS bezei2,
    tvv3t3~bezei AS bezei3,
    tvv4t4~bezei AS bezei4,
    tvv5t5~bezei AS bezei5,
    vbap~mvgr1,vbap~mvgr2,vbap~mvgr3,vbap~mvgr4,vbap~mvgr5,
    tvm1t~bezei AS mvgr1t,
    tvm2t~bezei AS mvgr2t,
    tvm3t~bezei AS mvgr3t,
    tvm4t~bezei AS mvgr4t,
    tvm5t~bezei AS mvgr5t,
    vbpa~kunnr  AS vbpa_kunnr,
    vbrk~bukrs,
mara~ean11,
     CASE WHEN substring( vbrp~matnr,13,1 ) = '1' AND ( vbrk~kdgrp =
     'CM' OR vbrk~kdgrp = 'CA' OR vbrk~kdgrp = 'CT' ) THEN 'OE'
         WHEN substring( vbrp~matnr,13,1 ) = '1' AND vbrk~kdgrp <> 'CM'
         AND vbrk~kdgrp <> 'CA' AND vbrk~kdgrp <> 'CT' THEN 'درجه1'
         WHEN substring( vbrp~matnr,13,1 ) = '2' THEN 'درجه2' END AS
         degree,

     SUM( DISTINCT b~kwert ) AS discount ,
SUM( DISTINCT c~kwert ) AS tax , d~kwert AS cleared_downpayment , SUM(
DISTINCT e~kwert ) AS delivery_cost , SUM( DISTINCT f~kwert ) AS
packaging_cost
, adrc~region AS partner_region,
     adrc~city1 AS partner_city1,
     adrc~post_code1 AS partner_post_code1,
     adrc~street AS partner_street,
     adrc~house_num1 AS partner_house_num1,
     adr3~fax_number AS partner_tel_number,
     adr2~tel_number AS partner_fax_number
      FROM vbrk INNER JOIN tvfkt  ON tvfkt~fkart = vbrk~fkart
              INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
              INNER JOIN  prcd_elements AS a ON a~knumv = vbrk~knumv AND
              a~kposn = vbrp~posnr
              LEFT OUTER  JOIN prcd_elements AS b ON b~knumv =
              vbrk~knumv AND  b~kposn = vbrp~posnr AND b~koaid = 'A' AND
              b~kstat = '' AND ( b~kschl LIKE 'ZD%' OR  b~kschl = 'R100'
              )
              INNER JOIN  prcd_elements AS c ON c~knumv = vbrk~knumv AND
              c~kposn = vbrp~posnr
              LEFT OUTER JOIN prcd_elements AS d ON d~knumv = vbrk~knumv
              AND  d~kposn = vbrp~posnr AND  d~kschl = 'AZWB'
              LEFT OUTER JOIN prcd_elements AS e ON e~knumv = vbrk~knumv
              AND  e~kposn = vbrp~posnr AND e~kstat = '' AND e~kschl
              LIKE 'ZH%' AND e~kinak = ''
              LEFT OUTER JOIN prcd_elements AS f ON f~knumv = vbrk~knumv
              AND  f~kposn = vbrp~posnr AND f~kstat = '' AND f~kschl
              LIKE 'ZK%' AND f~kinak = ''
              LEFT OUTER JOIN lips AS g ON g~vbeln = vbrp~vgbel AND
              g~posnr = vbrp~vgpos
              INNER JOIN mara ON mara~matnr = vbrp~matnr
              INNER JOIN t005u ON t005u~bland = vbrk~regio AND
              t005u~land1 = vbrk~land1 AND t005u~spras = 'E'
              INNER JOIN vbak ON vbak~vbeln = vbrp~aubel
              INNER JOIN vbap ON vbap~vbeln = vbrp~aubel AND vbap~posnr
              = vbrp~aupos
              LEFT OUTER JOIN bkpf ON bkpf~awkey = vbrk~vbeln
LEFT JOIN tvv1t AS tvv1t1 ON vbak~kvgr1 = tvv1t1~kvgr1 AND tvv1t1~spras
= 'E'
LEFT JOIN tvv2t AS tvv2t2 ON vbak~kvgr2 = tvv2t2~kvgr2 AND tvv2t2~spras
= 'E'
LEFT JOIN tvv3t AS tvv3t3 ON vbak~kvgr3 = tvv3t3~kvgr3 AND tvv3t3~spras
= 'E'
LEFT JOIN tvv4t AS tvv4t4 ON vbak~kvgr4 = tvv4t4~kvgr4 AND tvv4t4~spras
= 'E'
LEFT JOIN tvv5t AS tvv5t5 ON vbak~kvgr5 = tvv5t5~kvgr5 AND tvv5t5~spras
= 'E'
LEFT JOIN tvm1t ON vbap~mvgr1 = tvm1t~mvgr1 AND tvm1t~spras = 'E'
LEFT JOIN tvm2t ON vbap~mvgr2 = tvm2t~mvgr2 AND tvm2t~spras = 'E'
LEFT JOIN tvm3t ON vbap~mvgr3 = tvm3t~mvgr3 AND tvm3t~spras = 'E'
LEFT JOIN tvm4t ON vbap~mvgr4 = tvm4t~mvgr4 AND tvm4t~spras = 'E'
LEFT JOIN tvm5t ON vbap~mvgr5 = tvm5t~mvgr5 AND tvm5t~spras = 'E'
LEFT JOIN t023t ON mara~matkl = t023t~matkl AND t023t~spras = 'E'
LEFT JOIN vbpa  ON vbak~vbeln = vbpa~vbeln AND parvw = 'WE'
LEFT OUTER JOIN but020 ON but020~partner = vbrk~kunrg
    LEFT OUTER JOIN adrc ON adrc~addrnumber = but020~addrnumber AND
    adrc~langu = @p_lang
    LEFT OUTER JOIN  adr2 ON adr2~addrnumber = but020~addrnumber AND
    adr2~home_flag = 'X' AND adr2~flgdefault = 'X'
    LEFT OUTER JOIN  adr3 ON adr3~addrnumber = but020~addrnumber  AND
    adr3~home_flag = 'X' AND adr3~flgdefault = 'X'
WHERE
              vbrk~fkdat IN @p_fkdat AND vbrk~kunrg IN @p_kunrg AND
              vbrk~vbeln IN @p_vbeln
               AND buchk IN @p_buchk
              AND a~koaid = 'B' AND a~kstat = '' AND a~kinak = ''
              AND c~kstat = '' AND ( c~kschl = 'ZVAT' OR c~kschl =
              'ZVAD' ) AND tvfkt~spras = 'E'
              AND vbrk~fkart IN @p_fkart AND vbrk~spart IN @p_spart AND
              vbrk~vkorg IN @p_vkorg AND vbrk~regio IN @p_regio AND
              vbak~vkgrp IN @p_vkgrp AND vbrk~kdgrp IN @p_kdgrp
              AND vbrp~werks IN @p_werks
              AND vbrk~bukrs IN @p_bukrs
              AND mara~matkl IN @s_matkl
              AND vbap~mvgr5 IN @s_mvgr5
    GROUP BY  vbrp~ntgew , vbrp~brgew ,vbrk~buchk ,vbrk~vbeln ,
    vbrk~fkart ,vbrp~werks, tvfkt~vtext , vbrk~fkdat ,
              bkpf~xblnr , bkpf~belnr , vbrp~posnr , vbrp~matnr ,
              vbrp~arktx , vbrp~aubel , vbrp~aupos , vbrp~vgbel ,
              vbrp~vgpos , vbrp~fkimg , vbrp~spart ,
               a~kbetr , a~kwert , d~kwert , vbrk~cmwae , vbrk~zterm ,
               vbrp~vrkme , vbrk~kunrg, vbrk~spart , vbrk~kdgrp, g~charg
               , t005u~bezei
               , mara~matkl ,t023t~wgbez, mara~groes, vbrk~vbtyp,
               vbrk~fkart , vbrk~vkorg,vbrk~vtweg, vbak~erdat,
        vbak~kvgr1,vbak~kvgr2,vbak~kvgr3,vbak~kvgr4,vbak~kvgr5,
        vbrp~batch_ntgew,vbrp~batch_gewei,
    tvv1t1~bezei,
    tvv2t2~bezei,
    tvv3t3~bezei,
    tvv4t4~bezei,
    tvv5t5~bezei,
    vbap~mvgr1,vbap~mvgr2,vbap~mvgr3,vbap~mvgr4,vbap~mvgr5,
    tvm1t~bezei,
    tvm2t~bezei,
    tvm3t~bezei,
    tvm4t~bezei,
    tvm5t~bezei,
    vbpa~kunnr,
    vbrk~bukrs,
    mara~ean11, adrc~region,
     adrc~city1,
     adrc~post_code1,
     adrc~street,
     adrc~house_num1,
        adr3~fax_number,
     adr2~tel_number
        INTO CORRESPONDING FIELDS OF TABLE @lt_output.

  SORT lt_output BY vbeln ASCENDING.

  LOOP AT lt_output INTO ls_output.
    AT END OF vbeln.
      SUM.
      APPEND ls_output TO lt_vbeln.
    ENDAT.
  ENDLOOP.



  SELECT kunnr AS kunrg ktokd rpmkr FROM kna1  INTO CORRESPONDING FIELDS
  OF TABLE lt_customer FOR ALL ENTRIES IN lt_output  WHERE kunnr =
  lt_output-kunrg AND kunnr IN p_kunrg.

  LOOP AT lt_customer INTO ls_customer.

    SELECT SINGLE name_org1 name_first name_last FROM but000 INTO
    (ls_customer-name_org1 , ls_customer-name_first ,
    ls_customer-name_last) WHERE partner = ls_customer-kunrg.

    SELECT  taxnum taxtype FROM dfkkbptaxnum INTO CORRESPONDING FIELDS
    OF TABLE lt_taxnum WHERE taxtype IN ( 'IR0' ,'IR1' ,'IR2' ,'IR3' ,
    'IR4' ,'IR5'  ) AND partner = ls_customer-kunrg.


    LOOP AT lt_taxnum.
      CASE lt_taxnum-taxtype.
        WHEN 'IR0'.
          ls_customer-ecco_no = lt_taxnum-taxnum.
        WHEN 'IR1'.
          ls_customer-register_no = lt_taxnum-taxnum.
        WHEN 'IR2'.
          ls_customer-birth_no = lt_taxnum-taxnum.
        WHEN 'IR3'.
          ls_customer-national_id = lt_taxnum-taxnum.
        WHEN 'IR4'.
          ls_customer-nationalcode = lt_taxnum-taxnum.
        WHEN 'IR5'.
          ls_customer-passport_no = lt_taxnum-taxnum.

      ENDCASE.
    ENDLOOP.

*SELECT adrc~region adrc~city1 adrc~post_code1 adrc~street
*adrc~house_num1 UP TO 1 ROWS FROM but020
*     INNER JOIN adrc ON adrc~addrnumber = but020~addrnumber
*INTO (ls_customer-region , ls_customer-city1 , ls_customer-post_code1 ,
*ls_customer-street , ls_customer-house_num1 )
*WHERE partner = ls_customer-kunrg AND adrc~langu = p_lang  ORDER BY
*addr_valid_from DESCENDING.
*    ENDSELECT.
*
*    SELECT tel_number UP TO 1 ROWS FROM but020
*     INNER JOIN adr2 ON adr2~addrnumber = but020~addrnumber
*     INTO  ls_customer-tel_number
*WHERE partner = ls_customer-kunrg  ORDER BY addr_valid_from DESCENDING.
*    ENDSELECT.
*
*    SELECT fax_number UP TO 1 ROWS FROM but020
*     INNER JOIN adr3 ON adr3~addrnumber = but020~addrnumber
*     INTO  ls_customer-fax_number
*WHERE partner = ls_customer-kunrg  ORDER BY addr_valid_from DESCENDING.
*    ENDSELECT.

    MODIFY lt_customer FROM ls_customer.

  ENDLOOP.



  SELECT SINGLE butxt , stceg , adrc~region , adrc~city1 ,
  adrc~post_code1 , adrc~street , adrc~house_num1 , adrc~tel_number ,
  adrc~fax_number , adrc~sort2
 FROM t001
    INNER JOIN adrc ON adrc~addrnumber = t001~adrnr
    INTO @DATA(bukrs)
    WHERE  adrc~langu = @p_lang AND bukrs = @ls_output-bukrs.

  LOOP AT lt_output INTO  ls_output
                     GROUP BY ls_output-spart.
    CLEAR ls_error.
    AUTHORITY-CHECK OBJECT  'V_VBAK_VKO'
      ID 'ACTVT' FIELD '03'
      ID 'VKORG' FIELD ls_output-vkorg
      ID 'SPART' FIELD ls_output-spart.


    IF sy-subrc IS NOT INITIAL.
      CONCATENATE ' ,' ls_output-spart INTO ls_error.
    ENDIF.


  ENDLOOP.

  IF strlen( ls_error ) > 0.
    MESSAGE i008(zfi) WITH ls_error.
  ENDIF.


  CLEAR ls_error.
  LOOP AT lt_output INTO  ls_output
                  GROUP BY ls_output-kdgrp.

    AUTHORITY-CHECK OBJECT  'Z_CUST_GRP'
      ID 'ACTVT' FIELD '03'
      ID 'KDGRP' FIELD ls_output-kdgrp.


    IF sy-subrc IS NOT INITIAL.
      CONCATENATE ' ,' ls_output-kdgrp INTO ls_error.
    ENDIF.


  ENDLOOP.

  IF strlen( ls_error ) > 0.
    MESSAGE i009(zfi) WITH ls_error.
  ENDIF.

  CLEAR ls_error.
  LOOP AT lt_output INTO  ls_output
                  GROUP BY ls_output-fkart.

    AUTHORITY-CHECK OBJECT  'V_VBRK_FKA'
      ID 'ACTVT' FIELD '03'
      ID 'FKART' FIELD ls_output-fkart.

    IF sy-subrc IS NOT INITIAL.
      CONCATENATE ' ,' ls_output-fkart INTO ls_error.

    ENDIF.
  ENDLOOP.

  IF strlen( ls_error ) > 0.
    MESSAGE i006(zfi) WITH ls_error.

  ENDIF.


  LOOP AT lt_output INTO ls_output.

    CLEAR ls_error.
    AUTHORITY-CHECK OBJECT  'V_VBAK_VKO'
      ID 'ACTVT' FIELD '03'
      ID 'VKORG' FIELD ls_output-vkorg
      ID 'SPART' FIELD ls_output-spart.


    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.


    AUTHORITY-CHECK OBJECT  'Z_CUST_GRP'
      ID 'ACTVT' FIELD '03'
      ID 'KDGRP' FIELD ls_output-kdgrp.


    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.


    AUTHORITY-CHECK OBJECT  'V_VBRK_FKA'
      ID 'ACTVT' FIELD '03'
      ID 'FKART' FIELD ls_output-fkart.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.

    ENDIF.



    CLEAR ls_error.





    "SELECT SINGLE mblnr FROM mkpf INTO ls_output-mblnr WHERE xblnr =
    "ls_output-vgbel.

    MOVE-CORRESPONDING bukrs TO ls_output.


    ls_output-comp_nationalid = bukrs-sort2.

    READ TABLE lt_customer INTO ls_customer WITH TABLE KEY kunrg =
    ls_output-kunrg.
    IF sy-subrc IS INITIAL.

      ls_output-name_org1 = ls_customer-name_org1.
      ls_output-name_first = ls_customer-name_first.
      ls_output-name_last = ls_customer-name_last.


      ls_output-ecco_no = ls_customer-ecco_no.
      ls_output-register_no = ls_customer-register_no.
      ls_output-birth_no = ls_customer-birth_no.
      ls_output-national_id = ls_customer-national_id.
      ls_output-nationalcode = ls_customer-nationalcode.
      ls_output-passport_no = ls_customer-passport_no.

*      ls_output-partner_region = ls_customer-region.
*      ls_output-partner_city1 = ls_customer-city1.
*      ls_output-partner_post_code1 = ls_customer-post_code1.
*      ls_output-partner_street = ls_customer-street.
*      ls_output-partner_house_num1 = ls_customer-house_num1.
*      ls_output-partner_tel_number = ls_customer-tel_number.
*      ls_output-partner_fax_number = ls_customer-fax_number.
      ls_output-ktokd = ls_customer-ktokd.
      ls_output-rpmkr = ls_customer-rpmkr.
      SELECT SINGLE txt30 INTO ls_output-ktokdt FROM t077x
        WHERE spras = 'EN' AND ktokd = ls_output-ktokd.



      SELECT SINGLE bezei INTO ls_output-partner_bezei FROM t005u
        WHERE spras = 'EN' AND land1 = 'IR' AND bland =
        ls_output-partner_region.

      IF ls_customer-ktokd = 'C006'.
        SELECT SINGLE mtart FROM mara INTO @DATA(mtart) WHERE matnr =
        @ls_output-matnr.
        IF mtart = 'ZFER'.
          CONCATENATE  'OE-' ls_output-arktx  INTO ls_output-arktx.
        ENDIF.

      ENDIF.

    ENDIF.

    CONCATENATE ls_output-matnr ls_output-charg INTO lv_object
    RESPECTING BLANKS.


    ls_output-t1 = ls_customer-ecco_no.

    ls_output-t2 = ls_customer-register_no.

    ls_output-t3 = ls_customer-birth_no.

    ls_output-t4 = ls_customer-national_id.

    ls_output-t5 = ls_customer-nationalcode.

    ls_output-t6 = ls_customer-passport_no.

*SELECT SINGLE taxnum INTO ls_output-t1 FROM dfkkbptaxnum WHERE partner
*= ls_output-kunrg AND taxtype = 'IR0'.
*SELECT SINGLE taxnum INTO ls_output-t2 FROM dfkkbptaxnum WHERE partner
*= ls_output-kunrg AND taxtype = 'IR1'.
*SELECT SINGLE taxnum INTO ls_output-t3 FROM dfkkbptaxnum WHERE partner
*= ls_output-kunrg AND taxtype = 'IR2'.
*SELECT SINGLE taxnum INTO ls_output-t4 FROM dfkkbptaxnum WHERE partner
*= ls_output-kunrg AND taxtype = 'IR3'.
*SELECT SINGLE taxnum INTO ls_output-t5 FROM dfkkbptaxnum WHERE partner
*= ls_output-kunrg AND taxtype = 'IR4'.
*SELECT SINGLE taxnum INTO ls_output-t6 FROM dfkkbptaxnum WHERE partner
*= ls_output-kunrg AND taxtype = 'IR5'.

    SELECT SINGLE exti2 vttk~tknum INTO (ls_output-exti2,ls_output-tknum
    )
      FROM vttk
      JOIN vttp ON vttk~tknum = vttp~tknum AND vbeln = ls_output-vgbel.

    IF ls_output-mvgr5 IS INITIAL.
      SELECT SINGLE mvke~mvgr5 tvm5t~bezei AS mvgr5t INTO CORRESPONDING
      FIELDS OF ls_output
        FROM mvke
        LEFT JOIN tvm5t ON mvke~mvgr5 = tvm5t~mvgr5 AND tvm5t~spras =
        'E'
        WHERE matnr = ls_output-matnr AND
              vkorg = ls_output-vkorg AND
              vtweg = ls_output-vtweg .
    ENDIF.

    lv_mat = ls_output-matnr.
    SHIFT lv_mat LEFT DELETING LEADING '0'.

*    IF lv_mat+2(1) = '1'.
*    ls_output-degree = 'درجه 1'.
*    ELSEIF lv_mat+2(1) = '2'.
*      ls_output-degree = 'درجه 2'.
*    ENDIF.

*    PERFORM readclassification USING  'Z_PP_BATCH' '023' 'MCH1'.
*
*
*    LOOP AT lt_object INTO ls_object WHERE atnam = 'ZPP_WEIGHT'.
*
*      IF ls_object-ausp1 CA ' ' .
*
*        lv_index = sy-fdpos.
*
*        IF lv_index > 1.
*
*          lv_index = lv_index - 1.
*          ls_output-ausp1 = ls_object-ausp1(lv_index).
*          lv_index = lv_index + 2.
*          ls_output-meins = ls_object-ausp1+lv_index(3).
*
*          BATCH_NTGEW' 'وزن بچ'.
*  PERFORM add_cat USING 'BATCH_GEWEI
*
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.

    ls_output-ausp1 = ls_output-batch_ntgew.
    ls_output-meins = ls_output-batch_gewei..

    CLEAR lt_object.

    lv_object = ls_output-matnr.

    PERFORM readclassification USING  'Z_BR_PRODUCT_IRCOD' '001' 'MARA'.

    LOOP AT lt_object INTO ls_object WHERE atnam = 'Z_IRAN_CODE'.
      ls_output-irancode = ls_object-ausp1.
    ENDLOOP.


    LOOP AT lt_object INTO ls_object WHERE atnam = 'Z_SHENASEH'.
      ls_output-shenaseh = ls_object-ausp1.
    ENDLOOP.






    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = ls_output-vrkme
*       LANGUAGE             = SY-LANGU
      IMPORTING
*       LONG_TEXT            =
        output = ls_output-vrkme
*       SHORT_TEXT           =
*     EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
      EXPORTING
        im_datint   = ls_output-fkdat
        im_datfmdes = 'C'
      IMPORTING
        ex_datext   = ls_output-ffkdat
*       ex_datfmused =
      .

    READ TABLE lt_vbeln INTO ls_output1 WITH TABLE KEY vbeln =
    ls_output-vbeln.
    IF sy-subrc IS INITIAL.
      ls_output-total_delivery_cost = ls_output1-delivery_cost.
      ls_output-total_packaging_cost = ls_output1-packaging_cost.
      ls_output-total_netamount = ls_output1-kwert + ls_output1-discount
      ." + ls_output1-tax.
      ls_output-total_cleared_downpayment =
      ls_output1-cleared_downpayment.
      ls_output-payable_amount = ls_output-total_netamount +
      ls_output1-tax + ls_output-total_packaging_cost +
      ls_output-total_delivery_cost -
      ls_output-total_cleared_downpayment.
      ls_output-netamount = ls_output-kwert + ls_output-discount.
      " + ls_output-tax.
    ENDIF.

    SHIFT ls_output-matnr LEFT DELETING LEADING '0'.
    SHIFT ls_output-xblnr LEFT DELETING LEADING '0'.
    SHIFT ls_output-aubel LEFT DELETING LEADING '0'.
    SHIFT ls_output-vbeln LEFT DELETING LEADING '0'.
    SHIFT ls_output-kunrg LEFT DELETING LEADING '0'.


    CLEAR amounttsr.

    CALL FUNCTION 'BAPI_CURRENCY_GETDECIMALS'
      EXPORTING
        currency          = ls_output-cmwae
      IMPORTING
        currency_decimals = currdec
        return            = return.

    IF currdec-curdecimals = 0.


      CALL FUNCTION 'ZFI_CONVERT_AMOUNT_TO_TEXT'
        EXPORTING
          amount = ls_output-payable_amount * 100
        IMPORTING
          text   = amounttsr
*       EXCEPTIONS
*         DATA_TYPE_MISMATCH       = 1
*         OTHERS = 2
        .
    ELSE.



      CALL FUNCTION 'ZFI_CONVERT_AMOUNT_TO_TEXT'
        EXPORTING
          amount = ls_output-payable_amount
        IMPORTING
          text   = amounttsr
*       EXCEPTIONS
*         DATA_TYPE_MISMATCH       = 1
*         OTHERS = 2
        .


    ENDIF.

    SHIFT ls_output-vbeln LEFT DELETING LEADING '0'.

    ls_output-payable_amount2 = amounttsr.

    IF ls_output-fkart = 'ZT01' OR ls_output-fkart = 'ZT00'  OR
    ls_output-fkart = 'ZB01' OR ls_output-fkart = 'ZB00' OR
    ls_output-fkart = 'YT00' OR ls_output-fkart = 'YT01'.
      ls_output-fkimg = 0.
      ls_output-ausp1 = 0.
      ls_output-ntgew = 0.
      ls_output-brgew = 0.


    ENDIF.



    IF ( ls_output-vbtyp = 'O' AND ls_output-fkart <> 'ZT01' AND
    ls_output-fkart <> 'ZT00') OR ( ls_output-vbtyp = 'N' AND
    ls_output-fkart <> 'ZT01' AND ls_output-fkart <> 'ZT00').
      ls_output-fkimg = ls_output-fkimg * -1.
      ls_output-ausp1 = ls_output-ausp1 * -1.
      ls_output-ntgew = ls_output-ntgew * -1.
      ls_output-brgew = ls_output-brgew * -1.


      ls_output-kwert	= ls_output-kwert * -1.
      ls_output-kbetr =	ls_output-kbetr * -1.
      ls_output-discount =  ls_output-discount * -1.
      ls_output-tax =	ls_output-tax * -1.
      ls_output-cleared_downpayment =	ls_output-cleared_downpayment * -1
      .
      ls_output-delivery_cost =	ls_output-delivery_cost * -1.
      ls_output-packaging_cost =  ls_output-packaging_cost * -1.
      ls_output-netamount =	ls_output-netamount * -1.
      ls_output-total_cleared_downpayment =
        ls_output-total_cleared_downpayment * -1.
      ls_output-total_delivery_cost =	ls_output-total_delivery_cost * -1
      .
      ls_output-total_packaging_cost =  ls_output-total_packaging_cost *
      -1.
      ls_output-total_netamount =	ls_output-total_netamount * -1.
      ls_output-payable_amount =  ls_output-payable_amount * -1.
    ELSEIF  ls_output-fkart = 'ZT01' OR ls_output-fkart = 'ZT00'.
      " OR ls_output-fkart = 'YT01' OR ls_output-fkart = 'YB00' .
      ls_output-kwert	= ls_output-kwert * -1.
      ls_output-kbetr =	ls_output-kbetr * -1.
      ls_output-discount =  ls_output-discount * -1.
      ls_output-tax =	ls_output-tax * -1.
      ls_output-cleared_downpayment =	ls_output-cleared_downpayment * -1
      .
      ls_output-delivery_cost =	ls_output-delivery_cost * -1.
      ls_output-packaging_cost =  ls_output-packaging_cost * -1.
      ls_output-netamount =	ls_output-netamount * -1.
      ls_output-total_cleared_downpayment =
        ls_output-total_cleared_downpayment * -1.
      ls_output-total_delivery_cost =	ls_output-total_delivery_cost * -1
      .
      ls_output-total_packaging_cost =  ls_output-total_packaging_cost *
      -1.
      ls_output-total_netamount =	ls_output-total_netamount * -1.
      ls_output-payable_amount =  ls_output-payable_amount * -1.


    ENDIF.

    ls_output-billingdate_from = p_fkdat-low.
    ls_output-billingdate_to = p_fkdat-high.
    ls_output-currentdate = sy-datum.


    SELECT adrc~post_code1 UP TO 1 ROWS FROM but020
     INNER JOIN adrc ON adrc~addrnumber = but020~addrnumber
     INTO ls_output-vbpa_post_code1
     WHERE partner = ls_output-vbpa_kunnr AND adrc~langu = p_lang  ORDER
     BY addr_valid_from DESCENDING.
    ENDSELECT.

    ls_output-netvf05 = ls_output-kwert +
                        ls_output-discount +
                        ls_output-delivery_cost.

    ls_output-nettotal = ls_output-kwert +
                         ls_output-discount +
                         ls_output-delivery_cost +
                         ls_output-tax.


    APPEND ls_output TO lt_output1.


  ENDLOOP.




  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'NAME_ORG1'.
  wa_fieldcat-reptext = 'Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'NAME_FIRST'.
  wa_fieldcat-reptext = 'First Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'NAME_LAST'.
  wa_fieldcat-reptext = 'Last Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'ECCO_NO'.
  wa_fieldcat-reptext = 'IR0'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'REGISTER_NO'.
  wa_fieldcat-reptext = 'IR1'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BIRTH_NO'.
  wa_fieldcat-reptext = 'IR2'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'NATIONAL_ID'.
  wa_fieldcat-reptext = 'IR3'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'NATIONALCODE'.
  wa_fieldcat-reptext = 'IR4'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PASSPORT_NO'.
  wa_fieldcat-reptext = 'IR5'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'BUTXT'.
  wa_fieldcat-reptext = 'BUKRS_NAME'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'COMP_NATIONALID'.
  wa_fieldcat-reptext = 'BUKRS_NATIONALID'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'STCEG'.
  wa_fieldcat-reptext = 'VAT_Number'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'REGION'.
  wa_fieldcat-reptext = 'REGION'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'CITY1'.
  wa_fieldcat-reptext = 'CITY'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'POST_CODE1'.
  wa_fieldcat-reptext = 'POST_CODE'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'STREET'.
  wa_fieldcat-reptext = 'STREET'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'HOUSE_NUM1'.
  wa_fieldcat-reptext = 'HOUSE_NUM'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'TEL_NUMBER'.
  wa_fieldcat-reptext = 'TEL_NUMBER'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'FAX_NUMBER'.
  wa_fieldcat-reptext = 'FAX_NUMBER'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_REGION'.
  wa_fieldcat-reptext = 'CREGION'.

  APPEND wa_fieldcat TO it_fieldcat.



  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_BEZEI'.
  wa_fieldcat-reptext = 'CREGION'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_CITY1'.
  wa_fieldcat-reptext = 'CCITY'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_POST_CODE1'.
  wa_fieldcat-reptext = 'CPOST_CODE'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_STREET'.
  wa_fieldcat-reptext = 'CSTREET'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_HOUSE_NUM1'.
  wa_fieldcat-reptext = 'CHOUSE_NUM'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_TEL_NUMBER'.
  wa_fieldcat-reptext = 'CTEL_NUMBER'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'PARTNER_FAX_NUMBER'.
  wa_fieldcat-reptext = 'CFAX_NUMBER'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'KUNRG'.
  wa_fieldcat-reptext = 'CUSTOMERNO'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'VBELN'.
  wa_fieldcat-reptext = 'BILLINGNO'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'BUCHK'.
  wa_fieldcat-reptext = 'Posting Status'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'FKART'.
  wa_fieldcat-reptext = 'BILLINGTYPE'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'WERKS'.
  wa_fieldcat-reptext = 'Plant'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'TVFKT'.
  wa_fieldcat-fieldname = 'VTEXT'.
  wa_fieldcat-reptext = 'BILLINGTYPETEXT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'MARA'.
  wa_fieldcat-fieldname = 'MEINS'.
  wa_fieldcat-reptext = 'UNIT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'FKDAT'.
  wa_fieldcat-reptext = 'BILLINGDATE'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'XBLNR'.
  wa_fieldcat-reptext = 'REFERENCE'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'CMWAE'.
  wa_fieldcat-reptext = 'Currency'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'ZTERM'.
  wa_fieldcat-reptext = 'Payment Terms'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'SPART'.
  wa_fieldcat-reptext = 'DIVISION'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'KDGRP'.
  wa_fieldcat-reptext = 'CUSTOMERGROUP'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'POSNR'.
  wa_fieldcat-reptext = 'BILLINGITEM'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-reptext = 'MATERIALNO'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'ARKTX'.
  wa_fieldcat-reptext = 'MATERIALTXT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'CHARG'.
  wa_fieldcat-reptext = 'BATCH'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'MKPF'.
  wa_fieldcat-fieldname = 'MBLNR'.
  wa_fieldcat-reptext = 'MATERIALDOC'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'AUBEL'.
  wa_fieldcat-reptext = 'SALESDOC'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'AUPOS'.
  wa_fieldcat-reptext = 'SALESDOCITEM'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'VGBEL'.
  wa_fieldcat-reptext = 'DELIVERY'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'VGPOS'.
  wa_fieldcat-reptext = 'DELIVERYITEM'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'FKIMG'.
  wa_fieldcat-reptext = 'BILLEDQUANTITY'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'VRKME'.
  wa_fieldcat-reptext = 'UNIT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'SPARTI'.
  wa_fieldcat-reptext = 'Item Division'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AUSP1'.
  wa_fieldcat-reptext = 'Batch WEIGHT'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'NTGEW'.
  wa_fieldcat-reptext = 'Net WEIGHT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BRGEW'.
  wa_fieldcat-reptext = 'Gross WEIGHT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'IRANCODE'.
  wa_fieldcat-reptext = 'ايران کد'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'MARA'.
  wa_fieldcat-fieldname = 'MATKL'.
  wa_fieldcat-reptext = 'Material Group'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T023T'.
  wa_fieldcat-fieldname = 'WGBEZ'.
  wa_fieldcat-reptext = 'Material Group Desc.'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'MARA'.
  wa_fieldcat-fieldname = 'GROES'.
  wa_fieldcat-reptext = 'Size/dimensions'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBAK'.
  wa_fieldcat-fieldname = 'VBTYP'.
  wa_fieldcat-reptext = 'VBTYP'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'VKORG'.
  wa_fieldcat-reptext = 'Sales Org'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBAK'.
  wa_fieldcat-fieldname = 'ERDAT'.
  wa_fieldcat-reptext = 'Order Created On'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'FKART'.
  wa_fieldcat-reptext = 'FKART'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BELNR'.
  wa_fieldcat-reptext = 'FI DOC'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'REGIONTXT'.
  wa_fieldcat-reptext = 'REGIONTXT'.

  APPEND wa_fieldcat TO it_fieldcat.




  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'KBETR'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Price'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'KWERT'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-reptext = 'Amount'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-fieldname = 'DISCOUNT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'DISCOUNT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-fieldname = 'TAX'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'TAX'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'CLEARED_DOWNPAYMENT'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Cleared_Downpayment'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'DELIVERY_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Delivery_Cost'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'PACKAGING_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Packaging_Cost'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'NETAMOUNT'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'NETAMOUNT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'TOTAL_CLEARED_DOWNPAYMENT'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'TOTAL_Cleared_Downpayment'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'TOTAL_DELIVERY_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'TOTAL_Delivery_Cost'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'TOTAL_PACKAGING_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'TOTAL_Packaging_Cost'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'TOTAL_NETAMOUNT'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'TOTAL_NETAMOUNT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-fieldname = 'PAYABLE_AMOUNT'.
  wa_fieldcat-reptext = 'payable_amount'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PAYABLE_AMOUNT2'.
  wa_fieldcat-reptext = 'payable_amount_text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BILLINGDATE_FROM'.
  wa_fieldcat-reptext = 'BillingDate From'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BILLINGDATE_TO'.
  wa_fieldcat-reptext = 'BillingDate TO'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'CURRENTDATE'.
  wa_fieldcat-reptext = 'Current Date'.
  APPEND wa_fieldcat TO it_fieldcat.


  PERFORM add_cat USING 'KVGR1' 'Customer Group 1'.
  PERFORM add_cat USING 'BEZEI1' 'Customer Group 1'.
  PERFORM add_cat USING 'KVGR2' 'Customer Grade'.
  PERFORM add_cat USING 'BEZEI2' 'Customer Grade'.
  PERFORM add_cat USING 'KVGR3' 'Customer Group 3'.
  PERFORM add_cat USING 'BEZEI3' 'Customer Group 3'.
  PERFORM add_cat USING 'KVGR4' 'Customer Group 4'.
  PERFORM add_cat USING 'BEZEI4' 'Customer Group 4'.
  PERFORM add_cat USING 'KVGR5' 'Customer Group 5'.
  PERFORM add_cat USING 'BEZEI5' 'Customer Group 5'.

  PERFORM add_cat USING 'KDKG1' 'Condition group 1'.
  PERFORM add_cat USING 'KDKG2' 'Condition group 2'.
  PERFORM add_cat USING 'KDKG3' 'Condition group 3'.
  PERFORM add_cat USING 'KDKG4' 'Condition group 4'.
  PERFORM add_cat USING 'KDKG5' 'Condition group 5'.

  PERFORM add_cat USING 'MVGR1' 'Tire TREAD'.
  PERFORM add_cat USING 'MVGR1T' 'Tire TREAD'.
  PERFORM add_cat USING 'MVGR2' 'Layer'.
  PERFORM add_cat USING 'MVGR2T' 'Layer'.
  PERFORM add_cat USING 'MVGR3' 'Pattern'.
  PERFORM add_cat USING 'MVGR3T' 'Pattern'.
  PERFORM add_cat USING 'MVGR4' 'Size'.
  PERFORM add_cat USING 'MVGR4T' 'Size'.
  PERFORM add_cat USING 'MVGR5' 'Product Group'.
  PERFORM add_cat USING 'MVGR5T' 'Product Group'.


  PERFORM add_cat USING 'KTOKD' 'Acount Group'.
  PERFORM add_cat USING 'RPMKR' 'نقش تجاری نماینده'.
  PERFORM add_cat USING 'KTOKDT' 'Acount Group Desc.'.
  PERFORM add_cat USING 'T1' 'کد اقتصادي'.
  PERFORM add_cat USING 'T2' 'شماره ثبت'.
  PERFORM add_cat USING 'T3' 'شماره شناسنامه'.
  PERFORM add_cat USING 'T4' 'شناسه ملي'.
  PERFORM add_cat USING 'T5' 'کد ملي'.
  PERFORM add_cat USING 'T6' 'شماره پاسپورت'.

  PERFORM add_cat USING 'SHENASEH' 'شناسه کالا'.
  PERFORM add_cat USING 'EAN11' 'بارکد'.
  PERFORM add_cat USING 'EXTI2' 'شماره بارنامه'.
  PERFORM add_cat USING 'TKNUM' 'Shipment No'.
  PERFORM add_cat USING 'DEGREE' 'درجه محصول'.

  PERFORM add_cat USING 'BATCH_NTGEW' 'وزن بچ'.
  PERFORM add_cat USING 'BATCH_GEWEI' 'واحد وزن'.
  PERFORM add_cat USING 'VBPA_KUNNR' 'Ship to Party'.
  PERFORM add_cat USING 'VBPA_POST_CODE1' 'Ship to Party Postal Code'.
  "PERFORM add_cat USING 'NETVF05' 'Net Value VF05'.
  "PERFORM add_cat USING 'NETTOTAL' 'مبلغ کل فروش به ازاي آيتم'.
  PERFORM add_cat USING 'BUKRS' 'Company Code'.


  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-fieldname = 'NETVF05'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Net Value VF05'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-fieldname = 'NETTOTAL'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'مبلغ کل فروش به ازاي آيتم'.
  APPEND wa_fieldcat TO it_fieldcat.

  LOOP AT it_fieldcat INTO wa_fieldcat.
    MOVE-CORRESPONDING wa_fieldcat TO cs_fieldcat.
    cs_fieldcat-reptext_ddic = wa_fieldcat-reptext.
    cs_fieldcat-decimals_out = wa_fieldcat-decimals_o.
    APPEND cs_fieldcat TO ct_fieldcat.
  ENDLOOP.



  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME   =
*     I_BACKGROUND_ID    = ' '
*     I_GRID_TITLE       =
*     I_GRID_SETTINGS    =
*     IS_LAYOUT          = 'X'
      it_fieldcat        = ct_fieldcat
*     IT_EXCLUDING       =
*     IT_SPECIAL_GROUPS  =
*     IT_SORT            =
*     IT_FILTER          =
*     IS_SEL_HIDE        =
*     I_DEFAULT          = 'X'
      i_save             = 'A'
*     IS_VARIANT         =
*     IT_EVENTS          =
*     IT_EVENT_EXIT      =
*     IS_PRINT           =
*     IS_REPREP_ID       =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE  = 0
*     I_HTML_HEIGHT_TOP  = 0
*     I_HTML_HEIGHT_END  = 0
*     IT_ALV_GRAPHICS    =
*     IT_HYPERLINK       =
*     IT_ADD_FIELDCAT    =
*     IT_EXCEPT_QINFO    =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab           = lt_output1
*   EXCEPTIONS
*     PROGRAM_ERROR      = 1
*     OTHERS             = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
FORM readclassification USING class TYPE  klah-class classtype TYPE
klah-klart objecttable TYPE tcla-obtab.
  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
      class              = class
*     CLASSTEXT          = 'X'
      classtype          = classtype
*     CLINT              = 0
*     FEATURES           = 'X'
*     LANGUAGE           = SY-LANGU
      object             = lv_object
      objecttable        = objecttable
*     KEY_DATE           = SY-DATUM
*     INITIAL_CHARACT    = 'X'
*     NO_VALUE_DESCRIPT  =
*     CHANGE_SERVICE_CLF = 'X'
*     INHERITED_CHAR     = ' '
*     CHANGE_NUMBER      = ' '
    TABLES
      t_class            = lt_class
      t_objectdata       = lt_object
*     I_SEL_CHARACTERISTIC       =
*     T_NO_AUTH_CHARACT  =
    EXCEPTIONS
      no_classification  = 1
      no_classtypes      = 2
      invalid_class_type = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_CAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM add_cat  USING    lv1
                       lv2.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = lv1.
  wa_fieldcat-reptext   = lv2.
  APPEND wa_fieldcat TO it_fieldcat.


ENDFORM.
