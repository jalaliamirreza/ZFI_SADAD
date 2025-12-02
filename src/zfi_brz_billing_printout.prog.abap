*&---------------------------------------------------------------------*
*& Report ZFI_BRZ_BILLING_PRINTOUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_brz_billing_printout.

TABLES:vbrk, vbak, vbkd.

TYPES: BEGIN OF t_output,
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
         vtext                     TYPE tvfkt-vtext,
         fkdat                     TYPE vbrk-fkdat,
         ffkdat                    TYPE string,
         xblnr                     TYPE vbrk-xblnr,
         cmwae                     TYPE vbrk-cmwae,
         zterm                     TYPE vbrk-zterm,
         kunrg                     TYPE vbrk-kunrg,
         spart                     TYPE vbrk-spart,
         kdgrp                     TYPE vbrk-kdgrp,
         posnr                     TYPE vbrp-posnr,
         matnr                     TYPE vbrp-matnr,
         arktx                     TYPE vbrp-arktx,
         aubel                     TYPE vbrp-aubel,
         aupos                     TYPE vbrp-aupos,
         vgbel                     TYPE vbrp-vgbel,
         vgpos                     TYPE vbrp-vgpos,
         fkimg                     TYPE vbrp-fkimg,
         vrkme                     TYPE vbrp-vrkme,
         charg                     TYPE lips-charg,
         prdha                     TYPE mara-prdha,
         mblnr                     TYPE mkpf-mblnr,
         kbetr                     TYPE prcd_elements-kbetr,
         kwert                     TYPE prcd_elements-kwert,
         discount                  TYPE prcd_elements-kwert,
         tax                       TYPE prcd_elements-kwert,
         cleared_downpayment       TYPE prcd_elements-kwert,
         delivery_cost             TYPE prcd_elements-kwert,
         delivery_post              TYPE prcd_elements-kwert,
         inventory_cost            TYPE prcd_elements-kwert,
         packaging_cost            TYPE prcd_elements-kwert,
         c1_cost                   TYPE prcd_elements-kwert,
         tot_c1_cost               TYPE prcd_elements-kwert,
         c2_cost                   TYPE prcd_elements-kwert,
         tot_c2_cost               TYPE prcd_elements-kwert,
         netamount                 TYPE prcd_elements-kwert,
         total_cleared_downpayment TYPE prcd_elements-kwert,
         total_delivery_cost       TYPE prcd_elements-kwert,
         total_inventory_cost      TYPE prcd_elements-kwert,
         total_packaging_cost      TYPE prcd_elements-kwert,
         total_netamount           TYPE prcd_elements-kwert,
         payable_amount            TYPE prcd_elements-kwert,
         CostInstallation          TYPE prcd_elements-kwert,
         comp_nationalid           TYPE adrc-sort2,
         ausp1                     TYPE p LENGTH 6 DECIMALS 3,
         meins                     TYPE c LENGTH 3,
         irancode                  TYPE c LENGTH 40,
         bstkd                     TYPE vbkd-bstkd,
         ihrez                     TYPE vbkd-ihrez,
         bstkd_e                   TYPE vbkd-bstkd_e,
         ihrez_e                   TYPE vbkd-ihrez_e,
         werks                     TYPE vbap-werks,
         name1                     TYPE v_t001w-name1,
         land1                     TYPE v_t001w-land1,
         regio                     TYPE v_t001w-regio,
         bezei                     TYPE t005u-bezei,
         ort01                     TYPE v_t001w-ort01,
         pstlz                     TYPE v_t001w-pstlz,
         stras                     TYPE char100,
         tel_number1               TYPE adrc-tel_number,
         fax_number1               TYPE adrc-fax_number,
         kunnr                     TYPE vbpa-kunnr,
         name2                     TYPE adrc-name1,
         city2                     TYPE adrc-city1,
         post_code2                TYPE adrc-post_code1,
         street2                   TYPE adrc-street,
         tel_number2               TYPE adrc-tel_number,
         mobile                    TYPE adrc-tel_number,
         mvgr5                     TYPE vbap-mvgr5,
         mvgr5t                    TYPE tvm1t-bezei,
         chargt                    TYPE char10,
         text                      TYPE char600,
         sfakn1                    TYPE sfakn,
         sfakn2                    TYPE sfakn,
         shenaseh                  type zvmchar-ATWRT,
         Name                      TYPE Vbak-bname,
         lgort                     type lips-lgort,
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
      END OF ls_customer.

DATA lt_customer LIKE STANDARD TABLE OF ls_customer WITH KEY kunrg.

PARAMETERS p_bukrs TYPE bukrs OBLIGATORY.
*SELECT-OPTIONS p_bukrs1 FOR vbrk-bukrs obligatory.
SELECT-OPTIONS p_kunrg FOR vbrk-kunrg.
SELECT-OPTIONS p_kdgrp FOR vbrk-kdgrp.
SELECT-OPTIONS p_fkart FOR vbrk-fkart.
SELECT-OPTIONS p_spart FOR vbrk-spart.
SELECT-OPTIONS p_vbeln FOR vbrk-vbeln.
SELECT-OPTIONS p_fkdat FOR vbrk-fkdat.
SELECT-OPTIONS s_rfbsk FOR vbrk-rfbsk.
SELECT-OPTIONS p_zuonr FOR vbak-vbeln.
SELECT-OPTIONS p_bstkd FOR vbkd-BSTKD.


PARAMETERS p_lang TYPE adrc-langu OBLIGATORY.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT  'F_PAYR_BUK'
    ID 'ACTVT' FIELD '03'
    ID 'BUKRS' FIELD p_bukrs.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e002(zfi) WITH p_bukrs.
    EXIT.
  ENDIF.


*  SELECT DISTINCT fkart FROM tvfk INTO CORRESPONDING FIELDS OF TABLE lt_tvfk WHERE fkart IN p_fkart.
*  LOOP AT lt_tvfk INTO ls_tvfk.
*
*    AUTHORITY-CHECK OBJECT  'V_VBRK_FKA'
*      ID 'ACTVT' FIELD '03'
*      ID 'FKART' FIELD ls_tvfk-fkart.
*
*    IF sy-subrc IS NOT INITIAL.
*      MESSAGE e006(zfi) WITH ls_tvfk-fkart.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
  IF sy-tcode = 'ZSD02'.
    SELECT DISTINCT
       vbrk~vbeln , vbrk~fkart , tvfkt~vtext , vbrk~fkdat , bkpf~xblnr , vbrk~cmwae , vbrk~zterm , vbrk~kunrg , vbrk~spart ,
       vbrk~kdgrp  , vbrp~posnr , vbrp~matnr , vbrp~vrkme , vbrp~arktx ,
       vbrp~aubel , vbrp~aupos , vbrp~vgbel , vbrp~vgpos , vbrp~fkimg , a~kbetr , a~kwert , g~charg , mara~prdha ,
       vbkd~bstkd , vbkd~ihrez , vbkd~bstkd_e ,vbkd~ihrez_e,
       vbrk~sfakn   AS sfakn1,
       vbfa~vbeln   AS sfakn2,
           vbap~werks,
           t001w~name1,
           t001w~land1,
           t001w~regio,
           t001w~ort01,
           t001w~pstlz,
      concat_with_space( adrc1~street,adrc1~house_num1, 1 ) AS stras,

           t005u~bezei,
      adrc1~tel_number AS tel_number1,
      adrc1~fax_number AS fax_number1,
            vbpa~kunnr,
      adrc~name1 AS name2,
      adrc~city1 AS city2,
      adrc~post_code1 AS post_code2,
      adrc~street AS street2,
      adrc~tel_number AS tel_number2,
      adr2~tel_number AS MOBILE,
*      adr2~tel_number AS mobile,
      vbap~mvgr5,
      tvm5t~bezei AS mvgr5t,
      mchar2~atwrt as shenaseh,
      CASE WHEN substring( vbrp~matnr,13,1 ) = '1' AND ( vbrk~kdgrp = 'CM' OR vbrk~kdgrp = 'CA' OR vbrk~kdgrp = 'CT' ) THEN 'OE'
           WHEN substring( vbrp~matnr,13,1 ) = '1' AND vbrk~kdgrp <> 'CM' AND vbrk~kdgrp <> 'CA' AND vbrk~kdgrp <> 'CT' THEN 'درجه1'
           WHEN substring( vbrp~matnr,13,1 ) = '2' THEN 'درجه2' END AS chargt,
       SUM( DISTINCT b~kwert ) AS discount ,
       SUM( DISTINCT c~kwert ) AS tax , d~kwert AS cleared_downpayment , SUM( DISTINCT e~kwert ) AS delivery_cost ,sum( distinct i~kwert ) as delivery_post, SUM( DISTINCT f~kwert ) AS packaging_cost,
       SUM( DISTINCT k~kwert ) AS c1_cost,
       SUM( DISTINCT h~kwert ) AS c2_cost
        FROM vbrk INNER JOIN tvfkt  ON tvfkt~fkart = vbrk~fkart
*                LEFT OUTER JOIN bkpf ON vbrk~vbeln = bkpf~awkey
                JOIN bkpf ON vbrk~vbeln = bkpf~awkey  AND BKPF~XBLNR <> ''
                INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
                INNER JOIN  prcd_elements AS a ON a~knumv = vbrk~knumv AND  a~kposn = vbrp~posnr
                LEFT OUTER  JOIN prcd_elements AS b ON b~knumv = vbrk~knumv AND  b~kposn = vbrp~posnr AND b~koaid = 'A' AND b~kstat = '' AND ( b~kschl LIKE 'ZD%' OR b~kschl = 'R100')
                INNER JOIN  prcd_elements AS c ON c~knumv = vbrk~knumv AND  c~kposn = vbrp~posnr
                LEFT OUTER JOIN prcd_elements AS d ON d~knumv = vbrk~knumv AND  d~kposn = vbrp~posnr AND  d~kschl = 'AZWB'
                LEFT OUTER JOIN prcd_elements AS e ON e~knumv = vbrk~knumv AND  e~kposn = vbrp~posnr AND e~kstat = '' AND e~kschl LIKE 'ZH00' AND e~kinak = ''
                LEFT OUTER JOIN prcd_elements AS f ON f~knumv = vbrk~knumv AND  f~kposn = vbrp~posnr AND f~kstat = '' AND f~kschl LIKE 'ZK%' AND f~kinak = ''
                LEFT OUTER JOIN prcd_elements AS k ON k~knumv = vbrk~knumv AND  k~kposn = vbrp~posnr AND k~kschl = 'ZJIN' AND k~kinak = ''
                LEFT OUTER JOIN prcd_elements AS h ON h~knumv = vbrk~knumv AND  h~kposn = vbrp~posnr AND h~kschl = 'ZJGR' AND h~kinak = ''
                LEFT OUTER JOIN prcd_elements AS i ON i~knumv = vbrk~knumv AND  i~kposn = vbrp~posnr AND i~kstat = '' AND i~kschl LIKE 'ZH01' AND i~kinak = ''
                LEFT OUTER JOIN lips AS g ON g~vbeln = vbrp~vgbel AND g~posnr = vbrp~vgpos
                LEFT OUTER JOIN mara ON mara~matnr = vbrp~matnr
*              LEFT OUTER JOIN vbfa ON vbfa~vbeln = vbrp~vbeln AND vbfa~posnn = vbrp~posnr AND vbfa~vbtyp_v = 'C'
                LEFT OUTER JOIN vbfa ON vbfa~vbelv = vbrk~vbeln AND vbfa~posnv = vbrp~posnr AND
                ( ( vbfa~vbtyp_n = 'N'  AND vbfa~vbtyp_v = 'M' )  OR
                ( vbfa~vbtyp_n = 'S'  AND vbfa~vbtyp_v = 'O' ) )
                LEFT OUTER JOIN vbkd ON vbkd~vbeln = vbfa~vbelv AND vbkd~posnr = ''
                LEFT OUTER JOIN vbap ON vbap~vbeln = vbrp~aubel AND vbap~posnr = vbrp~aupos
                LEFT OUTER JOIN t001w ON t001w~werks = vbap~werks
                LEFT OUTER JOIN t005u ON t005u~land1 = t001w~land1 AND t005u~bland = t001w~regio AND t005u~spras = 'E'
                LEFT OUTER JOIN vbpa  ON vbpa~vbeln  = vbap~vbeln  AND vbpa~posnr = '000000' AND vbpa~parvw = 'AG'
                LEFT OUTER JOIN adrc  ON adrc~addrnumber = vbpa~adrnr
                LEFT OUTER JOIN adr2  ON adr2~addrnumber = vbpa~adrnr AND adr2~r3_user = '3'
                LEFT OUTER JOIN adrc AS adrc1 ON adrc1~addrnumber = t001w~adrnr
                LEFT JOIN tvm5t ON vbap~mvgr5 = tvm5t~mvgr5 AND tvm5t~spras = 'E'
                LEFT JOIN zvmchar      AS mchar2 ON mara~matnr        = mchar2~objek      AND
                                          mchar2~atnam      = 'Z_SHENASEH'
                WHERE
                vbrk~rfbsk IN @s_rfbsk AND
                vbrk~bukrs = @p_bukrs AND
                vbrk~fkart IN @p_fkart AND
                vbrk~kunrg IN @p_kunrg AND
                vbrk~vbeln IN @p_vbeln
                AND vbrk~spart IN  @p_spart
                AND vbrk~kdgrp IN @p_kdgrp
                AND vbrk~fkdat IN @p_fkdat
                AND VBRK~ZUONR IN @P_ZUONR
*              AND ( ( bkpf~xblnr <> '' AND buchk = 'C' AND vf_status = 'A' AND vbrk~fkart NOT IN ('ZF42','ZF40','ZF17','ZF28' ,'ZF41') )
*              OR ( vbrk~fkart IN ('ZF42','ZF40','ZF17','ZF28','ZF41') ) )
                AND a~koaid = 'B' AND a~kstat = '' AND a~kinak = '' "comment
                AND c~kstat = '' AND ( c~kschl = 'ZVAT' OR c~kschl = 'ZVAD' ) AND tvfkt~spras = 'E' AND vbrk~rfbsk <> 'E' "comment
      GROUP BY  vbrk~vbeln , vbrk~fkart , tvfkt~vtext , vbrk~fkdat ,
                bkpf~xblnr , vbrp~posnr , vbrp~matnr , vbrp~arktx , vbrp~aubel , vbrp~aupos , vbrp~vgbel , vbrp~vgpos , vbrp~fkimg ,
                 a~kbetr , a~kwert , d~kwert , vbrk~cmwae , vbrk~zterm , vbrp~vrkme , vbrk~kunrg, vbrk~spart , vbrk~kdgrp, mara~prdha, g~charg,
                 vbkd~bstkd , vbkd~ihrez , vbkd~bstkd_e ,vbkd~ihrez_e, vbrk~sfakn,vbfa~vbeln, vbap~werks,

           t001w~name1,
           t001w~land1,
           t001w~regio,
           t005u~bezei,
           t001w~ort01,
           t001w~pstlz,
           adrc1~street,adrc1~house_num1,
      adrc1~tel_number,
      adrc1~fax_number,
            vbpa~kunnr,
      adrc~name1,
      adrc~city1,
      adrc~post_code1,
      adrc~street,
      adrc~tel_number,adr2~tel_number,vbap~mvgr5,
      tvm5t~bezei,
      mchar2~atwrt
      INTO CORRESPONDING FIELDS OF TABLE @lt_output .
  ELSE.
    SELECT DISTINCT
    vbrk~vbeln , vbrk~fkart , tvfkt~vtext , vbrk~fkdat , bkpf~xblnr , vbrk~cmwae , vbrk~zterm , vbrk~kunrg , vbrk~spart ,
    vbrk~kdgrp  , vbrp~posnr , vbrp~matnr , vbrp~vrkme , vbrp~arktx ,
    vbrp~aubel , vbrp~aupos , vbrp~vgbel , vbrp~vgpos , vbrp~fkimg , a~kbetr , a~kwert , g~charg , mara~prdha ,
    vbkd~bstkd ,
    CASE WHEN ZSD_POS_PAYM~TRACEID IS NULL THEN vbkd~ihrez
         ELSE ZSD_POS_PAYM~TRACEID END   AS ihrez,
    vbkd~bstkd_e ,vbkd~ihrez_e,
    vbrk~sfakn   AS sfakn1,
    vbfa~vbeln   AS sfakn2,
    vbap~werks,
    t001w~name1,
    t001w~land1,
    t001w~regio,
    t001w~ort01,
    t001w~pstlz,
    concat_with_space( adrc1~street,adrc1~house_num1, 1 ) AS stras,

    t005u~bezei,
    adrc1~tel_number AS tel_number1,
    adrc1~fax_number AS fax_number1,
    vbpa~kunnr,
    adrc~name1 AS name2,
    adrc~city1 AS city2,
    adrc~post_code1 AS post_code2,
    adrc~street AS street2,
    adrc~tel_number AS tel_number2,
    ( CASE WHEN adr2~tel_number IS NULL THEN MOB~TEL_NUMBER ELSE adr2~tel_number END ) AS MOBILE,
    vbap~mvgr5,
    tvm5t~bezei AS mvgr5t,
    mchar2~atwrt as shenaseh,
    vbak~bname as name,
    g~LGORT,
    CASE WHEN substring( vbrp~matnr,13,1 ) = '1' AND ( vbrk~kdgrp = 'CM' OR vbrk~kdgrp = 'CA' OR vbrk~kdgrp = 'CT' ) THEN 'OE'
    WHEN substring( vbrp~matnr,13,1 ) = '1' AND vbrk~kdgrp <> 'CM' AND vbrk~kdgrp <> 'CA' AND vbrk~kdgrp <> 'CT' THEN 'درجه1'
    WHEN substring( vbrp~matnr,13,1 ) = '2' THEN 'درجه2' END AS chargt,
    SUM( DISTINCT b~kwert ) AS discount ,
    SUM( DISTINCT c~kwert ) AS tax , d~kwert AS cleared_downpayment , SUM( DISTINCT e~kwert ) AS delivery_cost ,SUM( DISTINCT i~kwert ) AS delivery_post, SUM( DISTINCT f~kwert ) AS packaging_cost,
    SUM( DISTINCT j~kwert ) AS inventory_cost,
    SUM( DISTINCT k~kwert ) AS c1_cost,
    SUM( DISTINCT h~kwert ) AS c2_cost,
    SUM( DISTINCT l~kwert ) AS CostInstallation

    FROM vbrk INNER JOIN tvfkt  ON tvfkt~fkart = vbrk~fkart
    LEFT JOIN ZSD_POS_PAYM ON ZSD_POS_PAYM~VBELN = VBRK~ZUONR
    LEFT OUTER JOIN bkpf ON vbrk~vbeln = bkpf~awkey
    INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
    INNER JOIN  prcd_elements AS a ON a~knumv = vbrk~knumv AND  a~kposn = vbrp~posnr
    LEFT OUTER  JOIN prcd_elements AS b ON b~knumv = vbrk~knumv AND  b~kposn = vbrp~posnr AND b~koaid = 'A' AND b~kstat = '' AND ( b~kschl LIKE 'ZD%' OR b~kschl = 'R100')
    INNER JOIN  prcd_elements AS c ON c~knumv = vbrk~knumv AND  c~kposn = vbrp~posnr
    LEFT OUTER JOIN prcd_elements AS d ON d~knumv = vbrk~knumv AND  d~kposn = vbrp~posnr AND  d~kschl = 'AZWB'
    LEFT OUTER JOIN prcd_elements AS e ON e~knumv = vbrk~knumv AND  e~kposn = vbrp~posnr AND e~kstat = '' AND e~kschl LIKE 'ZH00' AND e~kinak = ''
    LEFT OUTER JOIN prcd_elements AS L ON l~knumv = vbrk~knumv AND  l~kposn = vbrp~posnr AND l~kstat = '' AND l~kschl LIKE 'ZH02' AND l~kinak = ''
    LEFT OUTER JOIN prcd_elements AS f ON f~knumv = vbrk~knumv AND  f~kposn = vbrp~posnr AND f~kstat = '' AND f~kschl LIKE 'ZK%' AND f~kinak = ''
    LEFT OUTER JOIN prcd_elements AS k ON k~knumv = vbrk~knumv AND  k~kposn = vbrp~posnr AND k~kschl = 'ZJIN' AND k~kinak = ''
    LEFT OUTER JOIN prcd_elements AS h ON h~knumv = vbrk~knumv AND  h~kposn = vbrp~posnr AND h~kschl = 'ZJGR' AND h~kinak = ''
    LEFT OUTER JOIN prcd_elements AS j ON j~knumv = vbrk~knumv AND  j~kposn = vbrp~posnr AND j~kstat = '' AND j~kschl LIKE 'ZA%' AND j~kinak = ''
    LEFT OUTER JOIN prcd_elements AS i ON i~knumv = vbrk~knumv AND  i~kposn = vbrp~posnr AND i~kstat = '' AND i~kschl LIKE 'ZH01' AND i~kinak = ''
    LEFT OUTER JOIN lips AS g ON g~vbeln = vbrp~vgbel AND g~posnr = vbrp~vgpos
    LEFT OUTER JOIN mara ON mara~matnr = vbrp~matnr
    LEFT OUTER JOIN vbfa ON vbfa~vbeln = vbrp~vbeln AND vbfa~posnn = vbrp~posnr AND vbfa~vbtyp_v = 'C'
*    LEFT OUTER JOIN vbfa ON vbfa~vbelv = vbrk~vbeln AND vbfa~posnv = vbrp~posnr AND
*    ( ( vbfa~vbtyp_n = 'N'  AND vbfa~vbtyp_v = 'M' )  OR
*    ( vbfa~vbtyp_n = 'S'  AND vbfa~vbtyp_v = 'O' ) )
    LEFT OUTER JOIN vbkd ON vbkd~vbeln = vbfa~vbelv AND vbkd~posnr = ''
    LEFT OUTER JOIN vbap ON vbap~vbeln = vbrp~aubel AND vbap~posnr = vbrp~aupos
    join vbak           on vbap~vbeln   = vbak~vbeln
    LEFT OUTER JOIN t001w ON t001w~werks = vbap~werks
    LEFT OUTER JOIN t005u ON t005u~land1 = t001w~land1 AND t005u~bland = t001w~regio AND t005u~spras = 'E'
    LEFT OUTER JOIN vbpa  ON vbpa~vbeln  = vbap~vbeln  AND vbpa~posnr = '000000' AND vbpa~parvw = 'AG'
    LEFT OUTER JOIN adrc  ON adrc~addrnumber = vbpa~adrnr
    LEFT OUTER JOIN adr2  ON adr2~addrnumber = vbpa~adrnr AND adr2~r3_user = '3'
    left JOIN adr2 as mob ON mob~addrnumber = vbpa~adrnr AND mob~r3_user = '1'
    LEFT OUTER JOIN adrc AS adrc1 ON adrc1~addrnumber = t001w~adrnr
    LEFT JOIN tvm5t ON vbap~mvgr5 = tvm5t~mvgr5 AND tvm5t~spras = 'E'
    LEFT JOIN zvmchar      AS mchar2 ON mara~matnr        = mchar2~objek      AND
                                      mchar2~atnam      = 'Z_SHENASEH'

    WHERE
    vbrk~rfbsk IN @s_rfbsk AND
    vbrk~bukrs = @p_bukrs AND
    vbrk~fkart IN @p_fkart AND
    vbrk~kunrg IN @p_kunrg AND
    vbrk~vbeln IN @p_vbeln
    AND vbrk~spart IN  @p_spart
    AND vbrk~kdgrp IN @p_kdgrp
    AND vbrk~fkdat IN @p_fkdat
    AND VBRK~ZUONR IN @P_ZUONR
    and vbkd~bstkd in @p_bstkd
*              AND ( ( bkpf~xblnr <> '' AND buchk = 'C' AND vf_status = 'A' AND vbrk~fkart NOT IN ('ZF42','ZF40','ZF17','ZF28' ,'ZF41') )
*              OR ( vbrk~fkart IN ('ZF42','ZF40','ZF17','ZF28','ZF41') ) )
    AND a~koaid = 'B' AND a~kstat = '' AND a~kinak = '' "comment
    AND c~kstat = '' AND ( c~kschl = 'ZVAT' OR c~kschl = 'ZVAD' ) AND tvfkt~spras = 'E' AND vbrk~rfbsk <> 'E' "comment
    AND ( ZSD_POS_PAYM~TRACEID IS NULL or ZSD_POS_PAYM~TRACEID = ( SELECT MAX( J~TRACEID )
                                  FROM ZSD_POS_PAYM AS J
                                 WHERE J~VBELN = VBRK~ZUONR ) )
    GROUP BY  vbrk~vbeln , vbrk~fkart , tvfkt~vtext , vbrk~fkdat ,
    bkpf~xblnr , vbrp~posnr , vbrp~matnr , vbrp~arktx , vbrp~aubel , vbrp~aupos , vbrp~vgbel , vbrp~vgpos , vbrp~fkimg ,
    a~kbetr , a~kwert , d~kwert , vbrk~cmwae , vbrk~zterm , vbrp~vrkme , vbrk~kunrg, vbrk~spart , vbrk~kdgrp, mara~prdha, g~charg,
    vbkd~bstkd , vbkd~ihrez , vbkd~bstkd_e ,vbkd~ihrez_e, vbrk~sfakn,vbfa~vbeln, vbap~werks,
    t001w~name1,
    t001w~land1,
    t001w~regio,
    t005u~bezei,
    t001w~ort01,
    t001w~pstlz,
    adrc1~street,adrc1~house_num1,
    adrc1~tel_number,
    adrc1~fax_number,
    vbpa~kunnr,
    adrc~name1,
    adrc~city1,
    adrc~post_code1,
    adrc~street,
    adrc~tel_number,adr2~tel_number, MOB~TEL_NUMBER,vbap~mvgr5,
    tvm5t~bezei,
    ZSD_POS_PAYM~TRACEID, mchar2~atwrt, vbak~bname,g~LGORT
    INTO CORRESPONDING FIELDS OF TABLE @lt_output .
  ENDIF.
  SORT lt_output BY vbeln ASCENDING.

  BREAK moghaddam.
  BREAK omrani.
  LOOP AT lt_output INTO ls_output.
    ls_output-c1_cost = ls_output-c1_cost * -1.
    ls_output-c2_cost = ls_output-c2_cost * -1.
    MODIFY lt_output FROM ls_output.
    AT END OF vbeln.
      SUM.
      APPEND ls_output TO lt_vbeln.
    ENDAT.

  ENDLOOP.

  CLEAR ls_error.
  LOOP AT lt_output INTO  ls_output
                  GROUP BY ls_output-spart.

    AUTHORITY-CHECK OBJECT  'V_VBAK_VKO'
      ID 'ACTVT' FIELD '03'
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
                  GROUP BY ls_output-lgort.

    AUTHORITY-CHECK OBJECT  'M_MSEG_LGO'
      ID 'ACTVT' FIELD '03'
      ID 'LGORT' FIELD ls_output-lgort.


    IF sy-subrc IS NOT INITIAL.
      CONCATENATE ' ,' ls_output-lgort INTO ls_error.
    ENDIF.


  ENDLOOP.

  IF strlen( ls_error ) > 0.
    MESSAGE i027(zfi) WITH ls_error.
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



  SELECT kunnr AS kunrg ktokd FROM kna1  INTO CORRESPONDING FIELDS OF TABLE lt_customer FOR ALL ENTRIES IN lt_output  WHERE kunnr = lt_output-kunrg AND kunnr IN p_kunrg.

  LOOP AT lt_customer INTO ls_customer.

    SELECT SINGLE name_org1 name_first name_last FROM but000 INTO (ls_customer-name_org1 , ls_customer-name_first , ls_customer-name_last) WHERE partner = ls_customer-kunrg.


    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_customer-ecco_no  WHERE taxtype = 'IR0' AND partner = ls_customer-kunrg.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_customer-register_no  WHERE taxtype = 'IR1' AND partner = ls_customer-kunrg.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_customer-birth_no  WHERE taxtype = 'IR2' AND partner = ls_customer-kunrg.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_customer-national_id  WHERE taxtype = 'IR3' AND partner = ls_customer-kunrg.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_customer-nationalcode  WHERE taxtype = 'IR4' AND partner = ls_customer-kunrg.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_customer-passport_no  WHERE taxtype = 'IR5' AND partner = ls_customer-kunrg.

    SELECT adrc~region adrc~city1 adrc~post_code1 adrc~street adrc~house_num1 UP TO 1 ROWS FROM but020
     INNER JOIN adrc ON adrc~addrnumber = but020~addrnumber
     INTO (ls_customer-region , ls_customer-city1 , ls_customer-post_code1 , ls_customer-street , ls_customer-house_num1 )
     WHERE partner = ls_customer-kunrg AND adrc~langu = p_lang  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    SELECT tel_number UP TO 1 ROWS FROM but020
     INNER JOIN adr2 ON adr2~addrnumber = but020~addrnumber
     INTO  ls_customer-tel_number
     WHERE partner = ls_customer-kunrg  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    SELECT fax_number UP TO 1 ROWS FROM but020
     INNER JOIN adr3 ON adr3~addrnumber = but020~addrnumber
     INTO  ls_customer-fax_number
     WHERE partner = ls_customer-kunrg  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    MODIFY lt_customer FROM ls_customer.

  ENDLOOP.



  SELECT SINGLE butxt , stceg , adrc~region , adrc~city1 , adrc~post_code1 , adrc~street , adrc~house_num1 , adrc~tel_number , adrc~fax_number , adrc~sort2
 FROM t001
    INNER JOIN adrc ON adrc~addrnumber = t001~adrnr
    INTO @DATA(bukrs)
    WHERE  adrc~langu = @p_lang AND bukrs = @p_bukrs.




  LOOP AT lt_output INTO ls_output.


    AUTHORITY-CHECK OBJECT  'Z_CUST_GRP'
      ID 'ACTVT' FIELD '03'
      ID 'KDGRP' FIELD ls_output-kdgrp.

    CHECK sy-subrc IS INITIAL.

    AUTHORITY-CHECK OBJECT  'V_VBAK_VKO'
      ID 'ACTVT' FIELD '03'
      ID 'SPART' FIELD ls_output-spart.

    CHECK sy-subrc IS INITIAL.

    AUTHORITY-CHECK OBJECT  'M_MSEG_LGO'
      ID 'ACTVT' FIELD '03'
      ID 'LGORT' FIELD ls_output-lgort.

    CHECK sy-subrc IS INITIAL.


    PERFORM get_vbeln_long_text.


    SELECT SINGLE mblnr FROM mkpf INTO ls_output-mblnr WHERE xblnr = ls_output-vgbel.

    MOVE-CORRESPONDING bukrs TO ls_output.


    ls_output-comp_nationalid = bukrs-sort2.

    READ TABLE lt_customer INTO ls_customer WITH TABLE KEY kunrg = ls_output-kunrg.
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

      ls_output-partner_region = ls_customer-region.
      ls_output-partner_city1 = ls_customer-city1.
      ls_output-partner_post_code1 = ls_customer-post_code1.
      ls_output-partner_street = ls_customer-street.
      ls_output-partner_house_num1 = ls_customer-house_num1.
      ls_output-partner_tel_number = ls_customer-tel_number.
      ls_output-partner_fax_number = ls_customer-fax_number.

*      IF ls_customer-ktokd = 'C006'.
*        SELECT SINGLE mtart FROM mara INTO @DATA(mtart) WHERE matnr = @ls_output-matnr.
*        IF mtart = 'ZFER'AND p_bukrs1 = '1100'.
*          CONCATENATE  'OE-' ls_output-arktx  INTO ls_output-arktx.
*        ENDIF.

*      ENDIF.

    ENDIF.

    CONCATENATE ls_output-matnr ls_output-charg INTO lv_object RESPECTING BLANKS.


    CLEAR lt_object.

    PERFORM readclassification USING  'Z_PP_BATCH' '023' 'MCH1'.


    LOOP AT lt_object INTO ls_object WHERE atnam = 'ZPP_WEIGHT'.

      IF ls_object-ausp1 CA ' ' .

        lv_index = sy-fdpos.

        IF lv_index > 1.

          lv_index = lv_index - 1.
          ls_output-ausp1 = ls_object-ausp1(lv_index).
          lv_index = lv_index + 2.
          ls_output-meins = ls_object-ausp1+lv_index(3).

        ENDIF.
      ENDIF.

    ENDLOOP.

    CLEAR lt_object.
    lv_object = ls_output-matnr.

    PERFORM readclassification USING  'Z_BR_PRODUCT_IRCOD' '001' 'MARA'.

    LOOP AT lt_object INTO ls_object WHERE atnam = 'Z_IRAN_CODE'.

      ls_output-irancode = ls_object-ausp1.

    ENDLOOP.
*    BREAK MOGHADDAM.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input    = ls_output-vrkme
        language = sy-langu
      IMPORTING
*       LONG_TEXT            =
        output   = ls_output-vrkme
*       SHORT_TEXT           =
*     EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS   = 2
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

    READ TABLE lt_vbeln INTO ls_output1 WITH TABLE KEY vbeln = ls_output-vbeln.
    IF sy-subrc IS INITIAL.
      ls_output-total_delivery_cost = ls_output1-delivery_cost.
      ls_output-total_inventory_cost = ls_output1-inventory_cost.
      ls_output-tot_c1_cost = ls_output1-c1_cost.
      ls_output-tot_c2_cost = ls_output1-c2_cost.
      ls_output-total_packaging_cost = ls_output1-packaging_cost.
      ls_output-total_netamount = ls_output1-kwert + ls_output1-discount + ls_output1-tax - ls_output1-c1_cost - ls_output1-c2_cost.
      ls_output-total_cleared_downpayment = ls_output1-cleared_downpayment.
      ls_output-payable_amount = ls_output-total_netamount + ls_output-total_packaging_cost + ls_output-total_delivery_cost - ls_output-total_cleared_downpayment + ls_output-CostInstallation.
      ls_output-netamount = ls_output-kwert + ls_output-discount + ls_output-tax - ls_output-c1_cost - ls_output-c2_cost.
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
  wa_fieldcat-fieldname = 'FKART'.
  wa_fieldcat-reptext = 'BILLINGTYPE'.

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
  wa_fieldcat-reptext = 'DELIVERY TYPE'.

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
  wa_fieldcat-fieldname = 'AUSP1'.
  wa_fieldcat-reptext = 'WEIGHT'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'IRANCODE'.
  wa_fieldcat-reptext = 'IRANCODE'.

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
  wa_fieldcat-fieldname = 'CostInstallation'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'هزينه خدمات نصب'.

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
  wa_fieldcat-fieldname = 'DELIVERY_post'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Delivery_post'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'INVENTORY_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Inventory_Cost'.

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
  wa_fieldcat-fieldname = 'TOTAL_INVENTORY_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'TOTAL_Inventory_Cost'.

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
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'C1_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Insurance'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'C2_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Guarantee'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'TOT_C1_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Total Insurance'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-ref_table = 'PRCD_ELEMENTS'.
  wa_fieldcat-ref_field = 'KWERT'.
  wa_fieldcat-cfieldname = 'CMWAE'.
  wa_fieldcat-fieldname = 'TOT_C2_COST'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Total Guarantee'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PAYABLE_AMOUNT2'.
  wa_fieldcat-reptext = 'payable_amount_text'.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BSTKD'.
  wa_fieldcat-reptext = 'Customer Reference'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'IHREZ'.
  wa_fieldcat-reptext = 'Your Reference'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BSTKD_E'.
  wa_fieldcat-reptext = 'Purchase Order No.'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'IHREZ_E'.
  wa_fieldcat-reptext = 'Your Reference'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SHENASEH'.
  wa_fieldcat-reptext = 'Shenaseh kala'.
  APPEND wa_fieldcat TO it_fieldcat.

*CLEAR wa_fieldcat.
*wa_fieldcat-fieldname = 'SFAKN1'.
*wa_fieldcat-reptext = 'Canclld Bill.Dc'.
*APPEND wa_fieldcat TO it_fieldcat.
*
*CLEAR wa_fieldcat.
*wa_fieldcat-fieldname = 'SFAKN2'.
*wa_fieldcat-reptext = 'Follow-on Doc.'.
*APPEND wa_fieldcat TO it_fieldcat.


  PERFORM add_cat USING 'WERKS' 'Plant' .
  PERFORM add_cat USING 'NAME1' 'Plant' .
  PERFORM add_cat USING 'LAND1' 'Country' .
  PERFORM add_cat USING 'REGIO' 'Region' .
  PERFORM add_cat USING 'BEZEI' 'Region' .
  PERFORM add_cat USING 'ORT01' 'City' .
  PERFORM add_cat USING 'PSTLZ' 'Postal Code' .
  PERFORM add_cat USING 'STRAS' 'Street' .
  PERFORM add_cat USING 'TEL_NUMBER1' 'Tel' .
  PERFORM add_cat USING 'FAX_NUMBER1' 'Fax' .
  PERFORM add_cat USING 'KUNNR' 'Customer' .
  PERFORM add_cat USING 'NAME2' 'Customer Name' .
  PERFORM add_cat USING 'CITY2' 'City' .
  PERFORM add_cat USING 'POST_CODE2' 'Postal Code' .
  PERFORM add_cat USING 'STREET2' 'Street' .
  PERFORM add_cat USING 'TEL_NUMBER2' 'Tel' .
  PERFORM add_cat USING 'MOBILE' 'Mobile' .
  PERFORM add_cat USING 'MVGR5' 'Product Group'.
  PERFORM add_cat USING 'MVGR5T' 'Product Group'.
  PERFORM add_cat USING 'CHARGT' 'گريد'.
  PERFORM add_cat USING 'TEXT' 'Billing Text'.
  PERFORM add_cat USING 'SFAKN1' 'Canclld Bill.Doc'.
  PERFORM add_cat USING 'SFAKN2' 'Follow-on Doc.'.
  PERFORM add_cat USING 'Name' 'شماره پلاک'.



  LOOP AT it_fieldcat INTO wa_fieldcat.
    MOVE-CORRESPONDING wa_fieldcat TO cs_fieldcat.
    cs_fieldcat-reptext_ddic = wa_fieldcat-reptext.
    cs_fieldcat-decimals_out = wa_fieldcat-decimals_o.
    APPEND cs_fieldcat TO ct_fieldcat.
  ENDLOOP.


  DATA: is_variant LIKE  disvariant.

  is_variant-report = sy-repid.
  IF sy-tcode = 'ZSD02'.
    is_variant-handle = 'ALV1'.
  ELSEIF  sy-tcode = 'ZSD29'.
    is_variant-handle = 'ALV2'.
  ELSEIF sy-tcode  = 'ZSD30'.
    is_variant-handle = 'ALV3'.
  ENDIF.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK  = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE    = ' '
      i_callback_program = sy-repid
      is_variant         = is_variant
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
      i_save             = 'X'
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
FORM readclassification USING class TYPE  klah-class classtype TYPE  klah-klart objecttable TYPE  tcla-obtab.
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
FORM add_cat  USING lv_1
                    lv_2.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = lv_1.
  wa_fieldcat-reptext = lv_2.
  APPEND wa_fieldcat TO it_fieldcat.


ENDFORM.


FORM get_vbeln_long_text .

  DATA: lv_name  LIKE thead-tdname,
        ls_tline TYPE tline,
        tline    TYPE TABLE OF tline WITH HEADER LINE.

  CLEAR : lv_name ,tline[] ,tline ,ls_tline.

  MOVE ls_output-vbeln TO lv_name.
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = '0002'
      language                = sy-langu
      name                    = lv_name
      object                  = 'VBBK'
    TABLES
      lines                   = tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    LOOP AT tline INTO  ls_tline.
      CONCATENATE ls_output-text ls_tline-tdline
                  INTO ls_output-text SEPARATED BY space.
    ENDLOOP.
  ENDIF.
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

  s_rfbsk-sign   = 'I'.
  s_rfbsk-option = 'NE'.
  s_rfbsk-low    = 'E'.

  APPEND s_rfbsk.

ENDFORM.
