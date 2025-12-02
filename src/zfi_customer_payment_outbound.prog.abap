*&---------------------------------------------------------------------*
*& Report ZFI_CUSTOMER_PAYMENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_customer_payment_outbound.

TABLES: bseg, but000, vbak.
TYPE-POOLS:slis.

DATA: BEGIN OF ls_result,
        bukrs       TYPE bukrs,
        budatfrom   TYPE budat,
        budatto     TYPE budat,
        bounced_boe TYPE char1,
        open_boe    TYPE char1,
        kunnr       TYPE bseg-kunnr,
        xref1       TYPE bseg-xref1,
        h_budat     TYPE bseg-h_budat,
        netdt       TYPE bseg-netdt,
        wrbtr       TYPE bseg-wrbtr,
        shkzg       TYPE bseg-shkzg,
        sgtxt       TYPE bseg-sgtxt,
        hbkid       TYPE bseg-hbkid,
        name_org1   TYPE but000-name_org1,
        bu_group    TYPE but000-bu_group,
        txt40       TYPE tb002-txt40,
        banka       TYPE bnka-banka,
        paymeth     TYPE bseg-umskz,
        paymeth_txt TYPE t042zt-text2,
        salesdoc    TYPE bseg-vbel2,
        auart       TYPE vbak-auart,
        bezei       TYPE tvakt-bezei,
        umskz       TYPE bseg-umskz,
        augbl       TYPE bseg-augbl,
        belnr       TYPE bseg-belnr,
        buzei       TYPE bseg-buzei,
        gjahr       TYPE bseg-gjahr,
        vbel2       TYPE bseg-vbel2,
        dmbtr       TYPE bseg-dmbtr,
        h_waers     TYPE bseg-h_waers,
        h_blart     TYPE bseg-h_blart,
        bschl       TYPE bseg-bschl,
        aedtm       TYPE begda,
        vbeln       TYPE lips-vbeln,
        matnr       TYPE lips-matnr,
        lfimg       TYPE lips-lfimg,
        ntgw        TYPE lips-ntgew,
        spart       TYPE lips-spart,
        wbstk       TYPE likp-wbstk,
        lfart       TYPE likp-lfart,
        kdgrp       TYPE likp-kdgrp,
        mvgr5       TYPE mvke-mvgr5,
  vmvgr5       TYPE mvke-mvgr5,
        vvbeln      TYPE vbak-vbeln ,
        vspart      TYPE vbap-spart ,
        vkgrp       TYPE vbak-vkgrp ,
        vkorg       TYPE vbak-vkorg,
        posnr       TYPE vbap-posnr ,
  vauart            TYPE vbak-auart,

      END OF ls_result,
      ls_result1         LIKE ls_result,
      lt_result          LIKE TABLE OF ls_result,
      lt_result_outbound LIKE TABLE OF ls_result.

DATA lv_index     TYPE i.
DATA lv_zounr     TYPE bseg-zuonr.
DATA lv_mandt     TYPE mandt.
DATA lv_but000    TYPE but000.
DATA it_fieldcat TYPE slis_t_fieldcat_alv.
DATA wa_fieldcat LIKE LINE OF it_fieldcat.
DATA ls_variant TYPE disvariant .
DATA lv_begda TYPE begda.
DATA lv_endda TYPE endda.
DATA lv_fbegda TYPE string.
DATA lv_fendda TYPE string.
DATA lv_fdate TYPE string.
DATA lv_month TYPE n LENGTH 2.
DATA lv_year TYPE n LENGTH 4.

Data: r_field type range of vbfa-vbelv,
      wa_field LIKE LINE OF r_field.





PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY,
            p_begda TYPE begda OBLIGATORY,
            p_vkorg TYPE vkorg.

SELECT-OPTIONS: p_kunnr   FOR bseg-kunnr,
                p_cgrp    FOR but000-bu_group,
                p_ordtyp FOR vbak-auart.

START-OF-SELECTION.

*TRY.
  CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
    EXPORTING
      im_datint   = p_begda
      im_datfmdes = 'C'
    IMPORTING
      ex_datext   = lv_fdate
*     ex_datfmused =
    .
* CATCH cx_abap_datfm_format_unknown .
*ENDTRY.

  lv_year = lv_fdate(4).
  lv_month = lv_fdate+5(2).


  CONCATENATE lv_year '/' lv_month '/01' INTO  lv_fbegda.

  if lv_month <> '12'.

  lv_month = lv_month + 1.
  else.
    lv_year = lv_year + 1.
    lv_month = '01'.
    ENDIF.

  CONCATENATE lv_year '/' lv_month '/01' INTO lv_fendda.

*TRY.
  CALL METHOD cl_abap_datfm=>conv_date_ext_to_int
    EXPORTING
      im_datext   = lv_fbegda
      im_datfmdes = 'C'
    IMPORTING
      ex_datint   = lv_begda
*     ex_datfmused =
    .
* CATCH cx_abap_datfm_no_date .
* CATCH cx_abap_datfm_invalid_date .
* CATCH cx_abap_datfm_format_unknown .
* CATCH cx_abap_datfm_ambiguous .
*ENDTRY.

  CALL METHOD cl_abap_datfm=>conv_date_ext_to_int
    EXPORTING
      im_datext   = lv_fendda
      im_datfmdes = 'C'
    IMPORTING
      ex_datint   = lv_endda
*     ex_datfmused =
    .
* CATCH cx_abap_datfm_no_date .
* CATCH cx_abap_datfm_invalid_date .
* CATCH cx_abap_datfm_format_unknown .
* CATCH cx_abap_datfm_ambiguous .
*ENDTRY.


  AUTHORITY-CHECK OBJECT  'F_PAYR_BUK'
    ID 'ACTVT' FIELD '03'
    ID 'BUKRS' FIELD p_bukrs.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e002(zfi) WITH p_bukrs.
    EXIT.
  ENDIF.



  SELECT DISTINCT SUM( bseg~dmbtr ) AS dmbtr, bseg~h_waers,  bseg~h_blart,
        bseg~bschl , bseg~bukrs, bseg~augbl  , bseg~buzei , bseg~gjahr ,
  bseg~umskz , bseg~kunnr , bseg~xref1 , bseg~h_budat , bseg~netdt , bseg~ZLSCH as paymeth,
    SUM( bseg~wrbtr ) AS wrbtr , bseg~shkzg , bseg~sgtxt , bseg~hbkid , bseg~vbel2, but000~name_org1
    , but000~bu_group , tb002~txt40 , bnka~banka , bseg~vbel2 , lips~matnr ,
 lips~lfimg , lips~ntgew ,lips~spart ,likp~wbstk , likp~lfart , likp~kdgrp, likp~vbeln ,mvke~mvgr5
    , vbak~vbeln as vvbeln , vbak~vkgrp , vbap~spart as vspart , vbak~vkorg, vbap~posnr , vbak~auart as vauart , m~mvgr5 as vmvgr5
     FROM vbak
   left OUTER JOIN vbfa on vbak~vbeln = vbfa~vbelv
    INNER JOIN vbap on vbak~vbeln = vbap~vbeln
    left OUTER JOIN likp ON likp~vbeln = vbfa~vbeln AND vbfa~vbtyp_n = 'J' and likp~bldat = @p_begda
    left OUTER JOIN lips ON lips~vbeln = likp~vbeln
    inner JOIN bseg ON vbak~vbeln = bseg~vbel2  AND bseg~koart = 'D'

    inner JOIN bkpf  ON bseg~bukrs = bkpf~bukrs AND bseg~belnr = bkpf~belnr AND bseg~gjahr = bkpf~gjahr

    left OUTER JOIN but000 ON but000~partner = bseg~kunnr" and but000~bu_group IN @p_cgrp
    LEFT OUTER JOIN tb002 ON tb002~bu_group = but000~bu_group AND spras = 'E'
    LEFT OUTER JOIN t012 ON t012~hbkid = bseg~hbkid AND t012~bukrs =
    bseg~bukrs AND  t012~banks = 'IR'
    LEFT OUTER JOIN bnka ON bnka~bankl = t012~bankl AND  bnka~banks = 'IR'
    LEFT OUTER JOIN mvke as m ON m~matnr = vbap~matnr AND m~vkorg = @p_vkorg AND m~vtweg = '20'
    LEFT OUTER JOIN mvke ON mvke~matnr = lips~matnr AND mvke~vkorg = @p_vkorg AND mvke~vtweg = '20'
    INTO CORRESPONDING FIELDS OF TABLE @lt_result
    WHERE  vbak~vbtyp = 'C'
     AND ( ( umskz = 'A' AND bseg~bschl = '19' ) OR ( bseg~bschl = '09' AND umskz = 'F' AND augbl = '' ) OR ( bseg~bschl = '09'
    AND umskz = 'W' ) OR ( bseg~bschl = '15' AND umskz = '' ) ) "and bseg~kunnr IN @p_kunnr "and auart in @p_auart and
aND bkpf~bukrs = @p_bukrs AND BUDAT  BETWEEN @lv_begda AND @lv_endda and bkpf~stblg = '' AND ( blart = 'DA' OR blart = 'DZ' OR blart = 'Z2'  OR blart = 'OO' )
     GROUP BY  bseg~h_waers,  bseg~h_blart,
        bseg~bschl , bseg~bukrs, bseg~augbl  , bseg~buzei , bseg~gjahr ,
  bseg~umskz , bseg~kunnr , bseg~xref1 , bseg~h_budat , bseg~netdt ,
     bseg~shkzg , bseg~sgtxt , bseg~hbkid , but000~name_org1
    , but000~bu_group , tb002~txt40 , bnka~banka , bseg~vbel2 , lips~matnr ,
 lips~lfimg , lips~ntgew ,lips~spart ,likp~wbstk , likp~lfart , likp~kdgrp, likp~vbeln ,mvke~mvgr5 , bseg~ZLSCH
, vbak~vbeln, vbak~vkgrp , vbap~spart , vbak~vkorg, vbap~posnr , vbak~auart, m~mvgr5.

  LOOP AT lt_result INTO ls_result.
    lv_index = sy-tabix.
    IF ls_result-umskz = 'A'.
      LOOP AT lt_result INTO ls_result1  WHERE bukrs = ls_result-bukrs
      AND belnr = ls_result-belnr AND gjahr = ls_result-gjahr AND
      umskz = 'W'.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        DELETE lt_result INDEX lv_index.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF ls_result-umskz = 'W' AND ls_result-shkzg = 'S' AND
    ls_result-augbl <> ''.
      CONCATENATE ls_result-belnr ls_result-buzei ls_result-gjahr INTO
      lv_zounr.
      SELECT SINGLE mandt FROM bsid INTO lv_mandt WHERE kunnr =
      ls_result-kunnr AND bukrs = ls_result-bukrs AND bschl = '09' AND
      umskz = 'S'.
      IF sy-subrc IS INITIAL.
        ls_result-bounced_boe = 'X'.
      ENDIF.
      SELECT SINGLE mandt FROM bsad INTO lv_mandt WHERE kunnr =
      ls_result-kunnr AND bukrs = ls_result-bukrs AND bschl = '09' AND
      umskz = 'S'.
      IF sy-subrc IS INITIAL.
        ls_result-bounced_boe = 'X'.
      ENDIF.

    ENDIF.
    IF ls_result-umskz = 'W' AND ls_result-shkzg = 'S' AND
    ls_result-augbl = ''.
      ls_result-open_boe = 'X'.
    ENDIF.

*    IF ls_result-umskz = 'W'.
*      ls_result-paymeth = 'N'.
*    ENDIF.
*
*    "IF ls_result-umskz = 'A'.
*      SELECT SINGLE zlsch  FROM bsad INTO ls_result-paymeth
*        WHERE augbl = ls_result-belnr AND bukrs = ls_result-bukrs
*        "AND buzei = ls_result-buzei
*         AND umskz = 'F'.
*    "ENDIF.

    SELECT SINGLE text2 FROM t042zt INTO ls_result-paymeth_txt WHERE
    zlsch = ls_result-paymeth AND land1 = 'IR' AND spras = 'EN'.

    IF ls_result-vbel2 = '' AND ls_result-umskz = 'W'.
      SELECT SINGLE vbel2 FROM bseg INTO ls_result-vbel2
        WHERE belnr = ls_result-belnr AND bukrs = ls_result-bukrs AND
        vbel2 <> ''.

    ENDIF.

    SELECT SINGLE auart FROM vbak INTO ls_result-auart WHERE vbeln =
    ls_result-vbel2.

    IF sy-subrc IS INITIAL.
      IF ls_result-auart NOT IN p_ordtyp.
        DELETE lt_result INDEX lv_index.
        CONTINUE.
      ENDIF.
    ENDIF.

    SELECT SINGLE bezei FROM  tvakt INTO ls_result-bezei WHERE spras =
    'EN' AND auart = ls_result-auart.

*    IF ls_result-shkzg = 'H'.
*      ls_result-wrbtr = ls_result-wrbtr * -1.
*      ls_result-dmbtr = ls_result-dmbtr * -1.
*    ENDIF.

    ls_result-budatfrom = lv_begda.
    ls_result-budatto = lv_endda.

    IF ls_result-name_org1 IS INITIAL.
      CLEAR lv_but000.
      SELECT SINGLE * INTO lv_but000 FROM but000 WHERE partner = ls_result-kunnr.
      CONCATENATE lv_but000-name_first lv_but000-name_last INTO ls_result-name_org1 SEPARATED BY space.
    ENDIF.


    IF ls_result-bschl = '15'.
      READ TABLE lt_result TRANSPORTING NO FIELDS WITH KEY belnr = ls_result-belnr
                                                           bukrs = ls_result-bukrs
                                                           umskz = 'W'.
      IF sy-subrc EQ 0.
        CLEAR ls_result-belnr.
      ENDIF.
    ENDIF.

    ls_result-aedtm = sy-datum.

    MODIFY lt_result FROM ls_result.


  ENDLOOP.
  loop at lt_result INTO ls_result.

    wa_field-sign = 'I'.
    wa_field-option = 'EQ'.
    wa_field-low = ls_result-vbel2.
    append wa_field to r_field.

    ENDLOOP.
 SELECT  DISTINCT vbfa~vbelv AS vbel2 , lips~vbeln , lips~matnr ,
lips~lfimg ,  lips~ntgew , lips~spart , likp~wbstk ,  likp~lfart ,  likp~kdgrp , mvke~mvgr5
   , vbak~vbeln as vvbeln , vbak~vkgrp , vbap~spart as vspart , vbak~vkorg, vbap~posnr , vbak~auart as vauart , m~mvgr5 as vmvgr5
   FROM vbfa

    INNER JOIN vbak on vbak~vbeln = vbfa~vbelv
    INNER JOIN vbap on vbak~vbeln = vbap~vbeln
      INNER JOIN likp ON likp~vbeln = vbfa~vbeln
      INNER JOIN lips ON lips~vbeln = likp~vbeln
       LEFT OUTER JOIN mvke ON mvke~matnr = lips~matnr and   mvke~vkorg = @p_vkorg AND mvke~vtweg = '20'
   LEFT OUTER JOIN mvke as m ON m~matnr = vbap~matnr and  m~vkorg = @p_vkorg AND m~vtweg = '20'
      INTO CORRESPONDING FIELDS OF TABLE @lt_result_outbound

      WHERE vbfa~vbelv not in @r_field and bldat = @p_begda  AND vbfa~vbtyp_n = 'J' .


  LOOP AT lt_result_outbound INTO ls_result.
    APPEND ls_result TO lt_result.
  ENDLOOP.



  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BSEG'.
  wa_fieldcat-fieldname = 'BUKRS'.
  wa_fieldcat-reptext_ddic = 'Company Code'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BUDATFROM'.
  wa_fieldcat-reptext_ddic = 'Posting Date From'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BUDATTO'.
  wa_fieldcat-reptext_ddic = 'Posting Date To'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KUNNR'.
  wa_fieldcat-reptext_ddic = 'Customer'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'NAME_ORG1'.
  wa_fieldcat-reptext_ddic = 'Customer Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BU_GROUP'.
  wa_fieldcat-reptext_ddic = 'Customer Group'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'TXT40'.
  wa_fieldcat-reptext_ddic = 'Customer Group Text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PAYMETH'.
  wa_fieldcat-reptext_ddic = 'Payment Method'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PAYMETH_TXT'.
  wa_fieldcat-reptext_ddic = 'Payment Method Text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VBEL2'.
  wa_fieldcat-hotspot = 'X'.
  wa_fieldcat-reptext_ddic = 'Sales Doc'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AUART'.
  wa_fieldcat-reptext_ddic = 'Order Type'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BEZEI'.
  wa_fieldcat-reptext_ddic = 'Order Type Text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'XREF1'.
  wa_fieldcat-reptext_ddic = 'Reference1'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'H_BUDAT'.
  wa_fieldcat-reptext_ddic = 'Posting Date'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'NETDT'.
  wa_fieldcat-reptext_ddic = 'Net Due Date'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'WRBTR'.
  wa_fieldcat-reptext_ddic = 'Amount'.
  wa_fieldcat-cfieldname = 'H_WAERS'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SHKZG'.
  wa_fieldcat-reptext_ddic = 'Debit/Credit Ind.'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SGTXT'.
  wa_fieldcat-reptext_ddic = 'Doc_Text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'HBKID '.
  wa_fieldcat-reptext_ddic = 'House_Bank'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BANKA'.
  wa_fieldcat-reptext_ddic = 'House_Bank_Text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'DMBTR'.
  wa_fieldcat-reptext_ddic = 'Amount_Local_Currency'.
  wa_fieldcat-currency = 'IRR'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'H_WAERS'.
  wa_fieldcat-reptext_ddic = 'Currency'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'UMSKZ'.
  wa_fieldcat-reptext_ddic = 'Special_G/L_Indicator'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'H_BLART'.
  wa_fieldcat-reptext_ddic = 'Document_Type'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BSCHL'.
  wa_fieldcat-reptext_ddic = 'Posting_Key'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BELNR'.
  wa_fieldcat-hotspot = 'X'.
  wa_fieldcat-reptext_ddic = 'Document_Number'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BOUNCED_BOE'.
  wa_fieldcat-reptext_ddic = 'bounced_boe'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'OPEN_BOE'.
  wa_fieldcat-reptext_ddic = 'open_boe'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AEDTM'.
  wa_fieldcat-reptext_ddic = 'Current Date'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBFA'.
  wa_fieldcat-fieldname = 'VBELN'.
  wa_fieldcat-reptext_ddic = 'Outbound'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIKP'.
  wa_fieldcat-fieldname = 'WBSTK'.
  wa_fieldcat-reptext_ddic = 'delivery status'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIKP'.
  wa_fieldcat-fieldname = 'LFART'.
  wa_fieldcat-reptext_ddic = 'delivery type'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIKP'.
  wa_fieldcat-fieldname = 'KDGRP'.
  wa_fieldcat-reptext_ddic = 'out bound customer group'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-reptext_ddic = 'Material'.

  APPEND wa_fieldcat TO it_fieldcat.



  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'LFIMG'.
  wa_fieldcat-reptext_ddic = 'LFIMG'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'NTGEW'.
  wa_fieldcat-reptext_ddic = 'NTGEW'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'SPART'.
  wa_fieldcat-reptext_ddic = 'SPART'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'MVKE'.
  wa_fieldcat-fieldname = 'MVGR5'.
  wa_fieldcat-reptext_ddic = 'MVGR5'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'VVBELN'.
  wa_fieldcat-reptext_ddic = 'VVBELN'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'VKGRP'.
  wa_fieldcat-reptext_ddic = 'VKGRP'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'LIPS'.
  wa_fieldcat-fieldname = 'VKORG'.
  wa_fieldcat-reptext_ddic = 'VKORG'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBAK'.
  wa_fieldcat-fieldname = 'VSPART'.
  wa_fieldcat-reptext_ddic = 'VSPART'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBAP'.
  wa_fieldcat-fieldname = 'POSNR'.
  wa_fieldcat-reptext_ddic = 'POSNR'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'VBAK'.
  wa_fieldcat-fieldname = 'VAUART'.
  wa_fieldcat-reptext_ddic = 'VAUART'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'MVKE'.
  wa_fieldcat-fieldname = 'VMVGR5'.
  wa_fieldcat-reptext_ddic = 'VMVGR5'.

  APPEND wa_fieldcat TO it_fieldcat.






  ls_variant-report = sy-repid.





  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = sy-repid
*     I_INTERFACE_CHECK       = ' '
      i_bypassing_buffer      = 'A'
*     I_BUFFER_ACTIVE         = ' '
*     I_CALLBACK_PROGRAM      = ' '
*     I_CALLBACK_PF_STATUS_SET          = ' '
      i_callback_user_command = 'HANDLE_USER_COMMAND'
*     I_CALLBACK_TOP_OF_PAGE  = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME        =
*     I_BACKGROUND_ID         = ' '
*     I_GRID_TITLE            =
*     I_GRID_SETTINGS         =
*     IS_LAYOUT               =
      it_fieldcat             = it_fieldcat
*     IT_EXCLUDING            =
*     IT_SPECIAL_GROUPS       =
*     IT_SORT                 =
*     IT_FILTER               =
*     IS_SEL_HIDE             =
*     I_DEFAULT               = 'X'
      i_save                  = 'X'
      is_variant              = ls_variant
*     IT_EVENTS               =
*     IT_EVENT_EXIT           =
*     IS_PRINT                =
*     IS_REPREP_ID            =
*     I_SCREEN_START_COLUMN   = 0
*     I_SCREEN_START_LINE     = 0
*     I_SCREEN_END_COLUMN     = 0
*     I_SCREEN_END_LINE       = 0
*     I_HTML_HEIGHT_TOP       = 0
*     I_HTML_HEIGHT_END       = 0
*     IT_ALV_GRAPHICS         =
*     IT_HYPERLINK            =
*     IT_ADD_FIELDCAT         =
*     IT_EXCEPT_QINFO         =
*     IR_SALV_FULLSCREEN_ADAPTER        =
*     IMPORTING
*     E_EXIT_CAUSED_BY_CALLER =
*     ES_EXIT_CAUSED_BY_USER  =
    TABLES
      t_outtab                = lt_result
*     EXCEPTIONS
*     PROGRAM_ERROR           = 1
*     OTHERS                  = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

FORM handle_user_command USING r_ucomm LIKE sy-ucomm
                             rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN'&IC1'.
      CASE rs_selfield-fieldname.
        WHEN 'BELNR'.

          READ TABLE lt_result INTO ls_result INDEX rs_selfield-tabindex.
          IF sy-subrc IS INITIAL.

            SET PARAMETER ID 'BLN' FIELD rs_selfield-value.
            SET PARAMETER ID 'BUK' FIELD ls_result-bukrs.
            SET PARAMETER ID 'GJR' FIELD ls_result-h_budat(4).
            CALL TRANSACTION 'FB03'AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'VBEL2'.


          SET PARAMETER ID 'AUN' FIELD rs_selfield-value.

          CALL TRANSACTION 'VA03'AND SKIP FIRST SCREEN.

      ENDCASE.

  ENDCASE.

ENDFORM.
