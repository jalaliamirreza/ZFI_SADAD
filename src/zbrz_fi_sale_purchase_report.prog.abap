*&---------------------------------------------------------------------*
*& Report ZBRZ_FI_SALE_PURCHASE_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbrz_fi_sale_purchase_report_1.

TABLES:bkpf,LFA1,VBRK, ADRP, rseg.

DATA: BEGIN OF ls_output,
        fldkey         TYPE C LENGTH 30, "<<---S.A--->>
        bukrs          TYPE   bkpf-bukrs,
        blart          TYPE   bkpf-blart,
        butxt          TYPE   t001-butxt,
        stceg          TYPE   t001-stceg,
        gjahr          TYPE   bkpf-gjahr,
        monat          TYPE   bkpf-monat,
        belnr          TYPE   bkpf-belnr,
        budat          TYPE   bkpf-budat,
        xblnr          TYPE   bkpf-xblnr,
        kursf          TYPE   bkpf-kursf,
        rbelnr         TYPE   rbkp-belnr,
        bktxt          TYPE   rbkp-bktxt,
        xrech          TYPE    rbkp-xrech,
        xmwst          TYPE    rbkp-xmwst,
        kidno          TYPE    rbkp-kidno,
        ebeln          TYPE   rseg-ebeln,
        ebelp          TYPE   rseg-ebelp,
        cobl_nr        TYPE   rbco-cobl_nr,
        r1cobl_nr      TYPE   rbco-cobl_nr,
        buzei          TYPE   rseg-buzei,
        konnr          TYPE   ekpo-konnr,
        bedat          TYPE    ekko-bedat,
        kschl          TYPE   rseg-kschl,
        matnr          TYPE   rseg-matnr,
        maktx          TYPE   makt-maktx,
        pstyp          TYPE    ekpo-pstyp,
        txz01          TYPE   ekpo-txz01,
        matkl          TYPE   ekpo-matkl,
        wgbez          TYPE   t023t-wgbez,
        vtext          TYPE   t685t-vtext,
        anln1          TYPE   rbco-anln1,
        rwrbtr         TYPE   rbco-wrbtr,
        rmwskz         TYPE   rbco-mwskz,
        wrbtr          TYPE   rseg-wrbtr,
        mwskz          TYPE   rseg-mwskz,
        waers          TYPE   rbkp-waers,
        wmwst1         TYPE   rbkp-wmwst1,
        lifnr          TYPE   rbkp-lifnr,
        shkzg          TYPE   rseg-shkzg,
        vat            TYPE   konp-kbetr,
        vad            TYPE   konp-kbetr,
        vatirr         TYPE   konp-kbetr,
        vadirr         TYPE   konp-kbetr,
        but_type       TYPE   but000-type,
        bu_group       TYPE   but000-bu_group,
        name_org1      TYPE but000-name_org1,
        name_first     TYPE but000-name_first,
        name_last      TYPE but000-name_last,
        ecco_no        TYPE dfkkbptaxnum-taxnum,
        register_no    TYPE dfkkbptaxnum-taxnum,
        birth_no       TYPE dfkkbptaxnum-taxnum,
        national_id    TYPE dfkkbptaxnum-taxnum,
        nationalcode   TYPE dfkkbptaxnum-taxnum,
        passport_no    TYPE dfkkbptaxnum-taxnum,
        country        TYPE adrc-country,
        country_t      TYPE t005t-landx,
        region         TYPE adrc-region,
        city1          TYPE adrc-city1,
        post_code1     TYPE adrc-post_code1,
        street         TYPE adrc-street,
        house_num1     TYPE adrc-house_num1,
        tel_number     TYPE adrc-tel_number,
        fax_number     TYPE adrc-fax_number,
        postingquarter TYPE n LENGTH 1,
        vendortypecode TYPE c LENGTH 30,
        vendortype     TYPE c LENGTH 30,
        counter        TYPE i,
        waersirr       TYPE waers VALUE 'IRR',
        wrbtrirr       TYPE   rseg-wrbtr,
        wmwst          TYPE   rbtx-wmwst,
        hwste          TYPE   rbtx-hwste,
        fwbas          TYPE   rbtx-fwbas,
        hwbas          TYPE   rbtx-hwbas,
        mmwskz         TYPE mwskz,
        r1mwskz        TYPE mwskz,
        mwrbtr         TYPE wrbtr,
        r1wrbtr        TYPE wrbtr,
        rbuzei         TYPE buzei,
        txt50          TYPE skat-txt50,
        saknr          TYPE saknr,
        usnam          TYPE RBKP-USNAM,
        UFIRST         TYPE ADRP-NAME_FIRST,
        ULAST          TYPE ADRP-NAME_LAST,
        MENGE          type rseg-MENGE,
        meins          type rseg-meins,
      END OF ls_output,
      lt_result LIKE TABLE OF ls_output.

DATA: BEGIN OF ls_output1,
        bukrs          TYPE   bkpf-bukrs,
        butxt          TYPE   t001-butxt,
        stceg          TYPE   t001-stceg,
        gjahr          TYPE   bkpf-gjahr,
        monat          TYPE   bkpf-monat,
        belnr          TYPE   bkpf-belnr,
        budat          TYPE   bkpf-budat,
        blart          TYPE   bkpf-blart,
        xblnr          TYPE   bkpf-xblnr,
        kursf          TYPE   bkpf-kursf,
        vbeln          TYPE   vbrp-vbeln,
        posnr          TYPE   vbrp-posnr,
        knumv          TYPE   vbrk-knumv,
        fkart          TYPE   vbrk-fkart,
        matnr          TYPE   vbrp-matnr,
        arktx          TYPE   vbrp-arktx,
        matkl          TYPE   mara-matkl,
        wgbez          TYPE   t023t-wgbez,
        aubel          TYPE   vbrp-aubel,
        aupos          TYPE   vbrp-aupos,
        fkimg          TYPE   vbrp-fkimg,
        vrkme          TYPE   vbrp-vrkme,
        waers          TYPE   rbkp-waers,
        kunrg          TYPE   vbrk-kunrg,
        vbtyp          TYPE   vbrk-vbtyp,
        country        TYPE adrc-country,
        country_t      TYPE t005t-landx,
        region         TYPE adrc-region,
        city1          TYPE adrc-city1,
        post_code1     TYPE adrc-post_code1,
        street         TYPE adrc-street,
        house_num1     TYPE adrc-house_num1,
        fax_number     TYPE adrc-fax_number,
        tel_number     TYPE adrc-tel_number,
        kwert          TYPE   prcd_elements-kwert,
        discount       TYPE   prcd_elements-kwert,
        vat            TYPE   prcd_elements-kwert,
        vad            TYPE   prcd_elements-kwert,
        deliverycost   TYPE   prcd_elements-kwert,
        packingcost    TYPE   prcd_elements-kwert,
        deliverycosirr TYPE   prcd_elements-kwert,
        packingcostirr TYPE   prcd_elements-kwert,
        amountirr      TYPE   prcd_elements-kwert,
        amountbeftax   TYPE   prcd_elements-kwert,
        amountbetaxirr TYPE   prcd_elements-kwert,
        tax            TYPE   prcd_elements-kwert,
        taxirr         TYPE   prcd_elements-kwert,
        discountirr    TYPE   prcd_elements-kwert,
        vatirr         TYPE   prcd_elements-kwert,
        vadirr         TYPE   prcd_elements-kwert,
        type           TYPE   but000-type,
        name_org1      TYPE but000-name_org1,
        name_first     TYPE but000-name_first,
        name_last      TYPE but000-name_last,
        ecco_no        TYPE dfkkbptaxnum-taxnum,
        register_no    TYPE dfkkbptaxnum-taxnum,
        birth_no       TYPE dfkkbptaxnum-taxnum,
        national_id    TYPE dfkkbptaxnum-taxnum,
        nationalcode   TYPE dfkkbptaxnum-taxnum,
        passport_no    TYPE dfkkbptaxnum-taxnum,
        but_type       TYPE   but000-type,
        bu_group       TYPE   but000-bu_group,
        postingquarter TYPE n LENGTH 1,
        vendortypecode TYPE c LENGTH 30,
        vendortype     TYPE c LENGTH 30,
        counter        TYPE i,
        waersirr       TYPE   waers VALUE 'IRR',
        shkzg          TYPE shkzg,
      END OF ls_output1.


DATA: BEGIN OF ls_belnr,
        awkey TYPE awkey,
        belnr TYPE rbkp-belnr,
        gjahr TYPE gjahr,
      END OF ls_belnr,
      lt_belnr LIKE STANDARD TABLE OF ls_belnr WITH KEY awkey.

DATA   ecco_no      TYPE dfkkbptaxnum-taxnum.
DATA   register_no  TYPE dfkkbptaxnum-taxnum.
DATA   birth_no     TYPE dfkkbptaxnum-taxnum.
DATA   national_id  TYPE dfkkbptaxnum-taxnum.
DATA   nationalcode TYPE dfkkbptaxnum-taxnum.
DATA   passport_no  TYPE dfkkbptaxnum-taxnum.
DATA   region       TYPE adrc-region.
DATA   city1        TYPE  adrc-city1.
DATA   post_code1   TYPE adrc-post_code1.
DATA   street       TYPE adrc-street.
DATA   house_num1   TYPE adrc-house_num1.
DATA   tel_number   TYPE adr2-tel_number.
DATA   fax_number   TYPE adr3-fax_number.
DATA   name_org1    TYPE but000-name_org1.
DATA   name_first    TYPE but000-name_first.
DATA   name_last    TYPE but000-name_last.
DATA   wa_fieldcat  TYPE LINE OF lvc_t_fcat.
DATA   it_fieldcat  TYPE lvc_t_fcat.
DATA   ct_fieldcat  TYPE slis_t_fieldcat_alv.
DATA   cs_fieldcat  TYPE slis_fieldcat_alv.
DATA   lt_output LIKE STANDARD TABLE OF ls_output.
DATA   lt_output1 LIKE STANDARD TABLE OF ls_output1.
DATA   ls_period TYPE i.
DATA   flag.
DATA   flag2.
Data:  lt_output2 LIKE STANDARD TABLE OF ls_output, "<<---S.A--->>
       tmp_amount        type rseg-wrbtr,  "<<---S.A--->>
       tmp_tax_amount    type bseg-WMWST,  "<<---S.A--->>
       tmp_amountIRR     type prcd_elements-kwert,  "<<---S.A--->>
       tmp_tax_amountIRR type prcd_elements-kwert,  "<<---S.A--->>
       wa_tmp            LIKE  LS_OUTPUT. "<<---S.A--->>

DATA: ls_variant TYPE  disvariant.
DATA: BEGIN OF ls_taxnum ,
        taxnum  TYPE  dfkkbptaxnum-taxnum,
        taxtype TYPE  dfkkbptaxnum-taxtype,
      END OF ls_taxnum,
      lt_taxnum LIKE TABLE OF ls_taxnum WITH HEADER LINE.


ls_variant-report = sy-repid.
ls_variant-username = sy-uname.


CLASS lcl_alv_events DEFINITION.
  PUBLIC SECTION.
    METHODS:handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        e_row_id
        e_column_id
        es_row_no
        sender.
ENDCLASS.

DATA: cl_alv_cont       TYPE REF TO cl_gui_custom_container,
      cl_alv_grid       TYPE REF TO cl_gui_alv_grid,
      cl_event_receiver TYPE REF TO lcl_alv_events,
      cl_alv_cont1      TYPE REF TO cl_gui_custom_container,
      cl_alv_grid1      TYPE REF TO cl_gui_alv_grid.
"cl_event_receiver1 TYPE REF TO lcl_alv_events.

CREATE OBJECT cl_event_receiver.


SET HANDLER cl_event_receiver->handle_hotspot_click FOR ALL INSTANCES.


CLASS lcl_alv_events IMPLEMENTATION.

  METHOD handle_hotspot_click.
*    DATA:
*          ls_insname TYPE string.
*     DATA:
*    lref_obj TYPE REF TO cl_abap_typedescr.
*
*  lref_obj ?= cl_abap_objectdescr=>describe_by_object_ref( me ).
*
*    ls_insname = lref_obj->get_relative_name( ).

*    Case sender.
*      WHEN CL_ALV_GRID1.
*              CASE e_column_id.
*        WHEN 'BELNR'.
*
*          READ TABLE lt_output INTO ls_output INDEX es_row_no-row_id.
*          IF sy-subrc IS INITIAL.
*
*            SET PARAMETER ID 'BLN' FIELD ls_output-belnr.
*            SET PARAMETER ID 'BUK' FIELD ls_output-bukrs.
*            SET PARAMETER ID 'GJR' FIELD ls_output-budat(4).
*            CALL TRANSACTION 'FB03'AND SKIP FIRST SCREEN.
*
*          ENDIF.
*
*        WHEN 'AUBEL'.
*          READ TABLE lt_output INTO ls_output INDEX es_row_no-row_id.
*          IF sy-subrc IS INITIAL.
*
*
*          SET PARAMETER ID 'AUN' FIELD ls_output-au.
*
*          CALL TRANSACTION 'VA03'AND SKIP FIRST SCREEN.
*
*        WHEN 'VBELN'.
*
*          READ TABLE lt_output INTO ls_output INDEX es_row_no-row_id.
*          IF sy-subrc IS INITIAL.
*
*          SET PARAMETER ID 'VF' FIELD ls_output-vb
*
*          CALL TRANSACTION 'VF03'AND SKIP FIRST SCREEN.
*
*      ENDCASE.
*        when CL_ALV_GRID.
*                CASE rs_selfield-fieldname.
*        WHEN 'BELNR'.
*
*          READ TABLE lt_output1 INTO ls_output1 INDEX rs_selfield-tabindex.
*          IF sy-subrc IS INITIAL.
*
*            SET PARAMETER ID 'BLN' FIELD rs_selfield-value.
*            SET PARAMETER ID 'BUK' FIELD ls_output1-bukrs.
*            SET PARAMETER ID 'GJR' FIELD ls_output1-budat(4).
*            CALL TRANSACTION 'FB03'AND SKIP FIRST SCREEN.
*
*          ENDIF.
*
*        WHEN 'AUBEL'.
*
*
*          SET PARAMETER ID 'AUN' FIELD rs_selfield-value.
*
*          CALL TRANSACTION 'VA03'AND SKIP FIRST SCREEN.
*
*        WHEN 'VBELN'.
*
*          SET PARAMETER ID 'VF' FIELD rs_selfield-value.
*
*          CALL TRANSACTION 'VF03'AND SKIP FIRST SCREEN.
*
*      ENDCASE.
*          ENDCASE.

*    DATA: lv_sum   TYPE wrbtr.
*    READ TABLE lt_result ASSIGNING <fs_result>
*                             INDEX e_row_id-index.
*    IF sy-subrc IS INITIAL.
*      IF <fs_result>-select <> ''.
*        IF <fs_result>-sel = 'X'.
*          <fs_result>-sel = ''.
*        ELSE.
*          <fs_result>-sel = 'X'.
*        ENDIF.
*      ENDIF.
*
*
*      lv_debit = 0.
*      lv_credit = 0.
*      lv_balance = 0.
*
*
*      LOOP AT lt_result ASSIGNING <fs_result> WHERE sel = 'X'.
*        IF <fs_result>-db_cr_ind = 'H'.
*          lv_credit = lv_credit + <fs_result>-amount.
*        ELSE.
*          lv_debit = lv_debit + <fs_result>-amount.
*        ENDIF.
*
*      ENDLOOP.
*
*      lv_balance = lv_credit - lv_debit.
*
*      lv_debitc = lv_debit.
*      lv_creditc = lv_credit.
*      lv_balancec = lv_balance.
*
*
*    ENDIF.
*
*    cl_alv_grid->refresh_table_display( ).
*    IF sy-subrc <> 0.
**       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
*
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
**     EXPORTING
**       FUNCTIONCODE                 = '='
**     EXCEPTIONS
**       FUNCTION_NOT_SUPPORTED       = 1
**       OTHERS                       = 2
*      .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.



  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
PARAMETERS p_bukrs TYPE bukrs OBLIGATORY.
SELECT-OPTIONS p_dat   FOR bkpf-budat OBLIGATORY.
SELECT-OPTIONS p_lifnr FOR lfa1-lifnr.
SELECT-OPTIONS p_kunrg FOR vbrk-kunrg.
SELECT-OPTIONS p_ebeln  FOR rseg-ebeln.
PARAMETER :
p_purch RADIOBUTTON GROUP gr1,
p_sale RADIOBUTTON GROUP gr1,
p_import RADIOBUTTON GROUP gr1. "<<----S.A---->>

SELECTION-SCREEN END OF BLOCK bl1.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT  'F_BKPF_BUK'
    ID 'ACTVT' FIELD '03'
    ID 'BUKRS' FIELD p_bukrs.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e002(zfi) WITH p_bukrs.
    EXIT.
  ENDIF.

  IF p_purch = 'X'.
    PERFORM purchasereport.
  ELSEIF p_import = 'X'.  "<<----S.A---->>
    PERFORM importReport. "<<----S.A---->>
  ENDIF.
  IF p_sale = 'X'.
    PERFORM salereport.

  ENDIF.


FORM salereport.
  SELECT   bkpf~bukrs,
  t001~butxt,
  t001~stceg,
  bkpf~gjahr,
  bkpf~monat,
  bkpf~belnr,
  bkpf~budat,
  bkpf~blart,
  bkpf~xblnr,
  bkpf~kursf,
  vbrp~vbeln,
  vbrp~posnr,
  vbrk~knumv,
  vbrk~fkart,
  vbrp~matnr,
  vbrp~arktx,
  mara~matkl,
  t023t~wgbez,
  vbrp~aubel,
  vbrp~aupos,
  vbrp~fkimg,
  vbrp~vrkme,
  bkpf~waers,
  vbrk~kunrg,
  vbrk~vbtyp,
     adrc~country,
    t005t~landx AS country_t,
     adrc~region,
     adrc~city1,
     adrc~post_code1,
     adrc~street,
     adrc~house_num1,
        adr3~fax_number,
     adr2~tel_number
     FROM vbrk
    INNER JOIN t001 ON vbrk~bukrs = t001~bukrs
    INNER JOIN bkpf ON vbrk~vbeln = bkpf~awkey
    INNER JOIN vbrp ON vbrk~vbeln = vbrp~vbeln
    INNER JOIN mara ON mara~matnr = vbrp~matnr
    INNER JOIN t023t ON t023t~matkl = mara~matkl AND t023t~spras = 'E'
    LEFT OUTER JOIN but020 ON but020~partner = vbrk~kunrg
    LEFT OUTER JOIN adrc ON adrc~addrnumber = but020~addrnumber
    LEFT OUTER JOIN  adr2 ON adr2~addrnumber = but020~addrnumber  AND adr2~home_flag = 'X' AND adr2~flgdefault = 'X'
    LEFT OUTER JOIN  adr3 ON adr3~addrnumber = but020~addrnumber AND adr3~home_flag = 'X' AND adr3~flgdefault = 'X'
    LEFT JOIN t005t ON t005t~land1 = adrc~country AND t005t~spras = 'E'
    INTO TABLE @DATA(result1)
    WHERE
    bkpf~stblg = '' AND vbrk~rfbsk = 'C' AND vbrk~bukrs = @p_bukrs AND vbrk~fkdat IN @p_dat AND vbrk~kunrg IN @p_kunrg.

  LOOP AT result1 INTO ls_output1.

    ls_period = ls_output1-monat.

    ls_output1-postingquarter = trunc( ls_period / 3 ).
    IF ls_period MOD 3 <> 0.
      ls_output1-postingquarter = ls_output1-postingquarter + 1.
    ENDIF.

    SELECT SINGLE kwert FROM prcd_elements INTO ls_output1-kwert WHERE knumv = ls_output1-knumv AND kposn = ls_output1-posnr AND koaid = 'B' AND kstat = ''.
    SELECT SUM( kwert ) FROM prcd_elements INTO ls_output1-discount WHERE knumv = ls_output1-knumv AND kposn = ls_output1-posnr AND koaid = 'A' AND kstat = '' AND kschl LIKE 'ZD%'.
    SELECT SUM( kwert ) FROM prcd_elements INTO ls_output1-vat WHERE knumv = ls_output1-knumv AND kposn = ls_output1-posnr AND kstat = '' AND kschl = 'ZVAT'.
    SELECT SUM( kwert ) FROM prcd_elements INTO ls_output1-vad WHERE knumv = ls_output1-knumv AND kposn = ls_output1-posnr AND kstat = '' AND kschl = 'ZVAD'.
    SELECT SUM( kwert ) FROM prcd_elements INTO ls_output1-deliverycost WHERE knumv = ls_output1-knumv AND  kposn = ls_output1-posnr AND kstat = '' AND kschl LIKE 'ZH%' AND kinak = ''.
    SELECT SUM( kwert ) FROM prcd_elements INTO ls_output1-packingcost WHERE knumv = ls_output1-knumv AND  kposn = ls_output1-posnr AND kstat = '' AND kschl LIKE 'ZK%' AND kinak = ''.

    ls_output1-tax = ls_output1-vat + ls_output1-vad.
    ls_output1-amountbeftax = ls_output1-kwert + ls_output1-discount + ls_output1-deliverycost + ls_output1-packingcost.




    SELECT SINGLE name_org1 name_first name_last bu_group type FROM but000 INTO (ls_output1-name_org1 , ls_output1-name_first , ls_output1-name_last , ls_output1-bu_group , ls_output1-but_type) WHERE partner = ls_output1-kunrg.

    IF ls_output1-but_type = '1'.
      CONCATENATE ls_output1-name_first ls_output1-name_last INTO ls_output1-name_org1 SEPARATED BY ' '.

    ENDIF.

    IF ls_output1-bu_group = 'V002' OR ls_output1-bu_group = 'C002'.
      ls_output1-vendortype = 'طرف معامله خارجی'.
    ELSE.
      ls_output1-vendortype = 'عادي'.
    ENDIF.



    IF ls_output1-but_type = '1'.
      ls_output1-vendortypecode = ' 1: حقیقی'.
    ENDIF.
    IF ls_output1-bu_group ='V005' OR ls_output1-bu_group = 'C007'.
      ls_output1-vendortypecode = ' 3: حقوقی دولتی وزارت خانه ها و سازمان ها'.
    ENDIF.
    IF ls_output1-vendortypecode = ''.
      ls_output1-vendortypecode = ' 2: حقوقی غیر دولتی'.
    ENDIF.

    SELECT  taxnum taxtype FROM dfkkbptaxnum INTO CORRESPONDING FIELDS OF TABLE lt_taxnum WHERE taxtype IN ( 'IR0' ,'IR1' ,'IR2' ,'IR3' ,'IR4' ,'IR5'  ) AND partner = ls_output1-kunrg.


*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output1-ecco_no  WHERE taxtype = 'IR0' AND partner = ls_output1-kunrg.
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output1-register_no  WHERE taxtype = 'IR1' AND partner = ls_output1-kunrg.
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output1-birth_no  WHERE taxtype = 'IR2' AND partner = ls_output1-kunrg.
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output1-national_id  WHERE taxtype = 'IR3' AND partner = ls_output1-kunrg.
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output1-nationalcode  WHERE taxtype = 'IR4' AND partner = ls_output1-kunrg.
*    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output1-passport_no  WHERE taxtype = 'IR5' AND partner = ls_output1-kunrg.

    LOOP AT lt_taxnum.
      CASE lt_taxnum-taxtype.
        WHEN 'IR0'.
          ls_output1-ecco_no = lt_taxnum-taxnum.
        WHEN 'IR1'.
          ls_output1-register_no = lt_taxnum-taxnum.
        WHEN 'IR2'.
          ls_output1-birth_no = lt_taxnum-taxnum.
        WHEN 'IR3'.
          ls_output1-national_id = lt_taxnum-taxnum.
        WHEN 'IR4'.
          ls_output1-nationalcode = lt_taxnum-taxnum.
        WHEN 'IR5'.
          ls_output1-passport_no = lt_taxnum-taxnum.

      ENDCASE.
    ENDLOOP.
*
*    SELECT adr3~fax_number adr2~tel_number adrc~region adrc~city1 adrc~post_code1 adrc~street adrc~house_num1 UP TO 1 ROWS FROM but020
*     inner JOIN adrc ON adrc~addrnumber = but020~addrnumber
*     left OUTER JOIN  adr2 ON adr2~addrnumber = but020~addrnumber
*     left OUTER JOIN  adr3 ON adr3~addrnumber = but020~addrnumber
*     INTO (ls_output1-fax_number , ls_output1-tel_number , ls_output1-region , ls_output1-city1 , ls_output1-post_code1 , ls_output1-street , ls_output1-house_num1 )
*     WHERE partner = ls_output1-kunrg AND adrc~langu = 'E'  ORDER BY addr_valid_from DESCENDING.
*    ENDSELECT.

*    SELECT tel_number UP TO 1 ROWS FROM but020
*     INNER JOIN adr2 ON adr2~addrnumber = but020~addrnumber
*     INTO  ls_output1-tel_number
*     WHERE partner = ls_output1-kunrg  ORDER BY addr_valid_from DESCENDING.
*    ENDSELECT.
*
*    SELECT fax_number UP TO 1 ROWS FROM but020
*     INNER JOIN adr3 ON adr3~addrnumber = but020~addrnumber
*     INTO  ls_output1-fax_number
*     WHERE partner = ls_output1-kunrg  ORDER BY addr_valid_from DESCENDING.
*    ENDSELECT.

    ls_output1-counter = sy-tabix.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-deliverycost
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-deliverycosirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-packingcost
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-packingcostirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-tax
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-taxirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-amountbeftax
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-amountbetaxirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.





    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-kwert
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-amountirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-discount
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-discountirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-vat
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-vatirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output1-budat
        foreign_amount   = ls_output1-vad
        foreign_currency = ls_output1-waers
        local_currency   = 'IRR'
        rate             = ls_output1-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output1-vadirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = ls_output1-vrkme
*       LANGUAGE             = SY-LANGU
      IMPORTING
*       LONG_TEXT            =
        output = ls_output1-vrkme
*       SHORT_TEXT           =
*     EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.



    IF ls_output-shkzg = 'H' OR ls_output1-vbtyp = 'O' OR ls_output1-vbtyp = 'N' .
      ls_output1-amountirr      = ls_output1-amountirr   * -1.
      ls_output1-discountirr    = ls_output1-discountirr * -1.
      ls_output1-vatirr         = ls_output1-vatirr * -1.
      ls_output1-vadirr         = ls_output1-vadirr * -1.
      ls_output1-taxirr         = ls_output1-taxirr * -1.
      ls_output1-packingcostirr = ls_output1-packingcostirr * -1.
      ls_output1-deliverycosirr = ls_output1-deliverycosirr * -1.
      ls_output1-amountbetaxirr = ls_output1-amountbetaxirr * -1.
      "ls_output1-hwste         = ls_output1-hwste * -1.
      "ls_output1-hsl           = ls_output1-hsl * -1.
      ls_output1-kwert          = ls_output1-kwert * -1.
      ls_output1-discount       = ls_output1-discount * -1.
      ls_output1-vat            = ls_output1-vat * -1.
      ls_output1-vad            = ls_output1-vad * -1.
      ls_output1-tax            = ls_output1-tax * -1.
      ls_output1-packingcost    = ls_output1-packingcost * -1.
      ls_output1-deliverycost   = ls_output1-deliverycost * -1.
      ls_output1-amountbeftax   = ls_output1-amountbeftax * -1.
      "ls_output1-wrbtr       = ls_output1-wrbtr  * -1.
      "ls_output1-wmwst       = ls_output1-wmwst * -1.
    ENDIF.



    ls_output1-waersirr = 'IRR'.
    APPEND ls_output1 TO lt_output1.

  ENDLOOP.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'COUNTER'.
  wa_fieldcat-reptext = 'COUNTER'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BUKRS'.
  wa_fieldcat-reptext = 'Company Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'KUNRG'.
  wa_fieldcat-reptext = 'Customer Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'BUTXT'.
  wa_fieldcat-reptext = 'Company Name'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'STCEG'.
  wa_fieldcat-reptext = 'VAT_Number'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'GJAHR'.
  wa_fieldcat-reptext = 'Fiscal Year'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'MONAT'.
  wa_fieldcat-reptext = 'Posting Period'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'postingquarter'.
  wa_fieldcat-reptext = 'Posting Quarter'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BELNR'.
  wa_fieldcat-hotspot = 'X'.
  wa_fieldcat-reptext = 'FI Doc'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BUDAT'.
  wa_fieldcat-reptext = 'Posting Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'XBLNR'.
  wa_fieldcat-reptext = 'Reference'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'VBELN'.
  wa_fieldcat-hotspot = 'X'.
  wa_fieldcat-reptext = 'Billing Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'POSNR'.
  wa_fieldcat-reptext = 'Billing item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'VBRK'.
  wa_fieldcat-fieldname = 'FKART'.
  wa_fieldcat-reptext = 'Billing Type'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'AUBEL'.
  wa_fieldcat-hotspot = 'X'.
  wa_fieldcat-reptext = 'Sales Doc'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'AUPOS'.
  wa_fieldcat-reptext = 'Sales Doc Item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-reptext = 'Material'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'ARKTX'.
  wa_fieldcat-reptext = 'Material/Service Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'MARA'.
  wa_fieldcat-fieldname = 'MATKL'.
  wa_fieldcat-reptext = 'Material Group'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T023T'.
  wa_fieldcat-fieldname = 'WGBEZ'.
  wa_fieldcat-reptext = 'Material Group Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'VBRP'.
  wa_fieldcat-fieldname = 'VRKME'.
  wa_fieldcat-reptext = 'Unit'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'KWERT'.
  wa_fieldcat-reptext = 'Amount'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'DISCOUNT'.
  wa_fieldcat-reptext = 'Discount'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'VAT'.
  wa_fieldcat-reptext = 'VAT'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'VAD'.
  wa_fieldcat-reptext = 'VAD'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'PACKINGCOST'.
  wa_fieldcat-reptext = 'Packing Cost'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'DELIVERYCOST'.
  wa_fieldcat-reptext = 'Delivery Cost'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'TAX'.
  wa_fieldcat-reptext = 'Tax'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-fieldname = 'AMOUNTBEFTAX'.
  wa_fieldcat-reptext = 'Amount Before Tax'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = ' RBKP'.
  wa_fieldcat-fieldname = 'WAERS'.
  wa_fieldcat-reptext = 'Currency'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'AMOUNTIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'Amount Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'AMOUNTBETAXIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'Amount Before Tax Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'TAXIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'Tax Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'PACKINGCOSTIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'Packing Cost Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'DELIVERYCOSIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'Delivery Cost Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'DISCOUNTIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'Discount Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'VATIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'VAT Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'PRCD_ELEMENTS'.
  wa_fieldcat-fieldname = 'VADIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'VAD Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VENDORTYPECODE'.
  wa_fieldcat-reptext = 'Customer Type Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VENDORTYPE'.
  wa_fieldcat-reptext = 'Customer Type'.

  APPEND wa_fieldcat TO it_fieldcat.

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

  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'BU_GROUP'.
  wa_fieldcat-reptext = 'Vendor Group'.

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
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'COUNTRY'.
  wa_fieldcat-reptext = 'COUNTRY'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T005T'.
  wa_fieldcat-fieldname = 'COUNTRY_T'.
  wa_fieldcat-reptext = 'COUNTRY'.

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
  wa_fieldcat-fieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'IRR Currency'.


  APPEND wa_fieldcat TO it_fieldcat.



  LOOP AT it_fieldcat INTO wa_fieldcat.
    MOVE-CORRESPONDING wa_fieldcat TO cs_fieldcat.
    cs_fieldcat-reptext_ddic = wa_fieldcat-reptext.
    cs_fieldcat-decimals_out = wa_fieldcat-decimals_o.
    APPEND cs_fieldcat TO ct_fieldcat.
  ENDLOOP.



  CALL SCREEN 2000.


ENDFORM.
FORM handle_user_command USING r_ucomm LIKE sy-ucomm
                             rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN'&IC1'.
      CASE rs_selfield-fieldname.
        WHEN 'BELNR'.

          READ TABLE lt_output1 INTO ls_output1 INDEX rs_selfield-tabindex.
          IF sy-subrc IS INITIAL.

            SET PARAMETER ID 'BLN' FIELD rs_selfield-value.
            SET PARAMETER ID 'BUK' FIELD ls_output1-bukrs.
            SET PARAMETER ID 'GJR' FIELD ls_output1-budat(4).
            CALL TRANSACTION 'FB03'AND SKIP FIRST SCREEN.

          ENDIF.

        WHEN 'AUBEL'.


          SET PARAMETER ID 'AUN' FIELD rs_selfield-value.

          CALL TRANSACTION 'VA03'AND SKIP FIRST SCREEN.

        WHEN 'VBELN'.

          SET PARAMETER ID 'VF' FIELD rs_selfield-value.

          CALL TRANSACTION 'VF03'AND SKIP FIRST SCREEN.

      ENDCASE.

  ENDCASE.

ENDFORM.

FORM purchasereport.

  CLEAR lt_belnr[].

  SELECT awkey FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_belnr WHERE
    blart = 'RE' AND ldgrp = '' AND stblg = '' AND bukrs = p_bukrs AND budat IN p_dat.



  LOOP AT lt_belnr INTO ls_belnr.
    ls_belnr-belnr = ls_belnr(10).
    ls_belnr-gjahr = ls_belnr+10(4).
    MODIFY lt_belnr FROM ls_belnr.
  ENDLOOP.
  CHECK lt_belnr[] IS NOT INITIAL.

  SELECT DISTINCT bkpf~bukrs , bkpf~blart , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,   rbkp~xmwst  , rseg~ebeln , rseg~ebelp  ,  rseg~buzei ,
 ekpo~konnr, ekko~bedat , rseg~kschl , rseg~matnr , makt~maktx ,  ekpo~pstyp , ekpo~txz01 , ekpo~matkl, t023t~wgbez , t685t~vtext , a~anln1 ,
    a~wrbtr AS rwrbtr , RSEG~MENGE, RSEG~MEINS,
    a~mwskz AS rmwskz , rseg~mwskz
    , rbkp~waers , rbkp~wmwst1, a~cobl_nr,rseg~wrbtr ,  a~buzei AS rbuzei
     ,rbkp~lifnr , rseg~shkzg,rbkp~kidno,
  bkpf~kursf, RBKP~USNAM, ADRP~NAME_FIRST as UFIRST, ADRP~NAME_LAST AS ULAST
  FROM rbkp
  INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
  INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
  INNER JOIN rseg ON rbkp~belnr = rseg~belnr AND rbkp~gjahr = rseg~gjahr
  LEFT OUTER JOIN ekpo ON rseg~ebeln = ekpo~ebeln AND rseg~ebelp = ekpo~ebelp
  LEFT OUTER JOIN ekko ON ekpo~konnr = ekko~ebeln AND ekko~bukrs = @p_bukrs
  LEFT OUTER JOIN makt ON rseg~matnr = makt~matnr
  LEFT OUTER JOIN t023t ON t023t~matkl = ekpo~matkl AND t023t~spras = 'E'
  LEFT OUTER JOIN t685t ON t685t~kschl = rseg~kschl AND t685t~spras = 'E'
  LEFT OUTER JOIN rbco AS a ON a~belnr = rseg~belnr AND a~gjahr = rseg~gjahr AND a~buzei = rseg~buzei
  JOIN USR21 ON USR21~BNAME = RBKP~USNAM
  JOIN ADRP ON ADRP~PERSNUMBER = USR21~PERSNUMBER

  INTO CORRESPONDING FIELDS OF TABLE @lt_result
   FOR ALL ENTRIES IN @lt_belnr
  WHERE bkpf~blart = 'RE' AND rbkp~rbstat = '5' AND  rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey
    AND rseg~ebeln in @p_ebeln
  AND bkpf~doccat <> 'REVAL' AND bkpf~ldgrp = '' AND  rbkp~lifnr in @P_LIFNR.

  SELECT DISTINCT bkpf~bukrs , bkpf~blart , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,   rbkp~xmwst  ,
     rbkp~waers , rbkp~wmwst1,   b~buzei AS rbuzei,
    b~wrbtr AS r1wrbtr, b~saknr , b~mwskz AS r1mwskz ,  b~cobl_nr AS r1cobl_nr
    ,rbkp~lifnr , txt50 , b~shkzg
 FROM rbkp
 INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
 INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
 INNER JOIN  rbco AS b ON rbkp~belnr = b~belnr AND rbkp~gjahr = b~gjahr AND b~buzei = '000000'
 LEFT OUTER JOIN skat ON skat~saknr = b~saknr AND skat~ktopl = t001~ktopl
 APPENDING CORRESPONDING FIELDS OF TABLE @lt_result
  FOR ALL ENTRIES IN @lt_belnr
 WHERE bkpf~blart = 'RE' AND rbkp~rbstat = '5' AND  rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey
   AND rbkp~lifnr in @P_LIFNR
   AND bkpf~doccat <> 'REVAL' AND bkpf~ldgrp = '' AND skat~spras = 'E'.


  SELECT bkpf~bukrs , bkpf~blart , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,   rbkp~xmwst
     , makt~maktx , t023t~wgbez , rbkp~waers , rbkp~wmwst1,
     rbma~matnr  ,  rbma~wrbtr AS mwrbtr  , rbma~mwskz AS mmwskz
     ,rbkp~lifnr , rbma~shkzg
  FROM rbkp
  INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
  INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
  INNER JOIN rbma ON rbkp~belnr = rbma~belnr AND rbkp~gjahr = rbma~gjahr
  LEFT OUTER JOIN makt ON rbma~matnr = makt~matnr
  LEFT OUTER JOIN mara ON rbma~matnr = mara~matnr
  LEFT OUTER JOIN t023t ON t023t~matkl = mara~matkl AND t023t~spras = 'E'
  APPENDING CORRESPONDING FIELDS OF TABLE @lt_result
   FOR ALL ENTRIES IN @lt_belnr
  WHERE bkpf~blart = 'RE' AND rbkp~rbstat = '5' AND  rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey  AND rbkp~lifnr in @P_LIFNR
  AND bkpf~doccat <> 'REVAL' AND bkpf~ldgrp = ''.


  SELECT awkey FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_belnr WHERE
  blart = 'Z8' AND ldgrp = '' AND stblg = '' AND bukrs = p_bukrs.

  CLEAR lt_belnr[].

  LOOP AT lt_belnr INTO ls_belnr.
    ls_belnr-belnr = ls_belnr(10).
    ls_belnr-gjahr = ls_belnr+10(4).
    MODIFY lt_belnr FROM ls_belnr.
  ENDLOOP.

  IF sy-subrc IS INITIAL.

    SELECT bkpf~bukrs , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,
      mara~matkl , rkwa~matnr ,   t023t~wgbez  , rkwa~wrbtr, rkwa~mwskz
      , rbkp~waers , rbtx~wmwst , rbtx~hwste , rbtx~fwbas ,rbtx~hwbas , rbkp~lifnr
    FROM rbkp
    INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
    INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
    INNER JOIN rkwa ON rbkp~belnr = rkwa~belnr AND rbkp~gjahr = rkwa~gjahr
      LEFT OUTER JOIN rbtx ON rbtx~belnr = rkwa~belnr AND rbtx~gjahr = rkwa~gjahr AND rbtx~buzei = rkwa~buzei AND rbtx~mwskz = rkwa~mwskz
    LEFT OUTER JOIN mara ON mara~matnr = rkwa~matnr
    LEFT OUTER JOIN makt ON rkwa~matnr = makt~matnr
    LEFT OUTER JOIN t023t ON t023t~matkl = mara~matkl AND t023t~spras = 'E'
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_result
     FOR ALL ENTRIES IN @lt_belnr
    WHERE bkpf~blart = 'Z8' AND rbkp~rbstat = '5' AND rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey AND rbkp~lifnr in @P_LIFNR.
  ENDIF.

  SORT lt_result BY rbelnr.



  LOOP AT lt_result INTO ls_output.

    CLEAR flag.
    CLEAR flag2.

    AT NEW rbelnr.
      flag = 'X'.
    ENDAT.
    IF flag = 'X' AND ls_output-xmwst = ''.



      SELECT SINGLE fwste hwste FROM bset INTO ( ls_output-vad , ls_output-vadirr ) WHERE mwskz <> 'P0' AND kschl = 'PVAD' AND belnr = ls_output-belnr AND bukrs = ls_output-bukrs AND gjahr = ls_output-gjahr.
      SELECT SINGLE fwste hwste FROM bset INTO ( ls_output-vat , ls_output-vatirr ) WHERE mwskz <> 'P0' AND kschl = 'PVAT' AND belnr = ls_output-belnr AND bukrs = ls_output-bukrs AND gjahr = ls_output-gjahr.
      ls_output-wmwst = ls_output-wmwst1.


    ENDIF.


    ls_period = ls_output-monat.

    ls_output-postingquarter = trunc( ls_period / 3 ).
    IF ls_period MOD 3 <> 0.
      ls_output-postingquarter = ls_output-postingquarter + 1.
    ENDIF.




    SELECT SINGLE name_org1 name_first name_last bu_group type FROM but000 INTO (ls_output-name_org1 , ls_output-name_first , ls_output-name_last , ls_output-bu_group , ls_output-but_type) WHERE partner = ls_output-lifnr.

    IF ls_output-but_type = '1'.
      CONCATENATE ls_output-name_first ls_output-name_last INTO ls_output-name_org1 SEPARATED BY ' '.

    ENDIF.

    IF ls_output-bu_group = 'V002' OR ls_output-bu_group = 'C002'.
      ls_output-vendortype = 'طرف معامله خارجی'.
    ELSE.
      ls_output-vendortype = 'عادي'.
    ENDIF.



    IF ls_output-but_type = '1'.
      ls_output-vendortypecode = ' 1: حقیقی'.
    ENDIF.
    IF ls_output-bu_group ='V005' OR ls_output-bu_group = 'C007'.
      ls_output-vendortypecode = ' 3: حقوقی دولتی وزارت خانه ها و سازمان ها'.
    ENDIF.
    IF ls_output-vendortypecode = ''.
      ls_output-vendortypecode = ' 2: حقوقی غیر دولتی'.
    ENDIF.


    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-ecco_no  WHERE taxtype = 'IR0' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-register_no  WHERE taxtype = 'IR1' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-birth_no  WHERE taxtype = 'IR2' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-national_id  WHERE taxtype = 'IR3' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-nationalcode  WHERE taxtype = 'IR4' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-passport_no  WHERE taxtype = 'IR5' AND partner = ls_output-lifnr.





    IF ls_output-xmwst = 'X'.

      IF ls_output-buzei = '000000' AND ls_output-rbuzei = '000000' AND ls_output-r1cobl_nr = '0000'.
        ls_output-mwskz = ls_output-mmwskz.
        ls_output-wrbtr = ls_output-mwrbtr.
      ENDIF.
      IF ls_output-buzei = '000000' AND ls_output-r1cobl_nr <>  '0000'.
        ls_output-mwskz = ls_output-r1mwskz.
        ls_output-wrbtr = ls_output-r1wrbtr.
      ENDIF.

      IF ls_output-buzei <> '000000' AND ls_output-rbuzei <> '000000'.
        ls_output-mwskz = ls_output-rmwskz.
        ls_output-wrbtr = ls_output-rwrbtr.
      ENDIF.

      SELECT SINGLE konp~kbetr FROM a003 INNER JOIN konp ON konp~knumh = a003~knumh INTO ls_output-vat WHERE mwskz = ls_output-mwskz AND a003~kappl = 'TX' AND a003~kschl = 'PVAT' AND aland = 'IR'.
      SELECT SINGLE konp~kbetr FROM a003 INNER JOIN konp ON konp~knumh = a003~knumh INTO ls_output-vad WHERE mwskz = ls_output-mwskz AND a003~kappl = 'TX' AND a003~kschl = 'PVAD' AND aland = 'IR'.

      ls_output-vat = ls_output-vat / 1000 * ls_output-wrbtr.
      ls_output-vad = ls_output-vad / 1000 * ls_output-wrbtr.

      ls_output-wmwst = ls_output-vat  + ls_output-vad.

      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
*         CLIENT           = SY-MANDT
          date             = ls_output-budat
          foreign_amount   = ls_output-vat
          foreign_currency = ls_output-waers
          local_currency   = 'IRR'
          rate             = ls_output-kursf
*         TYPE_OF_RATE     = 'M'
*         READ_TCURR       = 'X'
        IMPORTING
*         EXCHANGE_RATE    =
*         FOREIGN_FACTOR   =
          local_amount     = ls_output-vatirr
*         LOCAL_FACTOR     =
*         EXCHANGE_RATEX   =
*         FIXED_RATE       =
*         DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*         NO_RATE_FOUND    = 1
*         OVERFLOW         = 2
*         NO_FACTORS_FOUND = 3
*         NO_SPREAD_FOUND  = 4
*         DERIVED_2_TIMES  = 5
*         OTHERS           = 6
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
*         CLIENT           = SY-MANDT
          date             = ls_output-budat
          foreign_amount   = ls_output-vad
          foreign_currency = ls_output-waers
          local_currency   = 'IRR'
          rate             = ls_output-kursf
*         TYPE_OF_RATE     = 'M'
*         READ_TCURR       = 'X'
        IMPORTING
*         EXCHANGE_RATE    =
*         FOREIGN_FACTOR   =
          local_amount     = ls_output-vadirr
*         LOCAL_FACTOR     =
*         EXCHANGE_RATEX   =
*         FIXED_RATE       =
*         DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*         NO_RATE_FOUND    = 1
*         OVERFLOW         = 2
*         NO_FACTORS_FOUND = 3
*         NO_SPREAD_FOUND  = 4
*         DERIVED_2_TIMES  = 5
*         OTHERS           = 6
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.


    ENDIF.



    IF ls_output-blart = 'RE'.

      IF ls_output-xrech = 'X'.
        ls_output-xrech = ''.
      ELSEIF ls_output-xrech = ''.
        ls_output-xrech = 'X'.
      ENDIF.

      IF ls_output-pstyp <> '9'.
        ls_output-txz01 = ''.
      ENDIF.

      IF ls_output-kschl <> ''.
        ls_output-matnr = ''.
        ls_output-maktx = ''.
      ENDIF.

      IF ls_output-rwrbtr <> 0.
        ls_output-wrbtr = ls_output-rwrbtr.
      ENDIF.
      IF ls_output-rmwskz <> ''.
        ls_output-mwskz = ls_output-rmwskz.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input  = ls_output-meins
*       LANGUAGE             = SY-LANGU
      IMPORTING
*       LONG_TEXT            =
        output = ls_output-meins
*       SHORT_TEXT           =
*     EXCEPTIONS
*       UNIT_NOT_FOUND       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output-budat
        foreign_amount   = ls_output-wrbtr
        foreign_currency = ls_output-waers
        local_currency   = 'IRR'
        rate             = ls_output-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output-wrbtrirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


    SELECT adrc~country t005t~landx adrc~region adrc~city1 adrc~post_code1 adrc~street adrc~house_num1 UP TO 1 ROWS FROM but020
     INNER JOIN adrc ON adrc~addrnumber = but020~addrnumber
         JOIN t005t ON t005t~land1 = adrc~country
     INTO (ls_output-country,ls_output-country_t,ls_output-region , ls_output-city1 , ls_output-post_code1 , ls_output-street , ls_output-house_num1 )
     WHERE partner = ls_output-lifnr AND adrc~langu = 'E'  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    SELECT tel_number UP TO 1 ROWS FROM but020
     INNER JOIN adr2 ON adr2~addrnumber = but020~addrnumber
     INTO  ls_output-tel_number
     WHERE partner = ls_output-lifnr  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    SELECT fax_number UP TO 1 ROWS FROM but020
     INNER JOIN adr3 ON adr3~addrnumber = but020~addrnumber
     INTO  ls_output-fax_number
     WHERE partner = ls_output-lifnr  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    ls_output-counter = sy-tabix.

    ls_output-waersirr = 'IRR'.




    IF ls_output-shkzg = 'H'.
      "ls_output-amountirr   = ls_output-amountirr   * -1.
      "ls_output-discountirr = ls_output-discountirr * -1.
      ls_output-vatirr      = ls_output-vatirr * -1.
      ls_output-vadirr      = ls_output-vadirr * -1.
      ls_output-hwste       = ls_output-hwste * -1.
      ls_output-wrbtrirr    = ls_output-wrbtrirr * -1.
      "ls_output-kwert       = ls_output-kwert * -1.
      "ls_output-discount    = ls_output-discount * -1.
      ls_output-vat         = ls_output-vat * -1.
      ls_output-vad         = ls_output-vad * -1.
      ls_output-wrbtr       = ls_output-wrbtr  * -1.
      ls_output-wmwst       = ls_output-wmwst * -1.
    ENDIF.




    APPEND ls_output TO lt_output.

  ENDLOOP.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'COUNTER'.
  wa_fieldcat-reptext = 'COUNTER'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BUKRS'.
  wa_fieldcat-reptext = 'Company Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'BUTXT'.
  wa_fieldcat-reptext = 'Company Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'STCEG'.
  wa_fieldcat-reptext = 'VAT_Number'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'GJAHR'.
  wa_fieldcat-reptext = 'Fiscal Year'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'MONAT'.
  wa_fieldcat-reptext = 'Posting Period'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'POSTINGQUARTER'.
  wa_fieldcat-reptext = 'Posting Quarter'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BELNR'.
  wa_fieldcat-reptext = 'FI Doc'.
  wa_fieldcat-hotspot = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BUDAT'.
  wa_fieldcat-reptext = 'Posting Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'XBLNR'.
  wa_fieldcat-reptext = 'Reference'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'RBELNR'.
  wa_fieldcat-reptext = 'Invoice Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'BKTXT'.
  wa_fieldcat-reptext = 'Header Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'XRECH'.
  wa_fieldcat-reptext = 'Credit Memo'.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'KIDNO'.
  wa_fieldcat-reptext = 'Payment Ref.'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'EBELN'.
  wa_fieldcat-reptext = 'Purchse Order'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'EBELP'.
  wa_fieldcat-reptext = 'PO Item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'EKPO'.
  wa_fieldcat-fieldname = 'KONNR'.
  wa_fieldcat-reptext = 'Contract Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'EKKO'.
  wa_fieldcat-fieldname = 'BEDAT'.
  wa_fieldcat-reptext = 'Contract Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-reptext = 'Material'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'MAKT'.
  wa_fieldcat-fieldname = 'MAKTX'.
  wa_fieldcat-reptext = 'Material Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'EKPO'.
  wa_fieldcat-fieldname = 'TXZ01'.
  wa_fieldcat-reptext = 'Service Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'EKPO'.
  wa_fieldcat-fieldname = 'MATKL'.
  wa_fieldcat-reptext = 'Material Group'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'KSCHL'.
  wa_fieldcat-reptext = 'Condition'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T023T'.
  wa_fieldcat-fieldname = 'WGBEZ'.
  wa_fieldcat-reptext = 'Material Group Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T685T'.
  wa_fieldcat-fieldname = 'VTEXT'.
  wa_fieldcat-reptext = 'Condition Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBCO'.
  wa_fieldcat-fieldname = 'ANLN1'.
  wa_fieldcat-reptext = 'Asset Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'WRBTR'.
  wa_fieldcat-reptext = 'Amount'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'MWSKZ'.
  wa_fieldcat-reptext = 'Tax Code'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBTX'.
  wa_fieldcat-fieldname = 'WMWST'.
  wa_fieldcat-reptext = 'Tax amount'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBTX'.
  wa_fieldcat-fieldname = 'HWSTE'.
  wa_fieldcat-reptext = 'Tax amount in IRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VAT'.
  wa_fieldcat-reptext = 'VAT'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VAD'.
  wa_fieldcat-reptext = 'VAD'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VATIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'VAT Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VADIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'VAD Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'WRBTRIRR'.
  wa_fieldcat-reptext = 'Amount in IRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'WAERS'.
  wa_fieldcat-reptext = 'Currency'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'LIFNR'.
  wa_fieldcat-reptext = 'Vendor'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VENDORTYPECODE'.
  wa_fieldcat-reptext = 'Vendor Type Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VENDORTYPE'.
  wa_fieldcat-reptext = 'Vendor Type'.

  APPEND wa_fieldcat TO it_fieldcat.

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

  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'BU_GROUP'.
  wa_fieldcat-reptext = 'Vendor Group'.

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
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'COUNTRY'.
  wa_fieldcat-reptext = 'COUNTRY'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T005T'.
  wa_fieldcat-fieldname = 'COUNTRY_T'.
  wa_fieldcat-reptext = 'COUNTRY'.

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
  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'USNAM'.
  wa_fieldcat-reptext = 'USER NAME'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRP'.
  wa_fieldcat-fieldname = 'UFIRST'.
  wa_fieldcat-reptext = 'USER FIRST NAME'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'MENGE'.
  wa_fieldcat-reptext = 'Quantity'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'MEINS'.
  wa_fieldcat-reptext = 'Order Unit'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'ADRP'.
  wa_fieldcat-fieldname = 'ULAST'.
  wa_fieldcat-reptext = 'USER LAST NAME'.


  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'IRR Currency'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SAKNR'.
  wa_fieldcat-reptext = 'Account Number'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'TXT50'.
  wa_fieldcat-reptext = 'Account Title'.


  APPEND wa_fieldcat TO it_fieldcat.




  LOOP AT it_fieldcat INTO wa_fieldcat.
    MOVE-CORRESPONDING wa_fieldcat TO cs_fieldcat.
    cs_fieldcat-reptext_ddic = wa_fieldcat-reptext.
    cs_fieldcat-decimals_out = wa_fieldcat-decimals_o.
    APPEND cs_fieldcat TO ct_fieldcat.
  ENDLOOP.


*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
**     I_INTERFACE_CHECK  = ' '
**     I_BYPASSING_BUFFER = ' '
**     I_BUFFER_ACTIVE    = ' '
*      i_callback_program = sy-repid
**     I_CALLBACK_PF_STATUS_SET          = ' '
**     I_CALLBACK_USER_COMMAND           = ' '
**     I_CALLBACK_TOP_OF_PAGE            = ' '
**     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
**     I_CALLBACK_HTML_END_OF_LIST       = ' '
**     I_STRUCTURE_NAME   =
**     I_BACKGROUND_ID    = ' '
**     I_GRID_TITLE       =
**     I_GRID_SETTINGS    =
**     IS_LAYOUT          = 'X'
*      it_fieldcat        = ct_fieldcat
**     IT_EXCLUDING       =
**     IT_SPECIAL_GROUPS  =
**     IT_SORT            =
**     IT_FILTER          =
**     IS_SEL_HIDE        =
**     I_DEFAULT          = 'X'
*      i_save             = 'X'
**     IS_VARIANT         =
**     IT_EVENTS          =
**     IT_EVENT_EXIT      =
**     IS_PRINT           =
**     IS_REPREP_ID       =
**     I_SCREEN_START_COLUMN             = 0
**     I_SCREEN_START_LINE               = 0
**     I_SCREEN_END_COLUMN               = 0
**     I_SCREEN_END_LINE  = 0
**     I_HTML_HEIGHT_TOP  = 0
**     I_HTML_HEIGHT_END  = 0
**     IT_ALV_GRAPHICS    =
**     IT_HYPERLINK       =
**     IT_ADD_FIELDCAT    =
**     IT_EXCEPT_QINFO    =
**     IR_SALV_FULLSCREEN_ADAPTER        =
**   IMPORTING
**     E_EXIT_CAUSED_BY_CALLER           =
**     ES_EXIT_CAUSED_BY_USER            =
*    TABLES
*      t_outtab           = lt_output
**   EXCEPTIONS
**     PROGRAM_ERROR      = 1
**     OTHERS             = 2
*    .
*  IF sy-subrc <> 0.
** Implement suitable error handling here
*  ENDIF.

  CALL SCREEN 1500.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  DATA: ls_sort TYPE lvc_s_sort.
  DATA: lt_sort TYPE lvc_t_sort.



  IF cl_alv_cont1 IS INITIAL.
    CREATE OBJECT cl_alv_cont1
      EXPORTING
        container_name = 'ALV_CONT'.

    CREATE OBJECT cl_alv_grid1
      EXPORTING
        i_parent = cl_alv_cont1.


*    CREATE OBJECT cl_event_receiver1.
*
*
*    SET HANDLER cl_event_receiver1->handle_hotspot_click FOR cl_alv_grid1.


    DELETE lt_output1 WHERE budat NOT IN p_dat.

    CALL METHOD cl_alv_grid1->set_table_for_first_display
      EXPORTING
*       i_buffer_active =     " Buffering Active
*       i_bypassing_buffer            =     " Switch Off Buffer
*       i_consistency_check           =     " Starting Consistency Check for Interface Error Recognition
*       i_structure_name              =     " Internal Output Table Structure Name
        is_variant      = ls_variant     " Layout
        i_save          = 'A'    " Save Layout
*       i_default       = 'X'    " Default Display Variant
*       is_layout       =     " Layout
*       is_print        =     " Print Control
*       it_special_groups             =     " Field Groups
*       it_toolbar_excluding          =     " Excluded Toolbar Standard Functions
*       it_hyperlink    =     " Hyperlinks
*       it_alv_graphics =     " Table of Structure DTC_S_TC
*       it_except_qinfo =     " Table for Exception Quickinfo
*       ir_salv_adapter =     " Interface ALV Adapter
      CHANGING
        it_outtab       = lt_output1 " Output Table
        it_fieldcatalog = it_fieldcat   " Field Catalog
*       it_sort         = lt_sort
*       it_filter       =     " Filter Criteria
*    EXCEPTIONS
*       invalid_parameter_combination = 1
*       program_error   = 2
*       too_many_lines  = 3
*       others          = 4
      .
    IF sy-subrc <> 0.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_1500 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1500 OUTPUT.
  SET PF-STATUS 'STANDARD'.



  IF cl_alv_cont IS INITIAL.
    CREATE OBJECT cl_alv_cont
      EXPORTING
        container_name = 'ALV_CONT'.

    CREATE OBJECT cl_alv_grid
      EXPORTING
        i_parent = cl_alv_cont.


    DELETE lt_output WHERE budat NOT IN p_dat.

    CALL METHOD cl_alv_grid->set_table_for_first_display
      EXPORTING
*       i_buffer_active =     " Buffering Active
*       i_bypassing_buffer            =     " Switch Off Buffer
*       i_consistency_check           =     " Starting Consistency Check for Interface Error Recognition
*       i_structure_name              =     " Internal Output Table Structure Name
        is_variant      = ls_variant     " Layout
        i_save          = 'A'     " Save Layout
*       i_default       = 'X'    " Default Display Variant
*       is_layout       =     " Layout
*       is_print        =     " Print Control
*       it_special_groups             =     " Field Groups
*       it_toolbar_excluding          =     " Excluded Toolbar Standard Functions
*       it_hyperlink    =     " Hyperlinks
*       it_alv_graphics =     " Table of Structure DTC_S_TC
*       it_except_qinfo =     " Table for Exception Quickinfo
*       ir_salv_adapter =     " Interface ALV Adapter
      CHANGING
        it_outtab       = lt_output " Output Table
        it_fieldcatalog = it_fieldcat   " Field Catalog
*       it_sort         = lt_sort
*       it_filter       =     " Filter Criteria
*    EXCEPTIONS
*       invalid_parameter_combination = 1
*       program_error   = 2
*       too_many_lines  = 3
*       others          = 4
      .
    IF sy-subrc <> 0.
*   MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1500 INPUT.
  IF sy-ucomm = 'BACK' OR sy-ucomm = 'UP' OR sy-ucomm = 'EXI'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.
  IF sy-ucomm = 'BACK' OR sy-ucomm = 'UP' OR sy-ucomm = 'EXI'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form IMPORTREPORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM IMPORTREPORT .

*<<-----------------------------S.A------------------------>>
  CLEAR lt_belnr[].

  SELECT awkey FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_belnr WHERE
    blart = 'RE' AND ldgrp = '' AND stblg = '' AND bukrs = p_bukrs AND budat IN p_dat.



  LOOP AT lt_belnr INTO ls_belnr.
    ls_belnr-belnr = ls_belnr(10).
    ls_belnr-gjahr = ls_belnr+10(4).
    MODIFY lt_belnr FROM ls_belnr.
  ENDLOOP.
  CHECK lt_belnr[] IS NOT INITIAL.

  SELECT DISTINCT bkpf~bukrs , bkpf~blart , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,
                  rbkp~xrech ,   rbkp~xmwst  , rseg~ebeln , rseg~ebelp  ,  rseg~buzei ,
 ekpo~konnr, ekko~bedat , rseg~kschl , rseg~matnr , makt~maktx ,  ekpo~pstyp , ekpo~txz01 , ekpo~matkl, t023t~wgbez , t685t~vtext , a~anln1 , a~wrbtr AS rwrbtr , a~mwskz AS rmwskz , rseg~mwskz
    , rbkp~waers , rbkp~wmwst1, a~cobl_nr,rseg~wrbtr ,  a~buzei AS rbuzei
     ,rbkp~lifnr , rseg~shkzg,rbkp~kidno,
  bkpf~kursf
  FROM rbkp
  INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
  INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
  INNER JOIN rseg ON rbkp~belnr = rseg~belnr AND rbkp~gjahr = rseg~gjahr
  LEFT OUTER JOIN ekpo ON rseg~ebeln = ekpo~ebeln AND rseg~ebelp = ekpo~ebelp
  LEFT OUTER JOIN ekko ON ekpo~konnr = ekko~ebeln AND ekko~bukrs = @p_bukrs
  LEFT OUTER JOIN makt ON rseg~matnr = makt~matnr
  LEFT OUTER JOIN t023t ON t023t~matkl = ekpo~matkl AND t023t~spras = 'E'
  LEFT OUTER JOIN t685t ON t685t~kschl = rseg~kschl AND t685t~spras = 'E'
  LEFT OUTER JOIN rbco AS a ON a~belnr = rseg~belnr AND a~gjahr = rseg~gjahr AND a~buzei = rseg~buzei

  INTO CORRESPONDING FIELDS OF TABLE @lt_result
   FOR ALL ENTRIES IN @lt_belnr
  WHERE bkpf~blart = 'RE' AND rbkp~rbstat = '5' AND  rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey  "AND rbkp~lifnr = @P_LIFNR
  AND bkpf~doccat <> 'REVAL' AND bkpf~ldgrp = ''.

  SELECT DISTINCT bkpf~bukrs , bkpf~blart , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,   rbkp~xmwst  ,
     rbkp~waers , rbkp~wmwst1,   b~buzei AS rbuzei,
    b~wrbtr AS r1wrbtr, b~saknr , b~mwskz AS r1mwskz ,  b~cobl_nr AS r1cobl_nr
    ,rbkp~lifnr , txt50 , b~shkzg
 FROM rbkp
 INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
 INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
 INNER JOIN  rbco AS b ON rbkp~belnr = b~belnr AND rbkp~gjahr = b~gjahr AND b~buzei = '000000'
 LEFT OUTER JOIN skat ON skat~saknr = b~saknr AND skat~ktopl = t001~ktopl

 APPENDING CORRESPONDING FIELDS OF TABLE @lt_result
  FOR ALL ENTRIES IN @lt_belnr
 WHERE bkpf~blart = 'RE' AND rbkp~rbstat = '5' AND  rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey "AND rbkp~lifnr = @P_LIFNR
 AND bkpf~doccat <> 'REVAL' AND bkpf~ldgrp = '' AND skat~spras = 'E'.


  SELECT bkpf~bukrs , bkpf~blart , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,   rbkp~xmwst
     , makt~maktx , t023t~wgbez , rbkp~waers , rbkp~wmwst1,
     rbma~matnr  ,  rbma~wrbtr AS mwrbtr  , rbma~mwskz AS mmwskz
     ,rbkp~lifnr , rbma~shkzg
  FROM rbkp
  INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
  INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
  INNER JOIN rbma ON rbkp~belnr = rbma~belnr AND rbkp~gjahr = rbma~gjahr
  LEFT OUTER JOIN makt ON rbma~matnr = makt~matnr
  LEFT OUTER JOIN mara ON rbma~matnr = mara~matnr
  LEFT OUTER JOIN t023t ON t023t~matkl = mara~matkl AND t023t~spras = 'E'
  APPENDING CORRESPONDING FIELDS OF TABLE @lt_result
   FOR ALL ENTRIES IN @lt_belnr
  WHERE bkpf~blart = 'RE' AND rbkp~rbstat = '5' AND  rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey "AND rbkp~lifnr = @P_LIFNR
  AND bkpf~doccat <> 'REVAL' AND bkpf~ldgrp = ''.


  SELECT awkey FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_belnr WHERE
  blart = 'Z8' AND ldgrp = '' AND stblg = '' AND bukrs = p_bukrs.

  CLEAR lt_belnr[].

  LOOP AT lt_belnr INTO ls_belnr.
    ls_belnr-belnr = ls_belnr(10).
    ls_belnr-gjahr = ls_belnr+10(4).
    MODIFY lt_belnr FROM ls_belnr.
  ENDLOOP.

  IF sy-subrc IS INITIAL.

    SELECT bkpf~bukrs , t001~butxt , t001~stceg, bkpf~gjahr , bkpf~monat , bkpf~belnr , bkpf~budat , bkpf~xblnr , rbkp~belnr AS rbelnr , rbkp~bktxt,  rbkp~xrech ,
      mara~matkl , rkwa~matnr ,   t023t~wgbez  , rkwa~wrbtr, rkwa~mwskz
      , rbkp~waers , rbtx~wmwst , rbtx~hwste , rbtx~fwbas ,rbtx~hwbas , rbkp~lifnr
    FROM rbkp
    INNER JOIN bkpf ON rbkp~bukrs = bkpf~bukrs
    INNER JOIN t001 ON t001~bukrs = bkpf~bukrs
    INNER JOIN rkwa ON rbkp~belnr = rkwa~belnr AND rbkp~gjahr = rkwa~gjahr
      LEFT OUTER JOIN rbtx ON rbtx~belnr = rkwa~belnr AND rbtx~gjahr = rkwa~gjahr AND rbtx~buzei = rkwa~buzei AND rbtx~mwskz = rkwa~mwskz
    LEFT OUTER JOIN mara ON mara~matnr = rkwa~matnr
    LEFT OUTER JOIN makt ON rkwa~matnr = makt~matnr
    LEFT OUTER JOIN t023t ON t023t~matkl = mara~matkl AND t023t~spras = 'E'
    APPENDING CORRESPONDING FIELDS OF TABLE @lt_result
     FOR ALL ENTRIES IN @lt_belnr
    WHERE bkpf~blart = 'Z8' AND rbkp~rbstat = '5' AND rbkp~belnr = @lt_belnr-belnr AND rbkp~gjahr = @lt_belnr-gjahr AND bkpf~awkey = @lt_belnr-awkey. "AND rbkp~lifnr = @P_LIFNR.
  ENDIF.

  SORT lt_result BY rbelnr.



  LOOP AT lt_result INTO ls_output.
    IF ls_output-KSCHL ne 'ZOA2' and ls_output-KSCHL ne 'ZOA1' and ls_output-KSCHL ne 'ZS12' and ls_output-KSCHL ne 'ZOC1' .
       delete LT_RESULT.
       else.

    CLEAR flag.
    CLEAR flag2.

    AT NEW rbelnr.
      flag = 'X'.
    ENDAT.
    IF flag = 'X' AND ls_output-xmwst = ''.



      SELECT SINGLE fwste hwste FROM bset INTO ( ls_output-vad , ls_output-vadirr ) WHERE mwskz <> 'P0' AND kschl = 'PVAD' AND belnr = ls_output-belnr AND bukrs = ls_output-bukrs AND gjahr = ls_output-gjahr.
      SELECT SINGLE fwste hwste FROM bset INTO ( ls_output-vat , ls_output-vatirr ) WHERE mwskz <> 'P0' AND kschl = 'PVAT' AND belnr = ls_output-belnr AND bukrs = ls_output-bukrs AND gjahr = ls_output-gjahr.
      ls_output-wmwst = ls_output-wmwst1.


    ENDIF.


    ls_period = ls_output-monat.

    ls_output-postingquarter = trunc( ls_period / 3 ).
    IF ls_period MOD 3 <> 0.
      ls_output-postingquarter = ls_output-postingquarter + 1.
    ENDIF.




    SELECT SINGLE name_org1 name_first name_last bu_group type FROM but000 INTO (ls_output-name_org1 , ls_output-name_first , ls_output-name_last , ls_output-bu_group , ls_output-but_type) WHERE partner = ls_output-lifnr.

    IF ls_output-but_type = '1'.
      CONCATENATE ls_output-name_first ls_output-name_last INTO ls_output-name_org1 SEPARATED BY ' '.

    ENDIF.

    IF ls_output-bu_group = 'V002' OR ls_output-bu_group = 'C002'.
      ls_output-vendortype = 'طرف معامله خارجی'.
    ELSE.
      ls_output-vendortype = 'عادي'.
    ENDIF.



    IF ls_output-but_type = '1'.
      ls_output-vendortypecode = ' 1: حقیقی'.
    ENDIF.
    IF ls_output-bu_group ='V005' OR ls_output-bu_group = 'C007'.
      ls_output-vendortypecode = ' 3: حقوقی دولتی وزارت خانه ها و سازمان ها'.
    ENDIF.
    IF ls_output-vendortypecode = ''.
      ls_output-vendortypecode = ' 2: حقوقی غیر دولتی'.
    ENDIF.


    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-ecco_no  WHERE taxtype = 'IR0' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-register_no  WHERE taxtype = 'IR1' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-birth_no  WHERE taxtype = 'IR2' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-national_id  WHERE taxtype = 'IR3' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-nationalcode  WHERE taxtype = 'IR4' AND partner = ls_output-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_output-passport_no  WHERE taxtype = 'IR5' AND partner = ls_output-lifnr.





    IF ls_output-xmwst = 'X'.

      IF ls_output-buzei = '000000' AND ls_output-rbuzei = '000000' AND ls_output-r1cobl_nr = '0000'.
        ls_output-mwskz = ls_output-mmwskz.
        ls_output-wrbtr = ls_output-mwrbtr.
      ENDIF.
      IF ls_output-buzei = '000000' AND ls_output-r1cobl_nr <>  '0000'.
        ls_output-mwskz = ls_output-r1mwskz.
        ls_output-wrbtr = ls_output-r1wrbtr.
      ENDIF.

      IF ls_output-buzei <> '000000' AND ls_output-rbuzei <> '000000'.
        ls_output-mwskz = ls_output-rmwskz.
        ls_output-wrbtr = ls_output-rwrbtr.
      ENDIF.

      SELECT SINGLE konp~kbetr FROM a003 INNER JOIN konp ON konp~knumh = a003~knumh INTO ls_output-vat WHERE mwskz = ls_output-mwskz AND a003~kappl = 'TX' AND a003~kschl = 'PVAT' AND aland = 'IR'.
      SELECT SINGLE konp~kbetr FROM a003 INNER JOIN konp ON konp~knumh = a003~knumh INTO ls_output-vad WHERE mwskz = ls_output-mwskz AND a003~kappl = 'TX' AND a003~kschl = 'PVAD' AND aland = 'IR'.

      ls_output-vat = ls_output-vat / 1000 * ls_output-wrbtr.
      ls_output-vad = ls_output-vad / 1000 * ls_output-wrbtr.

      ls_output-wmwst = ls_output-vat  + ls_output-vad.

      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
*         CLIENT           = SY-MANDT
          date             = ls_output-budat
          foreign_amount   = ls_output-vat
          foreign_currency = ls_output-waers
          local_currency   = 'IRR'
          rate             = ls_output-kursf
*         TYPE_OF_RATE     = 'M'
*         READ_TCURR       = 'X'
        IMPORTING
*         EXCHANGE_RATE    =
*         FOREIGN_FACTOR   =
          local_amount     = ls_output-vatirr
*         LOCAL_FACTOR     =
*         EXCHANGE_RATEX   =
*         FIXED_RATE       =
*         DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*         NO_RATE_FOUND    = 1
*         OVERFLOW         = 2
*         NO_FACTORS_FOUND = 3
*         NO_SPREAD_FOUND  = 4
*         DERIVED_2_TIMES  = 5
*         OTHERS           = 6
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
        EXPORTING
*         CLIENT           = SY-MANDT
          date             = ls_output-budat
          foreign_amount   = ls_output-vad
          foreign_currency = ls_output-waers
          local_currency   = 'IRR'
          rate             = ls_output-kursf
*         TYPE_OF_RATE     = 'M'
*         READ_TCURR       = 'X'
        IMPORTING
*         EXCHANGE_RATE    =
*         FOREIGN_FACTOR   =
          local_amount     = ls_output-vadirr
*         LOCAL_FACTOR     =
*         EXCHANGE_RATEX   =
*         FIXED_RATE       =
*         DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*         NO_RATE_FOUND    = 1
*         OVERFLOW         = 2
*         NO_FACTORS_FOUND = 3
*         NO_SPREAD_FOUND  = 4
*         DERIVED_2_TIMES  = 5
*         OTHERS           = 6
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.


    ENDIF.



    IF ls_output-blart = 'RE'.

      IF ls_output-xrech = 'X'.
        ls_output-xrech = ''.
      ELSEIF ls_output-xrech = ''.
        ls_output-xrech = 'X'.
      ENDIF.

      IF ls_output-pstyp <> '9'.
        ls_output-txz01 = ''.
      ENDIF.

      IF ls_output-kschl <> ''.
        ls_output-matnr = ''.
        ls_output-maktx = ''.
      ENDIF.

      IF ls_output-rwrbtr <> 0.
        ls_output-wrbtr = ls_output-rwrbtr.
      ENDIF.
      IF ls_output-rmwskz <> ''.
        ls_output-mwskz = ls_output-rmwskz.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'
      EXPORTING
*       CLIENT           = SY-MANDT
        date             = ls_output-budat
        foreign_amount   = ls_output-wrbtr
        foreign_currency = ls_output-waers
        local_currency   = 'IRR'
        rate             = ls_output-kursf
*       TYPE_OF_RATE     = 'M'
*       READ_TCURR       = 'X'
      IMPORTING
*       EXCHANGE_RATE    =
*       FOREIGN_FACTOR   =
        local_amount     = ls_output-wrbtrirr
*       LOCAL_FACTOR     =
*       EXCHANGE_RATEX   =
*       FIXED_RATE       =
*       DERIVED_RATE_TYPE       =
*     EXCEPTIONS
*       NO_RATE_FOUND    = 1
*       OVERFLOW         = 2
*       NO_FACTORS_FOUND = 3
*       NO_SPREAD_FOUND  = 4
*       DERIVED_2_TIMES  = 5
*       OTHERS           = 6
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT adrc~country t005t~landx adrc~region adrc~city1 adrc~post_code1 adrc~street adrc~house_num1 UP TO 1 ROWS FROM but020
     INNER JOIN adrc ON adrc~addrnumber = but020~addrnumber
         JOIN t005t ON t005t~land1 = adrc~country
     INTO (ls_output-country,ls_output-country_t,ls_output-region , ls_output-city1 , ls_output-post_code1 , ls_output-street , ls_output-house_num1 )
     WHERE partner = ls_output-lifnr AND adrc~langu = 'E'  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    SELECT tel_number UP TO 1 ROWS FROM but020
     INNER JOIN adr2 ON adr2~addrnumber = but020~addrnumber
     INTO  ls_output-tel_number
     WHERE partner = ls_output-lifnr  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    SELECT fax_number UP TO 1 ROWS FROM but020
     INNER JOIN adr3 ON adr3~addrnumber = but020~addrnumber
     INTO  ls_output-fax_number
     WHERE partner = ls_output-lifnr  ORDER BY addr_valid_from DESCENDING.
    ENDSELECT.

    ls_output-counter = sy-tabix.

    ls_output-waersirr = 'IRR'.




    IF ls_output-shkzg = 'H'.
      "ls_output-amountirr   = ls_output-amountirr   * -1.
      "ls_output-discountirr = ls_output-discountirr * -1.
      ls_output-vatirr      = ls_output-vatirr * -1.
      ls_output-vadirr      = ls_output-vadirr * -1.
      ls_output-hwste       = ls_output-hwste * -1.
      ls_output-wrbtrirr    = ls_output-wrbtrirr * -1.
      "ls_output-kwert       = ls_output-kwert * -1.
      "ls_output-discount    = ls_output-discount * -1.
      ls_output-vat         = ls_output-vat * -1.
      ls_output-vad         = ls_output-vad * -1.
      ls_output-wrbtr       = ls_output-wrbtr  * -1.
      ls_output-wmwst       = ls_output-wmwst * -1.
    ENDIF.



    ls_output-FLDKEY = LS_OUTPUT-EBELN && LS_OUTPUT-EBELP && LS_OUTPUT-LIFNR.
    APPEND ls_output TO lt_output.
      ENDIF.
  ENDLOOP.

  CLEAR: TMP_AMOUNT , tmp_amountIRR , tmp_tax_amount , tmp_tax_amountIRR .
  sort LT_OUTPUT by FLDKEY.
  LOOP AT LT_OUTPUT INTO LS_OUTPUT.

        WA_TMP = LS_OUTPUT.
        tmp_amount = tmp_amount + LS_OUTPUT-WRBTR.
        tmp_amountIRR = tmp_amountIRR + LS_OUTPUT-WRBTRIRR.
        tmp_tax_amount = tmp_tax_amount + LS_OUTPUT-WMWST.
        tmp_tax_amountIRR = tmp_tax_amountIRR + LS_OUTPUT-HWSTE.

        at END OF fldkey.
            LS_OUTPUT = WA_TMP  .
          MOVE tmp_amount  TO LS_OUTPUT-WRBTR.
          MOVE tmp_amountIRR  TO LS_OUTPUT-WRBTRIRR.
          MOVE tmp_tax_amount  TO LS_OUTPUT-WMWST.
          MOVE tmp_tax_amountIRR  TO LS_OUTPUT-HWSTE.
*          CONDENSE LS_OUTPUT-WRBTR.
*          CONDENSE LS_OUTPUT-WRBTRIRR.
*          CONDENSE LS_OUTPUT-WMWST.
*          CONDENSE LS_OUTPUT-HWSTE.
          APPEND LS_OUTPUT TO LT_OUTPUT2.
  CLEAR: TMP_AMOUNT , tmp_amountIRR , tmp_tax_amount , tmp_tax_amountIRR .
        ENDAT.
    ENDLOOP.
  refresh LT_OUTPUT[].
  LT_OUTPUT[] = LT_OUTPUT2[].

   SORT LT_OUTPUT[] .

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'COUNTER'.
  wa_fieldcat-reptext = 'COUNTER'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BUKRS'.
  wa_fieldcat-reptext = 'Company Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'BUTXT'.
  wa_fieldcat-reptext = 'Company Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'T001'.
  wa_fieldcat-fieldname = 'STCEG'.
  wa_fieldcat-reptext = 'VAT_Number'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'GJAHR'.
  wa_fieldcat-reptext = 'Fiscal Year'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'MONAT'.
  wa_fieldcat-reptext = 'Posting Period'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'POSTINGQUARTER'.
  wa_fieldcat-reptext = 'Posting Quarter'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BELNR'.
  wa_fieldcat-reptext = 'FI Doc'.
  wa_fieldcat-hotspot = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BUDAT'.
  wa_fieldcat-reptext = 'Posting Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'XBLNR'.
  wa_fieldcat-reptext = 'Reference'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'RBELNR'.
  wa_fieldcat-reptext = 'Invoice Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'BKTXT'.
  wa_fieldcat-reptext = 'Header Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'XRECH'.
  wa_fieldcat-reptext = 'Credit Memo'.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'KIDNO'.
  wa_fieldcat-reptext = 'Payment Ref.'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'EBELN'.
  wa_fieldcat-reptext = 'Purchse Order'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'EBELP'.
  wa_fieldcat-reptext = 'PO Item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'EKPO'.
  wa_fieldcat-fieldname = 'KONNR'.
  wa_fieldcat-reptext = 'Contract Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'EKKO'.
  wa_fieldcat-fieldname = 'BEDAT'.
  wa_fieldcat-reptext = 'Contract Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-reptext = 'Material'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'MAKT'.
  wa_fieldcat-fieldname = 'MAKTX'.
  wa_fieldcat-reptext = 'Material Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.


  wa_fieldcat-tabname = 'EKPO'.
  wa_fieldcat-fieldname = 'TXZ01'.
  wa_fieldcat-reptext = 'Service Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'EKPO'.
  wa_fieldcat-fieldname = 'MATKL'.
  wa_fieldcat-reptext = 'Material Group'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'KSCHL'.
  wa_fieldcat-reptext = 'Condition'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T023T'.
  wa_fieldcat-fieldname = 'WGBEZ'.
  wa_fieldcat-reptext = 'Material Group Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'T685T'.
  wa_fieldcat-fieldname = 'VTEXT'.
  wa_fieldcat-reptext = 'Condition Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBCO'.
  wa_fieldcat-fieldname = 'ANLN1'.
  wa_fieldcat-reptext = 'Asset Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'WRBTR'.
  wa_fieldcat-reptext = 'Amount'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RSEG'.
  wa_fieldcat-fieldname = 'MWSKZ'.
  wa_fieldcat-reptext = 'Tax Code'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBTX'.
  wa_fieldcat-fieldname = 'WMWST'.
  wa_fieldcat-reptext = 'Tax amount'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBTX'.
  wa_fieldcat-fieldname = 'HWSTE'.
  wa_fieldcat-reptext = 'Tax amount in IRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VAT'.
  wa_fieldcat-reptext = 'VAT'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VAD'.
  wa_fieldcat-reptext = 'VAD'.
  wa_fieldcat-cfieldname = 'WAERS'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VATIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'VAT Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'KNOP'.
  wa_fieldcat-fieldname = 'VADIRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'VAD Irr'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-fieldname = 'WRBTRIRR'.
  wa_fieldcat-reptext = 'Amount in IRR'.
  wa_fieldcat-cfieldname = 'WAERSIRR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'WAERS'.
  wa_fieldcat-reptext = 'Currency'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.

  wa_fieldcat-tabname = 'RBKP'.
  wa_fieldcat-fieldname = 'LIFNR'.
  wa_fieldcat-reptext = 'Vendor'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VENDORTYPECODE'.
  wa_fieldcat-reptext = 'Vendor Type Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VENDORTYPE'.
  wa_fieldcat-reptext = 'Vendor Type'.

  APPEND wa_fieldcat TO it_fieldcat.

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

  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'BU_GROUP'.
  wa_fieldcat-reptext = 'Vendor Group'.

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
  wa_fieldcat-tabname = 'ADRC'.
  wa_fieldcat-fieldname = 'COUNTRY'.
  wa_fieldcat-reptext = 'COUNTRY'.

  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'T005T'.
  wa_fieldcat-fieldname = 'COUNTRY_T'.
  wa_fieldcat-reptext = 'COUNTRY'.

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


  CLEAR wa_fieldcat..
  wa_fieldcat-fieldname = 'WAERSIRR'.
  wa_fieldcat-reptext = 'IRR Currency'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SAKNR'.
  wa_fieldcat-reptext = 'Account Number'.


  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'TXT50'.
  wa_fieldcat-reptext = 'Account Title'.


  APPEND wa_fieldcat TO it_fieldcat.




  LOOP AT it_fieldcat INTO wa_fieldcat.
    MOVE-CORRESPONDING wa_fieldcat TO cs_fieldcat.
    cs_fieldcat-reptext_ddic = wa_fieldcat-reptext.
    cs_fieldcat-decimals_out = wa_fieldcat-decimals_o.
    APPEND cs_fieldcat TO ct_fieldcat.
  ENDLOOP.

    CALL SCREEN 1500.
*<<----------------S.A---------------------->>
ENDFORM.
