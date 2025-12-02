*&---------------------------------------------------------------------*
*& Report ZBRZ_FI_GENERATE_CHEQUE_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbrz_fi_document_print.

TABLES:bkpf,acdoca.

DATA: BEGIN OF ls_result,
        rbukrs      TYPE   bukrs,
        gjahr       TYPE  gjahr,
        fgjahr      TYPE  gjahr,
        belnr       TYPE  belnr_d,
        docln       TYPE  docln,
        buzei       TYPE  buzei,
        awref       TYPE  acdoca-awref,
        rwcur       TYPE  rwcur,
        rhcur       TYPE  acdoca-rhcur,
        rkcur       TYPE  acdoca-rkcur,
        rocur       TYPE  acdoca-rocur,
        rvcur       TYPE  acdoca-rvcur,
        racct       TYPE  racct,
        rcntr       TYPE  acdoca-rcntr,
        prctr       TYPE  prctr,
        rfarea      TYPE  acdoca-rfarea,
        kokrs       TYPE  kokrs,
        wsl         TYPE  acdoca-wsl,
        hsl         TYPE  acdoca-hsl,
        ksl         TYPE  acdoca-ksl,
        osl         TYPE  acdoca-osl,
        vsl         TYPE  acdoca-vsl,
        drcrk       TYPE  acdoca-drcrk,
        poper       TYPE  poper,
        periv       TYPE  periv,
        fiscyearper TYPE  acdoca-fiscyearper,
        budat       TYPE  budat,
        bldat       TYPE  bldat,
        blart       TYPE  blart,
        zuonr       TYPE  acdoca-zuonr,
        bschl       TYPE  bschl,
        ebeln       TYPE  ebeln,
        ebelp       TYPE  ebelp,
        sgtxt       TYPE  sgtxt,
        kdauf       TYPE  kdauf,
        kdpos       TYPE  kdpos,
        matnr       TYPE  matnr,
        maktx       TYPE  maktx,
        werks       TYPE  acdoca-werks,
        koart       TYPE  koart,
        lifnr       TYPE  lifnr,
        kunnr       TYPE  kunnr,
        umskz       TYPE  umskz,
        anln1       TYPE  anln1,
        anln2       TYPE  anln2,
        bzdat       TYPE  bzdat,
        kursf       TYPE  kursf,
        bktxt       TYPE bktxt,
        xblnr       TYPE  xblnr,
        boeno       TYPE bsed-boeno,
        usnam       TYPE  usnam,
        bvorg       TYPE  bvorg,
        mdesc       TYPE maktx,
        vdesc       TYPE name1,
        cdesc       TYPE name1,
        gldesc      TYPE txt50,
        asdesc      TYPE txt50,
        vcontr      TYPE usnam,
        approv      TYPE usnam,
        vocver      TYPE usnam,
        fbudat      TYPE string,
        blartdesc   TYPE ltext_003t,
        augbl       TYPE augbl,
        auggj       TYPE auggj,
        chect       TYPE payr-chect,
        vcontr_name TYPE ad_namtext,
        approv_name TYPE ad_namtext,
        usnam_name  TYPE ad_namtext,
        vocver_name TYPE ad_namtext,
      END OF ls_result,
      lt_result     LIKE TABLE OF ls_result,
      lt_result_fin LIKE TABLE OF ls_result,
      where         TYPE string,
      ct_fieldcat   TYPE slis_t_fieldcat_alv,
      cs_fieldcat   TYPE slis_fieldcat_alv,
      wa_fieldcat   TYPE LINE OF lvc_t_fcat,
      it_fieldcat   TYPE lvc_t_fcat,
      dummy         TYPE string,
      len           TYPE i,
      currdec       TYPE bapi1090_1,
      return        TYPE bapireturn.


SELECTION-SCREEN BEGIN OF BLOCK block1
                          WITH FRAME TITLE TEXT-001.
PARAMETERS:
  p_bukrs TYPE bukrs OBLIGATORY,
  p_gjahr TYPE gjahr OBLIGATORY.
SELECT-OPTIONS:
  p_budat  FOR bkpf-budat ,
  p_belnr  FOR acdoca-belnr ,
   p_blart  FOR bkpf-blart ,
   p_rldnr  FOR acdoca-rldnr OBLIGATORY DEFAULT '0L',
    p_usnam  FOR bkpf-usnam .
SELECTION-SCREEN END OF BLOCK block1.
SELECTION-SCREEN BEGIN OF BLOCK block2
                          WITH FRAME TITLE TEXT-002.
PARAMETERS:
  p_vcontr TYPE syuname  MATCHCODE OBJECT sx_user,
  p_vocver TYPE syuname  MATCHCODE OBJECT sx_user,
  p_approv TYPE syuname  MATCHCODE OBJECT sx_user.

SELECTION-SCREEN END OF BLOCK block2.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT  'F_BKPF_BUK'
  ID 'ACTVT' FIELD '03'
  ID 'BUKRS' FIELD p_bukrs.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e002(zfi) WITH p_bukrs.
    EXIT.
  ENDIF.



  SELECT DISTINCT rbukrs  ,
    acdoca~gjahr   ,
    acdoca~belnr  ,
    acdoca~docln  ,
    acdoca~buzei  ,
    awref  ,
    rwcur	,
    rhcur	,
    rkcur	,
    rocur	,
    rvcur	,
    racct	,
    rcntr	,
    acdoca~prctr  ,
    rfarea  ,
    acdoca~kokrs  ,
    wsl	,
    hsl ,
    ksl	,
    osl	,
    vsl	,
    drcrk	,
    poper	,
    acdoca~periv  ,
    fiscyearper	,
    acdoca~budat  ,
    acdoca~bldat  ,
    acdoca~blart  ,
    acdoca~zuonr  ,
    acdoca~bschl  ,
    acdoca~ebeln  ,
    acdoca~ebelp  ,
    acdoca~sgtxt  ,
    kdpos	,
    acdoca~matnr  ,
    acdoca~werks  ,
    acdoca~koart  ,
    acdoca~lifnr  ,
    acdoca~werks  ,
    acdoca~kunnr  ,
    acdoca~umskz  ,
    acdoca~anln1  ,
    acdoca~anln2  ,
    acdoca~bzdat  ,
    bkpf~kursf  ,
    bkpf~bktxt 	,
    bkpf~xblnr  ,
    bkpf~usnam  ,
    bkpf~bvorg  ,
    bsed~boeno ,
    makt~maktx AS mdesc,
    a~name1 AS vdesc,
    b~name1 AS cdesc,
    skat~txt50 AS gldesc,
    anla~txt50 AS asdesc,
    @p_vcontr AS vcontr,
    @p_approv AS approv,
    @p_vocver AS vocver,
    ltext AS blartdesc,
    acdoca~augbl,
    acdoca~auggj,
    vbel2 AS kdauf,
    chect
    FROM acdoca INNER JOIN bkpf ON acdoca~belnr = bkpf~belnr AND acdoca~gjahr = bkpf~gjahr AND bkpf~bukrs = acdoca~rbukrs
    LEFT OUTER JOIN makt ON makt~matnr = acdoca~matnr AND makt~spras = 'E'
    LEFT OUTER JOIN lfa1 AS a ON a~lifnr = acdoca~lifnr AND koart = 'K'
    LEFT OUTER JOIN kna1 AS b ON b~kunnr = acdoca~kunnr  AND koart = 'D'
    LEFT OUTER JOIN skat ON skat~saknr = acdoca~racct AND skat~spras = 'E' AND skat~ktopl = acdoca~ktopl
    LEFT OUTER JOIN anla ON anla~bukrs = acdoca~rbukrs AND anla~anln1 = acdoca~anln1 AND anla~anln2 = acdoca~anln2 AND acdoca~koart = 'A'
    LEFT OUTER JOIN t003t ON acdoca~blart = t003t~blart AND t003t~spras = 'E'
    LEFT OUTER JOIN bseg ON acdoca~belnr = bseg~belnr AND acdoca~gjahr = bseg~gjahr AND bseg~bukrs = acdoca~rbukrs AND bseg~buzei = acdoca~buzei
    LEFT OUTER JOIN bsed ON acdoca~belnr = bsed~belnr AND acdoca~gjahr = bsed~gjahr AND bsed~bukrs = acdoca~rbukrs AND bsed~buzei = acdoca~buzei
    LEFT OUTER JOIN payr ON payr~vblnr = acdoca~belnr AND payr~gjahr = acdoca~gjahr AND payr~zbukr = acdoca~rbukrs
    INTO CORRESPONDING FIELDS OF TABLE @lt_result
    WHERE acdoca~rldnr in @P_RLDNR AND
          acdoca~rbukrs = @p_bukrs AND
          acdoca~gjahr = @p_gjahr AND
          acdoca~budat IN @p_budat AND
          acdoca~belnr IN @p_belnr AND
          acdoca~blart IN @p_blart AND
          acdoca~usnam IN @p_usnam.


  LOOP AT lt_result INTO ls_result.

    AUTHORITY-CHECK OBJECT  'F_BKPF_BLA'
 ID 'ACTVT' FIELD '03'
 ID 'BRGRU' FIELD ls_result-blart.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
      EXPORTING
        im_datint   = ls_result-budat
        im_datfmdes = 'C'
      IMPORTING
        ex_datext   = ls_result-fbudat
*       ex_datfmused =
      .


    CALL METHOD cl_abap_datfm=>conv_year_int_to_ext
      EXPORTING
        im_yearint  = ls_result-gjahr
        im_datfmdes = 'C'
      IMPORTING
        ex_yearext  = ls_result-fgjahr.

    SELECT SINGLE name_text FROM user_addrp INTO ls_result-vcontr_name WHERE bname = ls_result-vcontr.
    SELECT SINGLE name_text FROM user_addrp INTO ls_result-approv_name WHERE bname = ls_result-approv.
    SELECT SINGLE name_text FROM user_addrp INTO ls_result-usnam_name WHERE bname = ls_result-usnam.
    SELECT SINGLE name_text FROM user_addrp INTO ls_result-vocver_name WHERE bname = ls_result-vocver.

    SHIFT ls_result-matnr LEFT DELETING LEADING '0'.
    SHIFT ls_result-belnr LEFT DELETING LEADING '0'.
    SHIFT ls_result-awref LEFT DELETING LEADING '0'.
    SHIFT ls_result-racct LEFT DELETING LEADING '0'.
    SHIFT ls_result-zuonr LEFT DELETING LEADING '0'.
    SHIFT ls_result-kunnr LEFT DELETING LEADING '0'.
    SHIFT ls_result-lifnr LEFT DELETING LEADING '0'.
    SHIFT ls_result-xblnr LEFT DELETING LEADING '0'.
    SHIFT ls_result-augbl LEFT DELETING LEADING '0'.


    APPEND ls_result TO lt_result_fin.

  ENDLOOP.





  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RBUKRS'.
  wa_fieldcat-reptext = 'Company Code'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'GJAHR '.
  wa_fieldcat-reptext = 'Fiscal Year'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'FGJAHR '.
  wa_fieldcat-reptext = 'Fiscal Year'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BELNR'.
  wa_fieldcat-reptext = 'Document Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'DOCLN'.
  wa_fieldcat-reptext = 'Line Item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BUZEI'.
  wa_fieldcat-reptext = 'Line item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AWREF'.
  wa_fieldcat-reptext = 'Reference document'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RWCUR'.
  wa_fieldcat-reptext = 'Transaction Currency'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RHCUR'.
  wa_fieldcat-reptext = 'Company Code Currency'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RKCUR'.
  wa_fieldcat-reptext = 'Global Currency'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'ROCUR'.
  wa_fieldcat-reptext = 'Freely Defined Currency 1'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RVCUR'.
  wa_fieldcat-reptext = 'Freely Defined Currency 2'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RACCT'.
  wa_fieldcat-reptext = 'Account Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RCNTR'.
  wa_fieldcat-reptext = 'Cost Center'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PRCTR'.
  wa_fieldcat-reptext = 'Profit Center'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'RFAREA'.
  wa_fieldcat-reptext = 'Functional Area'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KOKRS'.
  wa_fieldcat-reptext = 'Controlling Area'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'WSL'.
  wa_fieldcat-reptext = 'Amount in Transaction Currency'.
  wa_fieldcat-cfieldname = 'RWCUR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'HSL'.
  wa_fieldcat-reptext = 'Amount in Company Code Currency'.
  wa_fieldcat-cfieldname = 'RHCUR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KSL'.
  wa_fieldcat-reptext = 'Amount in Global Currency'.
  wa_fieldcat-cfieldname = 'RKCUR'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'OSL'.
  wa_fieldcat-reptext = 'Amount in Freely Defined Currency 1'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VSL'.
  wa_fieldcat-reptext = 'Amount in Freely Defined Currency 2'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'DRCRK'.
  wa_fieldcat-reptext = 'Debit/Credit ind'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'POPER'.
  wa_fieldcat-reptext = 'Posting period'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PERIV'.
  wa_fieldcat-reptext = 'Fiscal Year Variant'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'FISCYEARPER'.
  wa_fieldcat-reptext = 'Period/year'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BUDAT'.
  wa_fieldcat-reptext = 'Posting Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BLDAT'.
  wa_fieldcat-reptext = 'Document Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BLART'.
  wa_fieldcat-reptext = 'Document type'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BLARTDESC'.
  wa_fieldcat-reptext = 'Doc Type Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'ZUONR'.
  wa_fieldcat-reptext = 'Assignment'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BSCHL'.
  wa_fieldcat-reptext = 'Posting Key'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'EBELN'.
  wa_fieldcat-reptext = 'Purchasing Document'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'EBELP'.
  wa_fieldcat-reptext = 'Item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SGTXT'.
  wa_fieldcat-reptext = 'Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KDAUF'.
  wa_fieldcat-reptext = 'Sales Order'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KDPOS'.
  wa_fieldcat-reptext = 'Sales order item'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'MATNR'.
  wa_fieldcat-reptext = 'Material'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'MDESC'.
  wa_fieldcat-reptext = ' Material Desription'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'WERKS'.
  wa_fieldcat-reptext = 'Plant'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KOART'.
  wa_fieldcat-reptext = 'Account type'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'LIFNR'.
  wa_fieldcat-reptext = 'Vendor'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VDESC'.
  wa_fieldcat-reptext = ' Vendor Description '.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KUNNR'.
  wa_fieldcat-reptext = 'Customer'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'CDESC'.
  wa_fieldcat-reptext = 'Customer Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'GlDESC'.
  wa_fieldcat-reptext = 'G/L Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'UMSKZ'.
  wa_fieldcat-reptext = 'Special G/L Ind'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'ANLN1'.
  wa_fieldcat-reptext = 'Asset'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'ANLN2'.
  wa_fieldcat-reptext = 'Sub-number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'ASDESC'.
  wa_fieldcat-reptext = 'Asset Description'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BZDAT'.
  wa_fieldcat-reptext = 'Asset Value Date'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'KURSF'.
  wa_fieldcat-reptext = 'Exchange rate'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BKTXT'.
  wa_fieldcat-reptext = 'Doc.Header Text'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'XBLNR'.
  wa_fieldcat-reptext = 'Reference'.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BOENO'.
  wa_fieldcat-reptext = 'Check No.'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'USNAM'.
  wa_fieldcat-reptext = 'User Name'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VCONTR'.
  wa_fieldcat-reptext = ' Voucher Controller'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'APPROV'.
  wa_fieldcat-reptext = ' Approver'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BVORG'.
  wa_fieldcat-reptext = 'Cross-CC Number'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AWKEY'.
  wa_fieldcat-reptext = 'Object key'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AUGBL'.
  wa_fieldcat-reptext = 'Clearing Doc No'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'AUGGJ'.
  wa_fieldcat-reptext = 'Clearing Doc Year'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'APPROV_NAME'.
  wa_fieldcat-reptext = 'Approver name'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VCONTR_NAME'.
  wa_fieldcat-reptext = 'controller name'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'USNAM_NAME'.
  wa_fieldcat-reptext = 'Creator Name'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VOCVER'.
  wa_fieldcat-reptext = ' Voucher Verifier'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'VOCVER_NAME'.
  wa_fieldcat-reptext = 'Verifier Name'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'CHECT'.
  wa_fieldcat-reptext = 'Check Number'.
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
*     IS_LAYOUT          =
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
      t_outtab           = lt_result_fin
*   EXCEPTIONS
*     PROGRAM_ERROR      = 1
*     OTHERS             = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
