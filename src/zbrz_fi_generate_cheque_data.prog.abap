*&---------------------------------------------------------------------*
*& Report ZBRZ_FI_GENERATE_CHEQUE_DATA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbrz_fi_generate_cheque_data.

TABLES:payr.

DATA: BEGIN OF ls_result,
        zbukr           TYPE  payr-zbukr,
        valut            TYPE  bseg-valut,
        fvalut           TYPE  char10,
        converteddate    TYPE  string,
        rwbtr            TYPE  bapi_incinv_create_tax-tax_amount,
        convertedamount  TYPE  string,
        convertedamount2 TYPE  string,
        waers            TYPE  payr-waers,
        lifnr            TYPE  payr-lifnr,
        znme1            TYPE  payr-znme1,
        name_org2        TYPE  but000-name_org2,
        natpers          TYPE  but000-natpers,
        hbkid            TYPE  payr-hbkid,
        hktid            TYPE  payr-hktid,
        chect            TYPE  payr-chect,
        description	     TYPE  string,
        pridt            TYPE  payr-pridt,
        vblnr            TYPE  payr-vblnr,
        gjahr            TYPE  payr-gjahr,
        zaldt            TYPE  payr-zaldt,
        fzaldt           TYPE  char10,
        national_id      TYPE  dfkkbptaxnum-taxnum,
        nationalcode     TYPE  dfkkbptaxnum-taxnum,
        passport_no      TYPE  dfkkbptaxnum-taxnum,
        remark           TYPE  string,
        bankn            TYPE  t012k-bankn,
        branch           TYPE  bnka-brnch,
        bankn2           TYPE  tiban-bankn,
        iban             TYPE  tiban-iban,
        WLZBP            TYPE  BSED-WLZBP,
        WBANK            TYPE  BSED-WBANK,
        sgtxt            TYPE  bseg-sgtxt,
        sum_price        TYPE  bapi_incinv_create_tax-tax_amount,
      END OF ls_result,
      lt_result   LIKE TABLE OF ls_result,
      where       TYPE string,
      ct_fieldcat TYPE slis_t_fieldcat_alv,
      cs_fieldcat TYPE slis_fieldcat_alv,
      wa_fieldcat TYPE LINE OF lvc_t_fcat,
      it_fieldcat TYPE lvc_t_fcat,
      dummy       TYPE string,
      len         TYPE i,
      currdec     TYPE bapi1090_1,
      return      TYPE bapireturn.

DATA:
  boe_com  TYPE bsed-bukrs,
  WLZBP     TYPE BSED-WLZBP,
  WBANK     TYPE BSED-WBANK,
  boe_doc  TYPE bsed-belnr,
  boe_year TYPE bsed-gjahr,
  boe_item TYPE bsed-buzei,
  lv_bvtyp TYPE bseg-bvtyp.


SELECTION-SCREEN BEGIN OF BLOCK block1
                          WITH FRAME TITLE TEXT-001.
PARAMETERS:
  p_zbukr TYPE payr-zbukr OBLIGATORY.
"p_gjahr TYPE payr-gjahr.
"SELECT-OPTIONS:
"p_vblnr FOR payr-vblnr,
"p_zaldt FOR payr-zaldt OBLIGATORY.
PARAMETERS:
  p_hbkid TYPE payr-hbkid OBLIGATORY,
  p_hktid TYPE payr-hktid OBLIGATORY,
 "p_chect TYPE payr-chect OBLIGATORY,
  p_txt   TYPE string.

select-options : p_chect for payr-chect,
                 p_vblnr for payr-vblnr.

SELECTION-SCREEN END OF BLOCK block1.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT  'F_PAYR_BUK'
  ID 'ACTVT' FIELD '03'
  ID 'BUKRS' FIELD p_zbukr.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e002(zfi) WITH p_zbukr.
    EXIT.
  ENDIF.


*  IF p_gjahr IS NOT INITIAL.
*    CONCATENATE where ' gjahr = ''' p_gjahr ''' and ' INTO where.
*  ENDIF.
*
*  IF p_hbkid IS NOT INITIAL.
*    CONCATENATE where ' hbkid = ''' p_hbkid ''' and ' INTO where.
*  ENDIF.
*
*  IF p_hktid IS NOT INITIAL.
*    CONCATENATE where ' hktid = ''' p_hktid ''' and ' INTO where.
*  ENDIF.
*
*  len = strlen( where ).
*
*  IF len > 0.
*    len = len - 4.
*    where = where(len).
*  ENDIF.

  SELECT zbukr , gjahr , vblnr , zaldt , hbkid ,hktid , rwbtr , waers , lifnr , chect , znme1 , znme2 && znme3 AS description , pridt
    FROM payr INTO CORRESPONDING FIELDS OF TABLE @lt_result
    WHERE zbukr =   @p_zbukr AND
          hbkid =   @p_hbkid AND
          hktid =   @p_hktid AND
          vblnr in  @p_vblnr AND
          chect in  @p_chect.


  LOOP AT lt_result INTO ls_result.

data: sum_p TYPE bapi_incinv_create_tax-tax_amount.
    ls_result-remark = p_txt.

    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_result-national_id  WHERE taxtype = 'IR3' AND partner = ls_result-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_result-nationalcode  WHERE taxtype = 'IR4' AND partner = ls_result-lifnr.
    SELECT SINGLE taxnum FROM dfkkbptaxnum INTO ls_result-passport_no  WHERE taxtype = 'IR5' AND partner = ls_result-lifnr.
    SELECT SINGLE name_org2 natpers FROM but000 INTO CORRESPONDING FIELDS OF ls_result WHERE partner = ls_result-lifnr AND type <> '1'.
    IF ls_result-natpers IS NOT INITIAL.
      ls_result-znme1 = ls_result-name_org2.
    ENDIF.

    SELECT SINGLE concat_with_space( but000~name_first ,but000~name_last ,1 ) AS name_org2 , natpers FROM but000 WHERE partner = @ls_result-lifnr AND type = '1' INTO CORRESPONDING FIELDS OF @ls_result.
    IF sy-subrc EQ 0.
      ls_result-znme1 = ls_result-name_org2.
    ENDIF.

    SELECT SINGLE valut sgtxt FROM bsis INTO (ls_result-valut,ls_result-sgtxt)
      WHERE bukrs = ls_result-zbukr AND
            gjahr = ls_result-gjahr AND
            belnr = ls_result-vblnr AND valut <> '00000000'.


    IF sy-subrc IS NOT INITIAL.
      CLEAR boe_item.

      SELECT SINGLE buzei WLZBP WBANK FROM bsed INTO (boe_item,ls_result-wlzbp , ls_result-wbank)
        WHERE boeno = ls_result-chect AND  bukrs = ls_result-zbukr AND gjahr = ls_result-gjahr AND belnr = ls_result-vblnr.

      SELECT SINGLE zfbdt sgtxt FROM bsik INTO (ls_result-valut,ls_result-sgtxt)
    WHERE bukrs = ls_result-zbukr AND
          gjahr = ls_result-gjahr AND
          belnr = ls_result-vblnr AND
          buzei = boe_item        AND ( umskz = 'W' or umskz = '3' ).
    ELSE.

      SELECT SINGLE bvtyp INTO lv_bvtyp
      FROM bseg WHERE bukrs = ls_result-zbukr AND
                     gjahr = ls_result-gjahr AND
                     belnr = ls_result-vblnr AND
                     lifnr <> ''.

    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      CLEAR boe_item.
      SELECT SINGLE buzei  WLZBP WBANK FROM bsed INTO (boe_item,ls_result-wlzbp , ls_result-wbank)
          WHERE boeno = ls_result-chect AND  bukrs = ls_result-zbukr AND gjahr = ls_result-gjahr AND belnr = ls_result-vblnr.
      SELECT SINGLE zfbdt sgtxt FROM bsak INTO (ls_result-valut,ls_result-sgtxt)
  WHERE bukrs = ls_result-zbukr AND
        gjahr = ls_result-gjahr AND
        belnr = ls_result-vblnr AND
        buzei = boe_item        AND  ( umskz = 'W' or umskz = '3' ).

    ENDIF.

    IF boe_item IS NOT INITIAL.
      SELECT SINGLE bvtyp INTO lv_bvtyp
        FROM bseg WHERE bukrs = ls_result-zbukr AND
                        gjahr = ls_result-gjahr AND
                        belnr = ls_result-vblnr AND
                        buzei = boe_item.
    ENDIF.

    CLEAR : ls_result-iban,ls_result-bankn2.
    IF lv_bvtyp IS NOT INITIAL.
      SELECT SINGLE tiban~iban tiban~bankn INTO (ls_result-iban,ls_result-bankn2) FROM tiban JOIN lfbk ON tiban~bankn = lfbk~bankn AND tiban~bankl = lfbk~bankl AND bvtyp = lv_bvtyp AND lifnr = ls_result-lifnr.
    ELSE.
      SELECT SINGLE tiban~iban tiban~bankn INTO (ls_result-iban,ls_result-bankn2) FROM tiban JOIN lfbk ON tiban~bankn = lfbk~bankn AND tiban~bankl = lfbk~bankl AND lifnr = ls_result-lifnr.
    ENDIF.

*    IF ls_result-zbukr = '1000'.
*    ls_result-valut = ls_result-zaldt.
*    ENDIF.

    PERFORM convertdate USING    ls_result-valut
                        CHANGING ls_result-converteddate ls_result-fvalut.

    PERFORM convertdate USING    ls_result-zaldt
                        CHANGING dummy ls_result-fzaldt.

    CALL FUNCTION 'BAPI_CURRENCY_GETDECIMALS'
      EXPORTING
        currency          = ls_result-waers
      IMPORTING
        currency_decimals = currdec
        return            = return.

    IF currdec-curdecimals = 0.
      ls_result-rwbtr = ls_result-rwbtr * 100.
    ENDIF.

    ls_result-rwbtr = ls_result-rwbtr * -1.


    CALL FUNCTION 'ZFI_CONVERT_AMOUNT_TO_TEXT'
      EXPORTING
        amount = ls_result-rwbtr
      IMPORTING
        text   = ls_result-convertedamount
*       EXCEPTIONS
*       DATA_TYPE_MISMATCH       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.



    SELECT SINGLE bankn INTO ls_result-bankn FROM t012k WHERE bukrs = p_zbukr         AND
                                                              hbkid = ls_result-hbkid AND
                                                              hktid = ls_result-hktid.


    SELECT SINGLE brnch INTO ls_result-branch
      FROM t012
      JOIN bnka ON t012~bankl = bnka~bankl
      WHERE bukrs = p_zbukr         AND
            hbkid = ls_result-hbkid.

    MODIFY lt_result FROM ls_result.


 sum_p = sum_p + ls_result-rwbtr.

  ENDLOOP.


  LOOP AT lt_result INTO ls_result.
  ls_result-sum_price = sum_p.


    CALL FUNCTION 'ZFI_CONVERT_AMOUNT_TO_TEXT'
      EXPORTING
        amount = ls_result-sum_price
      IMPORTING
        text   = ls_result-convertedamount2
*       EXCEPTIONS
*       DATA_TYPE_MISMATCH       = 1
*       OTHERS = 2
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.


      MODIFY lt_result FROM ls_result.

      ENDLOOP.

    IF   ls_result-zbukr = '1100'.
      SELECT SINGLE BUDAT FROM BKPF INTO (ls_result-pridt)
      WHERE bukrs = ls_result-zbukr AND
            gjahr = ls_result-gjahr AND
            belnr = ls_result-vblnr.
    ENDIF.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'ZBUKR'.
  wa_fieldcat-reptext = 'Company Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'FVALUT'.
  wa_fieldcat-reptext = 'Value Date'.
  wa_fieldcat-outputlen = 10.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'CONVERTEDDATE'.
  wa_fieldcat-reptext = 'Converted Date'.
  wa_fieldcat-outputlen = 15.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'RWBTR'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'Amount'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'CONVERTEDAMOUNT'.
  wa_fieldcat-reptext = 'Converted Amount'.
  wa_fieldcat-outputlen = 15.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SUM_PRICE'.
  wa_fieldcat-reptext = 'SUM_PRICE'.
  wa_fieldcat-decimals_o = 0.
 " wa_fieldcat-outputlen = 15.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'convertedamount2'.
  wa_fieldcat-reptext = 'convertedamount2'.
 " wa_fieldcat-outputlen = 15.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'WAERS'.
  wa_fieldcat-reptext = 'Currency'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'LIFNR'.
  wa_fieldcat-reptext = 'Vendor Account'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'ZNME1'.
  wa_fieldcat-reptext = 'Vendor Name'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'NAME_ORG2'.
  wa_fieldcat-reptext = 'Name 2'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'NATPERS'.
  wa_fieldcat-reptext = 'Natural Person'.
  wa_fieldcat-checkbox = 'X'.
  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'HBKID'.
  wa_fieldcat-reptext = 'House Bank'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'HKTID'.
  wa_fieldcat-reptext = 'Account ID'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'CHECT'.
  wa_fieldcat-reptext = 'Check Number'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'DESCRIPTION'.
  wa_fieldcat-reptext = 'Description'.
  wa_fieldcat-outputlen = 15.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'PRIDT'.
  wa_fieldcat-reptext = 'Create Date'.
  wa_fieldcat-outputlen = 10.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'VBLNR'.
  wa_fieldcat-reptext = 'Payment Doc No.'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'GJAHR'.
  wa_fieldcat-reptext = 'Fiscal Year'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'FZALDT'.
  wa_fieldcat-reptext = 'Payment Date'.
  wa_fieldcat-outputlen = 10.

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
  wa_fieldcat-fieldname = 'REMARK'.
  wa_fieldcat-reptext = 'REMARK'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BANKN'.
  wa_fieldcat-reptext = 'Bank account number'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BRANCH'.
  wa_fieldcat-reptext = 'Branch'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'IBAN'.
  wa_fieldcat-reptext = 'Sheba Code'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BANKN2'.
  wa_fieldcat-reptext = 'Vendor bank account'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SGTXT'.
  wa_fieldcat-reptext = 'Text'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'WLZBP'.
  wa_fieldcat-reptext = 'State Central Bank'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'WBANK'.
  wa_fieldcat-reptext = 'Bank Address'.
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
*     IS_VARIANT         = 'X'
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
      t_outtab           = lt_result
*   EXCEPTIONS
*     PROGRAM_ERROR      = 1
*     OTHERS             = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

FORM convertdate USING    begda TYPE begda
                 CHANGING convertdate TYPE string
                          fdate TYPE char10.

  DATA: year(4),
        month(2),
        day(2),
        days     TYPE string,
        months   TYPE string,
        years    TYPE string,
        len      TYPE i.


  CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
    EXPORTING
      im_datint   = begda
      im_datfmdes = 'C'
    IMPORTING
      ex_datext   = fdate
*     ex_datfmused =
    .


  year = fdate(4).
  month = fdate+5(2).
  day = fdate+8(2).

  CASE day.
    WHEN '01'.
      days = 'اول'.
    WHEN '02'.
      days = 'دوم'.
    WHEN '03'.
      days = 'سوم'.
    WHEN '04'.
      days = 'چهارم'.
    WHEN '05'.
      days = 'پنجم'.
    WHEN '06'.
      days = 'ششم'.
    WHEN '07'.
      days = 'هفتم'.
    WHEN '08'.
      days = 'هشتم'.
    WHEN '09'.
      days = 'نهم'.
    WHEN '10'.
      days = 'دهم'.
    WHEN '11'.
      days = 'يازده'.
    WHEN '12'.
      days = 'دوازده'.
    WHEN '13'.
      days = 'سيزده'.
    WHEN '14'.
      days = 'چهارده'.
    WHEN '15'.
      days = 'پانزده'.
    WHEN '16'.
      days = 'شانزده'.
    WHEN '17'.
      days = 'هفده'.
    WHEN '18'.
      days = 'هجده'.
    WHEN '19'.
      days = 'نوزده'.
    WHEN '20'.
      days = 'بيست'.
    WHEN '21'.
      days = 'بيست و يک'.
    WHEN '22'.
      days = 'بيست و دو'.
    WHEN '23'.
      days = 'بيست و سه'.
    WHEN '24'.
      days = 'بيست و چهار'.
    WHEN '25'.
      days = 'بيست و پنج'.
    WHEN '26'.
      days = 'بيست و شش'.
    WHEN '27'.
      days = 'بيست و هفت'.
    WHEN '28'.
      days = 'بيست و هشت'.
    WHEN '29'.
      days = 'بيست و نه'.
    WHEN '30'.
      days = 'سي'.
    WHEN '31'.
      days = 'سي و يک'.

  ENDCASE.

  CASE month.
    WHEN '01'.
      months = 'فروردين'.
    WHEN '02'.
      months = 'ارديبهشت'.
    WHEN '03'.
      months = 'خرداد'.
    WHEN '04'.
      months = 'تير'.
    WHEN '05'.
      months = 'مرداد'.
    WHEN '06'.
      months = 'شهريور'.
    WHEN '07'.
      months = 'مهر'.
    WHEN '08'.
      months = 'آبان'.
    WHEN '09'.
      months = 'آذر'.
    WHEN '10'.
      months = 'دي'.
    WHEN '11'.
      months = 'بهمن'.
    WHEN '12'.
      months = 'اسفند'.
  ENDCASE.

  years = 'هزار و '.

  CASE year+1(1).
    WHEN '3'.
      CONCATENATE years ' سيصد و ' INTO years.
    WHEN '4'.
      CONCATENATE years ' چهار صد  و ' INTO years.
    WHEN '5'.
      CONCATENATE years ' پانصد و ' INTO years.
    WHEN '6'.
      CONCATENATE years ' ششصد و ' INTO years.
    WHEN '7'.
      CONCATENATE years ' هفتصد و ' INTO years.
    WHEN '8'.
      CONCATENATE years ' هشتصد و ' INTO years.
    WHEN '9'.
      CONCATENATE years ' نهصد و ' INTO years.


  ENDCASE.

  CASE year+2(1).
    WHEN '2'.
      CONCATENATE years ' بيست و ' INTO years.
    WHEN '3'.
      CONCATENATE years ' سي و ' INTO years.
    WHEN '4'.
      CONCATENATE years ' چهل و ' INTO years.
    WHEN '5'.
      CONCATENATE years ' پنجاه و ' INTO years.
    WHEN '6'.
      CONCATENATE years ' شصت و ' INTO years.
    WHEN '7'.
      CONCATENATE years ' هفتاد و ' INTO years.
    WHEN '8'.
      CONCATENATE years ' هشتاد و ' INTO years.
    WHEN '9'.
      CONCATENATE years ' نود و ' INTO years.
  ENDCASE.

  CASE year+3(1).
    WHEN '0'.
      len = strlen( years ).
      len = len - 3.
      years = years(len).

    WHEN '1'.
      CONCATENATE years ' يک' INTO years.
    WHEN '2'.
      CONCATENATE years ' دو' INTO years.
    WHEN '3'.
      CONCATENATE years ' سه' INTO years.
    WHEN '4'.
      CONCATENATE years ' چهار' INTO years.
    WHEN '5'.
      CONCATENATE years ' پنج' INTO years.
    WHEN '6'.
      CONCATENATE years ' شش' INTO years.
    WHEN '7'.
      CONCATENATE years ' هفت' INTO years.
    WHEN '8'.
      CONCATENATE years ' هشت' INTO years.
    WHEN '9'.
      CONCATENATE years ' نه' INTO years.
  ENDCASE.

  CONCATENATE days months years INTO convertdate SEPARATED BY space.

ENDFORM.
