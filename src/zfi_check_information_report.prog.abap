

*&---------------------------------------------------------------------*
*& Report ZFI_CHECK_INFORMATION_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFI_CHECK_INFORMATION_REPORT.



TABLES:payr.

DATA: BEGIN OF ls_result,
        zbukr           TYPE  payr-zbukr,
        valut           TYPE  bseg-valut,
        fvalut          TYPE  char10,
        "converteddate   TYPE  string,
        rwbtr           TYPE 	bapi_incinv_create_tax-tax_amount,
       " convertedamount TYPE  string,
*{   INSERT         BPCK900044                                        9
      "  convertedamount2 TYPE string,
*}   INSERT
        waers           TYPE  payr-waers,
        lifnr           TYPE  payr-lifnr,
        znme1           TYPE  payr-znme1,
        name_org2       TYPE  but000-name_org2,
        natpers         TYPE  but000-natpers,
        hbkid           TYPE  payr-hbkid,
        hktid           TYPE  payr-hktid,
        chect           TYPE  payr-chect,
        bancd           TYPE  payr-bancd,
        voidd           TYPE  payr-voidd,
        bldat           TYPE  bkpf-bldat,
        sgtxt           TYPE  bseg-sgtxt,
        usnam           TYPE  bkpf-usnam,
*       bldat           TYPE  payr-bldat,
        description	    TYPE  string,
        pridt           TYPE  payr-pridt,
        vblnr           TYPE  payr-vblnr,
        gjahr           TYPE  payr-gjahr,
        zaldt           TYPE  payr-zaldt,
        fzaldt          TYPE  char10,
        national_id     TYPE  dfkkbptaxnum-taxnum,
        nationalcode    TYPE  dfkkbptaxnum-taxnum,
        passport_no     TYPE  dfkkbptaxnum-taxnum,
        remark          TYPE  string,
        bankn           TYPE  t012k-bankn,
        branch          TYPE  bnka-brnch,
        bankn2          TYPE  tiban-bankn,
        iban            TYPE  tiban-iban,
        boeno           TYPE  bsed-boeno,

*{   INSERT         BPCK900044                                        7
    "    sum_price       TYPE bapi_incinv_create_tax-tax_amount,
*}   INSERT
      END OF ls_result,
      lt_result   LIKE TABLE OF ls_result,
      where       TYPE string,
      ct_fieldcat TYPE slis_t_fieldcat_alv,
      cs_fieldcat TYPE slis_fieldcat_alv,
      wa_fieldcat TYPE LINE OF lvc_t_fcat,
      it_fieldcat TYPE lvc_t_fcat,
      dummy       TYPE string,
      len         TYPE i,
      currdec     TYPE bapi1090_1.
      "return      TYPE bapireturn.

DATA:
  boe_com  TYPE bsed-bukrs,
  boe_doc  TYPE bsed-belnr,
  boe_year TYPE bsed-gjahr,
  boe_item TYPE bsed-buzei,
  lv_bvtyp TYPE bseg-bvtyp.


SELECTION-SCREEN BEGIN OF BLOCK block1
                          WITH FRAME TITLE TEXT-001.


 select-options : p_zbukr FOR payr-zbukr OBLIGATORY NO INTERVALS NO-EXTENSION DEFAULT '1000',
                  p_chect for payr-chect,
                  p_hbkid FOR payr-hbkid ,
                  p_hktid FOR payr-hktid,
                  p_vblnr for payr-vblnr,
                  p_zaldt FOR payr-zaldt.




SELECTION-SCREEN END OF BLOCK block1.

START-OF-SELECTION.

  AUTHORITY-CHECK OBJECT  'F_PAYR_BUK'
  ID 'ACTVT' FIELD '03'
  ID 'BUKRS' FIELD p_zbukr.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e002(zfi) WITH p_zbukr.
    EXIT.
  ENDIF.





  SELECT payr~zbukr , payr~gjahr , payr~vblnr , payr~zaldt , payr~hbkid ,payr~hktid , payr~rwbtr , payr~waers , payr~lifnr , payr~chect , payr~znme1 , payr~znme2 && payr~znme3 AS description ,payr~pridt
    , bkpf~bldat , payr~bancd , payr~voidd , bkpf~usnam
    FROM payr
     LEFT JOIN  bkpf on bkpf~BELNR = payr~VBLNR
   " join bsed on bsed~boeno = payr~chect
    WHERE bkpf~BLART  In ( 'Z1','Z3', 'AB' )   AND   bkpf~BUKRS <> '1100'  AND
     zbukr in  @p_zbukr AND
     vblnr in  @p_vblnr AND
     hbkid in  @p_hbkid AND
     hktid in  @p_hktid AND
     chect in  @p_chect AND
     zaldt in  @p_zaldt

INTO CORRESPONDING FIELDS OF TABLE @lt_result.

   " ls_result-remark = p_txt.
LOOP AT lt_result INTO ls_result.

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

      SELECT SINGLE buzei FROM bsed INTO boe_item
        WHERE boeno = ls_result-chect   AND  bukrs = ls_result-zbukr AND gjahr = ls_result-gjahr AND belnr = ls_result-vblnr .

      SELECT SINGLE zfbdt sgtxt FROM bsik INTO (ls_result-valut,ls_result-sgtxt)
    WHERE bukrs = ls_result-zbukr AND
          gjahr = ls_result-gjahr AND
          belnr = ls_result-vblnr AND
          buzei = boe_item        AND umskz = 'W'.
    ELSE.

      SELECT SINGLE bvtyp INTO lv_bvtyp
      FROM bseg WHERE bukrs = ls_result-zbukr AND
                     gjahr = ls_result-gjahr AND
                     belnr = ls_result-vblnr AND
                     lifnr <> ''.

    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      CLEAR boe_item.
     SELECT SINGLE buzei FROM bsed INTO boe_item
         WHERE boeno = ls_result-chect AND  bukrs = ls_result-zbukr AND gjahr = ls_result-gjahr AND belnr = ls_result-vblnr.
      SELECT SINGLE zfbdt sgtxt FROM bsak INTO (ls_result-valut,ls_result-sgtxt)
  WHERE bukrs = ls_result-zbukr AND
        gjahr = ls_result-gjahr AND
        belnr = ls_result-vblnr AND
        buzei = boe_item        AND umskz = 'W'.
 ENDIF.
  IF sy-subrc IS NOT INITIAL.
      CLEAR boe_item.
     SELECT SINGLE buzei FROM bsed INTO boe_item
         WHERE boeno <> ls_result-chect AND  bukrs = ls_result-zbukr AND gjahr = ls_result-gjahr AND belnr = ls_result-vblnr.
      SELECT SINGLE zfbdt sgtxt FROM bsak INTO (ls_result-valut,ls_result-sgtxt)
  WHERE bukrs = ls_result-zbukr AND
        gjahr = ls_result-gjahr AND
        belnr = ls_result-vblnr AND
        buzei = boe_item        AND umskz = 'W'.
        ENDIF.
*ELSE.
*
*         SELECT SINGLE buzei FROM bsed INTO boe_item
*         WHERE boeno <> ls_result-chect AND  bukrs = ls_result-zbukr AND gjahr = ls_result-gjahr AND belnr = ls_result-vblnr.
*      SELECT SINGLE zfbdt sgtxt FROM bsak INTO (ls_result-valut,ls_result-sgtxt)
*  WHERE bukrs = ls_result-zbukr AND
*        gjahr = ls_result-gjahr AND
*        belnr = ls_result-vblnr AND
*        buzei = boe_item        AND umskz = 'W'.
*    ENDIF.



    IF boe_item IS NOT INITIAL.
      SELECT SINGLE bvtyp INTO lv_bvtyp
        FROM bseg WHERE bukrs = ls_result-zbukr AND
                        gjahr = ls_result-gjahr AND
                        belnr = ls_result-vblnr AND
                        buzei = boe_item.
    ENDIF.


*    PERFORM convertdate USING    ls_result-valut
*                        CHANGING ls_result-converteddate ls_result-fvalut.
*
*    PERFORM convertdate USING    ls_result-zaldt
*                        CHANGING dummy ls_result-fzaldt.

   ls_result-rwbtr = ls_result-rwbtr * 100.
  MODIFY lt_result FROM ls_result.
 ENDLOOP.
  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'ZBUKR'.
  wa_fieldcat-reptext = 'کد شرکت'.

  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'FVALUT'.
*  wa_fieldcat-reptext = 'Value Date'.
*  wa_fieldcat-outputlen = 10.
*
*  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'CONVERTEDDATE'.
*  wa_fieldcat-reptext = 'Converted Date'.
*  wa_fieldcat-outputlen = 15.
*
*  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'RWBTR'.
  wa_fieldcat-decimals_o = 0.
  wa_fieldcat-reptext = 'مبلغ '.

  APPEND wa_fieldcat TO it_fieldcat.
*
*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'CONVERTEDAMOUNT'.
*  wa_fieldcat-reptext = 'Converted Amount'.
*  wa_fieldcat-outputlen = 15.
*
*  APPEND wa_fieldcat TO it_fieldcat.

*{   INSERT         BPCK900044                                       10
*  CLEAR wa_fieldcat.
*
*  wa_fieldcat-fieldname = 'SUM_PRICE'.
*  wa_fieldcat-reptext = 'SUM_PRICE'.
*  wa_fieldcat-decimals_o = 0.
* " wa_fieldcat-outputlen = 15.
*
*  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'convertedamount2'.
*  wa_fieldcat-reptext = 'convertedamount2'.
* " wa_fieldcat-outputlen = 15.
*
*  APPEND wa_fieldcat TO it_fieldcat.

*}   INSERT
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
  wa_fieldcat-reptext = 'نام وندور'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BUT000'.
  wa_fieldcat-fieldname = 'NAME_ORG2'.
  wa_fieldcat-reptext = 'Name 2'.

  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-tabname = 'BUT000'.
*  wa_fieldcat-fieldname = 'NATPERS'.
*  wa_fieldcat-reptext = 'Natural Person'.
*  wa_fieldcat-checkbox = 'X'.
*  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'HBKID'.
  wa_fieldcat-reptext = 'نام بانک'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'HKTID'.
  wa_fieldcat-reptext = 'شماره حساب'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'CHECT'.
  wa_fieldcat-reptext = 'شماره چک'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'ZALDT'.
  wa_fieldcat-reptext = 'تاريخ سررسيد'.

  APPEND wa_fieldcat TO it_fieldcat.

 CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'BLDAT'.
  wa_fieldcat-reptext = 'تاريخ صدور'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'BANCD'.
  wa_fieldcat-reptext = 'تاريخ وصولي'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'VOIDD'.
  wa_fieldcat-reptext = 'تاريخ ابطال'.

  APPEND wa_fieldcat TO it_fieldcat.



  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'SGTXT'.
  wa_fieldcat-reptext = 'شرح'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'BKPF'.
  wa_fieldcat-fieldname = 'USNAM'.
  wa_fieldcat-reptext = 'Created By'.

  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'DESCRIPTION'.
  wa_fieldcat-reptext = 'Description'.
  wa_fieldcat-outputlen = 15.

  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'PRIDT'.
*  wa_fieldcat-reptext = 'Create Date'.
*  wa_fieldcat-outputlen = 10.
*
*  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-tabname = 'PAYR'.
  wa_fieldcat-fieldname = 'VBLNR'.
  wa_fieldcat-reptext = 'شماره سند'.

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

*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'REMARK'.
*  wa_fieldcat-reptext = 'REMARK'.
*
*  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BANKN'.
  wa_fieldcat-reptext = 'Bank account number'.

  APPEND wa_fieldcat TO it_fieldcat.


  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BRANCH'.
  wa_fieldcat-reptext = 'Branch'.

  APPEND wa_fieldcat TO it_fieldcat.

*  CLEAR wa_fieldcat.
*  wa_fieldcat-fieldname = 'IBAN'.
*  wa_fieldcat-reptext = 'Sheba Code'.
*
*  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR wa_fieldcat.
  wa_fieldcat-fieldname = 'BANKN2'.
  wa_fieldcat-reptext = 'Vendor bank account'.

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
** Implement suitable error handling here
  ENDIF.
"ENDLOOP.
*
*FORM convertdate USING    begda TYPE begda
*                 CHANGING convertdate TYPE string
*                         fdate TYPE char10.
*  ENDFORM.
*
