PROGRAM zrggbs000 .





*---------------------------------------------------------------------*
* Corrections/ repair
* wms092357 070703 Note 638886: template routines to be used for
*                  workaround to substitute bseg-bewar from bseg-xref1/2
*---------------------------------------------------------------------*
*                                                                     *
*   Substitutions: EXIT-Formpool for Uxxx-Exits                       *
*                                                                     *
*   This formpool is used by SAP for testing purposes only.           *
*                                                                     *
*   Note: If you define a new user exit, you have to enter your       *
*         user exit in the form routine GET_EXIT_TITLES.              *
*                                                                     *
*---------------------------------------------------------------------*
INCLUDE fgbbgd00.              "Standard data types


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*
*    PLEASE INCLUDE THE FOLLOWING "TYPE-POOL"  AND "TABLES" COMMANDS  *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM         *
TYPE-POOLS: gb002. " TO BE INCLUDED IN                       "wms092357
TABLES: bkpf,      " ANY SYSTEM THAT                         "wms092357
        bseg,      " HAS 'FI' INSTALLED                      "wms092357
        cobl,                                               "wms092357
        csks,                                               "wms092357
        anlz,                                               "wms092357
        glu1.                                               "wms092357
*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*


*----------------------------------------------------------------------*
*       FORM GET_EXIT_TITLES                                           *
*----------------------------------------------------------------------*
*       returns name and title of all available standard-exits         *
*       every exit in this formpool has to be added to this form.      *
*       You have to specify a parameter type in order to enable the    *
*       code generation program to determine correctly how to          *
*       generate the user exit call, i.e. how many and what kind of    *
*       parameter(s) are used in the user exit.                        *
*       The following parameter types exist:                           *
*                                                                      *
*       TYPE                Description              Usage             *
*    ------------------------------------------------------------      *
*       C_EXIT_PARAM_NONE   Use no parameter         Subst. and Valid. *
*                           except B_RESULT                            *
*       C_EXIT_PARAM_FIELD  Use one field as param.  Only Substitution *
*       C_EXIT_PARAM_CLASS  Use a type as parameter  Subst. and Valid  *
*                                                                      *
*----------------------------------------------------------------------*
*  -->  EXIT_TAB  table with exit-name and exit-titles                 *
*                 structure: NAME(5), PARAM(1), TITEL(60)
*----------------------------------------------------------------------*
FORM get_exit_titles TABLES etab.

  DATA: BEGIN OF exits OCCURS 50,
          name(5)   TYPE c,
          param     LIKE c_exit_param_none,
          title(60) TYPE c,
        END OF exits.

  exits-name  = 'U100'.
  exits-param = c_exit_param_none.
  exits-title = TEXT-100.             "Cost center from CSKS
  APPEND exits.

  exits-name  = 'U101'.
  exits-param = c_exit_param_field.
  exits-title = TEXT-101.             "Cost center from CSKS
  APPEND exits.

* begin of insertion                                          "wms092357
  exits-name  = 'U200'.
  exits-param = c_exit_param_field.
  exits-title = TEXT-200.             "Cons. transaction type
  APPEND exits.                       "from xref1/2




  exits-name  = 'Z201'.
  exits-param = c_exit_param_none.
  exits-title = 'Z201'.             "Cons. transaction type
  APPEND exits.                       "from xref1/2

  "from xref1/2


  exits-name  = 'Z202'.
  exits-param = c_exit_param_field.
  exits-title = 'Display Sales. Data in F-37'.
  APPEND exits.


 exits-name  = 'Z666'.
  exits-param = c_exit_param_field.
  exits-title = 'Journal Change'.
  APPEND exits.

* end of insertion                                            "wms092357

************************************************************************
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  EXITS-NAME  = 'U102'.
*  EXITS-PARAM = C_EXIT_PARAM_CLASS.
*  EXITS-TITLE = TEXT-102.             "Sum is used for the reference.
*  APPEND EXITS.


***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
  INCLUDE rggbs_ps_titles.

  REFRESH etab.
  LOOP AT exits.
    etab = exits.
    APPEND etab.
  ENDLOOP.

ENDFORM.                    "GET_EXIT_TITLES


* eject
*---------------------------------------------------------------------*
*       FORM U100                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table .                   *
*---------------------------------------------------------------------*
FORM u100.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COBL-KOSTL
*              AND KOKRS EQ COBL-KOKRS.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COBL-KOSTL.
*
*    ENDIF.
*  ENDSELECT.

ENDFORM.                                                    "U100

* eject
*---------------------------------------------------------------------*
*       FORM U101                                                     *
*---------------------------------------------------------------------*
*       Reads the cost-center from the CSKS table for accounting      *
*       area '0001'.                                                  *
*       This exit uses a parameter for the cost_center so it can      *
*       be used irrespective of the table used in the callup point.   *
*---------------------------------------------------------------------*
FORM u101 USING cost_center.

*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*  SELECT * FROM CSKS
*            WHERE KOSTL EQ COST_CENTER
*              AND KOKRS EQ '0001'.
*    IF CSKS-DATBI >= SY-DATUM AND
*       CSKS-DATAB <= SY-DATUM.
*
*      MOVE CSKS-ABTEI TO COST_CENTER .
*
*    ENDIF.
*  ENDSELECT.

ENDFORM.                                                    "U101

* eject
*---------------------------------------------------------------------*
*       FORM U102                                                     *
*---------------------------------------------------------------------*
*       Inserts the sum of the posting into the reference field.      *
*       This exit can be used in FI for the complete document.        *
*       The complete data is passed in one parameter.                 *
*---------------------------------------------------------------------*


*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
* PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINES *
*        IF THE ACCOUNTING MODULE IS INSTALLED IN YOUR SYSTEM:         *
*FORM u102 USING bool_data TYPE gb002_015.
*DATA: SUM(10) TYPE C.
*
*    LOOP AT BOOL_DATA-BSEG INTO BSEG
*                    WHERE    SHKZG = 'S'.
*       BSEG-ZUONR = 'Test'.
*       MODIFY BOOL_DATA-BSEG FROM BSEG.
*       ADD BSEG-DMBTR TO SUM.
*    ENDLOOP.
*
*    BKPF-XBLNR = TEXT-001.
*    REPLACE '&' WITH SUM INTO BKPF-XBLNR.
*
*ENDFORM.


***********************************************************************
** EXIT EXAMPLES FROM PUBLIC SECTOR INDUSTRY SOLUTION
**
** PLEASE DELETE THE FIRST '*' FORM THE BEGINING OF THE FOLLOWING LINE
** TO ENABLE PUBLIC SECTOR EXAMPLE SUBSTITUTION EXITS
***********************************************************************
*INCLUDE rggbs_ps_forms.


*eject
* begin of insertion                                          "wms092357
*&---------------------------------------------------------------------*
*&      Form  u200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM u200 USING e_rmvct TYPE bseg-bewar.
  PERFORM xref_to_rmvct USING bkpf bseg 1 CHANGING e_rmvct.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  xref_to_rmvct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM xref_to_rmvct
     USING    is_bkpf         TYPE bkpf
              is_bseg         TYPE bseg
              i_xref_field    TYPE i
     CHANGING c_rmvct         TYPE rmvct.

  DATA l_msgv TYPE symsgv.
  STATICS st_rmvct TYPE HASHED TABLE OF rmvct WITH UNIQUE DEFAULT KEY.

* either bseg-xref1 or bseg-xref2 must be used as source...
  IF i_xref_field <> 1 AND i_xref_field <> 2.
    MESSAGE x000(gk) WITH 'UNEXPECTED VALUE I_XREF_FIELD ='
      i_xref_field '(MUST BE = 1 OR = 2)' ''.
  ENDIF.
  IF st_rmvct IS INITIAL.
    SELECT trtyp FROM t856 INTO TABLE st_rmvct.
  ENDIF.
  IF i_xref_field = 1.
    c_rmvct = is_bseg-xref1.
  ELSE.
    c_rmvct = is_bseg-xref2.
  ENDIF.
  IF c_rmvct IS INITIAL.
    WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
    CONCATENATE TEXT-m00 l_msgv INTO l_msgv SEPARATED BY space.
*   cons. transaction type is not specified => send an error message...
    MESSAGE e123(g3) WITH l_msgv.
*   Bitte geben Sie im Feld &1 eine Konsolidierungsbewegungsart an
  ENDIF.
* c_rmvct <> initial...
  READ TABLE st_rmvct TRANSPORTING NO FIELDS FROM c_rmvct.
  CHECK NOT sy-subrc IS INITIAL.
* cons. transaction type does not exist => send error message...
  WRITE i_xref_field TO l_msgv LEFT-JUSTIFIED.
  CONCATENATE TEXT-m00 l_msgv INTO l_msgv SEPARATED BY space.
  MESSAGE e124(g3) WITH c_rmvct l_msgv.
* KonsBewegungsart &1 ist ungültig (bitte Eingabe im Feld &2 korrigieren
ENDFORM.
* end of insertion                                            "wms092357




FORM z201.

*  BREAK omrani.
*
*
*  DATA : lv_ce41100  TYPE ce41100,
*         wa_op       TYPE bapi0017-op_concern,
*         it_copa     TYPE TABLE OF bapi_copa_data WITH HEADER LINE,
*         it_field    TYPE TABLE OF bapi_copa_field WITH HEADER LINE,
*         it_ret      TYPE TABLE OF bapiret2 WITH HEADER LINE,
*         gd_fiscalyr TYPE bapi0002_4-fiscal_year,
*         gd_fiscalp  TYPE bapi0002_4-fiscal_period.
*
*  BREAK omrani.
*
*
*  IF cobl-paobjnr IS INITIAL .
*
*
*
*    CLEAR lv_ce41100.
*    SELECT SINGLE * INTO lv_ce41100 FROM ce41100
*      WHERE kokrs       = cobl-kokrs    AND
*            bukrs       = cobl-bukrs    AND
*            copa_kostl  = cobl-kostl    AND
*            prctr       = cobl-prctr    AND
*            fkber       = cobl-fkber.
*    IF sy-subrc EQ 0.
*      cobl-paobjnr  = lv_ce41100-paobjnr.
*    ELSE.
*
*
*      wa_op = '1100'.
*
*      CLEAR it_copa.
*      REFRESH it_copa.
*
*      it_copa-record_id  = '000001'.
*      it_copa-fieldname  = 'BUKRS'.
*      it_copa-value      = cobl-bukrs.
*      APPEND it_copa.
*      CLEAR  it_copa.
*
*      it_copa-record_id  = '000001'.
*      it_copa-fieldname  = 'KOKRS'.
*      it_copa-value      = cobl-kokrs.
*      APPEND it_copa.
*      CLEAR  it_copa.
*
*      it_copa-record_id  = '000001'.
*      it_copa-fieldname  = 'COPA_KOSTL'.
*      it_copa-value      = cobl-kostl.
*      APPEND it_copa.
*      CLEAR  it_copa.
*
*      it_copa-record_id  = '000001'.
*      it_copa-fieldname  = 'FKBER'.
*      it_copa-value      = cobl-fkber.
*      APPEND it_copa.
*      CLEAR  it_copa.
*
*      it_copa-record_id  = '000001'.
*      it_copa-fieldname  = 'PRCTR'.
*      it_copa-value      = cobl-prctr.
*      APPEND it_copa.
*      CLEAR  it_copa.
*
*      REFRESH it_field.
*      CLEAR it_field.
*
*      it_field-fieldname = 'BUKRS'.
*      APPEND it_field.
*      CLEAR it_field.
*
*      it_field-fieldname = 'KOKRS'.
*      APPEND it_field.
*      CLEAR it_field.
*
*      it_field-fieldname = 'COPA_KOSTL'.
*      APPEND it_field.
*      CLEAR it_field.
*
*      it_field-fieldname = 'FKBER'.
*      APPEND it_field.
*      CLEAR it_field.
*
*      it_field-fieldname = 'PRCTR'.
*      APPEND it_field.
*      CLEAR it_field.
*
*
*      REFRESH it_ret.
*
*      BREAK omrani.
*      CALL FUNCTION 'BAPI_COPAACTUALS_POSTCOSTDATA'
*        EXPORTING
*          operatingconcern = wa_op
*          testrun          = ' '
*        TABLES
*          inputdata        = it_copa
*          fieldlist        = it_field
*          return           = it_ret.
*
*
*      READ TABLE it_ret WITH KEY type = 'E'.
*      IF sy-subrc NE 0.
*        READ TABLE it_ret WITH KEY type = 'A'.
*      ENDIF.
*      IF sy-subrc  NE 0.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        CLEAR lv_ce41100.
*        SELECT SINGLE * INTO lv_ce41100 FROM ce41100
*          WHERE  kokrs       = cobl-kokrs    AND
*                 bukrs       = cobl-bukrs    AND
*                 copa_kostl  = cobl-kostl    AND
*                 prctr       = cobl-prctr    AND
*                 fkber       = cobl-fkber.
*        IF sy-subrc EQ 0.
*          cobl-paobjnr  = lv_ce41100-paobjnr.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*  ENDIF.


ENDFORM.

FORM z202 USING lv_vbel2 TYPE bseg-vbel2.

  DATA: lv_total      TYPE vbap-netwr,
        lv_pish       TYPE vbap-netwr,
        lv_txt1(100),
        lv_txt2(100),
        lv_txt3(100),
        lv_txt4(100),
        lv_txt5(100),
        lv_amount(15).

  BREAK omrani.

  CHECK lv_vbel2 IS NOT INITIAL.


  SELECT SINGLE kna1~kunnr kna1~name1 vbak~bukrs_vf INTO (lv_txt1,lv_txt2,lv_txt5)
    FROM kna1
    JOIN vbpa ON vbpa~kunnr = kna1~kunnr
    JOIN vbak ON vbak~vbeln = vbpa~vbeln
    WHERE vbak~vbeln = lv_vbel2 AND
          vbpa~parvw = 'RG'.


  IF lv_txt5 <> bseg-bukrs.
    MESSAGE e013(zfi).
  ENDIF.

  IF bseg-kunnr IS NOT INITIAL AND bseg-kunnr <> lv_txt1.
    MESSAGE e015(zfi).
  ENDIF.



  CLEAR: lv_total,lv_pish.
  SELECT SINGLE SUM( netwr + mwsbp ) INTO @lv_total FROM vbap WHERE vbeln = @lv_vbel2.

  SELECT SINGLE SUM( dmbtr ) INTO @lv_pish FROM bsid
    WHERE bukrs = @bseg-bukrs AND
          vbel2 = @lv_vbel2   AND
      ( ( umskz = 'A' AND shkzg = 'H' ) OR
        ( umskz = 'F' AND shkzg = 'S' ) ).


  SHIFT lv_txt1 LEFT DELETING LEADING '0'.
  SHIFT lv_txt1 LEFT DELETING LEADING space.
  CONCATENATE 'کد مشتري:' lv_txt1 INTO lv_txt1 SEPARATED BY space.
  CONCATENATE 'نام مشتري:' lv_txt2 INTO lv_txt2 SEPARATED BY space.

  CLEAR lv_amount.
  WRITE lv_total TO lv_amount CURRENCY 'IRR'.
  SHIFT lv_amount LEFT DELETING LEADING space.
  CONCATENATE 'مبلغ کل سفارش فروش:' lv_amount INTO lv_txt3 SEPARATED BY space.

  CLEAR lv_amount.
  WRITE lv_pish TO lv_amount CURRENCY 'IRR'.
  SHIFT lv_amount LEFT DELETING LEADING space.
  CONCATENATE 'مبلغ پيش دريافت:' lv_amount INTO lv_txt4 SEPARATED BY space.

  CONCATENATE 'کمپاني کد:' lv_txt5 INTO lv_txt5 SEPARATED BY space.


  CALL FUNCTION 'POPUP_FOR_INTERACTION'
    EXPORTING
      headline = 'اطلاعات سفارش'
      text1    = lv_txt1
      text2    = lv_txt2
      text3    = lv_txt3
      text4    = lv_txt4
      text5    = lv_txt5
      button_1 = 'بستن'.

ENDFORM.


FORM z666 USING lv_xref1 TYPE bseg-xref1.

  BREAK jalali.
 lv_xref1 = bkpf-awkey+10(4).
 bseg-xref1 = LV_XREF1.

ENDFORM.
