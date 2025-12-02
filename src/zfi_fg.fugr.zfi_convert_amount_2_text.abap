FUNCTION ZFI_CONVERT_AMOUNT_2_TEXT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(AMOUNT) TYPE  ZAMOUNT_NO_DEC
*"     REFERENCE(WAERS) TYPE  WAERS DEFAULT 'IRR'
*"  EXPORTING
*"     REFERENCE(TEXT) TYPE  ZAMOUNT_TO_TEXT
*"  EXCEPTIONS
*"      DATA_TYPE_MISMATCH
*"----------------------------------------------------------------------
  DATA: maxno TYPE p.
  DATA: ktext TYPE TCURT-ktext.
  maxno = 10 ** 13.
  IF ( amount >= maxno ).
    RAISE data_type_mismatch.
  ENDIF.

*data declaration-------------------------------------------------*
  DATA: ten(10),SINGLE(6),final(130),dec(20),res TYPE I,rp(7).
  DATA: a1 TYPE I,a2 TYPE I, a3 TYPE I,str(20),d TYPE p,M TYPE I,wrdrep(50),wrdrep1(50),wrdrep2(50),wrdrep23(50),wrdrep24(50).
  DATA: cntr TYPE I,ff1 TYPE I,ff2 TYPE I,ff3 TYPE I,ff4 TYPE I,ff5 TYPE I.
  DATA: ff6 TYPE I,ff7 TYPE I,ff8 TYPE I,ff9 TYPE I,ff10 TYPE I,ff11 TYPE I,ff12 TYPE I.
  DATA: itab_words TYPE TABLE OF ZAMOUNT_TO_TEXT.
*******************************************************************
  DATA: f1 TYPE p,f2 TYPE p,f3 TYPE p,f4 TYPE p.
  d = ( amount * 100  ) DIV 100.
  res = ( amount * 100 ) MOD 100.
  f1 = d MOD 1000.
  f2 = d MOD 1000000.
  f3 = d MOD 1000000000.
  f4 = d MOD 1000000000000.

  IF f2 <> 0.
    f2 = f2 - f1.
  ENDIF.
  IF f3 <> 0.
    f3 = f3 - f2 - f1.
  ENDIF.

  IF f4 <> 0.
    f4 = f4 - f3 - f2 - f1.
  ENDIF.



  IF f4 <> 0.
    f4 = f4 DIV 1000000000.
    PERFORM gen_amounts USING f4 CHANGING wrdrep24.
    CONCATENATE wrdrep24 'ميليارد'  INTO wrdrep24 SEPARATED BY ' '.
    APPEND wrdrep24 TO itab_words.
  ENDIF.
  IF f3 <> 0.
    f3 = f3 DIV  1000000.
    PERFORM gen_amounts USING f3 CHANGING wrdrep23.
    CONCATENATE wrdrep23 'ميليون'  INTO wrdrep23 SEPARATED BY ' '.
    APPEND wrdrep23 TO itab_words.
  ENDIF.
  IF f2 <> 0.
    f2 = f2 DIV 1000.
    PERFORM gen_amounts USING f2 CHANGING wrdrep2.
    CONCATENATE wrdrep2 'هزار'  INTO wrdrep2 SEPARATED BY ' '.
    APPEND wrdrep2 TO itab_words.
  ENDIF.
  IF f1 <> 0.
    PERFORM gen_amounts USING f1 CHANGING wrdrep1.
    APPEND wrdrep1 TO itab_words.
  ENDIF.
  DATA: wa LIKE LINE OF itab_words,
        SIZE TYPE I,
        IN TYPE I VALUE 1.
  DESCRIBE TABLE  itab_words LINES SIZE.
  WHILE SIZE > 0.
    READ TABLE itab_words INTO wa INDEX IN.
    IF IN = 1.
      CONCATENATE TEXT wa  INTO TEXT .
    ELSE.
      CONCATENATE TEXT wa  INTO TEXT SEPARATED BY ' و '.
    ENDIF.
    IN = IN + 1.
    SIZE = SIZE - 1.
*    ENDLOOP.
  ENDWHILE.


  IF waers <> 'IRR'.
    SELECT SINGLE ktext FROM TCURT INTO ktext WHERE spras = 'EN' AND waers = waers.
  ELSE.
    ktext = 'ريال'.
  ENDIF.

  IF AMOUNT <> 0.
    CONCATENATE TEXT KTEXT INTO TEXT SEPARATED BY SPACE.
  ELSE.
    CONCATENATE  'صفر' KTEXT INTO TEXT SEPARATED BY SPACE.
  ENDIF.



*  IF F4 <> 0 AND F3 <> 0 AND F2 <> 0 AND F1 <> 0.
*CONCATENATE WRDREP24 WRDREP23 WRDREP2 WRDREP1 INTO string SEPARATED BY ' و '.
*ELSEIF F4 = '' AND F3 <> 0 AND F2 <> 0 AND F1 <> 0.
*  CONCATENATE  WRDREP23 WRDREP2 WRDREP1 INTO string SEPARATED BY ' و '.
*ELSEIF F4 = '' AND F3 <> 0 AND F2 <> 0 AND F1 <> 0.
*ENDIF.
*************************************************************
ENDFUNCTION.

*&---------------------------------------------------------------------*
*&      Form  GEN_amountS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->amount        text
*      -->string  text
*----------------------------------------------------------------------*
FORM gen_amounts USING amount TYPE p CHANGING string.


*data declaration-------------------------------------------------*
  DATA: ten(10),SINGLE(6),final(130),dec(20),res TYPE I,rp(7).
  DATA: a1 TYPE I,a2 TYPE I, a3 TYPE I,str(20),d TYPE p,M TYPE I,wrdrep(50).
  DATA: cntr TYPE I,ff1 TYPE I,ff2 TYPE I,ff3 TYPE I,ff4 TYPE I,ff5 TYPE I.
  DATA: ff6 TYPE I,ff7 TYPE I,ff8 TYPE I,ff9 TYPE I,ff10 TYPE I,ff11 TYPE I,ff12 TYPE I.
  DATA: f1 TYPE I,f2 TYPE I,f3 TYPE I,f4 TYPE I.
**************************************************************************
  res = ( amount * 100 ) MOD 100.
  ff1 = res DIV 10.
  ff2 = res MOD 10.
  PERFORM setnum USING ff1 ff2  CHANGING wrdrep.
  ff1 = 0. ff2 = 0.
  dec = wrdrep.
  cntr = 1.
  d = amount.
*Go in a loop dividing the amounts by 10 and store the
*residues as a digit in f1 .... f9
  WHILE ( d > 0 ).
    M = d MOD 10.
    d = d DIV 10.

    CASE cntr.
    WHEN 1. ff1 = M.
    WHEN 2. ff2 = M.
    WHEN 3. ff3 = M.
    WHEN 4. ff4 = M.
    WHEN 5. ff5 = M.
    WHEN 6. ff6 = M.
    WHEN 7. ff7 = M.
    WHEN 8. ff8 = M.
    WHEN 9. ff9 = M.
*      WHEN 10. FF10 = M.
*      WHEN 11. FF11 = M.
*      WHEN 12. FF12 = M.
    ENDCASE.
    cntr = cntr + 1.
  ENDWHILE.
  cntr = cntr - 1.
*Going in loop and sending pair of digits to function setnum to get
*the standing value of digits in words
  WHILE ( cntr > 0 ).

    IF ( cntr <= 2 ).
      PERFORM setnum USING ff2 ff1  CHANGING wrdrep.
      CONCATENATE final wrdrep INTO final SEPARATED BY ' '.
  ELSEIF ( cntr = 3 ).
      IF ( ff3 <> 0 ).
        PERFORM setnum USING 0 ff3  CHANGING wrdrep.
        CONCATENATE final wrdrep 'صد' INTO final SEPARATED BY ' '.
      ENDIF.

    ENDIF.

    cntr = cntr - 2.
  ENDWHILE.


  string = final.

  DATA: sad TYPE string,
        hundred TYPE string,
        hundred1 TYPE string,
        hundred2 TYPE string,
        hundred3 TYPE string,
        hundred4 TYPE string,
        other TYPE string.
  IF string CS 'صد'.
*  SAD = SUBSTRING_before( VAL =  amount_in_words SUB = 'صد' ).
    SPLIT string AT 'صد' INTO sad other.
*  SPLIT hundred AT ' ' INTO HUNDRED1 HUNDRED1 HUNDRED1 OTHER.
*  other = substring( val = hundred sub = ' ' ).
    CONDENSE sad NO-GAPS.
*  HUNDRED = STRLEN( SAD ).
    CASE sad.
    WHEN 'يک '.hundred = 'يکصد'.
    WHEN 'دو'.hundred = 'دويست'.
    WHEN 'سه'. hundred = 'سيصد'.
    WHEN 'چهار'. hundred = 'چهارصد'.
    WHEN 'پنج'. hundred = 'پانصد'.
    WHEN 'شش'. hundred = 'ششصد'.
    WHEN 'هفت'. hundred = 'هفتصد'.
    WHEN 'هشت'. hundred = 'هشتصد'.
    WHEN 'نه'. hundred = 'نهصد'.
    ENDCASE.
    IF sad <> '' AND other <> ''.
      CONCATENATE hundred 'و' other INTO string SEPARATED BY ' '.
  ELSEIF sad = ''.
      string = other .
  ELSEIF other = ''.
      string = hundred.
    ENDIF.
  ENDIF.


ENDFORM.                    "GEN_amountS

*&---------------------------------------------------------------------*
*&      Form  SETNUM
*&---------------------------------------------------------------------*
*       converts a amount into words                                   *
*----------------------------------------------------------------------*
*  -->  a1,a2     two digits for 2nd and 1st place
*  <--  str       outpur in words
*----------------------------------------------------------------------*
DATA: ten(10),SINGLE(6),str(20).
*
FORM setnum USING a1 a2  CHANGING str.
  ten = ''.SINGLE = ''.
  IF ( a1 = 1 ).

    CASE a2.
    WHEN 0. ten = 'ده'.
    WHEN 1. ten = 'يازده'.
    WHEN 2. ten = 'دوازده'.
    WHEN 3. ten = 'سيزده'.
    WHEN 4. ten = 'چهارده'.
    WHEN 5. ten = 'پانزده'.
    WHEN 6. ten = 'شانزده'.
    WHEN 7. ten = 'هفده'.
    WHEN 8. ten = 'هجده'.
    WHEN 9. ten = 'نوزده'.
    ENDCASE.
  ELSE.

    CASE a2.
    WHEN 1. SINGLE = 'يک'.
    WHEN 2. SINGLE = 'دو'.
    WHEN 3. SINGLE = 'سه'.
    WHEN 4. SINGLE = 'چهار'.
    WHEN 5. SINGLE = 'پنج'.
    WHEN 6. SINGLE = 'شش'.
    WHEN 7. SINGLE = 'هفت'.
    WHEN 8. SINGLE = 'هشت'.
    WHEN 9. SINGLE = 'نه'.

    ENDCASE.

    CASE a1.
    WHEN 2. ten = 'بيست'.
    WHEN 3. ten = 'سي'.
    WHEN 4. ten = 'چهل'.
    WHEN 5. ten = 'پنجاه'.
    WHEN 6. ten = 'شصت'.
    WHEN 7. ten = 'هفتاد'.
    WHEN 8. ten = 'هشتاد'.
    WHEN 9. ten = 'نود'.
    ENDCASE.

  ENDIF.



  IF  ( SINGLE <> '' ) AND ( ten <> '' ).
    CONCATENATE  ten 'و' SINGLE INTO str SEPARATED BY ''.
ELSEIF SINGLE = ''.
    str = ten.
  ELSE.
    str = SINGLE.
  ENDIF.
*  ELSEIF HUNDRED = '' AND TEN = ''.
*    STR = SINGLE.
*  ELSEIF SINGLE = '' AND TEN = ''.
*    STR = HUNDRED.
*  ELSEIF SINGLE = '' AND HUNDRED = ''.
*    STR = TEN.
*  ELSEIF HUNDRED = ''.
*    CONCATENATE  TEN 'و' SINGLE INTO STR SEPARATED BY ''.
*  ELSEIF TEN = ''.
*    CONCATENATE  HUNDRED 'و' SINGLE INTO STR SEPARATED BY ''.
*  ELSEIF SINGLE = ''.
*    CONCATENATE  HUNDRED 'و' TEN INTO STR SEPARATED BY ''.
*  ENDIF.

ENDFORM.                    "SETNUM
