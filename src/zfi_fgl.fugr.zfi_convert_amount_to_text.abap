FUNCTION ZFI_CONVERT_AMOUNT_TO_TEXT.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(AMOUNT) TYPE  P
*"     REFERENCE(WAERS) TYPE  WAERS DEFAULT 'IRR'
*"  EXPORTING
*"     REFERENCE(TEXT) TYPE  STRING
*"  EXCEPTIONS
*"      DATA_TYPE_MISMATCH
*"----------------------------------------------------------------------
  DATA: maxno TYPE p.
  data: ktext TYPE TCURT-ktext.
  maxno = 10 ** 13.
  IF ( amount >= maxno ).
    RAISE data_type_mismatch.
  ENDIF.

*data declaration-------------------------------------------------*
  DATA: ten(10),single(6),final(130),dec(20),res TYPE i,rp(7).
  DATA: a1 TYPE i,a2 TYPE i, a3 TYPE i,str(20),d TYPE p,m TYPE i,wrdrep(50),wrdrep1(50),wrdrep2(50),wrdrep23(50),wrdrep24(50),wrdrep25(50).
  DATA: cntr TYPE i,ff1 TYPE i,ff2 TYPE i,ff3 TYPE i,ff4 TYPE i,ff5 TYPE i.
  DATA: ff6 TYPE i,ff7 TYPE i,ff8 TYPE i,ff9 TYPE i,ff10 TYPE i,ff11 TYPE i,ff12 TYPE i.
  DATA: itab_words TYPE TABLE OF ZAMOUNT_TO_TEXT.
*******************************************************************
  DATA: f1 TYPE p,f2 TYPE p,f3 TYPE p,f4 TYPE p,f5 TYPE p.
  d = ( amount * 100  ) DIV 100.
  res = ( amount * 100 ) MOD 100.
  f1 = d MOD 1000.
  f2 = d MOD 1000000.
  f3 = d MOD 1000000000.
  f4 = d MOD 1000000000000.
  f5 = d MOD 1000000000000000.

  IF f2 <> 0.
    f2 = f2 - f1.
  ENDIF.
  IF f3 <> 0.
    f3 = f3 - f2 - f1.
  ENDIF.

  IF f4 <> 0.
    f4 = f4 - f3 - f2 - f1.
  ENDIF.

  IF f5 <> 0.
    f5 = f5 - f4 - f3 - f2 - f1.
  ENDIF.

  IF f5 <> 0.
    f5 = f5 DIV 1000000000000.
    PERFORM gen_amounts USING f5 CHANGING wrdrep25.
    CONCATENATE wrdrep25 'تريليون'  INTO wrdrep25 SEPARATED BY ' '.
    APPEND wrdrep25 TO itab_words.
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
        size TYPE i,
        in TYPE i VALUE 1.
  DESCRIBE TABLE  itab_words LINES size.
  WHILE size > 0.
    READ TABLE itab_words INTO wa INDEX in.
    IF in = 1.
      CONCATENATE text wa  INTO TEXT .
    ELSE.
      CONCATENATE text wa  INTO text SEPARATED BY ' و '.
    ENDIF.
    in = in + 1.
    size = size - 1.
*    ENDLOOP.
  ENDWHILE.


  if waers <> 'IRR'.
    select SINGLE ktext FROM TCURT INTO ktext WHERE spras = 'EN' and waers = waers.
      else.
        ktext = 'ريال'.
    endif.

  if AMOUNT <> 0.
  CONCATENATE text KTEXT INTO text SEPARATED BY SPACE.
  else.
    CONCATENATE  'صفر' KTEXT INTO text SEPARATED BY SPACE.
  endif.



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
  DATA: ten(10),single(6),final(130),dec(20),res TYPE i,rp(7).
  DATA: a1 TYPE i,a2 TYPE i, a3 TYPE i,str(20),d TYPE p,m TYPE i,wrdrep(50).
  DATA: cntr TYPE i,ff1 TYPE i,ff2 TYPE i,ff3 TYPE i,ff4 TYPE i,ff5 TYPE i.
  DATA: ff6 TYPE i,ff7 TYPE i,ff8 TYPE i,ff9 TYPE i,ff10 TYPE i,ff11 TYPE i,ff12 TYPE i.
  DATA: f1 TYPE i,f2 TYPE i,f3 TYPE i,f4 TYPE i.
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
    m = d MOD 10.
    d = d DIV 10.

    CASE cntr.
      WHEN 1. ff1 = m.
      WHEN 2. ff2 = m.
      WHEN 3. ff3 = m.
      WHEN 4. ff4 = m.
      WHEN 5. ff5 = m.
      WHEN 6. ff6 = m.
      WHEN 7. ff7 = m.
      WHEN 8. ff8 = m.
      WHEN 9. ff9 = m.
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
DATA: ten(10),single(6),str(20).
*
FORM setnum USING a1 a2  CHANGING str.
  ten = ''.single = ''.
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
      WHEN 1. single = 'يک'.
      WHEN 2. single = 'دو'.
      WHEN 3. single = 'سه'.
      WHEN 4. single = 'چهار'.
      WHEN 5. single = 'پنج'.
      WHEN 6. single = 'شش'.
      WHEN 7. single = 'هفت'.
      WHEN 8. single = 'هشت'.
      WHEN 9. single = 'نه'.

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



  IF  ( single <> '' ) AND ( ten <> '' ).
    CONCATENATE  ten 'و' single INTO str SEPARATED BY ''.
  ELSEIF single = ''.
    str = ten.
  ELSE.
    str = single.
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
