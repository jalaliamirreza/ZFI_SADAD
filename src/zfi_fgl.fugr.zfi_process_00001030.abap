FUNCTION zfi_process_00001030.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) LIKE  BKDF STRUCTURE  BKDF
*"     VALUE(I_UF05A) LIKE  UF05A STRUCTURE  UF05A
*"     VALUE(I_XVBUP) LIKE  OFIWA-XVBUP DEFAULT 'X'
*"  TABLES
*"      T_AUSZ1 STRUCTURE  AUSZ1 OPTIONAL
*"      T_AUSZ2 STRUCTURE  AUSZ2 OPTIONAL
*"      T_AUSZ3 STRUCTURE  AUSZ_CLR OPTIONAL
*"      T_BKP1 STRUCTURE  BKP1
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEC STRUCTURE  BSEC
*"      T_BSED STRUCTURE  BSED
*"      T_BSEG STRUCTURE  BSEG
*"      T_BSET STRUCTURE  BSET
*"      T_BSEU STRUCTURE  BSEU
*"----------------------------------------------------------------------

  DATA: BEGIN OF tab_payr OCCURS 100.
          INCLUDE STRUCTURE payr.
        DATA:
                rwbtr_input  LIKE payr-rwbtr,
                valut_input  LIKE bseg-valut,
                bancd_input  LIKE payr-bancd,
                input_amount LIKE payr-rwbtr,
              END OF tab_payr.

  DATA: lv_header  TYPE          bapiache09,
        lv_it_ret  TYPE TABLE OF bapiret2,
        lv_wa_ret  LIKE LINE OF  lv_it_ret,
        lv_objkey  TYPE          bapiache02-obj_key,
        lv_it_curr TYPE TABLE OF bapiaccr09,
        lv_it_gl   TYPE TABLE OF bapiacgl09,lv_wa_gl   LIKE LINE OF lv_it_gl,
        lv_wa_curr LIKE LINE OF  lv_it_curr,
        lv_wa_bkpf LIKE LINE OF  t_bkpf,
        lv_wa_bseg LIKE LINE OF  t_bseg,
        lv_sum     TYPE          payr-rwbtr,
        lv_index   TYPE          i,
        indxkey    LIKE          indx-srtfd.


  BREAK omrani.
  CLEAR indxkey.
  CONCATENATE 'ZMFCHKI00' sy-uname INTO indxkey.
  IMPORT tab_payr FROM DATABASE rfdt(ck) ID indxkey. " Export in ZMFCHKI00.
  DELETE FROM DATABASE rfdt(ck) ID indxkey.

  CHECK tab_payr[] IS NOT INITIAL AND sy-tcode <> 'FB08'.

  CLEAR: lv_wa_bkpf,lv_wa_bseg,lv_sum.
  READ TABLE t_bkpf INTO lv_wa_bkpf INDEX 1.
  READ TABLE t_bseg INTO lv_wa_bseg WITH KEY koart = 'S'.

  lv_header-comp_code   = lv_wa_bkpf-bukrs.
  lv_header-doc_type    = 'ZB'.
  lv_header-doc_date    = lv_wa_bkpf-cpudt.
  lv_header-pstng_date  = lv_wa_bkpf-budat.
  lv_header-username    = sy-uname.



  CLEAR lv_index.



  LOOP AT tab_payr.

    lv_index = lv_index + 1.
    CLEAR lv_wa_gl.
    lv_wa_gl-itemno_acc     = lv_index.
    lv_wa_gl-gl_account     = lv_wa_bseg-hkont.
    lv_wa_gl-acct_type      = 'S'.
    lv_wa_gl-item_text      = 'انتقال از حساب پشتيبان به جاري'.
    lv_wa_gl-ref_key_3      = tab_payr-chect.
    APPEND lv_wa_gl TO lv_it_gl.

    CLEAR lv_wa_curr.
    lv_wa_curr-itemno_acc   = lv_index.
    lv_wa_curr-currency     = 'IRR'.
    lv_wa_curr-amt_doccur   = tab_payr-input_amount * 100.
    APPEND lv_wa_curr TO lv_it_curr.

    lv_sum = lv_sum + tab_payr-input_amount.


    lv_index = lv_index + 1.

    CLEAR lv_wa_gl.
    lv_wa_gl-itemno_acc     = lv_index.
    lv_wa_gl-acct_type      = 'S'.
    lv_wa_gl-item_text      = 'انتقال از حساب پشتيبان به جاري'.
    lv_wa_gl-ref_key_3      = ''.

    SELECT SINGLE bnkn2 FROM t012k INTO lv_wa_gl-gl_account WHERE hkont = lv_wa_bseg-hkont AND bukrs = lv_wa_bkpf-bukrs AND hbkid = tab_payr-hbkid AND hktid = tab_payr-hktid..
    CONCATENATE '000' lv_wa_gl-gl_account INTO lv_wa_gl-gl_account.

    APPEND lv_wa_gl TO lv_it_gl.

    CLEAR lv_wa_curr.
    lv_wa_curr-itemno_acc   = lv_index.
    lv_wa_curr-currency     = 'IRR'.
    lv_wa_curr-amt_doccur   = tab_payr-input_amount * - 100.
    APPEND lv_wa_curr TO lv_it_curr.



  ENDLOOP.




  REFRESH lv_it_ret.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = lv_header
    IMPORTING
      obj_key        = lv_objkey
    TABLES
      accountgl      = lv_it_gl
      currencyamount = lv_it_curr
      return         = lv_it_ret.



ENDFUNCTION.
