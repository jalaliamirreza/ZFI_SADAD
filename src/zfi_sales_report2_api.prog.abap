*&---------------------------------------------------------------------*
*& Include          ZFI_SALES_REPORT2_API
*&---------------------------------------------------------------------*

FORM create_api_data.
*  BREAK mamardani.
  CASE p_moad.
    WHEN '1'. "مودیان هدر 1
      PERFORM change_data_api1.
    WHEN '2'. "مودیان فایل 2
      PERFORM change_data_api2.
    WHEN '3'. "مودیان فایل 3
      PERFORM change_data_api3.
    WHEN '4'. "مودیان فایل 4
      PERFORM change_data_api4.
    WHEN '5'. "مودیان خرده فروشی
      PERFORM change_data_api5.
    WHEN '6'. "مودیان صادرات
      PERFORM change_data_api6.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*--------------------#####################----------------------------  محاسبه مودیان هدر 1
FORM change_data_api1.

  SELECT DISTINCT i~xblnr
    FROM @gv_it_output AS i
    INTO TABLE @DATA(lt_xblnr).

  SORT gv_it_output BY xblnr.

  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
    READ TABLE gv_it_output INTO DATA(ls_out_put)
         WITH KEY xblnr = ls_xblnr-xblnr BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR ls_moadian1.
      ls_moadian1-xblnr = ls_out_put-xblnr. "شماره صورتحساب
      ls_moadian1-fkdat = ls_out_put-fkdat. "تاریخ صورتحساب
*-------------------------- " نوع صورتحساب
      IF ls_out_put-ag_ktokd_t <> 'مشتریان یکبار فروش'.
        ls_moadian1-zktokd  = '1'.
      ELSE.
        ls_moadian1-zktokd  = '2'.
        CONTINUE.
      ENDIF.
*-------------------------- " الگوی صورتحساب
      IF ls_out_put-cmwae EQ 'IRR'.
        ls_moadian1-cmwae  = '1'.
      ELSEIF ls_out_put-cmwae EQ 'USD'.
        ls_moadian1-cmwae  = '0'.
      ENDIF.
*-------------------------- " اموضوع صورتحساب
      IF     ls_out_put-fkart = 'YF24'
          OR ls_out_put-fkart = 'YF40'
          OR ls_out_put-fkart = 'YF52'
          OR ls_out_put-fkart = 'YF56'
          OR ls_out_put-fkart = 'YF28'.
        ls_moadian1-fkart1 = '3'.

      ELSEIF ls_out_put-fkart = 'YB00'
          OR ls_out_put-fkart = 'YT00'
          OR ls_out_put-fkart = 'ZB00'
          OR ls_out_put-fkart = 'ZB01'
          OR ls_out_put-fkart = 'ZT00'
          OR ls_out_put-fkart = 'ZT01'.
        ls_moadian1-fkart1 = '2'.

      ELSEIF ls_out_put-fkart = 'ZF10'
          OR ls_out_put-fkart = 'ZF11'
          OR ls_out_put-fkart = 'ZF13'
          OR ls_out_put-fkart = 'ZF16'
          OR ls_out_put-fkart = 'ZF17'
          OR ls_out_put-fkart = 'ZF18'
          OR ls_out_put-fkart = 'ZF20'
          OR ls_out_put-fkart = 'ZF21'
          OR ls_out_put-fkart = 'ZF22'
          OR ls_out_put-fkart = 'ZF23'
          OR ls_out_put-fkart = 'ZF24'
          OR ls_out_put-fkart = 'ZF25'
          OR ls_out_put-fkart = 'ZF26'
          OR ls_out_put-fkart = 'ZF27'
          OR ls_out_put-fkart = 'ZF28'
          OR ls_out_put-fkart = 'ZF30'
          OR ls_out_put-fkart = 'ZF32'
          OR ls_out_put-fkart = 'ZF33'
          OR ls_out_put-fkart = 'ZF40'
          OR ls_out_put-fkart = 'ZF41'
          OR ls_out_put-fkart = 'ZF42'
          OR ls_out_put-fkart = 'ZF60'.
        ls_moadian1-fkart1 = '1'.

      ELSEIF ls_out_put-fkart = 'ZF50'
          OR ls_out_put-fkart = 'ZF51'
          OR ls_out_put-fkart = 'ZF52'
          OR ls_out_put-fkart = 'ZF53'
          OR ls_out_put-fkart = 'ZF54'
          OR ls_out_put-fkart = 'ZF55'
          OR ls_out_put-fkart = 'ZF56'
          OR ls_out_put-fkart = 'ZF57'
          OR ls_out_put-fkart = 'ZF58'
          OR ls_out_put-fkart = 'ZF59'
          OR ls_out_put-fkart = 'ZF61'
          OR ls_out_put-fkart = 'ZF62'.
        ls_moadian1-fkart1 = '4'.
      ENDIF.
*-------------------------- " نوع انجام معامله
      IF     ls_out_put-fkart = 'ZF33'.
        ls_moadian1-fkart2 = '2'.
      ELSE.
        ls_moadian1-fkart2 = '1'.
      ENDIF.
*-------------------------- " نوع خریدار
      IF     ls_out_put-ag_org = ' '.
        ls_moadian1-ag_org1 = '1'.
      ELSE.
        ls_moadian1-ag_org1 = '2'.
      ENDIF.
*-------------------------- " شناسه ملی خریدار
      IF ls_out_put-ag_org = ''.
        ls_moadian1-ag_org2 = ls_out_put-ag_ir4.
      ELSE.
        ls_moadian1-ag_org2 = ls_out_put-ag_ir3.
      ENDIF.
      ls_moadian1-ag_ir0 = ls_out_put-ag_ir0. "کد اقتصادی خریدار
      ls_moadian1-ag_post_code = ls_out_put-ag_post_code. "کد پستی خریدار
      ls_moadian1-tasvieh = '1'. " روش تسويه
      ls_moadian1-billref = ls_out_put-billref. "ساعت صورتحساب
      ls_moadian1-bstkd = ls_out_put-bstkd. " کد شعبه خریدار
      SHIFT ls_out_put-vbeln_s LEFT DELETING LEADING '0'.
      CONCATENATE 'پیش فاکتور' ' ' ls_out_put-vbeln_s INTO ls_moadian1-description SEPARATED BY ' '.

      APPEND ls_moadian1 TO gt_moadian1.
    ENDIF.
  ENDLOOP.
  PERFORM display_alv1.
ENDFORM.
FORM display_alv1.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod1.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog1[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gt_moadian1
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
FORM build_fieldcat_mod1.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_MOADIAN1'
    CHANGING
      ct_fieldcat      = fieldcatalog1[].


ENDFORM.
*--------------------#####################---------------------------- مودیان فایل 2
FORM change_data_api2 .
  SELECT DISTINCT xblnr, matnr FROM @gv_it_output AS i INTO TABLE @DATA(lt_xblnr).
  DATA: lv_index TYPE i.
  CLEAR lv_index.
  lv_index = 1.
  SORT:lt_xblnr BY xblnr matnr,gv_it_output BY xblnr matnr .
  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
    LOOP AT gv_it_output INTO DATA(ls_out_put) FROM lv_index.
      IF ls_xblnr-xblnr NE ls_out_put-xblnr AND ls_xblnr-matnr NE ls_out_put-matnr.
        lv_index = sy-tabix.
        EXIT.
      ENDIF.
      ls_moadian2-xblnr = ls_out_put-xblnr. " شماره صورتحساب
      ls_moadian2-fkdat = ls_out_put-fkdat. " تاریخ صورتحساب
      ls_moadian2-matnr = ls_out_put-matnr. " شناسه کالا / خدمات
*------------------------------------ واحد اندازه گیری و سنجش
      CASE ls_out_put-vrkme.
        WHEN 'KG'.
          ls_moadian2-vrkme = '164'.
        WHEN 'ST'.
          ls_moadian2-vrkme = '1668'.
        WHEN 'L'.
          ls_moadian2-vrkme = '1637'.
        WHEN 'TAG'.
          ls_moadian2-vrkme = '16104'.
      ENDCASE.
      ls_moadian2-fkimg = ls_out_put-fkimg. " مقدار / تعداد
*------------------------------------ نوع ارز
      CASE ls_out_put-cmwae.
        WHEN 'IRR'.
          ls_moadian2-cmwae = '364'.
        WHEN 'USD'.
          ls_moadian2-cmwae = '840'.
        WHEN 'AED'.
          ls_moadian2-cmwae = '784'.
      ENDCASE.
      ls_moadian2-amount1 = ls_out_put-amount1. " مبلغ واحد
      ls_moadian2-amount5 = ls_out_put-amount5 * -1. " مبلغ تخفیف
      ls_moadian2-percentage = '10'. " درصد مالکیت
      ls_moadian2-amount13 = ls_out_put-amount13 * 10 / 100. " مبلغ مالیات

      APPEND ls_moadian2 TO gt_moadian2.
    ENDLOOP.
  ENDLOOP.


  PERFORM display_alv2.
ENDFORM.
FORM display_alv2.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod2.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog2[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gt_moadian2
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
FORM build_fieldcat_mod2.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_MOADIAN2'
    CHANGING
      ct_fieldcat      = fieldcatalog2[].
ENDFORM.
**--------------------#####################----------------------------  مودیان فایل 3
FORM change_data_api3 .
***  SELECT DISTINCT xblnr FROM @gv_it_output AS i INTO TABLE @DATA(lt_xblnr).
***  DATA: lv_index TYPE i.
***  CLEAR lv_index.
***  lv_index = 1.
***  SORT:lt_xblnr BY xblnr, gv_it_output BY xblnr.
***  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
***    LOOP AT gv_it_output INTO DATA(ls_out_put) FROM lv_index.
***      IF ls_xblnr-xblnr NE ls_out_put-xblnr.
***        lv_index = sy-tabix.
***        EXIT.
***      ENDIF.
***      ls_moadian3-xblnr = ls_out_put-xblnr. " شماره صورتحساب
***      ls_moadian3-fkdat = ls_out_put-fkdat. " تاریخ صورتحساب
***      ls_moadian3-pardakht = '7'. " روش پرداخت
***      ls_moadian3-amount = ls_out_put-amount13 + ls_out_put-amount4 + ls_out_put-amount6. " مبلغ پرداختی
***      ls_moadian3-fkdat2 = ls_out_put-fkdat. " تاریخ پرداخت
***      APPEND ls_moadian3 TO gt_moadian3.
***    ENDLOOP.
***  ENDLOOP.

  SELECT DISTINCT i~xblnr
    FROM @gv_it_output AS i
    INTO TABLE @DATA(lt_xblnr).

  SORT gv_it_output BY xblnr.

  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
    READ TABLE gv_it_output INTO DATA(ls_out_put)
         WITH KEY xblnr = ls_xblnr-xblnr BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR ls_moadian3.
      ls_moadian3-xblnr  = ls_out_put-xblnr.
      ls_moadian3-fkdat  = ls_out_put-fkdat.
      ls_moadian3-pardakht = '7'.
      ls_moadian3-amount = ls_out_put-amount13
                         + ls_out_put-amount4
                         + ls_out_put-amount6.
      ls_moadian3-fkdat2 = ls_out_put-fkdat.
      APPEND ls_moadian3 TO gt_moadian3.
    ENDIF.
  ENDLOOP.



  PERFORM display_alv3.
ENDFORM.
FORM display_alv3.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod3.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog3[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gt_moadian3
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
FORM build_fieldcat_mod3.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_MOADIAN3'
    CHANGING
      ct_fieldcat      = fieldcatalog3[].


ENDFORM.
**--------------------#####################----------------------------  مودیان فایل 4
FORM change_data_api4 .


data ls_out type zfi_sales_report.
ls_out-xblnr = '145577'.
ls_out-matnr = '222'.
ls_out-amount8 = '4338.65'.
APPEND ls_out to gv_it_output.

ls_out-xblnr = '145577'.
ls_out-matnr = '111'.
ls_out-amount19 = '1488.76'.
APPEND ls_out to gv_it_output.


  SELECT DISTINCT xblnr FROM @gv_it_output AS i INTO TABLE @DATA(lt_xblnr).
  DATA: lv_index TYPE i,
  lv_amount TYPE p DECIMALS 2.
  CLEAR lv_index.
  lv_index = 1.

  SORT:lt_xblnr BY xblnr, gv_it_output BY xblnr.
  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
    LOOP AT gv_it_output INTO DATA(ls_out_put) FROM lv_index.
      IF ls_xblnr-xblnr NE ls_out_put-xblnr.
        lv_index = sy-tabix.
        EXIT.
      ENDIF.
      ls_moadian4-xblnr = ls_out_put-xblnr. " شماره صورتحساب
      ls_moadian4-fkdat = ls_out_put-fkdat. " تاریخ صورتحساب
*------------------------------------ واحد اندازه گیری و سنجش
      CASE ls_out_put-vrkme.
        WHEN 'KG'.
          ls_moadian4-vrkme = '164'.
        WHEN 'ST'.
          ls_moadian4-vrkme = '1668'.
        WHEN 'L'.
          ls_moadian4-vrkme = '1637'.
      ENDCASE.
      ls_moadian4-fkimg = '1'. " مقدار / تعداد
*------------------------------------ نوع ارز
      CASE ls_out_put-cmwae.
        WHEN 'IRR'.
          ls_moadian4-cmwae = '364'.
        WHEN 'USD'.
          ls_moadian4-cmwae = '840'.
        WHEN 'AED'.
          ls_moadian4-cmwae = '784'.
      ENDCASE.
      ls_moadian4-percentage = '10'. " درصد مالکیت

*      -------------Amount8---هزينه حمل------- شرط براي تعداد سطرها
      IF ls_out_put-amount8 IS NOT INITIAL. "
        ls_moadian4-matnr = '11111111'. " شناسه کالا / خدمات
        lv_amount = ls_out_put-amount8 * 100.
        ls_moadian4-amount6 = lv_amount. " مبلغ واحد
        ls_moadian4-amount62 = lv_amount * 10 . " مبلغ مالیات
        CLEAR lv_amount.
        APPEND ls_moadian4 TO gt_moadian4.

      ENDIF.

*      -------------Amount19----هزينه خدمات و نصب------ شرط براي تعداد سطرها
      IF ls_out_put-amount19 IS NOT INITIAL. "
        ls_moadian4-matnr = '22222222'. " شناسه کالا / خدمات
        lv_amount = ls_out_put-amount19 * 100. " مبلغ واحد
        ls_moadian4-amount6 = lv_amount. " مبلغ واحد
        ls_moadian4-amount62 = lv_amount * 10 / 100. " مبلغ مالیات
        CLEAR lv_amount.
        APPEND ls_moadian4 TO gt_moadian4.
      ENDIF.

*      -------------Amount20----ساير خدمات------ شرط براي تعداد سطرها
      IF ls_out_put-amount20 IS NOT INITIAL. "
        ls_moadian4-matnr = '33333333'. " شناسه کالا / خدمات
        lv_amount = ls_out_put-amount20 * 100. " مبلغ واحد
        ls_moadian4-amount6 = lv_amount. " مبلغ واحد
        ls_moadian4-amount62 = lv_amount * 10 / 100. " مبلغ مالیات
        CLEAR lv_amount.
        APPEND ls_moadian4 TO gt_moadian4.
      ENDIF.
*      APPEND ls_moadian4 TO gt_moadian4.
    ENDLOOP.
  ENDLOOP.
  PERFORM display_alv4.
ENDFORM.
FORM display_alv4.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod4.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog4[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gt_moadian4
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
FORM build_fieldcat_mod4.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_MOADIAN4'
    CHANGING
      ct_fieldcat      = fieldcatalog4[].
ENDFORM.

**--------------------#####################----------------------5------ مودیان خرده فروشی
FORM change_data_api5 .
***  SELECT DISTINCT xblnr FROM @gv_it_output AS i INTO TABLE @DATA(lt_xblnr).
***  DATA: lv_index TYPE i.
***  CLEAR lv_index.
***  lv_index = 1.
***  SORT:lt_xblnr BY xblnr, gv_it_output BY xblnr.
***  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
***    LOOP AT gv_it_output INTO DATA(ls_out_put) FROM lv_index.
***      IF ls_xblnr-xblnr NE ls_out_put-xblnr.
***        lv_index = sy-tabix.
***        EXIT.
***      ENDIF.
  SELECT DISTINCT i~xblnr
  FROM @gv_it_output AS i
  INTO TABLE @DATA(lt_xblnr).

  SORT gv_it_output BY xblnr.

  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
    READ TABLE gv_it_output INTO DATA(ls_out_put)
         WITH KEY xblnr = ls_xblnr-xblnr BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR ls_moadian5.
      ls_moadian5-xblnr = ls_out_put-xblnr. " شماره صورتحساب
      ls_moadian5-fkdat = ls_out_put-fkdat. " تاریخ صورتحساب
      ls_moadian5-zktokd  = '2'.
*-------------------------- " الگوی صورتحساب
      IF ls_out_put-cmwae EQ 'IRR'.
        ls_moadian5-cmwae  = '1'.
      ELSEIF ls_out_put-cmwae EQ 'USD'.
        ls_moadian5-cmwae  = '2'.
      ENDIF.
*-------------------------- " اموضوع صورتحساب
      IF     ls_out_put-fkart = 'YF24'
          OR ls_out_put-fkart = 'YF40'
          OR ls_out_put-fkart = 'YF52'
          OR ls_out_put-fkart = 'YF56'
          OR ls_out_put-fkart = 'YF28'.
        ls_moadian5-fkart1 = '3'.

      ELSEIF ls_out_put-fkart = 'YB00'
          OR ls_out_put-fkart = 'YT00'
          OR ls_out_put-fkart = 'ZB00'
          OR ls_out_put-fkart = 'ZB01'
          OR ls_out_put-fkart = 'ZT00'
          OR ls_out_put-fkart = 'ZT01'.
        ls_moadian5-fkart1 = '2'.

      ELSEIF ls_out_put-fkart = 'ZF10'
          OR ls_out_put-fkart = 'ZF11'
          OR ls_out_put-fkart = 'ZF13'
          OR ls_out_put-fkart = 'ZF16'
          OR ls_out_put-fkart = 'ZF17'
          OR ls_out_put-fkart = 'ZF18'
          OR ls_out_put-fkart = 'ZF20'
          OR ls_out_put-fkart = 'ZF21'
          OR ls_out_put-fkart = 'ZF22'
          OR ls_out_put-fkart = 'ZF23'
          OR ls_out_put-fkart = 'ZF24'
          OR ls_out_put-fkart = 'ZF25'
          OR ls_out_put-fkart = 'ZF26'
          OR ls_out_put-fkart = 'ZF27'
          OR ls_out_put-fkart = 'ZF28'
          OR ls_out_put-fkart = 'ZF30'
          OR ls_out_put-fkart = 'ZF32'
          OR ls_out_put-fkart = 'ZF33'
          OR ls_out_put-fkart = 'ZF40'
          OR ls_out_put-fkart = 'ZF41'
          OR ls_out_put-fkart = 'ZF42'
          OR ls_out_put-fkart = 'ZF60'.
        ls_moadian5-fkart1 = '1'.

      ELSEIF ls_out_put-fkart = 'ZF50'
          OR ls_out_put-fkart = 'ZF51'
          OR ls_out_put-fkart = 'ZF52'
          OR ls_out_put-fkart = 'ZF53'
          OR ls_out_put-fkart = 'ZF54'
          OR ls_out_put-fkart = 'ZF55'
          OR ls_out_put-fkart = 'ZF56'
          OR ls_out_put-fkart = 'ZF57'
          OR ls_out_put-fkart = 'ZF58'
          OR ls_out_put-fkart = 'ZF59'
          OR ls_out_put-fkart = 'ZF61'
          OR ls_out_put-fkart = 'ZF62'.
        ls_moadian5-fkart1 = '4'.
      ENDIF.

      ls_moadian5-fkart2 = '1'. " نوع انجام معامله
      ls_moadian5-ag_org1 = '1'. " نوع خریدار
      ls_moadian5-ag_ir0 = ls_out_put-ag_ir0. " کد اقتصادي
*-------------------------- " شناسه ملی خریدار
      IF    ls_out_put-ag_org = ''.
        ls_moadian5-ag_org2 = ls_out_put-ag_ir4.
      ELSE.
        ls_moadian5-ag_org2 = ls_out_put-ag_ir3.
      ENDIF.
      APPEND ls_moadian5 TO gt_moadian5.
    ENDIF.
  ENDLOOP.
  PERFORM display_alv5.
ENDFORM.
FORM display_alv5.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod5.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog5[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gt_moadian5
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
FORM build_fieldcat_mod5.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_MOADIAN5'
    CHANGING
      ct_fieldcat      = fieldcatalog5[].
ENDFORM.
*--------------------#####################---------------------------- مودیان فایل 6
FORM change_data_api6 .
***  SELECT DISTINCT xblnr, matnr FROM @gv_it_output AS i INTO TABLE @DATA(lt_xblnr).
***  DATA: lv_index TYPE i.
***  CLEAR lv_index.
***  lv_index = 1.
***  SORT:lt_xblnr BY xblnr matnr,gv_it_output BY xblnr matnr .
***  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
***    LOOP AT gv_it_output INTO DATA(ls_out_put) FROM lv_index.
***      IF ls_xblnr-xblnr NE ls_out_put-xblnr AND ls_xblnr-matnr NE ls_out_put-matnr.
***        lv_index = sy-tabix.
***        EXIT.
***      ENDIF.

  SELECT DISTINCT i~xblnr
  FROM @gv_it_output AS i
  INTO TABLE @DATA(lt_xblnr).
  SORT gv_it_output BY xblnr.
  LOOP AT lt_xblnr INTO DATA(ls_xblnr).
    READ TABLE gv_it_output INTO DATA(ls_out_put)
         WITH KEY xblnr = ls_xblnr-xblnr BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR ls_moadian6.
      ls_moadian6-xblnr = ls_out_put-xblnr. " شماره صورتحساب
      ls_moadian6-fkdat = ls_out_put-fkdat. " تاریخ صورتحساب
      ls_moadian6-matnr = ls_out_put-matnr. " شناسه کالا / خدمات
*      -------------------------------- نوع کالا و خدمات
      IF ls_out_put-fkart EQ 'ZF28' OR ls_out_put-fkart EQ 'YF28'.
        ls_moadian6-fkart = '2'.
      ELSE.
        ls_moadian6-fkart = '1'.
      ENDIF.
*------------------------------------ واحد اندازه گیری و سنجش
      CASE ls_out_put-vrkme.
        WHEN 'KG'.
          ls_moadian6-vrkme = '164'.
        WHEN 'ST'.
          ls_moadian6-vrkme = '1668'.
        WHEN 'L'.
          ls_moadian6-vrkme = '1637'.
      ENDCASE.
      ls_moadian6-fkimg = ls_out_put-fkimg. " مقدار / تعداد
*------------------------------------ نوع ارز
      CASE ls_out_put-cmwae.
        WHEN 'IRR'.
          ls_moadian6-cmwae = '364'.
        WHEN 'USD'.
          ls_moadian6-cmwae = '840'.
        WHEN 'AED'.
          ls_moadian6-cmwae = '784'.
      ENDCASE.
      ls_moadian6-kurrf_rate = ls_out_put-kurrf_rate. "نرخ برابری ارز به ریال
      ls_moadian6-amount1 = ls_out_put-amount1 * 100. " میزان ارز
      ls_moadian6-amount = ls_out_put-amount1 * ls_out_put-kurrf_rate * 100. " مبلغ واحد
      APPEND ls_moadian6 TO gt_moadian6.
    ENDIF.
  ENDLOOP.

  PERFORM display_alv6.
ENDFORM.
FORM display_alv6.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod6.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog6[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gt_moadian6
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
FORM build_fieldcat_mod6.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_MOADIAN6'
    CHANGING
      ct_fieldcat      = fieldcatalog6[].
ENDFORM.
