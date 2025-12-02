*&---------------------------------------------------------------------*
*& Include          ZFI_SADAD_FORM
*&---------------------------------------------------------------------*

*--------------------#####################---------------------ALV-------  محاسبه مودیان هدر 1
FORM create_api_data_and_display.
  data: ls_moadian type zst_sadad.
  LOOP AT gv_it_output INTO DATA(ls_out_put).
    CLEAR ls_moadian.
    ls_moadian-internalserialnumber =  ls_out_put-xblnr. "شماره صورتحساب
    ls_moadian-issuingdatetime = ls_out_put-fkdat. "تاریخ صورتحساب
    ls_moadian-stuffServiceId = ls_out_put-shenaseh. " شناسه کالا / خدمات
    ls_moadian-matnr = ls_out_put-matnr.
    ls_moadian-stuffServiceDescription = ls_out_put-arktx. " شرح کالا/خدمت
    ls_moadian-SETTLEMENTMETHOD = 1.  " نقدي
    ls_moadian-asn_num = ls_out_put-asn_num.
    ls_moadian-asn_dat = ls_out_put-asn_dat.
    ls_moadian-netweight = ls_out_put-ntgew.
    ls_moadian-WEIGHTUOM = ls_out_put-GEWEI.
    ls_moadian-buyerpostalcode = ls_out_put-ag_post_code.
    ls_moadian-vbeln_s = ls_out_put-vbeln_s.
    ls_moadian-vbeln_b = ls_out_put-vbeln_b.
**-------------------------- " نوع صورتحساب
    IF ls_out_put-vkgrp = '170'.
      ls_moadian-invoiceType  = '2'.
    ELSE.
      ls_moadian-invoiceType  = '1'.
    ENDIF.
**-------------------------- " الگوی صورتحساب
    IF ls_out_put-CMWAE = 'IRR'.
      ls_moadian-invoicePattern  = '1'.
    ELSEIF ls_out_put-CMWAE = 'USD'.
      ls_moadian-invoicePattern  = '2'.
    ENDIF.

**-------------------------- " اموضوع صورتحساب
    IF     ls_out_put-fkart = 'YF24'
        OR ls_out_put-fkart = 'YF40'
        OR ls_out_put-fkart = 'YF52'
        OR ls_out_put-fkart = 'YF56'
        OR ls_out_put-fkart = 'YF28'.
      ls_moadian-invoiceSubject = '3'.

    ELSEIF ls_out_put-fkart = 'YB00'
        OR ls_out_put-fkart = 'YT00'
        OR ls_out_put-fkart = 'ZB00'
        OR ls_out_put-fkart = 'ZB01'
        OR ls_out_put-fkart = 'ZT00'
        OR ls_out_put-fkart = 'ZT01'.
      ls_moadian-invoiceSubject = '2'.

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
      ls_moadian-invoiceSubject = '1'.

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
      ls_moadian-invoiceSubject = '4'.
    ENDIF.
*
**-------------------------- " نوع خریدار
    IF     ls_out_put-ag_org = ' '.
      ls_moadian-typeOfBuyer = '1'.
    ELSE.
      ls_moadian-typeOfBuyer = '2'.
    ENDIF.
**-------------------------- " شناسه ملی خریدار
    IF ls_out_put-ag_org = ''.
      ls_moadian-buyerNationalIdentity = ls_out_put-ag_ir4.
    ELSE.
      ls_moadian-buyerNationalIdentity = ls_out_put-ag_ir3.
    ENDIF.
    ls_moadian-buyerEconomicNumber = ls_out_put-ag_ir0. "کد اقتصادی خریدار
    ls_moadian-buyerbranchcode = '0000'.
    ls_moadian-quantity = ls_out_put-fkimg. " مقدار / تعداد
    ls_moadian-fee = ls_out_put-amount1. " مبلغ واحد
    ls_moadian-discount = ls_out_put-amount5 * -1. " مبلغ تخفیف

*------------------------------------ نوع ارز
    IF ls_out_put-vkgrp EQ '200'.
      ls_moadian-currencyType = '840'.
    ELSE.
      ls_moadian-currencyType = '0'.
      ls_moadian-fee = ls_moadian-fee * 100.
    ENDIF.
*    *------------------------------------ واحد اندازه گیری و سنجش
    CASE ls_out_put-vrkme.
      WHEN 'KG'.
        ls_moadian-measurementUnit = '164'.
      WHEN 'ST'.
        ls_moadian-measurementUnit = '1668'.
      WHEN 'L'.
        ls_moadian-measurementUnit = '1637'.
      WHEN 'TAG'.
        ls_moadian-measurementUnit = '16104'.
    ENDCASE.
    IF ls_out_put-ag_org IS INITIAL.  " نام خريدار
      CONCATENATE ls_out_put-ag_last ' ' ls_out_put-ag_first INTO ls_moadian-buyerName SEPARATED BY ' '.
    ELSE.
    ENDIF.

    APPEND ls_moadian TO gt_moadian.
  ENDLOOP.

   PERFORM display_alv.

ENDFORM.

FORM display_alv.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.
  PERFORM build_fieldcat_mod.
  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'ALL_EVENT'
      it_fieldcat              = fieldcatalog[]
      i_save                   = 'X'
      is_layout                = is_layout
    TABLES
      t_outtab                 = gt_moadian
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.



ENDFORM.
FORM build_fieldcat_mod.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_SADAD'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


*   loop at fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_fcat>).
*     case <fs_fcat>-fieldname.
*       when 'ERROR1' or 'ERROR2' or 'ERROR3' or 'ERROR4' or 'ERROR5' OR
*            'ERROR6' or 'ERROR7' or 'ERROR8' or 'ERROR9'.
*         <fs_fcat>-seltext_s = <fs_fcat>-fieldname.
*         <fs_fcat>-seltext_m = <fs_fcat>-fieldname.
*         <fs_fcat>-seltext_l = <fs_fcat>-fieldname.
*     ENDCASE.
*   ENDLOOP.

ENDFORM.
*--------------------#####################---------------------------- مودیان فایل 2


FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ST1000'.
ENDFORM.

FORM all_event USING r_ucomm TYPE sy-ucomm  xs_selfield  TYPE slis_selfield.
  CASE r_ucomm.
    WHEN 'MOADIAN'.
      if gv_flg_token is INITIAL.
        PERFORM send_request.   "کد ارسال اينجاست
      else.
        MESSAGE 'ديتاي اين صفحه ديگر معتبر نيست به صفحه قبل برگرديد' type 'S' DISPLAY LIKE 'E'.
      endif.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
