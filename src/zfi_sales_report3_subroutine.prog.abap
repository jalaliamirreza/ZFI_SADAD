*&---------------------------------------------------------------------*
*& Include          ZFI_SALES_REPORT3_SUBROUTINE
*&---------------------------------------------------------------------*
FORM items_type_one using is_invoice_header type gtyp_result. " ---------------------------------- نوع 1



  ls_request-clientid = '1E8FO4SD3ANB'.   " شناسه کلاينت
****    CONCATENATE 'پیش فاکتور' ' ' ls_out_put-vbeln_s INTO ls_request-description SEPARATED BY ' '. " توضيحات
***
***    "******************************************************** Shipping
***    ls_shipping-stuffServiceId = ls_out_put-matnr. " شناسه کالا / خدمات
***    ls_shipping-stuffServiceDescription = ls_out_put-arktx. " شرح کالا/خدمت
***
***    "******************************************************************************** Header
* *-------------------------- " نوع صورتحساب
*    IF ls_out_put-vkgrp = '170'.
*      zktokd  = '2'.
*    ELSE.
*      zktokd  = '1'.
*    ENDIF.
  PERFORM get_current_timestamp_ms CHANGING lv_timestamp_ms.


  ls_request-internalSerialNumber = is_invoice_header-internalserialnumber. " شماره صورتحساب

**-------------------------- " الگوی صورتحساب
*  IF ls_out_put-CMWAE = 'IRR'.
*    zcmwae  = '1'.
*  ELSEIF ls_out_put-CMWAE = 'USD'.
*    zcmwae  = '2'.
*  ENDIF.
**-------------------------- " نوع خریدار
*  IF     ls_out_put-ag_org = ''.
*    zag_org1 = '1'.
*  ELSE.
*    zag_org1 = '2'.
*  ENDIF.
****    *-------------------------- " شناسه ملی خریدار
***    IF ls_out_put-ag_org = ''.
***      zg_org2 = ls_out_put-ag_ir4.
***    ELSE.
***      zg_org2 = ls_out_put-ag_ir3.
***    ENDIF.
*-------------------------- " اموضوع صورتحساب

*CLEAR zfkart.
*  IF     ls_out_put-fkart = 'YF24'
*      OR ls_out_put-fkart = 'YF40'
*      OR ls_out_put-fkart = 'YF52'
*      OR ls_out_put-fkart = 'YF56'
*      OR ls_out_put-fkart = 'YF28'.
*    zfkart = '3'.
*
*  ELSEIF ls_out_put-fkart = 'YB00'
*      OR ls_out_put-fkart = 'YT00'
*      OR ls_out_put-fkart = 'ZB00'
*      OR ls_out_put-fkart = 'ZB01'
*      OR ls_out_put-fkart = 'ZT00'
*      OR ls_out_put-fkart = 'ZT01'.
*    zfkart = '2'.
*
*  ELSEIF ls_out_put-fkart = 'ZF10'
*      OR ls_out_put-fkart = 'ZF11'
*      OR ls_out_put-fkart = 'ZF13'
*      OR ls_out_put-fkart = 'ZF16'
*      OR ls_out_put-fkart = 'ZF17'
*      OR ls_out_put-fkart = 'ZF18'
*      OR ls_out_put-fkart = 'ZF20'
*      OR ls_out_put-fkart = 'ZF21'
*      OR ls_out_put-fkart = 'ZF22'
*      OR ls_out_put-fkart = 'ZF23'
*      OR ls_out_put-fkart = 'ZF24'
*      OR ls_out_put-fkart = 'ZF25'
*      OR ls_out_put-fkart = 'ZF26'
*      OR ls_out_put-fkart = 'ZF27'
*      OR ls_out_put-fkart = 'ZF28'
*      OR ls_out_put-fkart = 'ZF30'
*      OR ls_out_put-fkart = 'ZF32'
*      OR ls_out_put-fkart = 'ZF33'
*      OR ls_out_put-fkart = 'ZF40'
*      OR ls_out_put-fkart = 'ZF41'
*      OR ls_out_put-fkart = 'ZF42'
*      OR ls_out_put-fkart = 'ZF60'.
*    zfkart = '1'.
*
*  ELSEIF ls_out_put-fkart = 'ZF50'
*      OR ls_out_put-fkart = 'ZF51'
*      OR ls_out_put-fkart = 'ZF52'
*      OR ls_out_put-fkart = 'ZF53'
*      OR ls_out_put-fkart = 'ZF54'
*      OR ls_out_put-fkart = 'ZF55'
*      OR ls_out_put-fkart = 'ZF56'
*      OR ls_out_put-fkart = 'ZF57'
*      OR ls_out_put-fkart = 'ZF58'
*      OR ls_out_put-fkart = 'ZF59'
*      OR ls_out_put-fkart = 'ZF61'
*      OR ls_out_put-fkart = 'ZF62'.
*    zfkart = '4'.
*  ENDIF.
*    ------------------------------ Header
  DATA(ls_head) = VALUE zmoad_header(
    invoiceType       = is_invoice_header-invoicetype " نوع صورتحساب
    invoicePattern            = is_invoice_header-INVOICEPATTERN " الگوی صورتحساب
    invoiceSubject = is_invoice_header-INVOICESUBJECT " موضوع صورتحساب
    settlementMethod = is_invoice_header-SETTLEMENTMETHOD " روش تسويه
    typeOfBuyer            = is_invoice_header-TYPEOFBUYER " نوع شخص خریدار
    buyerEconomicNumber = is_invoice_header-BUYERECONOMICNUMBER "کد اقتصادی خریدار
    buyerNationalIdentityCode = is_invoice_header-BUYERNATIONALIDENTITY "کد ملي خريدار
    buyerPostalCode = is_invoice_header-buyerpostalcode "کد پستي خريدار
    buyerBranchCode = is_invoice_header-BUYERbranchcode "کد شعبه خريدار
    issuingDateTime = lv_timestamp_ms "1751981468000 " تاریخ و زمان صدور صورتحساب میلادی
    buyerName            = is_invoice_header-buyername " نام خريدار
    buyerId = '' "'868c5288-7591-4a7a-95a0-71e67c514185'
    cottageDeclarationCustomsNu = is_invoice_header-asn_num " شماره کوتاژ اظهارنامه گمرکی
    cottageDeclarationCustomsDate = is_invoice_header-asn_dat " تاریخ کوتاژ اظهارنامه گمرکی

*      invoiceShippingGoods = VALUE zmoad_t_shipping( ( ls_shipping ) )
).

  ls_request-invoiceheader = ls_head.
***
***    "******************************************************************************** Bodies
*  *------------------------------------ واحد اندازه گیری و سنجش
*CLEAR zvrkme.
*  CASE ls_out_put-vrkme.
*    WHEN 'KG'.
*      zvrkme = '164'.
*    WHEN 'ST'.
*      zvrkme = '1668'.
*    WHEN 'L'.
*      zvrkme = '1637'.
*    WHEN 'TAG'.
*      zvrkme = '16104'.
*  ENDCASE.
****------------------------------------ نوع ارز
***    IF ls_out_put-vkgrp EQ '200'.
***      ycmwae = '840'.
***    ELSE.
***      ycmwae = '0'.
***    ENDIF.

loop at gt_moadian1 into ls_moadian1 WHERE internalserialnumber = is_invoice_header-internalserialnumber.
*    ------------------------------ body
  DATA(ls_body) = VALUE zmoad_bodies(
    stuffServiceId =   ls_moadian1-STUFFSERVICEID " شناسه کالا / خدمات
    stuffServiceDescription = ls_moadian1-STUFFSERVICEDESCRIPTION " شرح کالا/خدمت
    quantity            = ls_moadian1-QUANTITY " مقدار / تعداد
    measurementUnit = ls_moadian1-MEASUREMENTUNIT " واحد اندازه گیری و سنجش
    fee =  ls_moadian1-FEE " مبلغ واحد
    discount =   ls_moadian1-discount * -1  " مبلغ تخفیف



*      netWeight =  ls_out_put-brgew " وزن خالص
*      netWeight =  ls_out_put-NTGEW " وزن خالص
      netWeight =  ls_moadian1-netweight " وزن خالص
      currencyType =  ls_moadian1-currencytype " نوع ارز
      sourceVat =   '10'  " ماخذ مالیات بر ارزش افزوده
*    id = 1
*      measurementUnitName = 'tetst'
     ).
  APPEND ls_body TO ls_request-invoicebodies.

 ENDLOOP.

ENDFORM.

*FORM items_type_two. " ---------------------------------------------- نوغ 2
*  ls_request2-clientid = '1E8FO4SD3ANB'.   " شناسه کلاينت
*
*  PERFORM get_current_timestamp_ms CHANGING lv_timestamp_ms. " format date
**-------------------------- " الگوی صورتحساب
*  IF ls_out_put-CMWAE = 'IRR'.
*    zcmwae  = '1'.
*  ELSEIF ls_out_put-CMWAE = 'USD'.
*    zcmwae  = '2'.
*  ENDIF.
**  *-------------------------- " شناسه ملی خریدار
*    IF ls_out_put-ag_org = ''.
*      ag_org2 = ls_out_put-ag_ir4.
*    ELSE.
*      ag_org2 = ls_out_put-ag_ir3.
*    ENDIF.
**    ------------------------------ Header2
*  DATA(ls_head2) = VALUE zmoad_header2(
*    issuingDateTime = lv_timestamp_ms " صدور زمان و تاریخ صورتحساب
*    invoiceType = 2 " نوع صورتحساب
*    invoicePattern            = zcmwae " الگوی صورتحساب
*    typeOfBuyer            = zag_org1 " نوع شخص خریدار
*    buyerName            = ls_out_put-ag_org " نام خريدار
*    buyerNationalIdentityCode = AG_ORG2 "کد ملي خريدار
*    buyerEconomicNumber = ls_out_put-ag_ir0 "کد اقتصادی خریدار
*    buyerPostalCode = ls_out_put-AG_POST_CODE "کد پستي خريدار
*    buyerBranchCode = ls_out_put-bstkd " کد شعبه خریدار
*
*    cottageDeclarationCustomsNu = ls_out_put-asn_num " شماره کوتاژ اظهارنامه گمرکی
*    cottageDeclarationCustomsDate = ls_out_put-asn_dat " تاریخ کوتاژ اظهارنامه گمرکی
*    settlementMethod = 1 " روش تسويه
*).
*
*  ls_request2-invoiceheader = ls_head2.
*
**  *------------------------------------ واحد اندازه گیری و سنجش
*CLEAR zvrkme.
*  CASE ls_out_put-vrkme.
*    WHEN 'KG'.
*      zvrkme = '164'.
*    WHEN 'ST'.
*      zvrkme = '1668'.
*    WHEN 'L'.
*      zvrkme = '1637'.
*    WHEN 'TAG'.
*      zvrkme = '16104'.
*  ENDCASE.
*
**------------------------------------ نوع ارز
*    IF ls_out_put-vkgrp EQ '200'.
*      ycmwae = '840'.
*    ELSE.
*      ycmwae = '0'.
*    ENDIF.
**    ------------------------------ body2
*  DATA(ls_body2) = VALUE zmoad_bodies2(
*
*    stuffServiceId = ls_out_put-matnr " شناسه کالا / خدمات
*    stuffServiceDescription = ls_out_put-arktx " شرح کالا/خدمت
*    quantity            = ls_out_put-fkimg " مقدار / تعداد
*    measurementUnit = zvrkme " واحد اندازه گیری و سنجش
*    netWeight =  ls_out_put-brgew " وزن خالص
*    fee =  ls_out_put-amount1 " مبلغ واحد
*    currencyType =  ycmwae " نوع ارز
*    discount =   ls_out_put-amount5 * -1  " مبلغ تخفیف
*    sourceVat =   '10'  " ماخذ مالیات بر ارزش افزوده
**      id = 1
**      measurementUnitName = 'tetst'
* ).
*  APPEND ls_body2 TO ls_request2-invoicebodies.
*
*ENDFORM.
FORM translate_fields.

  lt_mapping = VALUE /ui2/cl_json=>name_mappings(
    ( abap = 'CLIENTID'                            json = 'clientId' )
    ( abap = 'ISFAKE'                              json = 'isFake' )
    ( abap = 'ISPROFORMAINVOICE'                   json = 'isProformaInvoice' )
    ( abap = 'ISUPDATE'                            json = 'isUpdate' )
    ( abap = 'DESCRIPTION'                         json = 'description' )
    ( abap = 'INTERNALSERIALNUMBER'                json = 'internalSerialNumber' )
    ( abap = 'INVOICEHEADER'                       json = 'invoiceHeader' )
    ( abap = 'ISSUINGDATETIME'                     json = 'issuingDateTime' )
    ( abap = 'INVOICETYPE'                         json = 'invoiceType' )
    ( abap = 'INVOICEPATTERN'                      json = 'invoicePattern' )
    ( abap = 'INVOICESUBJECT'                      json = 'invoiceSubject' )
    ( abap = 'TYPEOFBUYER'                         json = 'typeOfBuyer' )
    ( abap = 'BUYERNAME'                           json = 'buyerName' )
    ( abap = 'BUYERLANDLINENUMBER'                 json = 'buyerLandlineNumber' )
    ( abap = 'BUYERADDRESS'                        json = 'buyerAddress' )
    ( abap = 'BUYERNATIONALIDENTITYCODE'           json = 'buyerNationalIdentityCode' )
    ( abap = 'BUYERECONOMICNUMBER'                 json = 'buyerEconomicNumber' )
    ( abap = 'BUYERPOSTALCODE'                     json = 'buyerPostalCode' )
    ( abap = 'BUYERBRANCHCODE'                     json = 'buyerBranchCode' )
    ( abap = 'FLIGHTTYPE'                          json = 'flightType' )
    ( abap = 'BUYERPASSPORTNUMBER'                 json = 'buyerPassportNumber' )
    ( abap = 'SERIALCUSTOMSLICENSENUMBER'          json = 'serialCustomsLicenseNumber' )
    ( abap = 'SELLERCUSTOMSCODE'                   json = 'sellerCustomsCode' )
    ( abap = 'COTTAGEDECLARATIONCUSTOMSNU'         json = 'cottageDeclarationCustomsNumber' )
    ( abap = 'COTTAGEDECLARATIONCUSTOMSDATE'       json = 'cottageDeclarationCustomsDate' )
    ( abap = 'CONTACTREGISTRATIONNUMBER'           json = 'contactRegistrationNumber' )
    ( abap = 'BILLID'                              json = 'billId' )
    ( abap = 'SETTLEMENTMETHOD'                    json = 'settlementMethod' )
    ( abap = 'CASHPAYMENTVALUE'                    json = 'cashPaymentValue' )
    ( abap = 'TAX17'                               json = 'tax17' )
    ( abap = 'AGENCYEConOMICNUMBER'                json = 'agencyEconomicNumber' )
    ( abap = 'TOTALNETWEIGHT'                      json = 'totalNetWeight' )
    ( abap = 'LADINGNUMBER'                        json = 'ladingNumber' )
    ( abap = 'LADINGREFERENCENUMBER'               json = 'ladingReferenceNumber' )
    ( abap = 'ORIGINCOUNTRY'                       json = 'originCountry' )
    ( abap = 'ORIGINCITY'                          json = 'originCity' )
    ( abap = 'DESTINATIONCOUNTRY'                  json = 'destinationCountry' )
    ( abap = 'DESTINATIONCITY'                     json = 'destinationCity' )
    ( abap = 'TRANSMITTERNATIONALIDENTITY'         json = 'transmitterNationalIdentityCode' )
    ( abap = 'RECEIVERNATIONALIDENTITYCODE'        json = 'receiverNationalIdentityCode' )
    ( abap = 'LADINGTYPE'                          json = 'ladingType' )
    ( abap = 'CARRIERNUMBER'                       json = 'carrierNumber' )
    ( abap = 'DRIVERNATIONALIDENTITYCODE'          json = 'driverNationalIdentityCode' )
    ( abap = 'ANNOUNCEMENTSALESNUMBER'             json = 'announcementSalesNumber' )
    ( abap = 'ANNOUNCEMENTSALESDATE'               json = 'announcementSalesDate' )
    ( abap = 'INVOICESHIPPINGGOODS'                json = 'invoiceShippingGoods' )
    ( abap = 'STUFFSERVICEID'                      json = 'stuffServiceId' )
    ( abap = 'STUFFSERVICEDESCRIPTION'             json = 'stuffServiceDescription' )
    ( abap = 'INVOICEHEADERID'                     json = 'invoiceHeaderId' )
    ( abap = 'BUYERID'                             json = 'buyerId' )
    ( abap = 'INVOICEBODIES'                       json = 'invoiceBodies' )
    ( abap = 'QUANTITY'                            json = 'quantity' )
    ( abap = 'ID'                                  json = 'id' )
    ( abap = 'MEASUREMENTUNIT'                     json = 'measurementUnit' )
    ( abap = 'MEASUREMENTUNITNAME'                 json = 'measurementUnitName' )
    ( abap = 'NETWEIGHT'                           json = 'netWeight' )
    ( abap = 'FEE'                                 json = 'fee' )
    ( abap = 'CURRENCYUNITAMOUNT'                  json = 'currencyUnitAmount' )
    ( abap = 'CURRENCYTYPE'                        json = 'currencyType' )
    ( abap = 'EXCHANGERATE'                        json = 'exchangeRate' )
    ( abap = 'STUFFSERVICERIALVALUE'              json = 'stuffServiceRialValue' )
    ( abap = 'STUFFSERVICECURRENCYVALUE'           json = 'stuffServiceCurrencyValue' )
    ( abap = 'DISCOUNT'                            json = 'discount' )
    ( abap = 'AFTERDISCOUNT'                       json = 'afterDiscount' )
    ( abap = 'OTHERDUTIESTAX'                      json = 'otherDutiesTax' )
    ( abap = 'OTHERDUTIESRATE'                     json = 'otherDutiesRate' )
    ( abap = 'OTHERDUTIESAMOUNT'                   json = 'otherDutiesAmount' )
    ( abap = 'OTHERLEGALTAX'                       json = 'otherLegalTax' )
    ( abap = 'OTHERLEGALRATE'                      json = 'otherLegalRate' )
    ( abap = 'OTHERLEGALAMOUNT'                    json = 'otherLegalAmount' )
    ( abap = 'CONSTRUCTIONFEE'                     json = 'constructionFee' )
    ( abap = 'SELLERPROFIT'                        json = 'sellerProfit' )
    ( abap = 'OPERATIONSTIPEND'                    json = 'operationStipend' )
    ( abap = 'OPERATIONSTIPENDREGISTERNUMBER'      json = 'operationStipendRegisterNumber' )
    ( abap = 'CUTIE'                               json = 'cutie' )
    ( abap = 'CURRENCYPRICE'                       json = 'currencyPrice' )
    ( abap = 'SOURCEVAT'                           json = 'sourceVat' )
    ( abap = 'INVOICEPAYMENTS'                     json = 'invoicePayments' )
    ( abap = 'PAYMENTSWITCHNUMBER'                 json = 'paymentSwitchNumber' )
    ( abap = 'ACCEPTANCENUMBER'                    json = 'acceptanceNumber' )
    ( abap = 'TERMINAL'                            json = 'terminal' )
    ( abap = 'PAYMENTTYPE'                         json = 'paymentType' )
    ( abap = 'TRACKPAYMENTNUMBER'                  json = 'trackPaymentNumber' )
    ( abap = 'PAYERCARDNUMBER'                     json = 'payerCardNumber' )
    ( abap = 'PAYERNATIONALIDENTITYCODE'           json = 'payerNationalIdentityCode' )
    ( abap = 'PAYMENTDATE'                         json = 'paymentDate' )
    ( abap = 'PAYMENTVALUE'                        json = 'paymentValue' )
  ).

*  CALL METHOD /ui2/cl_json=>serialize
*    EXPORTING
*      data          = ls_request
*      name_mappings = lt_mapping
*    RECEIVING
*      r_json        = lv_json.
*
*  cl_demo_output=>display( lv_json ).

ENDFORM.
FORM display_alv.
  DATA: is_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output.

  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'ALL_EVENT'
      it_fieldcat              = fieldcatalog1[]
      i_save                   = 'X'
      is_layout                = is_layout
    TABLES
      t_outtab                 = gt_moadian1
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

FORM get_current_timestamp_ms CHANGING ev_timestamp_ms TYPE int8.

  DATA: lv_current_ts   TYPE timestampl,
        lv_epoch_ts     TYPE timestampl VALUE '19700101000000.0000000',
        lv_diff_seconds TYPE i.

  GET TIME STAMP FIELD lv_current_ts.

  CALL METHOD cl_abap_tstmp=>subtract
    EXPORTING
      tstmp1 = lv_current_ts
      tstmp2 = lv_epoch_ts
    RECEIVING
      r_secs = lv_diff_seconds.

  ev_timestamp_ms = lv_diff_seconds * 1000.

ENDFORM.
