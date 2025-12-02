*&---------------------------------------------------------------------*
*& Include          ZFI_SADAD_API
*&---------------------------------------------------------------------*

FORM send_request .
  gv_flg_token = ''.
  DATA: lv_token TYPE string,
        toTime   TYPE p LENGTH 16 DECIMALS 0,
        lt_invoice_header type table of gtyp_result,
        ls_invoice_header type gtyp_result,
        ls_request type zmoad_main.



if p_test = 'X'.
  lv_mobile_number = gc_mobile_number_test.
  lv_password = gc_password_test.
  lv_shenaseh_client = gc_shenaseh_client_test.
  lv_destination_api = gc_destination_api_test.
  lv_dest_token = gc_dest_token_test.
else.
  lv_mobile_number = gc_mobile_number.
  lv_password = gc_password.
  lv_shenaseh_client = gc_shenaseh_client.
  lv_destination_api = gc_destination_api.
  lv_dest_token = gc_dest_token.
endif.

  clear: ls_request.

  loop at gt_moadian into data(ls_moadian_temp).
    MOVE-CORRESPONDING ls_moadian_temp to ls_invoice_header.
    clear: ls_invoice_header-taxid,
           ls_invoice_header-error1,
           ls_invoice_header-error2,
           ls_invoice_header-error3,
           ls_invoice_header-error4,
           ls_invoice_header-error5.

    append ls_invoice_header to lt_invoice_header.
  ENDLOOP.

  sort lt_invoice_header by internalserialnumber.
  delete ADJACENT DUPLICATES FROM lt_invoice_header COMPARING internalserialnumber.

  PERFORM get_token USING lv_token.

  if lv_token is INITIAL.
    MESSAGE 'ارتباط برقرار نشد' type 'S' DISPLAY LIKE 'E'.
    return.
  endif.

  LOOP AT lt_invoice_header ASSIGNING FIELD-SYMBOL(<fs_invoice_header>).

    PERFORM create_request USING <fs_invoice_header> ls_request.

    PERFORM api_send_data USING lv_token ls_request CHANGING <fs_invoice_header>.

    CLEAR:ls_request.

  ENDLOOP.


  PERFORM display_alv_result CHANGING lt_invoice_header.



ENDFORM.



FORM get_token USING lv_token TYPE string.

  DATA: lv_body     TYPE string,
        lv_response TYPE string,
        lv_err_text TYPE string.



  DATA: lo_client    TYPE REF TO if_http_client,
        lo_result    TYPE REF TO data,
        lo_data_node TYPE REF TO data.

  FIELD-SYMBOLS: <fs_result> TYPE any,
                 <fs_data>   TYPE any,
                 <fs_token>  TYPE any.


  CONCATENATE '{ "mobileNumber": "' lv_mobile_number '", "password": "' lv_password '" }' into lv_body.
  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = lv_dest_token      " Defined in SM59
    IMPORTING
      client      = lo_client.          " HTTP Client Abstraction

  IF sy-subrc <> 0 OR lo_client IS INITIAL.
    WRITE: 'خطا در ساخت کلاینت HTTP'.
    RETURN.
  ENDIF.

  lo_client->request->set_header_field( name = 'Content-Type' value = 'application/json-patch+json' ).
  lo_client->request->set_cdata( lv_body ).

  CALL METHOD lo_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    lo_client->get_last_error( IMPORTING message = lv_err_text ).
    WRITE: 'خطا در ارسال درخواست:', lv_err_text.
    RETURN.
  ENDIF.

  CALL METHOD lo_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    lo_client->get_last_error( IMPORTING message = lv_err_text ).
    WRITE: 'خطا در دریافت پاسخ:', lv_err_text.
    RETURN.
  ENDIF.

  IF lo_client->response IS BOUND.
    lv_response = lo_client->response->get_cdata( ).

  ELSE.
    WRITE: 'پاسخی دریافت نشد'.
    RETURN.
  ENDIF.


  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_response
    CHANGING
      data = lo_result.


  ASSIGN lo_result->* TO <fs_result>.
  IF <fs_result> IS ASSIGNED.
    ASSIGN COMPONENT 'DATA' OF STRUCTURE <fs_result> TO <fs_data>.
    IF <fs_data> IS ASSIGNED and <fs_data> is not INITIAL.
      ASSIGN COMPONENT 'ACCESSTOKEN' OF STRUCTURE <fs_data>->* TO <fs_token>.
      IF <fs_token> IS ASSIGNED and <fs_token> is not INITIAL.
        gv_flg_token = 'X'.
        lv_token = <fs_token>->*.
        WRITE: 'Access Token:', lv_token.
      ELSE.
        WRITE: 'توکن پیدا نشد'.
      ENDIF.
    ELSE.
      WRITE: 'داده در پاسخ یافت نشد'.
    ENDIF.
  ELSE.
    WRITE: 'پاسخ قابل تبدیل نبود'.
  ENDIF.

ENDFORM.

FORM api_send_data  USING lv_token TYPE string
                          is_request TYPE zmoad_main
                    CHANGING cs_result type gtyp_result.

  DATA: lo_client    TYPE REF TO if_http_client,
        lo_result    TYPE REF TO data,
        lo_data_node TYPE REF TO data,
        lv_taxid type string,
        lv_guid type string,
        lv_error_count type i,
        ls_zsadad type zsadad_log.

  DATA: successMsg TYPE string,
        errorMsg TYPE string.

  FIELD-SYMBOLS: <fs_result2> TYPE any,
                 <fs_data2>   TYPE any,
                 <fs_total>   TYPE any,
                 <data> type any,
                 <invoiceheader> type any,
                 <taxid> type any,
                 <guid> type any,
                 <description> type any,
                 <error_result> type any,
                 <error_result_item> type any,
                 <messagetype> type any,
                 <error_description> type any.

  data: lv_json type string.

  check lv_token is not INITIAL.

  PERFORM translate_fields. " change field name


    CALL METHOD /ui2/cl_json=>serialize
      EXPORTING
        data          = is_request
        name_mappings = gt_mapping
      RECEIVING
        r_json        = lv_json.

    replace ALL OCCURRENCES OF '"null"' IN lv_json WITH 'null'.

if p_ex = 'X'.
    cl_demo_output=>display( lv_json ).
endif.


  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_response    TYPE string,
        lv_code        TYPE i.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = lv_destination_api            " Defined in SM59
    IMPORTING
      client      = lo_http_client.          " HTTP Client Abstraction

  IF sy-subrc <> 0.
    MESSAGE 'خطا در ایجاد کلاینت HTTP' TYPE 'E'.
  ENDIF.

***** header: content-type
  lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json-patch+json' ).

****** header: authorization
  lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { lv_token }| ).

  lo_http_client->request->set_method( 'POST' ).
  lo_http_client->request->set_cdata( lv_json ).

  CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    MESSAGE 'خطا در ارسال درخواست' TYPE 'E'.
  ENDIF.

  CALL METHOD lo_http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  IF sy-subrc <> 0.
    MESSAGE 'خطا در دریافت پاسخ' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  lv_response = lo_http_client->response->get_cdata( ).

if p_ex = 'X'.
  cl_demo_output=>display( lv_response ).
endif.


  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_response
    CHANGING
      data = lo_result.

**************************  cl_demo_output=>display( lv_response ). " response SADAD


  clear: ls_zsadad.

  CONCATENATE 'ارسال اين صورتحساب با موفقيت انجام شد' cs_result-internalserialnumber INTO successMsg.
  CONCATENATE 'اين صورتحساب ارسال نشد' cs_result-internalserialnumber INTO errorMsg.
  ASSIGN lo_result->* TO <fs_result2>.
  IF <fs_result2> IS ASSIGNED and <fs_result2> is not INITIAL.
    assign COMPONENT 'DATA' of STRUCTURE <fs_result2> to <data>.
    ASSIGN COMPONENT 'TOTALCOUNT' OF STRUCTURE <fs_result2> TO <fs_total>.
    ASSIGN COMPONENT 'ID' OF STRUCTURE <fs_result2> TO <guid>.
    ASSIGN COMPONENT 'DESCRIPTION' OF STRUCTURE <fs_result2> TO <description>.
    IF <fs_total> is ASSIGNED and <fs_total> IS INITIAL. " اگر پذيرفته شد در سامانه اينجا يک رکورد در جدول ايجاد کن

*************گرفتن شماره منحصر به فرد مالياتي
      clear: lv_taxid, lv_guid.
      if <data> is ASSIGNED and <data> is not INITIAL.
        ASSIGN COMPONENT 'INVOICEHEADER' of STRUCTURE <data>->* to <invoiceheader>.
        if <invoiceheader> is ASSIGNED and <invoiceheader> is not INITIAL.
          ASSIGN COMPONENT 'TAXID' of STRUCTURE <invoiceheader>->* to <taxid>.
          if <taxid> is ASSIGNED and <taxid> is not INITIAL.
            lv_taxid = <taxid>->*.
            cs_result-taxid = lv_taxid.
          endif.
          if <guid> is ASSIGNED and <guid> is not INITIAL.
            lv_guid = <guid>->*.
            cs_result-guid = lv_guid.
          endif.
        endif.
      endif.

      if lv_taxid is not INITIAL.
        data: lv_xblnr_string type string.
        lv_xblnr_string = cs_result-internalserialnumber.
        split lv_xblnr_string at '-' into data(lv_str1) data(lv_str2).

        ls_zsadad = VALUE zsadad_log(
          xblnr = lv_str1
          serial = lv_str2
          fkdat = cs_result-issuingdatetime
          send_date = sy-datum
          taxid = lv_taxid
          guid = lv_guid
          ZKTOKD = cs_result-invoicetype
          status = '1'
         ).

        modify zsadad_log FROM ls_zsadad.
        MESSAGE successMsg TYPE 'S'.
      else.
        cs_result-error1 = 'عمليات موفقيت آميز نبود'.
        if <description> is ASSIGNED and <description>->* is not INITIAL.
          DESCRIBE FIELD <description>->* type data(l_typ).
          if l_typ = 'h'.
            lv_error_count = 0.
            loop at <description>->* ASSIGNING <error_description>.
              if <error_description> is ASSIGNED.
                lv_error_count = lv_error_count + 1.
                case lv_error_count.
                  when '1'.
                    cs_result-error1 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error1 WITH 'ي'.
                  when '2'.
                    cs_result-error2 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error2 WITH 'ي'.
                  when '3'.
                    cs_result-error3 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error3 WITH 'ي'.
                  when '4'.
                    cs_result-error4 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error4 WITH 'ي'.
                  when '5'.
                    cs_result-error5 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error5 WITH 'ي'.
                ENDCASE.
              endif.
            ENDLOOP.
          endif.
        endif.
      endif.



    ELSE.
      clear: <error_result>.
      if <data> is ASSIGNED.
        ASSIGN COMPONENT 'RESULT' of STRUCTURE <data>->* to <error_result>.
      endif.
      if <error_result> is ASSIGNED.
        lv_error_count = 0.
        loop at <error_result>->* ASSIGNING <error_result_item>.
          ASSIGN COMPONENT 'MESSAGETYPE' of STRUCTURE <error_result_item>->* to <messagetype>.
          if <messagetype> is ASSIGNED.
            if <messagetype>->* <> '2'.
              ASSIGN COMPONENT 'DESCRIPTION' of STRUCTURE <error_result_item>->* to <error_description>.
              if <error_description> is ASSIGNED.
                lv_error_count = lv_error_count + 1.
                case lv_error_count.
                  when '1'.
                    cs_result-error1 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error1 WITH 'ي'.
                  when '2'.
                    cs_result-error2 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error2 WITH 'ي'.
                  when '3'.
                    cs_result-error3 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error3 WITH 'ي'.
                  when '4'.
                    cs_result-error4 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error4 WITH 'ي'.
                  when '5'.
                    cs_result-error5 = <error_description>->*.
                    REPLACE ALL OCCURRENCES OF 'ي' in cs_result-error5 WITH 'ي'.
                ENDCASE.
              endif.
            endif.
          endif.
        ENDLOOP.
      endif.




    ENDIF.

  ELSE.
    MESSAGE 'پاسخ قابل تبدیل نبود' type 'S' DISPLAY LIKE 'E'.
  ENDIF.





ENDFORM.


FORM display_alv_result USING it_result type gtype_t_result.
  DATA: ls_layout TYPE slis_layout_alv,
        ls_data   LIKE LINE OF gv_it_output,
        lt_fieldcat_result type slis_t_fieldcat_alv WITH HEADER LINE,
        ls_fieldcat_result type slis_fieldcat_alv.

  refresh: lt_fieldcat_result.

  clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'INTERNALSERIALNUMBER'.
  ls_fieldcat_result-seltext_s = 'صورتحساب'.
  ls_fieldcat_result-seltext_m = 'شماره صورتحساب'.
  ls_fieldcat_result-seltext_l = 'شماره صورتحساب'.
  append ls_fieldcat_result to lt_fieldcat_result.

clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'TAXID'.
  ls_fieldcat_result-seltext_s = 'کد يکتا'.
  ls_fieldcat_result-seltext_m = 'کد يکتاي مالياتي'.
  ls_fieldcat_result-seltext_l = 'کد يکتاي مالياتي'.
  append ls_fieldcat_result to lt_fieldcat_result.

clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'ERROR1'.
  ls_fieldcat_result-seltext_s = 'خطاي يک'.
  ls_fieldcat_result-seltext_m = 'خطاي يک'.
  ls_fieldcat_result-seltext_l = 'خطاي يک'.
  append ls_fieldcat_result to lt_fieldcat_result.

clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'ERROR2'.
  ls_fieldcat_result-seltext_s = 'خطاي دو'.
  ls_fieldcat_result-seltext_m = 'خطاي دو'.
  ls_fieldcat_result-seltext_l = 'خطاي دو'.
  append ls_fieldcat_result to lt_fieldcat_result.

clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'ERROR3'.
  ls_fieldcat_result-seltext_s = 'خطاي سه'.
  ls_fieldcat_result-seltext_m = 'خطاي سه'.
  ls_fieldcat_result-seltext_l = 'خطاي سه'.
  append ls_fieldcat_result to lt_fieldcat_result.

clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'ERROR4'.
  ls_fieldcat_result-seltext_s = 'خطاي چهار'.
  ls_fieldcat_result-seltext_m = 'خطاي چهار'.
  ls_fieldcat_result-seltext_l = 'خطاي چهار'.
  append ls_fieldcat_result to lt_fieldcat_result.

clear: ls_fieldcat_result.
  ls_fieldcat_result-fieldname = 'ERROR5'.
  ls_fieldcat_result-seltext_s = 'خطاي پنج'.
  ls_fieldcat_result-seltext_m = 'خطاي پنج'.
  ls_fieldcat_result-seltext_l = 'خطاي پنج'.
  append ls_fieldcat_result to lt_fieldcat_result.


  ls_layout-zebra = 'X'.
  ls_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      it_fieldcat              = lt_fieldcat_result[]
      i_save                   = 'X'
      is_layout                = ls_layout
    TABLES
      t_outtab                 =  it_result
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.


FORM create_request
  using is_invoice_header type gtyp_result
        os_request type zmoad_main.

  data: lv_timestamp_ms TYPE int8,
        lv_timestamp_ms_cot type int8.

  os_request-clientid = lv_shenaseh_client.

  PERFORM get_current_timestamp_ms using is_invoice_header-issuingdatetime CHANGING lv_timestamp_ms.
  PERFORM get_current_timestamp_ms using is_invoice_header-asn_dat CHANGING lv_timestamp_ms_cot.


  os_request-internalSerialNumber = is_invoice_header-internalserialnumber. " شماره صورتحساب

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
    cottageDeclarationCustomsDate = lv_timestamp_ms_cot " تاریخ کوتاژ اظهارنامه گمرکی
).

  if ls_head-cottagedeclarationcustomsdate = '00000000'.
    ls_head-cottagedeclarationcustomsdate = 'null'.
  endif.

  os_request-invoiceheader = ls_head.
***
***    "******************************************************************************** Bodies
*  *------------------------------------ واحد اندازه گیری و سنجش


loop at gt_moadian into data(ls_moadian) WHERE internalserialnumber = is_invoice_header-internalserialnumber.
*    ------------------------------ body
  DATA(ls_body) = VALUE zmoad_bodies(
    stuffServiceId =   ls_moadian-STUFFSERVICEID " شناسه کالا / خدمات
    stuffServiceDescription = ls_moadian-STUFFSERVICEDESCRIPTION " شرح کالا/خدمت
    quantity            = ls_moadian-QUANTITY " مقدار / تعداد
    measurementUnit = ls_moadian-MEASUREMENTUNIT " واحد اندازه گیری و سنجش
    fee =  ls_moadian-FEE " مبلغ واحد
    discount =   ls_moadian-discount * -1  " مبلغ تخفیف

      netWeight =  ls_moadian-netweight " وزن خالص
      currencyType =  ls_moadian-currencytype " نوع ارز
      sourceVat =   '10'  " ماخذ مالیات بر ارزش افزوده
     ).

  if ls_body-currencytype = '0'.
    ls_body-currencytype = 'null'.
  endif.

  APPEND ls_body TO os_request-invoicebodies.

 ENDLOOP.

ENDFORM.


FORM translate_fields.

  gt_mapping = VALUE /ui2/cl_json=>name_mappings(
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

ENDFORM.

FORM get_current_timestamp_ms USING iv_ISSUINGDATE type ZFKDAT CHANGING ev_timestamp_ms TYPE int8.

  DATA: lv_current_ts   TYPE timestampl,
        lv_epoch_ts     TYPE timestampl VALUE '19700101000000.0000000',
        lv_diff_seconds TYPE i,
        lv_str1 type string,
        lv_str2 type string,
        lv_str3 type string.

*  GET TIME STAMP FIELD lv_current_ts.
if iv_issuingdate is not INITIAL and p_time is INITIAL.
  lv_str1 = iv_issuingdate.
else.
 lv_str1 = sy-datlo.
endif.
lv_str2 = '000000'.
CONCATENATE lv_str1 lv_str2 into lv_str3.
lv_current_ts = lv_str3.

  CALL METHOD cl_abap_tstmp=>subtract
    EXPORTING
      tstmp1 = lv_current_ts
      tstmp2 = lv_epoch_ts
    RECEIVING
      r_secs = lv_diff_seconds.

  ev_timestamp_ms = lv_diff_seconds * 1000.

ENDFORM.
