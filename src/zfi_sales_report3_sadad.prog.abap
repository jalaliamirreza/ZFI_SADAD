*&---------------------------------------------------------------------*
*& Include          ZFI_SALES_REPORT3_SADAD
*&---------------------------------------------------------------------*
FORM data_request.
  flg_token = ''.
  DATA: lv_token TYPE string,
        toTime   TYPE p LENGTH 16 DECIMALS 0,
        lt_invoice_header type table of gtyp_result,
        ls_invoice_header type gtyp_result.
*  LOOP AT gv_it_output INTO ls_out_put.


  loop at gt_moadian1 into data(ls_moadian_temp).
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

*    IF ls_moadian1-invoicetype  = '1'.
      PERFORM items_type_one USING <fs_invoice_header>.
*    ELSEIF ls_moadian1-invoicetype  = '2'.
*      PERFORM items_type_two.
*    ENDIF.

*    IF flg_token <> 'X'. " يکبار براي گرفتن توکن ميره به فرم زير
*      PERFORM get_token USING lv_token.
*    ENDIF.

    PERFORM api_send_data USING lv_token ls_request CHANGING <fs_invoice_header>.

    CLEAR: lv_json, ls_request.
*    REFRESH: lt_request.
  ENDLOOP.


  PERFORM display_alv_result CHANGING lt_invoice_header.



ENDFORM.

*FORM json_request USING lv_token TYPE string .
*  PERFORM translate_fields. " change field name
*
**  IF ls_moadian1-invoicetype = '1'.
*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data          = ls_request
*        name_mappings = lt_mapping
*      RECEIVING
*        r_json        = lv_json.
*  ELSEIF ls_moadian1-invoicetype = '2'.
*    CALL METHOD /ui2/cl_json=>serialize
*      EXPORTING
*        data          = ls_request2
*        name_mappings = lt_mapping
*      RECEIVING
*        r_json        = lv_json.
*  ENDIF.

*  IF lv_token IS NOT INITIAL.
*    PERFORM api_send_data USING lv_token ls_request CHANGING <.
*  ENDIF.
*ENDFORM.

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

*  lv_body = `{ "mobileNumber": "09210171978", "password": "123456789#Mm" }`.
  CONCATENATE '{ "mobileNumber": "' gc_mobile_number '", "password": "' gc_password '" }' into lv_body.
  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = 'SADAD_TEST'      " Defined in SM59
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
        flg_token = 'X'.
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
        name_mappings = lt_mapping
      RECEIVING
        r_json        = lv_json.


  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_response    TYPE string,
        lv_code        TYPE i.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = 'sadad_api'            " Defined in SM59
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
    IF <fs_total> IS INITIAL. " اگر پذيرفته شد در سامانه اينجا يک رکورد در جدول ايجاد کن

*************گرفتن شماره منحصر به فرد مالياتي
      clear: lv_taxid.
      if <data> is ASSIGNED and <data> is not INITIAL.
        ASSIGN COMPONENT 'INVOICEHEADER' of STRUCTURE <data>->* to <invoiceheader>.
        if <invoiceheader> is ASSIGNED and <invoiceheader> is not INITIAL.
          ASSIGN COMPONENT 'TAXID' of STRUCTURE <invoiceheader>->* to <taxid>.
          if <taxid> is ASSIGNED and <taxid> is not INITIAL.
            lv_taxid = <taxid>->*.
            cs_result-taxid = lv_taxid.
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
          fkdat = sy-datum
          taxid = lv_taxid
          ZKTOKD = cs_result-invoicetype
          status = '1'
         ).

        INSERT zsadad_log FROM ls_zsadad.
        MESSAGE successMsg TYPE 'S'.
      else.
        cs_result-error1 = 'عمليات موفقيت آميز نبود'.
      endif.



    ELSE.
      ASSIGN COMPONENT 'RESULT' of STRUCTURE <data>->* to <error_result>.
      if <error_result> is ASSIGNED.
        lv_error_count = 0.
        loop at <error_result>->* ASSIGNING <error_result_item>.
          ASSIGN COMPONENT 'MESSAGETYPE' of STRUCTURE <error_result_item>->* to <messagetype>.
          if <messagetype> is ASSIGNED.
            if <messagetype>->* = '1'.
              ASSIGN COMPONENT 'DESCRIPTION' of STRUCTURE <error_result_item>->* to <error_description>.
              if <error_description> is ASSIGNED.
                lv_error_count = lv_error_count + 1.
                case lv_error_count.
                  when '1'.
                    cs_result-error1 = <error_description>->*.
                  when '2'.
                    cs_result-error2 = <error_description>->*.
                  when '3'.
                    cs_result-error3 = <error_description>->*.
                  when '4'.
                    cs_result-error4 = <error_description>->*.
                  when '5'.
                    cs_result-error5 = <error_description>->*.
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
