*&---------------------------------------------------------------------*
*& Report ZFI_SADAD_GET_JOB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_sadad_get_job.

CONSTANTS: gc_mobile_number type string value '09103071164',  "تلفن آقاي ميرزايي
           gc_password type string value 'JBMTI4Q0JDLUhTMj',
           gc_destination_api type string value 'SADAD_API',
           gc_dest_token      type string value 'SADAD_GET',

           "for test
           gc_mobile_number_test type string value '09210171978',  "شماره خانم درويش فرد
           gc_password_test type string value '123456789#Mm',
           gc_destination_api_test type string value 'SADAD_API_TEST',
           gc_dest_token_test      type string value 'SADAD_GET_TEST'.

DATA: gt_sadad        TYPE TABLE OF zsadad_log,
      ls_sadad        TYPE zsadad_log,
      gv_flg_token(1) TYPE c,
      lv_mobile_number type string,
      lv_password type string,
      lv_destination_api type char60,
      lv_dest_token type char60.

PARAMETERS: p_ex TYPE c AS CHECKBOX,
            p_test type c as CHECKBOX.

START-OF-SELECTION.

  PERFORM run_program.

FORM run_program.

if p_test = 'X'.
  lv_mobile_number = gc_mobile_number_test.
  lv_password = gc_password_test.
  lv_destination_api = gc_destination_api_test.
  lv_dest_token = gc_dest_token_test.
else.
  lv_mobile_number = gc_mobile_number.
  lv_password = gc_password.
  lv_destination_api = gc_destination_api.
  lv_dest_token = gc_dest_token.
endif.

  REFRESH: gt_sadad.
  CLEAR: ls_sadad, gv_flg_token.


  SELECT * FROM zsadad_log INTO TABLE gt_sadad  WHERE status = '1'.
  CHECK gt_sadad IS NOT INITIAL.


  PERFORM send_request.

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


  CONCATENATE '{ "mobileNumber": "' lv_mobile_number '", "password": "' lv_password '" }' INTO lv_body.
  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = lv_dest_token     " Defined in SM59
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
    IF <fs_data> IS ASSIGNED AND <fs_data> IS NOT INITIAL.
      ASSIGN COMPONENT 'ACCESSTOKEN' OF STRUCTURE <fs_data>->* TO <fs_token>.
      IF <fs_token> IS ASSIGNED AND <fs_token> IS NOT INITIAL.
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



FORM send_request .
  gv_flg_token = ''.
  DATA: lv_token TYPE string.



  PERFORM get_token USING lv_token.

  IF lv_token IS INITIAL.
    MESSAGE 'ارتباط برقرار نشد' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT gt_sadad INTO DATA(ls_sadad).

*    PERFORM api_send_data USING lv_token ls_request CHANGING <fs_invoice_header>.
    PERFORM api_send_data USING lv_token CHANGING ls_sadad.
    MODIFY zsadad_log FROM ls_sadad.
    COMMIT WORK.

  ENDLOOP.





ENDFORM.


FORM api_send_data  USING lv_token TYPE string
                    CHANGING     cs_sadad TYPE zsadad_log.

  DATA: lo_client      TYPE REF TO if_http_client,
        lo_result      TYPE REF TO data,
        lo_data_node   TYPE REF TO data,
        lv_error_count TYPE i,
        ls_zsadad      TYPE zsadad_log.

  FIELD-SYMBOLS: <fs_result2>        TYPE any,
                 <fs_data2>          TYPE any,
                 <fs_total>          TYPE any,
                 <data>              TYPE any,
                 <invoiceheader>     TYPE any,
                 <taxid>             TYPE any,
                 <error_result>      TYPE any,
                 <error_result_item> TYPE any,
                 <message>           TYPE any,
                 <status>            TYPE any.

  DATA: lv_json TYPE string.

  CHECK lv_token IS NOT INITIAL.


  DATA: lo_http_client TYPE REF TO if_http_client,
        lv_response    TYPE string,
        lv_code        TYPE i,
        lv_reason      TYPE string.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = lv_destination_api           " Defined in SM59
    IMPORTING
      client      = lo_http_client.          " HTTP Client Abstraction

  IF sy-subrc <> 0.
    MESSAGE 'خطا در ایجاد کلاینت HTTP' TYPE 'E'.
  ENDIF.

***** header: content-type
  lo_http_client->request->set_header_field( name = 'Accept' value = '*/*' ).

  DATA: lv_str1 TYPE string,
        lv_str2 TYPE string,
        lv_str3 TYPE string.
  CLEAR: lv_str1, lv_str2, lv_str3.

  lv_str1 = '/'.
  lv_str2 = cs_sadad-guid.
  CONCATENATE lv_str1 lv_str2 INTO lv_str1.

  lo_http_client->request->set_header_field( name = '~request_uri' value = lv_str1 ).

****** header: authorization
  lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { lv_token }| ).

  lo_http_client->request->set_method( 'GET' ).
*  lo_http_client->request->set_cdata( lv_json ).

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


  CLEAR: lv_code, lv_reason.
  lo_http_client->response->get_status(
    IMPORTING
      code   =     lv_code
      reason =     lv_reason
  ).

  lo_http_client->close( ).

  IF p_ex = 'X'.
    cl_demo_output=>display( lv_response ).
  ENDIF.


  CALL METHOD /ui2/cl_json=>deserialize
    EXPORTING
      json = lv_response
    CHANGING
      data = lo_result.

**************************  cl_demo_output=>display( lv_response ). " response SADAD


  CLEAR: ls_zsadad.


  ASSIGN lo_result->* TO <fs_result2>.
  IF <fs_result2> IS ASSIGNED AND <fs_result2> IS NOT INITIAL.
    ASSIGN COMPONENT 'DATA' OF STRUCTURE <fs_result2> TO <data>.
*    ASSIGN COMPONENT 'TOTALCOUNT' OF STRUCTURE <fs_result2> TO <fs_total>.
*    IF <fs_total> IS INITIAL. " اگر پذيرفته شد در سامانه اينجا يک رکورد در جدول ايجاد کن
*
*
*
*
*
*
*    ELSE.
    IF <data> IS ASSIGNED AND <data> IS NOT INITIAL AND <data>->* IS NOT INITIAL.
      ASSIGN COMPONENT 'ERRORS' OF STRUCTURE <data>->* TO <error_result>.
      IF <error_result> IS ASSIGNED AND <error_result> IS NOT INITIAL.
        lv_error_count = 0.
        IF <error_result>->* IS NOT INITIAL.
          DESCRIBE FIELD <error_result>->* type data(l_typ).
          if l_typ = 'h'.


          LOOP AT <error_result>->* ASSIGNING <error_result_item>.
            ASSIGN COMPONENT 'MESSAGE' OF STRUCTURE <error_result_item>->* TO <message>.
            IF <message> IS ASSIGNED.


              lv_error_count = lv_error_count + 1.
              CASE lv_error_count.
                WHEN '1'.
                  cs_sadad-error1 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error1 WITH 'ي'.
                WHEN '2'.
                  cs_sadad-error2 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error2 WITH 'ي'.
                WHEN '3'.
                  cs_sadad-error3 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error3 WITH 'ي'.
                WHEN '4'.
                  cs_sadad-error4 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error4 WITH 'ي'.
                WHEN '5'.
                  cs_sadad-error5 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error5 WITH 'ي'.
                WHEN '6'.
                  cs_sadad-error6 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error6 WITH 'ي'.
                WHEN '7'.
                  cs_sadad-error7 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error7 WITH 'ي'.
                WHEN '8'.
                  cs_sadad-error8 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error8 WITH 'ي'.
                WHEN '9'.
                  cs_sadad-error9 = <message>->*.
                  REPLACE ALL OCCURRENCES OF 'ي' IN cs_sadad-error9 WITH 'ي'.
              ENDCASE.



            endif.
          ENDLOOP.

         ENDIF.
        ENDIF.

      ENDIF.
      ASSIGN COMPONENT 'STATUS' OF STRUCTURE <data>->* TO <status>.
      IF <status> IS ASSIGNED AND <status> IS NOT INITIAL.
        CASE <status>->*.
          WHEN '2'.
            cs_sadad-status = 3.  "rad shode
          WHEN '1'.
            cs_sadad-status = 2.  "taeed shode
        ENDCASE.

      ENDIF.



    ENDIF.

  ELSE.
    MESSAGE 'پاسخ قابل تبدیل نبود' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.





ENDFORM.
