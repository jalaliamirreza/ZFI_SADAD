*&---------------------------------------------------------------------*
*& Report ZFI_SALES_REPORT_GET
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_sales_report_get.
DATA:  lv_token TYPE string.

START-OF-SELECTION.


  PERFORM get_token USING lv_token.

  PERFORM api_get_data USING lv_token.

FORM get_token USING lv_token TYPE string.

  DATA: lv_url      TYPE string VALUE 'https://api.trustedtsp.ir/api/v1/users/login-with-password',
        lv_body     TYPE string,
        lv_response TYPE string,
        lv_err_text TYPE string.

  DATA: lo_client    TYPE REF TO if_http_client,
        lo_result    TYPE REF TO data,
        lo_data_node TYPE REF TO data.

  FIELD-SYMBOLS: <fs_result> TYPE any,
                 <fs_data>   TYPE any,
                 <fs_token>  TYPE any.

  lv_body = `{ "mobileNumber": "09210171978", "password": "123456789#Mm" }`.
  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = 'SADAD_TEST'            "  sm59
    IMPORTING
      client      = lo_client.          " HTTP Client Abstraction
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

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
    IF <fs_data> IS ASSIGNED.
      ASSIGN COMPONENT 'ACCESSTOKEN' OF STRUCTURE <fs_data>->* TO <fs_token>.
      IF <fs_token> IS ASSIGNED.
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

FORM api_get_data  USING lv_token TYPE string.


  DATA: lv_url         TYPE string VALUE 'https://api.trustedtsp.ir/api/v1/invoice',
        lo_http_client TYPE REF TO if_http_client,
        lv_response    TYPE string,
        lv_code        TYPE i.

  CALL METHOD cl_http_client=>create_by_destination
    EXPORTING
      destination = 'sadad_api'            " Logical destination (specified in function call)
    IMPORTING
      client      = lo_http_client.          " HTTP Client Abstraction

  IF sy-subrc <> 0.
    MESSAGE 'خطا در ایجاد کلاینت HTTP' TYPE 'E'.
  ENDIF.

***** header: content-type
  lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json-patch+json' ).

****** header: authorization
  lo_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { lv_token }| ).

  " تنظیم header ها
  lo_http_client->request->set_header_field( name = 'Authorization' value = lv_token ).
  lo_http_client->request->set_header_field( name = 'Accept' value = '*/*' ).
  lo_http_client->request->set_header_field( name = 'clientid' value = '1E8FO4SD3ANB' ). " اگر API نیاز داره توی header هم باشه


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
*  cl_demo_output=>display( lv_response ).
    MESSAGE lv_response TYPE 'S' DISPLAY LIKE 'E'.





ENDFORM.
