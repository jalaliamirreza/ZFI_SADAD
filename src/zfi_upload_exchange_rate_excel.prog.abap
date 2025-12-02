*&---------------------------------------------------------------------*
*& Report ZUPDATE_CUSTOMER_FROM_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_upload_exchange_rate_excel.


TYPE-POOLS: slis,truxs.




TYPES: BEGIN OF gt_excel_file,
         rate_type  TYPE  kurst_curr,
         from_curr  TYPE  fcurr_curr,
         to_currncy TYPE  tcurr_curr,
         valid_from TYPE  gdatu_cur,
         exch_rate  TYPE  ukursp,
       END OF gt_excel_file.

       TYPES: BEGIN OF gt_excel_file_temp,
         rate_type  TYPE  kurst_curr,
         from_curr  TYPE  c LENGTH 10,
         to_currncy TYPE  tcurr_curr,
         valid_from TYPE  gdatu_cur,
         exch_rate  TYPE  p LENGTH 9,
       END OF gt_excel_file_temp.


DATA: fieldcatalog TYPE                   slis_t_fieldcat_alv WITH HEADER LINE,
      gv_it_excel  TYPE STANDARD TABLE OF gt_excel_file,
      gv_wa_excel  LIKE LINE OF           gv_it_excel,
      gv_it_log    TYPE STANDARD TABLE OF zupdate_customer_from_excel,
      gv_wa_log    TYPE                   zupdate_customer_from_excel,
      gv_it_raw    TYPE                   truxs_t_text_data,
      tmp_it_excel TYPE STANDARD TABLE OF gt_excel_file_temp,
      tmp_wa_excel TYPE                   gt_excel_file_temp,
      gv_wa_excel1 LIKE LINE OF           gv_it_excel.


SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME .
PARAMETERS: rad1 RADIOBUTTON GROUP rd1 USER-COMMAND a,
            rad2 RADIOBUTTON GROUP rd1 DEFAULT 'X'.

PARAMETERS: p_file  TYPE rlgrap-filename MODIF ID bk1,
            p_path  TYPE dirprofilenames DEFAULT 'DIR_HR' MODIF ID bk1,
            p_filen TYPE char30          DEFAULT 'FILE.CSV' MODIF ID bk1.
SELECTION-SCREEN: END   OF BLOCK blk1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.


AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_screen.

START-OF-SELECTION.

  PERFORM check_input_data.
  if strlen( p_file ) > 0.
  PERFORM read_excel.
  else.
  PERFORM read_path.
  endif.
  PERFORM update_rate.
  PERFORM ConvertToUsd.
  PERFORM ConvertToEuro.



*&---------------------------------------------------------------------*
*&      Form  f4_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f4_file .

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FILE'
    IMPORTING
      file_name  = p_file.


ENDFORM.                                                    "f4_file


*&---------------------------------------------------------------------*
*&      Form  read_excel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_excel .

  data: lv_dif          TYPE i,
        lv_to           TYPE i,
        lv_tcurf        TYPE tcurf.

  CHECK p_file IS NOT INITIAL.

  REFRESH gv_it_raw.
  REFRESH gv_it_excel.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = gv_it_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = tmp_it_excel[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error in read Excel file!' TYPE 'E'.
  ENDIF.

  loop at tmp_it_excel INTO tmp_wa_excel.
    gv_wa_excel-to_currncy = tmp_wa_excel-to_currncy.
    gv_wa_excel-valid_from = tmp_wa_excel-valid_from.
    gv_wa_excel-rate_type = tmp_wa_excel-rate_type.

      IF strlen( tmp_wa_excel-from_curr ) > 3.
        lv_to = strlen( tmp_wa_excel-from_curr ) - 3.
        lv_dif = tmp_wa_excel-from_curr+3(lv_to).
        gv_wa_excel-from_curr = tmp_wa_excel-from_curr(3).
        tmp_wa_excel-exch_rate = tmp_wa_excel-exch_rate / lv_dif.
        else.
          gv_wa_excel-from_curr = tmp_wa_excel-from_curr.

      ENDIF.

      CLEAR lv_tcurf.
      SELECT * UP TO 1 ROWS
        INTO lv_tcurf
        FROM tcurf
        WHERE kurst =  gv_wa_excel-rate_type        AND
              fcurr =  gv_wa_excel-from_curr        AND
              tcurr =  gv_wa_excel-to_currncy       AND
              gdatu => gv_wa_excel-valid_from
        ORDER BY gdatu DESCENDING.
      ENDSELECT.
      IF sy-subrc NE 0.
        gv_wa_excel-exch_rate = tmp_wa_excel-exch_rate.
      ELSE.
        gv_wa_excel-exch_rate = tmp_wa_excel-exch_rate * ( lv_tcurf-ffact / lv_tcurf-tfact ).
      ENDIF.

      APPEND gv_wa_excel TO gv_it_excel.


    ENDLOOP.







ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_CUSTOMER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_rate .

  DATA: lv_wa_rate TYPE bapi1093_0,
        lv_wa_ret  TYPE bapiret2,
        lv_date    TYPE gdatu_invv.



  LOOP AT gv_it_excel INTO gv_wa_excel.

    CLEAR: lv_wa_rate,lv_wa_ret,lv_date.


    MOVE-CORRESPONDING gv_wa_excel TO lv_wa_rate.


    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_wa_rate-valid_from
      IMPORTING
        output = lv_date.

    SELECT ffact tfact UP TO 1 ROWS
      INTO (lv_wa_rate-from_factor,lv_wa_rate-to_factor)
      FROM tcurf
      WHERE kurst =  lv_wa_rate-rate_type        AND
            fcurr =  lv_wa_rate-from_curr        AND
            tcurr =  lv_wa_rate-to_currncy       AND
            gdatu => lv_date
      ORDER BY gdatu DESCENDING.
    ENDSELECT.
    IF sy-subrc NE 0.
      lv_wa_rate-from_factor = 1.
      lv_wa_rate-to_factor   = 1.
    ENDIF.

    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
      EXPORTING
        exch_rate = lv_wa_rate
      IMPORTING
        return    = lv_wa_ret.


    IF lv_wa_ret-type = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      MESSAGE lv_wa_ret-message TYPE 'S'.
      WRITE:lv_wa_rate-rate_type,'-',lv_wa_rate-from_curr,'-',lv_wa_rate-to_currncy,'-',lv_wa_rate-valid_from,'-',lv_wa_rate-exch_rate,'-',lv_wa_ret-message.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      WRITE:lv_wa_rate-rate_type,'-',lv_wa_rate-from_curr,'-',lv_wa_rate-to_currncy,'-',lv_wa_rate-valid_from,'-',lv_wa_rate-exch_rate,'-','Uploaded'.
    ENDIF.
    ULINE.

  ENDLOOP.


ENDFORM.
FORM converttousd.

  DATA: lv_wa_rate TYPE bapi1093_0,
        lv_wa_ret  TYPE bapiret2,
        lv_date    TYPE gdatu_invv.

  LOOP AT gv_it_excel INTO gv_wa_excel1 WHERE from_curr = 'USD'.
  ENDLOOP.
  IF sy-subrc IS INITIAL.

    LOOP AT gv_it_excel INTO gv_wa_excel WHERE from_curr <> 'USD'.

      CLEAR: lv_wa_rate,lv_wa_ret,lv_date.


      MOVE-CORRESPONDING gv_wa_excel TO lv_wa_rate.

      lv_wa_rate-exch_rate = gv_wa_excel-exch_rate / gv_wa_excel1-exch_rate.


      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = lv_wa_rate-valid_from
        IMPORTING
          output = lv_date.

      SELECT ffact tfact UP TO 1 ROWS
        INTO (lv_wa_rate-from_factor,lv_wa_rate-to_factor)
        FROM tcurf
        WHERE kurst =  lv_wa_rate-rate_type        AND
              fcurr =  lv_wa_rate-from_curr        AND
              tcurr =  'USD'                       AND
              gdatu => lv_date
        ORDER BY gdatu DESCENDING.
      ENDSELECT.
      IF sy-subrc NE 0.
        lv_wa_rate-from_factor = 1.
        lv_wa_rate-to_factor   = 1.
      ENDIF.

      lv_wa_rate-to_currncy = 'USD'.



      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
        EXPORTING
          exch_rate = lv_wa_rate
        IMPORTING
          return    = lv_wa_ret.


      IF lv_wa_ret-type = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        MESSAGE lv_wa_ret-message TYPE 'S'.
        WRITE:lv_wa_rate-rate_type,'-',lv_wa_rate-from_curr,'-',lv_wa_rate-to_currncy,'-',lv_wa_rate-valid_from,'-',lv_wa_rate-exch_rate,'-',lv_wa_ret-message.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        WRITE:lv_wa_rate-rate_type,'-',lv_wa_rate-from_curr,'-',lv_wa_rate-to_currncy,'-',lv_wa_rate-valid_from,'-',lv_wa_rate-exch_rate,'-','Uploaded'.
      ENDIF.
      ULINE.

    ENDLOOP.
  ENDIF.
ENDFORM.
FORM converttoeuro.

  DATA: lv_wa_rate TYPE bapi1093_0,
        lv_wa_ret  TYPE bapiret2,
        lv_date    TYPE gdatu_invv.

  LOOP AT gv_it_excel INTO gv_wa_excel1 WHERE from_curr = 'EUR'.
  ENDLOOP.
  IF sy-subrc IS INITIAL.

    LOOP AT gv_it_excel INTO gv_wa_excel WHERE from_curr <> 'EUR'.

      CLEAR: lv_wa_rate,lv_wa_ret,lv_date.


      MOVE-CORRESPONDING gv_wa_excel TO lv_wa_rate.

      lv_wa_rate-exch_rate = gv_wa_excel-exch_rate / gv_wa_excel1-exch_rate.


      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = lv_wa_rate-valid_from
        IMPORTING
          output = lv_date.

      SELECT ffact tfact UP TO 1 ROWS
        INTO (lv_wa_rate-from_factor,lv_wa_rate-to_factor)
        FROM tcurf
        WHERE kurst =  lv_wa_rate-rate_type        AND
              fcurr =  lv_wa_rate-from_curr        AND
              tcurr =  'EUR'                       AND
              gdatu => lv_date
        ORDER BY gdatu DESCENDING.
      ENDSELECT.
      IF sy-subrc NE 0.
        lv_wa_rate-from_factor = 1.
        lv_wa_rate-to_factor   = 1.
      ENDIF.

      lv_wa_rate-to_currncy = 'EUR'.




      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
        EXPORTING
          exch_rate = lv_wa_rate
        IMPORTING
          return    = lv_wa_ret.


      IF lv_wa_ret-type = 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        MESSAGE lv_wa_ret-message TYPE 'S'.
        WRITE:lv_wa_rate-rate_type,'-',lv_wa_rate-from_curr,'-',lv_wa_rate-to_currncy,'-',lv_wa_rate-valid_from,'-',lv_wa_rate-exch_rate,'-',lv_wa_ret-message.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        WRITE:lv_wa_rate-rate_type,'-',lv_wa_rate-from_curr,'-',lv_wa_rate-to_currncy,'-',lv_wa_rate-valid_from,'-',lv_wa_rate-exch_rate,'-','Uploaded'.
      ENDIF.
      ULINE.

    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_input_data .

  DATA: lv_dirname(100).


  IF p_file IS INITIAL AND p_path IS INITIAL.
    MESSAGE 'آدرس فايل را وارد نماييد' TYPE 'E'.
  ENDIF.


  IF p_path IS NOT INITIAL.
    CLEAR: lv_dirname.
    SELECT SINGLE dirname INTO lv_dirname FROM user_dir WHERE  aliass = p_path.
    IF sy-subrc NE 0.
      MESSAGE 'مسير وجود ندارد' TYPE 'E'.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_PATH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM read_path .

  DATA: lv_dirname(100),
        lv_fname        TYPE string,
        lv_txt(100),
        lv_rate(9),
        lv_tcurf        TYPE tcurf,
        lv_rate2 TYPE p LENGTH 9,
        lv_from TYPE c LENGTH 10,
        lv_dif          TYPE i,
        lv_to           TYPE i.

  CHECK p_path IS NOT INITIAL.

  CLEAR: lv_dirname.
  SELECT SINGLE dirname INTO lv_dirname FROM user_dir WHERE  aliass = p_path.

  CLEAR lv_fname.
  CONCATENATE lv_dirname '/' p_filen INTO lv_fname.



  OPEN DATASET lv_fname FOR INPUT IN TEXT MODE ENCODING DEFAULT  .
  IF sy-subrc NE 0.
    MESSAGE 'Unable to open file' TYPE 'I'.
  ENDIF.
  DO.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    READ DATASET lv_fname INTO lv_txt.
    IF sy-subrc = 0.
      CLEAR lv_rate.
      SPLIT lv_txt AT ',' INTO gv_wa_excel-rate_type
                               lv_from
                               gv_wa_excel-to_currncy
                               gv_wa_excel-valid_from
                               lv_rate.

      IF strlen( lv_from ) > 3.
        lv_to = strlen( lv_from ) - 3.
        lv_dif = lv_from+3(lv_to).
        gv_wa_excel-from_curr = lv_from(3).
        lv_rate2 = lv_rate / lv_dif.
        else.
          gv_wa_excel-from_curr = lv_from.
          lv_rate2 = lv_rate.
      ENDIF.

      CLEAR lv_tcurf.
      SELECT * UP TO 1 ROWS
        INTO lv_tcurf
        FROM tcurf
        WHERE kurst =  gv_wa_excel-rate_type        AND
              fcurr =  gv_wa_excel-from_curr        AND
              tcurr =  gv_wa_excel-to_currncy       AND
              gdatu => gv_wa_excel-valid_from
        ORDER BY gdatu DESCENDING.
      ENDSELECT.
      IF sy-subrc NE 0.
        gv_wa_excel-exch_rate = lv_rate2.
      ELSE.
        gv_wa_excel-exch_rate = lv_rate2 * ( lv_tcurf-ffact / lv_tcurf-tfact ).
      ENDIF.

      APPEND gv_wa_excel TO gv_it_excel.
      CLEAR gv_wa_excel.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLOSE DATASET lv_fname.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTION_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM selection_screen .


  LOOP AT SCREEN.

    IF rad1 = 'X'.
      IF screen-name = 'P_PATH' OR screen-name = 'P_FILEN'.
        screen-input     = 0.
      ELSE.
        screen-input     = 1.
      ENDIF.
    ELSE.
      IF screen-name = 'P_FILE'.
        screen-input     = 0.
      ELSE.
        screen-input     = 1.
      ENDIF.
    ENDIF.


    MODIFY SCREEN.
  ENDLOOP.



ENDFORM.
