*&---------------------------------------------------------------------*
*& Report ZUPDATE_CUSTOMER_FROM_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_upload_exchange_rate.


TYPE-POOLS: slis,truxs.




TYPES: BEGIN OF gt_excel_file,
         rate_type  TYPE  kurst_curr,
         curr_text  TYPE  char10,
         to_currncy TYPE  tcurr_curr,
         valid_from TYPE  gdatu_cur,
         rate       TYPE  p LENGTH 9,
         from_curr  TYPE  fcurr_curr,
         per        TYPE  i,
         exch_rate  TYPE  ukursp,
       END OF gt_excel_file.

DATA: fieldcatalog TYPE                   slis_t_fieldcat_alv WITH HEADER LINE,
      gv_it_excel  TYPE STANDARD TABLE OF gt_excel_file,
      gv_wa_excel  LIKE LINE OF           gv_it_excel,
      gv_it_log    TYPE STANDARD TABLE OF zupdate_customer_from_excel,
      gv_wa_log    TYPE                   zupdate_customer_from_excel,
      gv_it_raw    TYPE                   truxs_t_text_data.


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
  PERFORM read_excel.
  PERFORM read_path.
  PERFORM process_data.
  PERFORM update_rate.









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

  CHECK p_file IS NOT INITIAL AND rad1 = 'X'.

  REFRESH gv_it_raw.
  REFRESH gv_it_excel.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = gv_it_raw
      i_filename           = p_file
    TABLES
      i_tab_converted_data = gv_it_excel[]
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Error in read Excel file!' TYPE 'E'.
  ENDIF.





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
        lv_date    TYPE gdatu_invv,
        lv_tcurf   TYPE tcurf.


  BREAK omrani.
  LOOP AT gv_it_excel INTO gv_wa_excel.

    CLEAR: lv_wa_rate,lv_wa_ret,lv_date.




    CLEAR lv_tcurf.
    SELECT * UP TO 1 ROWS
      INTO lv_tcurf
      FROM tcurf
      WHERE kurst =  gv_wa_excel-rate_type        AND
            fcurr =  gv_wa_excel-from_curr        AND
            tcurr =  gv_wa_excel-to_currncy       AND
            gdatu => gv_wa_excel-valid_from
      ORDER BY gdatu ASCENDING.
    ENDSELECT.
    IF sy-subrc NE 0.
      IF gv_wa_excel-to_currncy = 'IRR'.
        gv_wa_excel-exch_rate = ( gv_wa_excel-rate / gv_wa_excel-per ).
      ENDIF.
    ELSE.
      IF gv_wa_excel-to_currncy = 'IRR'.
        gv_wa_excel-exch_rate = ( gv_wa_excel-rate      / gv_wa_excel-per ) * ( lv_tcurf-ffact / lv_tcurf-tfact ).
      ELSE.
        gv_wa_excel-exch_rate = ( gv_wa_excel-exch_rate / gv_wa_excel-per ) * ( lv_tcurf-ffact / lv_tcurf-tfact ).
      ENDIF.
    ENDIF.



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
      ORDER BY gdatu ASCENDING.
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
        lv_rate         TYPE c LENGTH 20,
        lv_tcurf        TYPE tcurf.

  CHECK p_path IS NOT INITIAL AND rad2 = 'X'.

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
                               gv_wa_excel-curr_text
                               gv_wa_excel-to_currncy
                               gv_wa_excel-valid_from
                               lv_rate.

*      CLEAR lv_tcurf.
*      SELECT * UP TO 1 ROWS
*        INTO lv_tcurf
*        FROM tcurf
*        WHERE kurst =  gv_wa_excel-rate_type        AND
*              fcurr =  gv_wa_excel-from_curr        AND
*              tcurr =  gv_wa_excel-to_currncy       AND
*              gdatu => gv_wa_excel-valid_from
*        ORDER BY gdatu DESCENDING.
*      ENDSELECT.
*      IF sy-subrc NE 0.
*        gv_wa_excel-exch_rate = lv_rate.
*      ELSE.
*        gv_wa_excel-exch_rate = lv_rate * ( lv_tcurf-ffact / lv_tcurf-tfact ).
*      ENDIF.

      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN lv_rate WITH space.
      gv_wa_excel-rate = lv_rate .
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
*&---------------------------------------------------------------------*
*& Form PROCESS_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data .

  DATA: lv_it_excel TYPE STANDARD TABLE OF gt_excel_file,
        lv_wa_excel LIKE LINE OF           lv_it_excel,
        lv_tmp_data LIKE LINE OF           lv_it_excel.


  REFRESH lv_it_excel.
  LOOP AT gv_it_excel INTO gv_wa_excel.

    IF gv_wa_excel-curr_text(1) CA 'ZzYy'.

      IF  strlen( gv_wa_excel-curr_text ) > 4.
        gv_wa_excel-from_curr = gv_wa_excel-curr_text(4).
        REPLACE ALL OCCURRENCES OF gv_wa_excel-from_curr IN gv_wa_excel-curr_text WITH space.
        gv_wa_excel-per = gv_wa_excel-curr_text.
      ELSE.
        gv_wa_excel-from_curr = gv_wa_excel-curr_text.
        gv_wa_excel-per       = 1.
      ENDIF.

    ELSE.

      IF  strlen( gv_wa_excel-curr_text ) > 3.
        gv_wa_excel-from_curr = gv_wa_excel-curr_text(3).
        REPLACE ALL OCCURRENCES OF gv_wa_excel-from_curr IN gv_wa_excel-curr_text WITH space.
        gv_wa_excel-per = gv_wa_excel-curr_text.
      ELSE.
        gv_wa_excel-from_curr = gv_wa_excel-curr_text.
        gv_wa_excel-per       = 1.
      ENDIF.


    ENDIF.


    MODIFY gv_it_excel FROM gv_wa_excel TRANSPORTING from_curr per.
  ENDLOOP.

  CLEAR lv_tmp_data.
  READ TABLE gv_it_excel INTO lv_tmp_data WITH KEY from_curr = 'USD'.
  IF sy-subrc EQ 0.
    LOOP AT gv_it_excel INTO gv_wa_excel WHERE from_curr <> 'USD'.
      CLEAR lv_wa_excel.
      lv_wa_excel-from_curr  = gv_wa_excel-from_curr.
      lv_wa_excel-to_currncy = 'USD'.
      lv_wa_excel-per        = 1.
      lv_wa_excel-valid_from = gv_wa_excel-valid_from.
      lv_wa_excel-rate_type  = gv_wa_excel-rate_type.
      lv_wa_excel-exch_rate  = ( gv_wa_excel-rate / gv_wa_excel-per ) / lv_tmp_data-rate.
      APPEND lv_wa_excel TO lv_it_excel.
    ENDLOOP.
  ENDIF.

  CLEAR lv_tmp_data.
  READ TABLE gv_it_excel INTO lv_tmp_data WITH KEY from_curr = 'EUR'.
  IF sy-subrc EQ 0.
    LOOP AT gv_it_excel INTO gv_wa_excel WHERE from_curr <> 'EUR'.
      CLEAR lv_wa_excel.
      lv_wa_excel-from_curr  = gv_wa_excel-from_curr.
      lv_wa_excel-to_currncy = 'EUR'.
      lv_wa_excel-per        = 1.
      lv_wa_excel-valid_from = gv_wa_excel-valid_from.
      lv_wa_excel-rate_type  = gv_wa_excel-rate_type.
      lv_wa_excel-exch_rate  = ( gv_wa_excel-rate / gv_wa_excel-per ) / lv_tmp_data-rate.
      APPEND lv_wa_excel TO lv_it_excel.
    ENDLOOP.
  ENDIF.

  APPEND LINES OF lv_it_excel TO gv_it_excel.

ENDFORM.
