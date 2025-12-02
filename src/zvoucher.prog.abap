*&---------------------------------------------------------------------*
*& Report ZVOUCHER
*&---------------------------------------------------------------------*
*& Improved Version - Accounting Document List Report
*& Modern ABAP syntax with better structure and maintainability
*&---------------------------------------------------------------------*
REPORT zvoucher.

*&---------------------------------------------------------------------*
*& Type Definitions
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_voucher,
         mandt   TYPE zvoucher-mandt,
         bukrs   TYPE zvoucher-bukrs,
         umskz   TYPE zvoucher-umskz,
         buzei   TYPE zvoucher-buzei,
         belnr   TYPE zvoucher-belnr,
         gjahr   TYPE zvoucher-gjahr,
         monat   TYPE zvoucher-monat,
         bldat   TYPE zvoucher-bldat,
         budat   TYPE zvoucher-budat,
         hkont   TYPE zvoucher-hkont,
         txt20   TYPE zvoucher-txt20,
         kunnr   TYPE zvoucher-kunnr,
         lifnr   TYPE zvoucher-lifnr,
         kostl   TYPE zvoucher-kostl,
         bed_irr TYPE zvoucher-bed_irr,
         bes_irr TYPE zvoucher-bes_irr,
         bed     TYPE zvoucher-bed,
         bes     TYPE zvoucher-bes,
         waers   TYPE zvoucher-waers,
         augbl   TYPE zvoucher-augbl,
         vbeln   TYPE zvoucher-vbeln,
         vbel2   TYPE zvoucher-vbel2,
         ebeln   TYPE zvoucher-ebeln,
         ebelp   TYPE zvoucher-ebelp,
         bschl   TYPE zvoucher-bschl,
         valut   TYPE zvoucher-valut,
         sgtxt   TYPE zvoucher-sgtxt,
         werks   TYPE zvoucher-werks,
         aufnr   TYPE zvoucher-aufnr,
         matnr   TYPE zvoucher-matnr,
         bvtyp   TYPE zvoucher-bvtyp,
         qty     TYPE zvoucher-qty,
         meins   TYPE zvoucher-meins,
         awkey   TYPE zvoucher-awkey,
         blart   TYPE zvoucher-blart,
         cputm   TYPE zvoucher-cputm,
         xblnr   TYPE zvoucher-xblnr,
         koart   TYPE zvoucher-koart,
         usnam   TYPE zvoucher-usnam,
         tcode   TYPE zvoucher-tcode,
         kursf   TYPE zvoucher-kursf,
       END OF ty_voucher.

*&---------------------------------------------------------------------*
*& Data Declarations
*&---------------------------------------------------------------------*
DATA: gt_voucher TYPE STANDARD TABLE OF ty_voucher,
      go_alv     TYPE REF TO cl_salv_table,
      gx_error   TYPE REF TO cx_salv_msg.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Selection Screen - Same as Version 1
*&---------------------------------------------------------------------*
DATA: lv_bukrs   TYPE zvoucher-bukrs,
      lv_gjahr   TYPE zvoucher-gjahr,
      lv_monat   TYPE zvoucher-monat,
      lv_budat   TYPE zvoucher-budat,
      lv_belnr   TYPE zvoucher-belnr,
      lv_hkont   TYPE zvoucher-hkont,
      lv_kunnr   TYPE zvoucher-kunnr,
      lv_lifnr   TYPE zvoucher-lifnr,
      lv_matnr   TYPE zvoucher-matnr,
      lv_ebeln   TYPE zvoucher-ebeln,
      lv_vbel2   TYPE zvoucher-vbel2,
      lv_augbl   TYPE zvoucher-augbl,
      lv_aufnr   TYPE zvoucher-aufnr,
      lv_kostl   TYPE zvoucher-kostl,
      lv_umskz   TYPE zvoucher-umskz,
      lv_bed_irr TYPE bseg-dmbtr,
      lv_bes_irr TYPE bseg-dmbtr.

PARAMETERS: p_bukrs TYPE bukrs DEFAULT '1000'.

SELECT-OPTIONS: p_gjahr FOR lv_gjahr,
                p_monat FOR lv_monat,
                p_budat FOR lv_budat,
                p_belnr FOR lv_belnr,
                p_hkont FOR lv_hkont,
                p_kunnr FOR lv_kunnr,
                p_lifnr FOR lv_lifnr,
                p_matnr FOR lv_matnr,
                p_ebeln FOR lv_ebeln,
                p_vbel2 FOR lv_vbel2,
                p_augbl FOR lv_augbl,
                p_aufnr FOR lv_aufnr,
                p_kostl FOR lv_kostl,
                p_umskz FOR lv_umskz,
                p_bedir FOR lv_bed_irr,
                p_besir FOR lv_bes_irr.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
" TEXT-001: 'Output Options' or 'گزینه‌های خروجی'
PARAMETERS: p_excel AS CHECKBOX DEFAULT ' '. " Export to CSV
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& Initialization
*&---------------------------------------------------------------------*
INITIALIZATION.

*&---------------------------------------------------------------------*
*& Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.

  IF p_excel = abap_true.
    PERFORM download_excel.
  ELSE.
    PERFORM display_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data.
  " Retrieve voucher data from database
  SELECT *
    FROM zvoucher
    INTO CORRESPONDING FIELDS OF TABLE @gt_voucher
    WHERE bukrs   = @p_bukrs
      AND belnr  IN @p_belnr
      AND hkont  IN @p_hkont
      AND kunnr  IN @p_kunnr
      AND lifnr  IN @p_lifnr
      AND matnr  IN @p_matnr
      AND ebeln  IN @p_ebeln
      AND vbel2  IN @p_vbel2
      AND augbl  IN @p_augbl
      AND aufnr  IN @p_aufnr
      AND kostl  IN @p_kostl
      AND budat  IN @p_budat
      AND gjahr  IN @p_gjahr
      AND monat  IN @p_monat
      AND umskz  IN @p_umskz
      AND bed_irr IN @p_bedir
      AND bes_irr IN @p_besir.

  IF sy-subrc <> 0.
    MESSAGE 'No data found for the given selection criteria' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
FORM display_alv.
  " Check if data exists
  IF gt_voucher IS INITIAL.
    MESSAGE 'No data to display' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Create ALV object
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_voucher ).

    CATCH cx_salv_msg INTO gx_error.
      MESSAGE gx_error->get_text( ) TYPE 'E'.
      RETURN.
  ENDTRY.

  " Configure ALV
  PERFORM configure_alv.

  " Display ALV
  go_alv->display( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form configure_alv
*&---------------------------------------------------------------------*
FORM configure_alv.
  DATA: lo_functions TYPE REF TO cl_salv_functions_list,
        lo_columns   TYPE REF TO cl_salv_columns_table,
        lo_column    TYPE REF TO cl_salv_column_table,
        lo_display   TYPE REF TO cl_salv_display_settings,
        lv_header    TYPE lvc_title,
        lv_lines     TYPE i.

  " Enable all standard ALV functions (export, sort, filter, etc.)
  lo_functions = go_alv->get_functions( ).
  lo_functions->set_all( abap_true ).

  " Get columns object
  lo_columns = go_alv->get_columns( ).

  " Set columns to be optimized
  lo_columns->set_optimize( abap_true ).

  " Hide MANDT column
  TRY.
      lo_column ?= lo_columns->get_column( 'MANDT' ).
      lo_column->set_visible( abap_false ).
    CATCH cx_salv_not_found.
      " Column not found - ignore
  ENDTRY.

  " Configure specific columns
  PERFORM set_column_properties USING lo_columns.

  " Set list header
  lo_display = go_alv->get_display_settings( ).
  lv_lines = lines( gt_voucher ).
  lv_header = |مشاهده اسناد حسابداری   { lv_lines }  رکورد|.
  lo_display->set_list_header( lv_header ).
  lo_display->set_striped_pattern( abap_true ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form set_column_properties - Same as Version 1
*&---------------------------------------------------------------------*
FORM set_column_properties USING io_columns TYPE REF TO cl_salv_columns_table.
  DATA: lo_column TYPE REF TO cl_salv_column.

  lo_column = io_columns->get_column( 'BUKRS' ).
  lo_column = io_columns->get_column( 'UMSKZ' ).
  lo_column->set_output_length( 8 ).
  lo_column = io_columns->get_column( 'BUZEI' ).
  lo_column->set_output_length( 6 ).
  lo_column = io_columns->get_column( 'BELNR' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'GJAHR' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'MONAT' ).
  lo_column->set_output_length( 4 ).
  lo_column = io_columns->get_column( 'BLDAT' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'BUDAT' ).
  lo_column->set_output_length( 8 ).
  lo_column = io_columns->get_column( 'HKONT' ).
  lo_column->set_output_length( 8 ).
  lo_column = io_columns->get_column( 'TXT20' ).
  lo_column->set_output_length( 20 ).
  lo_column = io_columns->get_column( 'LIFNR' ).
  lo_column->set_output_length( 8 ).
  lo_column = io_columns->get_column( 'KUNNR' ).
  lo_column->set_output_length( 8 ).
  lo_column = io_columns->get_column( 'KOSTL' ).
  lo_column->set_output_length( 8 ).
  lo_column = io_columns->get_column( 'AUFNR' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'MATNR' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'BVTYP' ).
  lo_column->set_output_length( 6 ).

  lo_column = io_columns->get_column( 'BED' ).
  lo_column->set_long_text( 'BED' ).
  lo_column->set_medium_text( 'BED' ).
  lo_column->set_short_text( 'BED' ).
  lo_column->set_output_length( 12 ).

  lo_column = io_columns->get_column( 'BES' ).
  lo_column->set_long_text( 'BES' ).
  lo_column->set_medium_text( 'BES' ).
  lo_column->set_short_text( 'BES' ).
  lo_column->set_output_length( 12 ).

  lo_column = io_columns->get_column( 'WAERS' ).
  lo_column->set_output_length( 6 ).

  lo_column = io_columns->get_column( 'BED_IRR' ).
  lo_column->set_long_text( 'BED_IRR' ).
  lo_column->set_medium_text( 'BED_IRR' ).
  lo_column->set_short_text( 'BED_IRR' ).
  lo_column->set_output_length( 12 ).
  lo_column->set_decimals( '0' ).

  lo_column = io_columns->get_column( 'BES_IRR' ).
  lo_column->set_long_text( 'BES_IRR' ).
  lo_column->set_medium_text( 'BES_IRR' ).
  lo_column->set_short_text( 'BES_IRR' ).
  lo_column->set_output_length( 12 ).
  lo_column->set_decimals( '0' ).

  lo_column = io_columns->get_column( 'AUGBL' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'VBELN' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'VBEL2' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'EBELN' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'EBELP' ).
  lo_column->set_output_length( 5 ).
  lo_column = io_columns->get_column( 'BSCHL' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'VALUT' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'SGTXT' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'WERKS' ).
  lo_column->set_output_length( 10 ).

  lo_column = io_columns->get_column( 'QTY' ).
  lo_column->set_long_text( 'QTY' ).
  lo_column->set_output_length( 10 ).

  lo_column = io_columns->get_column( 'MEINS' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'AWKEY' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'BLART' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'CPUTM' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'XBLNR' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'KOART' ).
  lo_column->set_output_length( 10 ).
  lo_column = io_columns->get_column( 'USNAM' ).
  lo_column->set_output_length( 12 ).
  lo_column = io_columns->get_column( 'TCODE' ).
  lo_column->set_output_length( 6 ).

  lo_column = io_columns->get_column( 'KURSF' ).
  lo_column->set_long_text( 'Exchange rate' ).
  lo_column->set_output_length( 10 ).
ENDFORM.

*& Form download_excel
*&---------------------------------------------------------------------*
*& Export data to CSV files with automatic splitting at 1 million rows
*&---------------------------------------------------------------------*
FORM download_excel.
  CONSTANTS: lc_chunk_size TYPE i VALUE 1000000.  " 1 million rows per file

  DATA: lv_filename      TYPE string,
        lv_base_filename TYPE string,
        lv_path          TYPE string,
        lv_fullpath      TYPE string,
        lv_action        TYPE i,
        lv_total_lines   TYPE i,
        lv_line          TYPE string,
        lt_csv_data      TYPE TABLE OF string,
        ls_voucher       TYPE ty_voucher,
        lv_chunk_num     TYPE i,
        lv_total_chunks  TYPE i,
        lv_from_index    TYPE i,
        lv_to_index      TYPE i,
        lv_chunk_lines   TYPE i,
        lv_files_created TYPE i,
        lv_msg           TYPE string.

  " Check if data exists
  IF gt_voucher IS INITIAL.
    MESSAGE 'No data to export' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Count total records
  lv_total_lines = lines( gt_voucher ).

  " Calculate number of chunks needed (convert to float to avoid integer division)
  DATA(lv_total_lines_f) = CONV f( lv_total_lines ).
  DATA(lc_chunk_size_f) = CONV f( lc_chunk_size ).
  lv_total_chunks = CEIL( lv_total_lines_f / lc_chunk_size_f ).

  " Default base filename with timestamp
  CONCATENATE 'Accounting_Documents_'
              sy-datum '_' sy-uzeit
         INTO lv_base_filename.

  " Show file save dialog for the first file or path selection
  IF lv_total_chunks = 1.
    CONCATENATE lv_base_filename '.csv' INTO lv_filename.
  ELSE.
    CONCATENATE lv_base_filename '_Part1.csv' INTO lv_filename.
  ENDIF.

  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      window_title      = 'Select Directory and Base Filename for CSV Export'
      default_extension = 'csv'
      default_file_name = lv_filename
      file_filter       = 'CSV Files (*.csv)|*.csv|All Files (*.*)|*.*'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_action
    EXCEPTIONS
      OTHERS            = 1 ).

  IF sy-subrc <> 0 OR lv_action = cl_gui_frontend_services=>action_cancel.
    MESSAGE 'Export cancelled by user' TYPE 'S'.
    RETURN.
  ENDIF.

  " Extract base path (remove extension from selected path)
  DATA: lv_base_path TYPE string,
        lv_current_file TYPE string.

  " Get base path without extension
  IF lv_total_chunks > 1.
    " Remove _Part1.csv to get base
    lv_base_path = lv_fullpath.
    REPLACE '_Part1.csv' IN lv_base_path WITH ''.
  ELSE.
    " Remove .csv to get base
    lv_base_path = lv_fullpath.
    REPLACE '.csv' IN lv_base_path WITH ''.
  ENDIF.

  " Process data in chunks
  lv_chunk_num = 1.
  lv_files_created = 0.

  " Debug: Show processing info
  lv_msg = |Processing { lv_total_lines } records in { lv_total_chunks } file(s)|.
  MESSAGE lv_msg TYPE 'I'.

  WHILE lv_chunk_num <= lv_total_chunks.
    " Calculate index range for this chunk
    lv_from_index = ( lv_chunk_num - 1 ) * lc_chunk_size + 1.
    lv_to_index = lv_chunk_num * lc_chunk_size.

    IF lv_to_index > lv_total_lines.
      lv_to_index = lv_total_lines.
    ENDIF.

    " Clear CSV data table for this chunk
    CLEAR lt_csv_data.

    " Build CSV Header (Persian labels matching ALV)
    " Note: UTF-8 BOM will be added via codepage during download
    CONCATENATE 'کد شرکت' 'نوع حساب ویژه' 'ردیف' 'شماره سند' 'سال مالی' 'دوره'
                'تاریخ سند' 'تاریخ ثبت' 'حساب' 'شرح حساب' 'مشتری' 'فروشنده'
                'مرکز هزینه' 'بدهکار ریال' 'بستانکار ریال' 'بدهکار ارز' 'بستانکار ارز' 'واحد پول'
                'سند تسویه' 'سند فروش' 'سند فروش 2' 'سفارش خرید' 'ردیف سفارش' 'کلید ثبت'
                'تاریخ ارزش' 'شرح سند' 'کارخانه' 'سفارش' 'کالا' 'نوع تراکنش'
                'مقدار' 'واحد اندازه' 'کلید مرجع' 'نوع سند' 'زمان ثبت' 'شماره مرجع'
                'نوع حساب' 'کاربر' 'تراکنش' 'نرخ ارز'
           INTO lv_line SEPARATED BY ';'.

    APPEND lv_line TO lt_csv_data.

    " Build CSV Data rows for this chunk only
    LOOP AT gt_voucher INTO ls_voucher FROM lv_from_index TO lv_to_index.
      CLEAR lv_line.

      " Build line with all fields (converting numerics to string)
      lv_line = |{ ls_voucher-bukrs };|
             && |{ ls_voucher-umskz };|
             && |{ ls_voucher-buzei };|
             && |{ ls_voucher-belnr };|
             && |{ ls_voucher-gjahr };|
             && |{ ls_voucher-monat };|
             && |{ ls_voucher-bldat DATE = USER };|
             && |{ ls_voucher-budat DATE = USER };|
             && |{ ls_voucher-hkont };|
             && |{ ls_voucher-txt20 };|
             && |{ ls_voucher-kunnr };|
             && |{ ls_voucher-lifnr };|
             && |{ ls_voucher-kostl };|
             && |{ ls_voucher-bed_irr };|
             && |{ ls_voucher-bes_irr };|
             && |{ ls_voucher-bed };|
             && |{ ls_voucher-bes };|
             && |{ ls_voucher-waers };|
             && |{ ls_voucher-augbl };|
             && |{ ls_voucher-vbeln };|
             && |{ ls_voucher-vbel2 };|
             && |{ ls_voucher-ebeln };|
             && |{ ls_voucher-ebelp };|
             && |{ ls_voucher-bschl };|
             && |{ ls_voucher-valut DATE = USER };|
             && |{ ls_voucher-sgtxt };|
             && |{ ls_voucher-werks };|
             && |{ ls_voucher-aufnr };|
             && |{ ls_voucher-matnr };|
             && |{ ls_voucher-bvtyp };|
             && |{ ls_voucher-qty };|
             && |{ ls_voucher-meins };|
             && |{ ls_voucher-awkey };|
             && |{ ls_voucher-blart };|
             && |{ ls_voucher-cputm };|
             && |{ ls_voucher-xblnr };|
             && |{ ls_voucher-koart };|
             && |{ ls_voucher-usnam };|
             && |{ ls_voucher-tcode };|
             && |{ ls_voucher-kursf }|.

      APPEND lv_line TO lt_csv_data.
    ENDLOOP.

    " Build filename for this chunk using base path
    IF lv_total_chunks = 1.
      lv_current_file = |{ lv_base_path }.csv|.
    ELSE.
      lv_current_file = |{ lv_base_path }_Part{ lv_chunk_num }.csv|.
    ENDIF.

    " Debug: Show file info
    DATA(lv_csv_lines) = lines( lt_csv_data ).
    lv_msg = |Saving file { lv_chunk_num }: { lv_csv_lines } lines (incl. header)|.
    MESSAGE lv_msg TYPE 'I'.

    " Download CSV file for this chunk
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename              = lv_current_file
        filetype              = 'ASC'
        write_field_separator = ' '
        trunc_trailing_blanks = abap_true
        codepage              = '4110'  " UTF-8 encoding for Persian support
      CHANGING
        data_tab              = lt_csv_data
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24 ).

    IF sy-subrc = 0.
      lv_files_created = lv_files_created + 1.
      lv_chunk_lines = lv_to_index - lv_from_index + 1.

      " Show progress message for each file
      IF lv_total_chunks > 1.
        lv_msg = |Part { lv_chunk_num }/{ lv_total_chunks } exported: { lv_chunk_lines } records|.
        MESSAGE lv_msg TYPE 'S'.
      ENDIF.
    ELSE.
      lv_msg = |Error downloading Part { lv_chunk_num }|.
      MESSAGE lv_msg TYPE 'E'.
      RETURN.
    ENDIF.

    " Move to next chunk
    lv_chunk_num = lv_chunk_num + 1.
  ENDWHILE.

  " Final success message
  IF lv_total_chunks = 1.
    MESSAGE |CSV file exported successfully: { lv_total_lines } records| TYPE 'S'.
  ELSE.
    lv_msg = |Export completed: { lv_files_created } files, { lv_total_lines } total records|.
    MESSAGE lv_msg TYPE 'S'.
  ENDIF.
ENDFORM.
