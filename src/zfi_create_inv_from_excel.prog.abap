*&---------------------------------------------------------------------*
*& Report ZFI_CREATE_INV_FROM_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_create_inv_from_excel.

TYPE-POOLS: slis,truxs.

TYPES: BEGIN OF gt_excel_file,
         hkont  TYPE acgl_item-hkont,
         shkzg1 TYPE acgl_item-shkzg,
         matnr  TYPE cki_mr22_0250-matnr,
         bwkey  TYPE cki_mr22_0250-bwkey,
         bwtar  TYPE cki_mr22_0250-bwtar,
         shkzg2 TYPE acgl_item-shkzg,
         zuumb  TYPE cki_mr22_0250-zuumb,
         menge  TYPE cki_mr22_0250-menge,
         mwskz  TYPE mwskz_mrm,
       END OF gt_excel_file.

DATA: fieldcatalog TYPE                   slis_t_fieldcat_alv WITH HEADER LINE,
      gv_it_excel  TYPE STANDARD TABLE OF gt_excel_file,
      gv_wa_excel  LIKE LINE OF           gv_it_excel,
      gv_it_log    TYPE TABLE OF          zfi_create_inv_from_excel,
      gv_wa_log    LIKE LINE OF           gv_it_log,
      gv_it_raw    TYPE                   truxs_t_text_data.






SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME .
PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY,
            p_bukrs TYPE rbkp-bukrs OBLIGATORY,
            p_bldat TYPE rbkp-bldat OBLIGATORY,
            p_budat TYPE rbkp-budat OBLIGATORY,
            p_bline TYPE rbkp-budat OBLIGATORY,
            p_xblnr TYPE rbkp-xblnr OBLIGATORY,
            p_waers TYPE rbkp-waers OBLIGATORY,
            p_sgtxt TYPE rbkp-sgtxt OBLIGATORY,
            p_lifnr TYPE rbkp-lifnr OBLIGATORY,
            p_bktxt TYPE rbkp-bktxt OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK blk1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.



START-OF-SELECTION.

  PERFORM read_excel.



  LOOP AT gv_it_excel INTO gv_wa_excel.
    PERFORM convert_input CHANGING gv_wa_excel-hkont.
    PERFORM convert_matnr CHANGING gv_wa_excel-matnr.
    PERFORM create_inv.

  ENDLOOP.

  PERFORM build_fieldcatalog.
  PERFORM display_grid.




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
*& Form CONVERT_MATNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_GV_WA_EXCEL_MATNR  text
*&---------------------------------------------------------------------*
FORM convert_matnr  CHANGING lv_matnr.


  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = lv_matnr
    IMPORTING
      output       = lv_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_INV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_inv .

  DATA: lv_header       TYPE          bapi_incinv_create_header,
        lv_it_item      TYPE TABLE OF bapi_incinv_create_item,
        lv_wa_item      LIKE LINE OF  lv_it_item,
        lv_it_mat       TYPE TABLE OF bapi_incinv_create_material,
        lv_wa_mat       LIKE LINE OF  lv_it_mat,
        lv_it_gl        TYPE TABLE OF bapi_incinv_create_gl_account,
        lv_wa_gl        LIKE LINE OF  lv_it_gl,
        lv_it_ret       TYPE TABLE OF bapiret2,
        lv_wa_ret       LIKE LINE OF  lv_it_ret,
        lv_inv_doc      TYPE          bapi_incinv_fld-inv_doc_no,
        lv_inv_doc_year TYPE          bapi_incinv_fld-fisc_year.


  CLEAR lv_header.
  REFRESH: lv_it_item,lv_it_ret,lv_it_mat,lv_it_gl.


  lv_header-invoice_ind    = 'X'.
  lv_header-doc_type       = 'RE'.
  lv_header-doc_date       = p_bldat.
  lv_header-pstng_date     = p_budat.
  lv_header-bline_date     = p_bline.
  lv_header-ref_doc_no     = p_xblnr.
  lv_header-comp_code      = p_bukrs.
  lv_header-diff_inv       = p_lifnr.
  lv_header-currency       = p_waers.
  lv_header-header_txt     = p_bktxt.
  lv_header-item_text      = p_sgtxt.


  CLEAR lv_wa_mat.
  lv_wa_mat-invoice_doc_item = 1.
  lv_wa_mat-material         = gv_wa_excel-matnr.
  lv_wa_mat-tax_code         = gv_wa_excel-mwskz.
  lv_wa_mat-val_area         = gv_wa_excel-bwkey.
  lv_wa_mat-valuation_type   = gv_wa_excel-bwtar.
  lv_wa_mat-db_cr_ind        = gv_wa_excel-shkzg2.
  lv_wa_mat-item_amount      = gv_wa_excel-zuumb.
  lv_wa_mat-quantity         = gv_wa_excel-menge.
  SELECT SINGLE meins INTO lv_wa_mat-base_uom FROM mara WHERE matnr = gv_wa_excel-matnr.
  APPEND lv_wa_mat TO lv_it_mat.

  CLEAR lv_wa_gl.
  lv_wa_gl-invoice_doc_item  = 1.
  lv_wa_gl-gl_account        = gv_wa_excel-hkont.
  lv_wa_gl-item_amount       = gv_wa_excel-zuumb.
  lv_wa_gl-db_cr_ind         = gv_wa_excel-shkzg1.
  lv_wa_gl-comp_code         = p_bukrs.
  APPEND lv_wa_gl TO lv_it_gl.


  BREAK omrani.
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE1'
    EXPORTING
      headerdata       = lv_header
    IMPORTING
      invoicedocnumber = lv_inv_doc
      fiscalyear       = lv_inv_doc_year
    TABLES
      itemdata         = lv_it_item
      glaccountdata    = lv_it_gl
      materialdata     = lv_it_mat
      return           = lv_it_ret.


  LOOP AT lv_it_ret INTO lv_wa_ret WHERE type = 'E'.
    CLEAR gv_wa_log.
    gv_wa_log-matnr = gv_wa_excel-matnr.
    MOVE-CORRESPONDING lv_wa_ret TO gv_wa_log.
    APPEND gv_wa_log TO gv_it_log.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    CLEAR gv_wa_log.
    gv_wa_log-matnr = gv_wa_excel-matnr.
    gv_wa_log-type = 'S'.
    gv_wa_log-message_v1 = lv_inv_doc.
    gv_wa_log-message_v2 = lv_inv_doc_year.
    CONCATENATE 'Document Number:' lv_inv_doc '-' lv_inv_doc_year 'Created' INTO gv_wa_log-message SEPARATED BY space.
    APPEND gv_wa_log TO gv_it_log.
  ENDIF.


ENDFORM.

FORM build_fieldcatalog .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_CREATE_INV_FROM_EXCEL'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


ENDFORM.

FORM display_grid .

  DATA: is_layout TYPE slis_layout_alv .

  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog[]
      i_save             = 'X'
      is_layout          = is_layout
    TABLES
      t_outtab           = gv_it_log
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERT_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_GV_WA_EXCEL_HKONT  text
*&---------------------------------------------------------------------*
FORM convert_input  CHANGING lv_hkont.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_hkont
    IMPORTING
      output = lv_hkont.



ENDFORM.
