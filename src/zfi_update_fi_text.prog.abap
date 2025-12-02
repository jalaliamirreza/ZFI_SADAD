*&---------------------------------------------------------------------*
*& Report ZFI_UPDATE_FI_TEXT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_update_fi_text.




TABLES : acdoca.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_update_fi_text,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_log    TYPE TABLE OF zfi_update_fi_text,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS : s_rbukrs   FOR  acdoca-rbukrs NO INTERVALS NO-EXTENSION OBLIGATORY,
                 s_gjahr    FOR  acdoca-gjahr  NO INTERVALS NO-EXTENSION OBLIGATORY,
                 s_belnr    FOR  acdoca-belnr.

PARAMETERS: p_text TYPE bseg-sgtxt.

SELECTION-SCREEN: END   OF BLOCK blk1.


START-OF-SELECTION.


  PERFORM check_input_data.
  PERFORM get_fi_doc.
  PERFORM fill_text.
  PERFORM update_text.
  PERFORM build_fieldcatalog.
  PERFORM display_grid.








*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcatalog .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZFI_UPDATE_FI_TEXT'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'F1'.
        fieldcatalog-seltext_l     = 'text'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN OTHERS.
    ENDCASE.



    MODIFY fieldcatalog INDEX sy-tabix.
  ENDLOOP.


ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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

ENDFORM.                    " DISPLAY_GRID
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_input_data .

  IF s_belnr IS INITIAL AND p_text IS NOT INITIAL.
    MESSAGE 'شماره سند مالي را وارد نماييد' TYPE 'E'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FI_DOC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_fi_doc .


  REFRESH gv_it_output.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE gv_it_output
    FROM acdoca
    WHERE rbukrs IN s_rbukrs AND
          gjahr  IN s_gjahr  AND
          belnr  IN s_belnr  AND
          "awtyp  =  'MKPF'   AND
          rldnr  = '0L'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fill_text .

  DATA : lv_mat(8).

  LOOP AT gv_it_output INTO gv_wa_output.

    IF p_text IS INITIAL.
      gv_wa_output-mblnr = gv_wa_output-awref.
      gv_wa_output-mjahr = gv_wa_output-aworg.
      gv_wa_output-zeile = gv_wa_output-awitem.
      SELECT SINGLE bwart matnr INTO (gv_wa_output-bwart,gv_wa_output-matnr)
        FROM mseg WHERE mblnr = gv_wa_output-mblnr AND
                        mjahr = gv_wa_output-mjahr AND
                        zeile = gv_wa_output-zeile.
      IF sy-subrc EQ 0.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = gv_wa_output-matnr
          IMPORTING
            output = lv_mat.

        CONCATENATE gv_wa_output-bwart lv_mat INTO gv_wa_output-sgtxt SEPARATED BY space.

      ENDIF.
    ELSE.
      gv_wa_output-sgtxt = p_text.
    ENDIF.

    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM update_text .

  DATA: lv_bseg       TYPE         bseg,
        lv_it_buztab  TYPE         tpit_t_buztab,
        lv_wa_buztab  LIKE LINE OF lv_it_buztab,
        lv_it_errtab  TYPE         tpit_t_errdoc,
        lv_wa_errtab  LIKE LINE OF lv_it_errtab,
        lv_it_fldtab  TYPE         tpit_t_fname,
        lv_wa_fldtab  LIKE LINE OF lv_it_fldtab,
        lv_last_belnr TYPE         bseg-belnr.


  REFRESH gv_it_log.
  CLEAR lv_last_belnr.
  SORT gv_it_output BY zeile.
  LOOP AT gv_it_output INTO gv_wa_output WHERE sgtxt IS NOT INITIAL .

    REFRESH: lv_it_buztab,lv_it_errtab,lv_it_fldtab.
    CLEAR lv_bseg.

    SELECT * FROM bseg INTO CORRESPONDING FIELDS OF TABLE lv_it_buztab
      WHERE bukrs = gv_wa_output-rbukrs AND
            gjahr = gv_wa_output-gjahr  AND
            belnr = gv_wa_output-belnr  AND
            buzei = gv_wa_output-docln  AND
            sgtxt = ''.
    IF sy-subrc EQ 0.
      lv_wa_fldtab-fname = 'SGTXT'.
      lv_wa_fldtab-aenkz = 'M'.
      APPEND lv_wa_fldtab TO lv_it_fldtab.
      lv_bseg-sgtxt = gv_wa_output-sgtxt.

      CALL FUNCTION 'FI_ITEMS_MASS_CHANGE'
        EXPORTING
          s_bseg     = lv_bseg
        IMPORTING
          errtab     = lv_it_errtab
        TABLES
          it_buztab  = lv_it_buztab
          it_fldtab  = lv_it_fldtab
        EXCEPTIONS
          bdc_errors = 1
          OTHERS     = 2.

      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

      LOOP AT lv_it_errtab INTO lv_wa_errtab.
        MOVE-CORRESPONDING lv_wa_errtab-err TO gv_wa_output.
        APPEND gv_wa_output TO gv_it_log.
      ENDLOOP.
      IF sy-subrc NE 0.
        APPEND gv_wa_output TO gv_it_log.
      ENDIF.


      IF gv_wa_output-belnr = lv_last_belnr.
        WAIT UP TO 1 SECONDS.
      ENDIF.

      lv_last_belnr = gv_wa_output-belnr.
    ENDIF.

  ENDLOOP.


ENDFORM.
