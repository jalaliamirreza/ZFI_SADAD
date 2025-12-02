*&---------------------------------------------------------------------*
*& Report ZFI_RENUMBERING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_renumbering.




TABLES : bkpf.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_renumbering,
      gv_wa_output LIKE LINE OF  gv_it_output,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
PARAMETERS: p_bukrs TYPE bkpf-bukrs OBLIGATORY,
            p_gjahr TYPE bkpf-gjahr OBLIGATORY.
SELECT-OPTIONS : s_belnr    FOR  bkpf-belnr,
                 s_budat    FOR  bkpf-budat OBLIGATORY,
                 s_blart    FOR  bkpf-blart OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK blk1.



INITIALIZATION.
  PERFORM initialization.


START-OF-SELECTION.

  PERFORM check_input.
  PERFORM get_data.
  PERFORM updata_data.
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
      i_structure_name = 'ZFI_RENUMBERING'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'F1'.
        PERFORM set_catalog_text USING 'Txt' CHANGING fieldcatalog.

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
      t_outtab           = gv_it_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.                    " DISPLAY_GRID
*&---------------------------------------------------------------------*
*& Form SET_CATALOG_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      <--P_LV_WA_FIELDCATALOG  text
*&---------------------------------------------------------------------*
FORM set_catalog_text  USING    lv_text            TYPE char100
                       CHANGING lv_wa_fieldcatalog TYPE slis_fieldcat_alv.


  lv_wa_fieldcatalog-seltext_l     = lv_text.
  lv_wa_fieldcatalog-seltext_m     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-seltext_s     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-reptext_ddic  = lv_wa_fieldcatalog-seltext_l.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

  REFRESH gv_it_output.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gv_it_output
    FROM t003_i
    JOIN bkpf ON t003_i~blart = bkpf~blart
    WHERE bkpf~bukrs =  p_bukrs AND
          bkpf~belnr IN s_belnr AND
          bkpf~gjahr =  p_gjahr AND
          bkpf~blart IN s_blart AND
          bkpf~budat IN s_budat AND
          t003_i~land1 = 'IR'
    ORDER BY bkpf~budat ASCENDING bkpf~gjahr bkpf~belnr.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATA_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM updata_data .

  DATA: lv_it_accchg TYPE TABLE OF accchg,
        lv_wa_accchg LIKE LINE OF  lv_it_accchg,
        lv_xblnr     TYPE          bkpf-xblnr,
        lv_date      TYPE          datum.

  DATA: doccls      TYPE doccls,
        groupnumber TYPE nrnr,
        nrlevel     TYPE nrlevel,
        lastxblnr   TYPE xblnr,
        lastnr      TYPE nrlevel,
        toyear      TYPE inri-toyear.



  LOOP AT gv_it_output INTO gv_wa_output.
    lv_date = gv_wa_output-budat.

    AT FIRST.
      SELECT SINGLE doccls FROM t003_i INTO doccls WHERE blart IN s_blart AND land1 = 'IR'.
        IF p_bukrs = '1100'.
           doccls  = 'Z01'.
        ENDIF.
      SELECT SINGLE groupnumber FROM ofnum_it_1 INTO (groupnumber) WHERE docclass = doccls AND bukrs = p_bukrs.
      SELECT SINGLE nrlevel
        FROM nriv INTO nrlevel
        WHERE object    = 'FIN2_IT'   AND
              subobject = p_bukrs     AND
              nrrangenr = groupnumber AND
              toyear    = p_gjahr.

      SELECT MAX( xblnr ) FROM bkpf INTO lastxblnr WHERE blart IN s_blart AND bukrs = p_bukrs AND budat < lv_date AND gjahr = p_gjahr.
      IF sy-subrc EQ 0 AND lastxblnr <> 0.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lastxblnr
          IMPORTING
            output = lastnr.

        nrlevel = lastnr + 1.

      ELSE.
        toyear = p_gjahr.
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = groupnumber
            object      = 'FIN2_IT'
            quantity    = '1'
            subobject   = p_bukrs
            toyear      = toyear
          IMPORTING
            number      = nrlevel.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = nrlevel
        IMPORTING
          output = lv_xblnr.
      SHIFT lv_xblnr LEFT DELETING LEADING '0'.
    ENDAT.



    IF lv_xblnr IS NOT INITIAL.
      REFRESH lv_it_accchg.
      CLEAR lv_wa_accchg.

      SHIFT lv_xblnr LEFT DELETING LEADING '0'.
      SHIFT lv_xblnr LEFT DELETING LEADING space.
      lv_wa_accchg-fdname = 'XBLNR'.
      lv_wa_accchg-newval = lv_xblnr.
      lv_wa_accchg-oldval = gv_wa_output-xblnr.
      APPEND lv_wa_accchg TO lv_it_accchg.
      gv_wa_output-xblnr  = lv_xblnr.


      CALL FUNCTION 'FI_DOCUMENT_CHANGE'
        EXPORTING
          i_obzei              = 1
          i_bukrs              = gv_wa_output-bukrs
          i_belnr              = gv_wa_output-belnr
          i_gjahr              = gv_wa_output-gjahr
        TABLES
          t_accchg             = lv_it_accchg
        EXCEPTIONS
          no_reference         = 1
          no_document          = 2
          many_documents       = 3
          wrong_input          = 4
          overwrite_creditcard = 5
          OTHERS               = 6.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.


    lv_xblnr = lv_xblnr + 1.

    MODIFY gv_it_output FROM gv_wa_output.

  ENDLOOP.










ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

  " p_bukrs = '1000'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_input .



  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
      ID 'ACTVT' FIELD '03'
      ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE e002(zfi).
  ENDIF.

ENDFORM.
