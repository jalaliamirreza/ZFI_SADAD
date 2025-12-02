*&---------------------------------------------------------------------*
*& Report ZFI_CHECK_VENDOR_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_check_vendor_report.


TABLES : ekko,ekbe,payr.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_check_vendor_report,
      gv_wa_output LIKE LINE OF  gv_it_output,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS : s_bukrs    FOR  ekko-bukrs NO-DISPLAY DEFAULT '1000',
                 s_lifnr    FOR  ekko-lifnr OBLIGATORY,
                 s_ebeln    FOR  ekko-ebeln,
                 s_invoc    FOR  ekbe-belnr,
                 s_chect    FOR  payr-chect.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM check_company_code_auth.
  PERFORM get_data.
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
      i_structure_name = 'ZFI_CHECK_VENDOR_REPORT'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.

      WHEN 'FI_GJAHR'.
        PERFORM set_catalog_text USING 'Invoice-FI Doc Year' CHANGING fieldcatalog.
      WHEN 'FI_BELNR'.
        PERFORM set_catalog_text USING 'Invoice-FI Doc' CHANGING fieldcatalog.
      WHEN 'FI_BUZEI'.
        PERFORM set_catalog_text USING 'Invoice-FI Item' CHANGING fieldcatalog.
      WHEN 'PY_GJAHR'.
        PERFORM set_catalog_text USING 'Payment Req. Year' CHANGING fieldcatalog.
      WHEN 'PY_BELNR'.
        PERFORM set_catalog_text USING 'Payment Req. No.' CHANGING fieldcatalog.
      WHEN 'PY_BUZEI'.
        PERFORM set_catalog_text USING 'Payment Req. Item' CHANGING fieldcatalog.
      WHEN 'PY_ZFDAT'.
        PERFORM set_catalog_text USING 'Payment Req. Due On' CHANGING fieldcatalog.
      WHEN 'PY_DMBTR'.
        PERFORM set_catalog_text USING 'Payment Req. Amount' CHANGING fieldcatalog.
      WHEN 'PY_AUGBL'.
        PERFORM set_catalog_text USING 'Payment Doc.' CHANGING fieldcatalog.
      WHEN 'PY_AUGDT'.
        PERFORM set_catalog_text USING 'Payment Doc. Date' CHANGING fieldcatalog.
      WHEN 'DP_BELNR'.
        PERFORM set_catalog_text USING 'Down Payment No.' CHANGING fieldcatalog.
      WHEN 'DP_GJAHR'.
        PERFORM set_catalog_text USING 'Down Payment Year' CHANGING fieldcatalog.
      WHEN 'CHECT'.
        PERFORM set_catalog_text USING 'Check No.' CHANGING fieldcatalog.
      WHEN 'AMOUNT'.
        PERFORM set_catalog_text USING 'Invoice Amount' CHANGING fieldcatalog.

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
FORM get_data.


  SELECT DISTINCT
    ekko~ebeln,
    ekko~lifnr,
    ekko~bukrs,
    ekbe~belnr   AS invoc,
    ekbe~gjahr,
    rbkp~rmwwr   AS amount,
    lfa1~name1,
    acdoca~gjahr AS fi_gjahr,
    acdoca~belnr AS fi_belnr,
    acdoca~docln AS fi_buzei,
    bseg~gjahr   AS py_gjahr,
    bseg~belnr   AS py_belnr,
    bseg~buzei   AS py_buzei,
    bseg~zfbdt   AS py_zfdat,
    bseg~dmbtr   AS py_dmbtr,
    'IRR'        AS py_waers,
    bseg~augbl   AS py_augbl,
    bseg~augdt   AS py_augdt,
    payr~hbkid,
    payr~hktid,
    payr~chect,
    payr~rwbtr,
    payr~waers   AS ch_waers,
    CASE WHEN bsis~valut <> '00000000' THEN bsis~valut ELSE bseg1~zfbdt END AS valut
  FROM ekko
  JOIN ekbe   ON ekko~ebeln       =  ekbe~ebeln    AND
                 ekbe~vgabe       IN (2,3)
  JOIN rbkp   ON ekbe~belnr       =  rbkp~belnr    AND
                 ekbe~gjahr       =  rbkp~gjahr
  JOIN acdoca ON ekbe~belnr       =  acdoca~awref  AND
                 ekbe~gjahr       =  acdoca~aworg  AND
                 ekko~bukrs       =  acdoca~rbukrs AND
                 acdoca~blart     =  'RE'          AND
                 acdoca~rldnr     =  '0L'          AND
                 acdoca~awref_rev =  ''            AND
                 acdoca~koart     =  'K'
  JOIN bseg   ON acdoca~gjahr     =  bseg~rebzj    AND
                 acdoca~belnr     =  bseg~rebzg    AND
                 substring( acdoca~docln,4,3 )     =  bseg~rebzz
  JOIN bkpf   ON bseg~bukrs       =  bkpf~bukrs    AND
                 bseg~belnr       =  bkpf~belnr    AND
                 bseg~gjahr       =  bkpf~gjahr    AND
                 bkpf~stblg       =  ''
  JOIN payr   ON bseg~auggj       =  payr~gjahr    AND
                 bseg~augbl       =  payr~vblnr    AND
                 payr~vblnr       <> ''
  LEFT JOIN lfa1 ON ekko~lifnr = lfa1~lifnr
  LEFT JOIN bsed ON bsed~bukrs = payr~zbukr AND
                    bsed~gjahr = payr~gjahr AND
                    bsed~belnr = payr~vblnr AND
                    bsed~boeno = payr~chect
  LEFT JOIN bseg AS bseg1 ON bseg1~bukrs = payr~zbukr AND
                             bseg1~gjahr = payr~gjahr AND
                             bseg1~belnr = payr~vblnr AND
                             bseg1~buzei = bsed~buzei AND
                             bseg1~umskz = 'W'
  LEFT JOIN bsis_view AS bsis ON bsis~bukrs = payr~zbukr AND
                                 bsis~gjahr = payr~gjahr AND
                                 bsis~belnr = payr~vblnr AND
                                 bsis~valut <> '00000000'
  WHERE ekko~ebeln IN @s_ebeln AND
        ekko~bukrs IN @s_bukrs AND
        ekko~lifnr IN @s_lifnr AND
        ekbe~belnr IN @s_invoc AND
        payr~chect IN @s_chect
  INTO CORRESPONDING FIELDS OF TABLE @gv_it_output.


  SELECT DISTINCT
    ekko~ebeln,
    ekko~lifnr,
    ekko~bukrs,
    ekbe~belnr   AS dp_belnr,
    ekbe~gjahr   AS dp_gjahr,
    lfa1~name1,
    payr~hbkid,
    payr~hktid,
    payr~chect,
    payr~rwbtr,
    payr~waers   AS ch_waers,
    CASE WHEN bsis~valut <> '00000000' THEN bsis~valut ELSE bseg~zfbdt END AS valut
  FROM ekko
  JOIN ekbe   ON ekko~ebeln       =  ekbe~ebeln    AND
                 ekbe~vgabe       IN (4)
  JOIN payr   ON payr~gjahr       =  ekbe~gjahr    AND
                 payr~vblnr       =  ekbe~belnr
  LEFT JOIN lfa1 ON ekko~lifnr = lfa1~lifnr
  LEFT JOIN bsed ON bsed~bukrs = payr~zbukr AND
                    bsed~gjahr = payr~gjahr AND
                    bsed~belnr = payr~vblnr AND
                    bsed~boeno = payr~chect
  LEFT JOIN bseg ON bseg~bukrs = payr~zbukr AND
                    bseg~gjahr = payr~gjahr AND
                    bseg~belnr = payr~vblnr AND
                    bseg~buzei = bsed~buzei AND
                    bseg~umskz = 'W'
  LEFT JOIN bsis_view AS bsis ON bsis~bukrs = payr~zbukr AND
                                 bsis~gjahr = payr~gjahr AND
                                 bsis~belnr = payr~vblnr AND
                                 bsis~valut <> '00000000'
  WHERE ekko~ebeln IN @s_ebeln AND
        ekko~bukrs IN @s_bukrs AND
        ekko~lifnr IN @s_lifnr AND
        ekbe~belnr IN @s_invoc AND
        payr~chect IN @s_chect
  APPENDING CORRESPONDING FIELDS OF TABLE @gv_it_output.






ENDFORM.


FORM check_company_code_auth.

  DATA: lv_t001 TYPE t001.
  RANGES: s_bukrs1 FOR bkpf-bukrs.

  REFRESH s_bukrs1.
  s_bukrs1[] = s_bukrs[].

  REFRESH s_bukrs.

  SELECT * INTO lv_t001 FROM t001 WHERE bukrs IN s_bukrs1.

    AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
        ID 'ACTVT' FIELD '03'
        ID 'BUKRS' FIELD lv_t001-bukrs.
    IF sy-subrc EQ 0.
      CLEAR s_bukrs.
      s_bukrs-low    = lv_t001-bukrs.
      s_bukrs-sign   = 'I'.
      s_bukrs-option = 'EQ'.
      APPEND s_bukrs.
    ENDIF.
  ENDSELECT.

  IF s_bukrs[] IS INITIAL.
    MESSAGE e002(zfi).
    EXIT.
  ENDIF.


ENDFORM.
