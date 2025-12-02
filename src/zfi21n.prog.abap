*&---------------------------------------------------------------------*
*& Report ZFI21N
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZFI21N.

TABLES : bkpf,lfa1,bsik .

TYPE-POOLS:slis.

DATA: gv_it_output TYPE TABLE OF zfi_vendor_balance_rep,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_subrc(1),
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE.


SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .

PARAMETERS: r_loc RADIOBUTTON GROUP rd1 USER-COMMAND comm DEFAULT 'X',
            r_doc RADIOBUTTON GROUP rd1.

PARAMETERS : p_bukrs TYPE t001-bukrs DEFAULT '1000'.
SELECT-OPTIONS : s_lifnr    FOR  lfa1-lifnr,
                 s_budat    FOR  bkpf-budat NO-EXTENSION,
                 s_waers    FOR  bkpf-waers,
                 s_umskz    FOR  bsik-umskz .
PARAMETERS : p_cust AS CHECKBOX,
             p_ded  AS CHECKBOX.
SELECTION-SCREEN: END   OF BLOCK blk1.



AT SELECTION-SCREEN OUTPUT.
  PERFORM selection_screen.


START-OF-SELECTION.



  PERFORM check_input_data CHANGING gv_subrc.

  IF gv_subrc IS INITIAL.
    PERFORM set_waers.
    PERFORM calc_data.
    PERFORM build_fieldcatalog.
    PERFORM display_grid.
  ENDIF.



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
      i_structure_name = 'ZFI_VENDOR_BALANCE_REP'
    CHANGING
      ct_fieldcat      = fieldcatalog[].


  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.
      WHEN 'BEGIN'.
        fieldcatalog-seltext_l     = 'Balance Carryforward'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN 'DEBIT'.
        fieldcatalog-seltext_l     = 'Debit Balance of Period'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN 'CREDIT'.
        fieldcatalog-seltext_l     = 'Credit Balance of Period'.
        fieldcatalog-seltext_m     = fieldcatalog-seltext_l.
        fieldcatalog-seltext_s     = fieldcatalog-seltext_l.
        fieldcatalog-reptext_ddic  = fieldcatalog-seltext_l.

      WHEN 'ACCUM'.
        fieldcatalog-seltext_l     = 'Accumulated Balance'.
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
      i_callback_program      = sy-repid
      it_fieldcat             = fieldcatalog[]
      i_callback_user_command = 'USER_COMMAND'
      i_save                  = 'X'
      is_layout               = is_layout
    TABLES
      t_outtab                = gv_it_output
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    " DISPLAY_GRID
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_data .


  DATA: BEGIN OF lv_it_data OCCURS 0,
          hkont TYPE  bsik-hkont,
          umskz TYPE  bsik-umskz,
        END OF lv_it_data.


  DATA: lv_it_lfa1 TYPE TABLE OF lfa1,
        lv_wa_lfa1 TYPE          lfa1,
        lv_wa_kna1 TYPE          kna1,
        lv_it_bsik TYPE TABLE OF bsik,
        lv_wa_bsik LIKE LINE OF  lv_it_bsik,
        lv_it_bsak TYPE TABLE OF bsak,
        lv_wa_bsak LIKE LINE OF  lv_it_bsak,
        lv_it_bsid TYPE TABLE OF bsid,
        lv_wa_bsid LIKE LINE OF  lv_it_bsid,
        lv_it_bsad TYPE TABLE OF bsad,
        lv_wa_bsad LIKE LINE OF  lv_it_bsad,
        lv_wa_data LIKE LINE OF  lv_it_data,
        lv_amount  TYPE          zamount.



  REFRESH: lv_it_bsik,lv_it_bsak,lv_it_data.

  REFRESH lv_it_lfa1.
  SELECT * INTO TABLE lv_it_lfa1 FROM lfa1 WHERE lifnr IN s_lifnr.



  LOOP AT lv_it_lfa1 INTO lv_wa_lfa1.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_bsik
      FROM bsik
      WHERE budat <= s_budat-high     AND
            "blart <> 'ZN'             AND
            lifnr =  lv_wa_lfa1-lifnr AND
            bukrs =  p_bukrs          AND
            waers IN s_waers.


    SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_bsak
      FROM bsak
      WHERE budat <= s_budat-high     AND
            "blart <> 'ZN'             AND
            lifnr =  lv_wa_lfa1-lifnr AND
            bukrs =  p_bukrs          AND
            waers IN s_waers.



    LOOP AT lv_it_bsik INTO lv_wa_bsik.
      CLEAR lv_wa_data.
      lv_wa_data-hkont = lv_wa_bsik-hkont.
      lv_wa_data-umskz = lv_wa_bsik-umskz.
      COLLECT lv_wa_data INTO lv_it_data.
    ENDLOOP.

    LOOP AT lv_it_bsak INTO lv_wa_bsak.
      CLEAR lv_wa_data.
      lv_wa_data-hkont = lv_wa_bsak-hkont.
      lv_wa_data-umskz = lv_wa_bsak-umskz.
      COLLECT lv_wa_data INTO lv_it_data.
    ENDLOOP.




    LOOP AT lv_it_data INTO lv_wa_data.

      CLEAR gv_wa_output.
      gv_wa_output-bukrs      = p_bukrs.
      gv_wa_output-lifnr      = lv_wa_lfa1-lifnr.
      gv_wa_output-lifnr_name = lv_wa_lfa1-name1.
      IF r_loc = 'X'.
        gv_wa_output-waers = 'IRR'.
      ENDIF.
      gv_wa_output-hkont = lv_wa_data-hkont.
      gv_wa_output-umskz = lv_wa_data-umskz.

      CLEAR : gv_wa_output-begin,
              gv_wa_output-debit,
              gv_wa_output-credit,
              gv_wa_output-accum.


      SELECT SINGLE txt50 INTO gv_wa_output-hkont_name FROM skat WHERE spras = 'EN'               AND
                                                                       ktopl = 'BARZ'             AND
                                                                       saknr = gv_wa_output-hkont.

      SELECT SINGLE ltext INTO gv_wa_output-umskz_name FROM t074t WHERE spras = 'EN'               AND
                                                                        koart = 'K'                AND
                                                                        shbkz = gv_wa_output-umskz.

      LOOP AT lv_it_bsik INTO lv_wa_bsik WHERE hkont = lv_wa_data-hkont AND umskz = lv_wa_data-umskz.


        CLEAR : gv_wa_output-begin,
                gv_wa_output-debit,
                gv_wa_output-credit,
                gv_wa_output-accum.

        IF r_loc = ''.
          gv_wa_output-waers = lv_wa_bsik-waers.
          lv_wa_bsik-dmbtr   = lv_wa_bsik-wrbtr.
        ENDIF.


        IF lv_wa_bsik-budat IN s_budat.

          CASE lv_wa_bsik-shkzg.
            WHEN 'H'.
              gv_wa_output-credit = lv_wa_bsik-dmbtr.
            WHEN 'S'.
              gv_wa_output-debit  = lv_wa_bsik-dmbtr.
          ENDCASE.

        ENDIF.


        IF lv_wa_bsik-budat < s_budat-low.

          CASE lv_wa_bsik-shkzg.
            WHEN 'H'.
              gv_wa_output-begin =  lv_wa_bsik-dmbtr * -1.
            WHEN 'S'.
              gv_wa_output-begin =  lv_wa_bsik-dmbtr .
          ENDCASE.

        ENDIF.

        gv_wa_output-accum = gv_wa_output-begin + gv_wa_output-debit - gv_wa_output-credit.
        COLLECT gv_wa_output INTO gv_it_output.
      ENDLOOP.


      LOOP AT lv_it_bsak INTO lv_wa_bsak WHERE hkont = lv_wa_data-hkont AND umskz = lv_wa_data-umskz.

        CLEAR : gv_wa_output-begin,
                gv_wa_output-debit,
                gv_wa_output-credit,
                gv_wa_output-accum.

        IF r_loc = ''.
          gv_wa_output-waers = lv_wa_bsak-waers.
          lv_wa_bsak-dmbtr   = lv_wa_bsak-wrbtr.
        ENDIF.

        IF lv_wa_bsak-budat IN s_budat.

          CASE lv_wa_bsak-shkzg.
            WHEN 'H'.
              gv_wa_output-credit =  lv_wa_bsak-dmbtr.
            WHEN 'S'.
              gv_wa_output-debit  =  lv_wa_bsak-dmbtr.
          ENDCASE.

        ENDIF.


        IF lv_wa_bsak-budat < s_budat-low.

          CASE lv_wa_bsak-shkzg.
            WHEN 'H'.
              gv_wa_output-begin = lv_wa_bsak-dmbtr * -1.
            WHEN 'S'.
              gv_wa_output-begin = lv_wa_bsak-dmbtr.
          ENDCASE.

        ENDIF.

        gv_wa_output-accum = gv_wa_output-begin + gv_wa_output-debit - gv_wa_output-credit.
        COLLECT gv_wa_output INTO gv_it_output.
      ENDLOOP.


    ENDLOOP.



    IF p_cust = 'X' AND lv_wa_lfa1-kunnr IS NOT INITIAL.



      REFRESH: lv_it_bsid,lv_it_bsad,lv_it_data.

      CLEAR lv_wa_kna1.
      SELECT SINGLE * INTO lv_wa_kna1 FROM kna1 WHERE kunnr = lv_wa_kna1-kunnr.



      SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_bsid
        FROM bsid
        WHERE budat <= s_budat-high     AND
              "blart <> 'ZN'             AND
              kunnr =  lv_wa_kna1-kunnr AND
              bukrs =  p_bukrs          AND
              waers IN s_waers.


      SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_bsad
        FROM bsad
        WHERE budat <= s_budat-high     AND
              "blart <> 'ZN'             AND
              kunnr =  lv_wa_kna1-kunnr AND
              bukrs =  p_bukrs          AND
              waers IN s_waers.



      LOOP AT lv_it_bsid INTO lv_wa_bsid.
        CLEAR lv_wa_data.
        lv_wa_data-hkont = lv_wa_bsid-hkont.
        lv_wa_data-umskz = lv_wa_bsid-umskz.
        COLLECT lv_wa_data INTO lv_it_data.
      ENDLOOP.

      LOOP AT lv_it_bsad INTO lv_wa_bsad.
        CLEAR lv_wa_data.
        lv_wa_data-hkont = lv_wa_bsad-hkont.
        lv_wa_data-umskz = lv_wa_bsad-umskz.
        COLLECT lv_wa_data INTO lv_it_data.
      ENDLOOP.




      LOOP AT lv_it_data INTO lv_wa_data.

        CLEAR gv_wa_output.
        gv_wa_output-bukrs      = p_bukrs.
        gv_wa_output-kunnr      = lv_wa_kna1-kunnr.
        gv_wa_output-kunnr_name = lv_wa_kna1-name1.
        IF r_loc = 'X'.
          gv_wa_output-waers = 'IRR'.
        ENDIF.
        gv_wa_output-hkont = lv_wa_data-hkont.
        gv_wa_output-umskz = lv_wa_data-umskz.

        CLEAR : gv_wa_output-begin,
                gv_wa_output-debit,
                gv_wa_output-credit,
                gv_wa_output-accum.

        SELECT SINGLE txt50 INTO gv_wa_output-hkont_name FROM skat WHERE spras = 'EN'               AND
                                                                         ktopl = 'BARZ'             AND
                                                                         saknr = gv_wa_output-hkont.

        SELECT SINGLE ltext INTO gv_wa_output-umskz_name FROM t074t WHERE spras = 'EN'               AND
                                                                          koart = 'K'                AND
                                                                          shbkz = gv_wa_output-umskz.

        LOOP AT lv_it_bsid INTO lv_wa_bsid WHERE hkont = lv_wa_data-hkont AND umskz = lv_wa_data-umskz.

          CLEAR : gv_wa_output-begin,
                  gv_wa_output-debit,
                  gv_wa_output-credit,
                  gv_wa_output-accum.

          IF r_loc = ''.
            gv_wa_output-waers = lv_wa_bsid-waers.
            lv_wa_bsid-dmbtr   = lv_wa_bsid-wrbtr.
          ENDIF.

          IF lv_wa_bsid-budat IN s_budat.

            CASE lv_wa_bsid-shkzg.
              WHEN 'H'.
                gv_wa_output-credit = lv_wa_bsid-dmbtr.
              WHEN 'S'.
                gv_wa_output-debit  = lv_wa_bsid-dmbtr.
            ENDCASE.

          ENDIF.


          IF lv_wa_bsid-budat < s_budat-low.

            CASE lv_wa_bsid-shkzg.
              WHEN 'H'.
                gv_wa_output-begin = lv_wa_bsid-dmbtr * -1.
              WHEN 'S'.
                gv_wa_output-begin = lv_wa_bsid-dmbtr.
            ENDCASE.

          ENDIF.

          gv_wa_output-accum = gv_wa_output-begin + gv_wa_output-debit - gv_wa_output-credit.
          COLLECT gv_wa_output INTO gv_it_output.
        ENDLOOP.


        LOOP AT lv_it_bsad INTO lv_wa_bsad WHERE hkont = lv_wa_data-hkont AND umskz = lv_wa_data-umskz.

          CLEAR : gv_wa_output-begin,
                  gv_wa_output-debit,
                  gv_wa_output-credit,
                  gv_wa_output-accum.

          IF r_loc = ''.
            gv_wa_output-waers = lv_wa_bsad-waers.
            lv_wa_bsad-dmbtr   = lv_wa_bsad-wrbtr.
          ENDIF.

          IF lv_wa_bsad-budat IN s_budat.

            CASE lv_wa_bsad-shkzg.
              WHEN 'H'.
                gv_wa_output-credit = lv_wa_bsad-dmbtr.
              WHEN 'S'.
                gv_wa_output-debit =  lv_wa_bsad-dmbtr.
            ENDCASE.

          ENDIF.


          IF lv_wa_bsad-budat < s_budat-low.

            CASE lv_wa_bsad-shkzg.
              WHEN 'H'.
                gv_wa_output-begin = lv_wa_bsad-dmbtr * -1.
              WHEN 'S'.
                gv_wa_output-begin = lv_wa_bsad-dmbtr.
            ENDCASE.

          ENDIF.

          gv_wa_output-accum = gv_wa_output-begin + gv_wa_output-debit - gv_wa_output-credit.
          COLLECT gv_wa_output INTO gv_it_output.
        ENDLOOP.

      ENDLOOP.




    ENDIF.

    IF p_ded = 'X'.
      PERFORM calc_gl TABLES lv_it_bsik lv_it_bsak USING lv_wa_lfa1.
    ENDIF.

  ENDLOOP.


  LOOP AT gv_it_output INTO gv_wa_output.
    gv_wa_output-datum_from = s_budat-low.
    gv_wa_output-datum_to   = s_budat-high.
    gv_wa_output-datum      = sy-datum.
    IF gv_wa_output-umskz   = 'F'.
      CLEAR lv_amount.
      lv_amount           = gv_wa_output-debit.
      gv_wa_output-debit  = gv_wa_output-credit.
      gv_wa_output-credit = lv_amount.
    ENDIF.
    MODIFY gv_it_output FROM gv_wa_output TRANSPORTING datum datum_from datum_to debit credit.
  ENDLOOP.

  IF s_umskz IS NOT INITIAL .
    LOOP AT gv_it_output INTO gv_wa_output WHERE umskz NOT IN s_umskz .
      DELETE gv_it_output INDEX sy-tabix .
    ENDLOOP.
  ENDIF .




ENDFORM.                    " calc_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_data CHANGING lv_subrc TYPE char1.

  DATA: lv_lifnr TYPE lfa1-lifnr.

  CLEAR lv_subrc.

  IF p_bukrs IS INITIAL.
    MESSAGE 'Fill Company Code.' TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  IF s_lifnr[] IS INITIAL.
    MESSAGE 'Fill Vendor.' TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  CLEAR lv_subrc.
  SELECT SINGLE lifnr INTO lv_lifnr FROM lfa1 WHERE lifnr IN s_lifnr.
  IF sy-subrc NE 0.
    MESSAGE 'Vendor not found.' TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

  CHECK lv_subrc IS INITIAL.

  IF s_budat-low IS INITIAL OR s_budat-high IS INITIAL.
    MESSAGE 'Fill From/To Dates.' TYPE 'S' DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.


  CHECK lv_subrc IS INITIAL.

  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
      ID 'ACTVT' FIELD '03'
      ID 'BUKRS' FIELD p_bukrs.
  IF sy-subrc NE 0.
    MESSAGE s002(zfi) DISPLAY LIKE 'E'.
    lv_subrc = 1.
  ENDIF.

ENDFORM.                    " CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALC_GL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_IT_BSIK  text
*      -->P_LV_IT_BSAK  text
*      -->P_LV_WA_LFA1  text
*----------------------------------------------------------------------*
FORM calc_gl  TABLES   lv_it_bsik STRUCTURE bsik
                       lv_it_bsak STRUCTURE bsak
              USING    lv_wa_lfa1 TYPE      lfa1.



  DATA: BEGIN OF lv_it_data OCCURS 0,
          hkont TYPE  bsid-hkont,
        END OF lv_it_data.



  DATA: lv_it_bsis TYPE TABLE OF bsis,
        lv_wa_bsis LIKE LINE OF  lv_it_bsis,
        lv_it_bsas TYPE TABLE OF bsas,
        lv_wa_bsas LIKE LINE OF  lv_it_bsas,
        lv_wa_data LIKE LINE OF  lv_it_data,
        lv_zuonr   TYPE          bsis-zuonr.



  lv_zuonr = lv_wa_lfa1-lifnr.
  SHIFT lv_zuonr LEFT DELETING LEADING '0'.

  REFRESH lv_it_bsis.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_bsis
    FROM  bseg
    JOIN  bsis ON bsis~bukrs = bseg~bukrs AND
                  bsis~belnr = bseg~belnr AND
                  bsis~gjahr = bseg~gjahr AND
                  bsis~buzei = bseg~buzei
    WHERE bsis~budat <= s_budat-high     AND
          bsis~zuonr =  lv_zuonr         AND
          bsis~bukrs =  p_bukrs          AND
          bsis~waers IN s_waers          AND
          bseg~ktosl IN ('PVD','PVT')    AND
          bsis~hkont IN ('0003160000','0003160001','0003130003','0003130004').



  SORT lv_it_bsis.
  DELETE ADJACENT DUPLICATES FROM lv_it_bsis COMPARING ALL FIELDS.

  REFRESH lv_it_data.
  LOOP AT lv_it_bsis INTO lv_wa_bsis.

    CLEAR lv_wa_data.
    lv_wa_data-hkont = lv_wa_bsis-hkont.
    COLLECT lv_wa_data INTO lv_it_data.

  ENDLOOP.


  REFRESH lv_it_bsas.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_bsas
    FROM  bseg
    JOIN  bsas ON bsas~bukrs = bseg~bukrs AND
                  bsas~belnr = bseg~belnr AND
                  bsas~gjahr = bseg~gjahr AND
                  bsas~buzei = bseg~buzei
    WHERE bsas~budat <= s_budat-high     AND
          bsas~zuonr =  lv_zuonr         AND
          bsas~bukrs =  p_bukrs          AND
          bsas~waers IN s_waers          AND
          bseg~ktosl IN ('PVD','PVT')    AND
          bsas~hkont IN ('0003160000','0003160001','0003130003','0003130004').



  SORT lv_it_bsas.
  DELETE ADJACENT DUPLICATES FROM lv_it_bsas COMPARING ALL FIELDS.

  LOOP AT lv_it_bsas INTO lv_wa_bsas.

    CLEAR lv_wa_data.
    lv_wa_data-hkont = lv_wa_bsas-hkont.
    COLLECT lv_wa_data INTO lv_it_data.

  ENDLOOP.

  LOOP AT lv_it_data INTO lv_wa_data.

    CLEAR gv_wa_output.
    gv_wa_output-bukrs      = p_bukrs.
    gv_wa_output-lifnr      = lv_wa_lfa1-lifnr.
    gv_wa_output-lifnr_name = lv_wa_lfa1-name1.
    IF r_loc = 'X'.
      gv_wa_output-waers = 'IRR'.
    ENDIF.
    gv_wa_output-hkont = lv_wa_data-hkont.
    gv_wa_output-umskz = ''.

    CLEAR : gv_wa_output-begin,
            gv_wa_output-debit,
            gv_wa_output-credit,
            gv_wa_output-accum.


    SELECT SINGLE txt50 INTO gv_wa_output-hkont_name FROM skat WHERE spras = 'EN'               AND
                                                                     ktopl = 'BARZ'             AND
                                                                     saknr = gv_wa_output-hkont.

    CLEAR gv_wa_output-umskz_name.

    LOOP AT lv_it_bsis INTO lv_wa_bsis WHERE hkont = lv_wa_data-hkont.

      CLEAR : gv_wa_output-begin,
              gv_wa_output-debit,
              gv_wa_output-credit,
              gv_wa_output-accum.

      IF r_loc = ''.
        gv_wa_output-waers = lv_wa_bsis-waers.
        lv_wa_bsis-dmbtr   = lv_wa_bsis-wrbtr.
      ENDIF.

      IF lv_wa_bsis-budat IN s_budat.

        CASE lv_wa_bsis-shkzg.
          WHEN 'H'.
            gv_wa_output-credit = lv_wa_bsis-dmbtr.
          WHEN 'S'.
            gv_wa_output-debit  = lv_wa_bsis-dmbtr.
        ENDCASE.

      ENDIF.


      IF lv_wa_bsis-budat < s_budat-low.

        CASE lv_wa_bsis-shkzg.
          WHEN 'H'.
            gv_wa_output-begin = lv_wa_bsis-dmbtr * -1.
          WHEN 'S'.
            gv_wa_output-begin = lv_wa_bsis-dmbtr.
        ENDCASE.

      ENDIF.


      gv_wa_output-accum = gv_wa_output-begin + gv_wa_output-debit - gv_wa_output-credit.
      COLLECT gv_wa_output INTO gv_it_output.
    ENDLOOP.

    LOOP AT lv_it_bsas INTO lv_wa_bsas WHERE hkont = lv_wa_data-hkont.

      CLEAR : gv_wa_output-begin,
              gv_wa_output-debit,
              gv_wa_output-credit,
              gv_wa_output-accum.

      IF r_loc = ''.
        gv_wa_output-waers = lv_wa_bsas-waers.
        lv_wa_bsas-dmbtr   = lv_wa_bsas-wrbtr.
      ENDIF.

      IF lv_wa_bsas-budat IN s_budat.

        CASE lv_wa_bsas-shkzg.
          WHEN 'H'.
            gv_wa_output-credit = lv_wa_bsas-dmbtr.
          WHEN 'S'.
            gv_wa_output-debit  = lv_wa_bsas-dmbtr.
        ENDCASE.

      ENDIF.


      IF lv_wa_bsas-budat < s_budat-low.

        CASE lv_wa_bsas-shkzg.
          WHEN 'H'.
            gv_wa_output-begin = lv_wa_bsas-dmbtr * -1.
          WHEN 'S'.
            gv_wa_output-begin = lv_wa_bsas-dmbtr.
        ENDCASE.

      ENDIF.


      gv_wa_output-accum = gv_wa_output-begin + gv_wa_output-debit - gv_wa_output-credit.
      COLLECT gv_wa_output INTO gv_it_output.
    ENDLOOP.

  ENDLOOP.



ENDFORM.                    " CALC_GL
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_screen .


  LOOP AT SCREEN.

    IF screen-name(7) = 'S_WAERS'.
      IF r_loc = 'X'.
        screen-input       = 0.
      ELSE.
        screen-input       = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " SELECTION_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_WAERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_waers .


  CHECK r_loc = 'X'.

  REFRESH s_waers.
  CLEAR s_waers.


ENDFORM.                    " SET_WAERS



FORM user_command  USING    r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield.

  IF r_ucomm = '&IC1' AND rs_selfield-fieldname = 'BEGIN'.
    PERFORM call_display_det USING rs_selfield-tabindex 'B'.
  ENDIF.

  IF r_ucomm = '&IC1' AND rs_selfield-fieldname = 'DEBIT'.
    PERFORM call_display_det USING rs_selfield-tabindex 'D'.
  ENDIF.

  IF r_ucomm = '&IC1' AND rs_selfield-fieldname = 'CREDIT'.
    PERFORM call_display_det USING rs_selfield-tabindex 'C'.
  ENDIF.



  rs_selfield-refresh = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_DISPLAY_DET
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_RS_SELFIELD_TABINDEX  text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM call_display_det  USING lv_index TYPE sy-tabix
                             lv_case  TYPE char1.

  DATA: lv_it_item TYPE TABLE OF bapi3008_2,
        lv_wa_item LIKE LINE OF  lv_it_item,
        lv_fieldca TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
        is_layout  TYPE          slis_layout_alv,
        lv_date_f  TYPE          datum,
        lv_date_t  TYPE          datum,
        lv_belnr   TYPE          bseg-belnr,
        lv_ind     TYPE          sy-tabix.

  IF lv_index IS NOT INITIAL.
    READ TABLE gv_it_output INTO gv_wa_output INDEX lv_index.

    IF lv_case = 'B'.
      lv_date_f = '20150101'.
      lv_date_t = gv_wa_output-datum_from - 1.
    ELSE.
      lv_date_f = gv_wa_output-datum_from.
      lv_date_t = gv_wa_output-datum_to.
    ENDIF.

    CALL FUNCTION 'BAPI_AP_ACC_GETSTATEMENT'
      EXPORTING
        companycode = gv_wa_output-bukrs
        vendor      = gv_wa_output-lifnr
        date_from   = lv_date_f
        date_to     = lv_date_t
        noteditems  = 'X'
      TABLES
        lineitems   = lv_it_item.

  ENDIF.

  DELETE lv_it_item WHERE sp_gl_ind <> gv_wa_output-umskz.

  IF gv_wa_output-umskz = 'F'.
    LOOP AT lv_it_item INTO lv_wa_item.
      IF lv_wa_item-db_cr_ind = 'S'.
        lv_wa_item-db_cr_ind = 'H'.
      ELSE.
        lv_wa_item-db_cr_ind = 'S'.
      ENDIF.
      MODIFY lv_it_item FROM lv_wa_item.
    ENDLOOP.
  ENDIF.

  IF lv_case = 'D'.
    DELETE lv_it_item WHERE db_cr_ind <> 'S'.
  ELSEIF lv_case = 'C'.
    DELETE lv_it_item WHERE db_cr_ind <> 'H'.
  ENDIF.


  LOOP AT lv_it_item INTO lv_wa_item.
    lv_ind = sy-tabix.
    SELECT SINGLE belnr INTO lv_belnr  FROM bseg WHERE bukrs = lv_wa_item-comp_code AND
                                                       gjahr = lv_wa_item-fisc_year AND
                                                       belnr = lv_wa_item-doc_no    AND
                                                       buzei = lv_wa_item-item_num  AND
                                                       hkont = gv_wa_output-hkont.
    IF sy-subrc NE 0.
      DELETE lv_it_item INDEX lv_ind.
    ENDIF.
  ENDLOOP.



  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'BAPI3008_2'
    CHANGING
      ct_fieldcat      = lv_fieldca[].



  LOOP AT lv_it_item INTO lv_wa_item WHERE db_cr_ind = 'H'.
    lv_wa_item-lc_amount  = lv_wa_item-lc_amount  * -1.
    lv_wa_item-amt_doccur = lv_wa_item-amt_doccur * -1.
    MODIFY lv_it_item FROM lv_wa_item.
  ENDLOOP.


  LOOP AT lv_fieldca.

    CASE lv_fieldca-fieldname.
      WHEN 'LC_AMOUNT'  OR
           'AMT_DOCCUR' OR
           'LC_TAX'     OR
           'TX_DOC_CUR'.
        lv_fieldca-decimals_out = 0.
        lv_fieldca-do_sum       = 'X'.
      WHEN OTHERS.
    ENDCASE.

    MODIFY lv_fieldca.
  ENDLOOP.





  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = lv_fieldca[]
      i_save                = 'X'
      is_layout             = is_layout
      i_screen_start_column = 1
      i_screen_start_line   = 1
      i_screen_end_column   = 150
      i_screen_end_line     = 20
    TABLES
      t_outtab              = lv_it_item
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.


ENDFORM.
