*&---------------------------------------------------------------------*
*& Report ZFI_PO_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_po_report.





TABLES : lfa1,ekko,rm06e,ekbe,ekpo.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_po_report,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_data   TYPE TABLE OF zfi_po_report,
      gv_wa_data   LIKE LINE OF  gv_it_data,
      gv_it_vendor TYPE TABLE OF zfi_po_report,
      gv_wa_vendor LIKE LINE OF  gv_it_vendor,
      gv_it_detail TYPE TABLE OF zfi_po_report_det,
      gv_wa_detail LIKE LINE OF  gv_it_detail,
      gv_it_disp   TYPE TABLE OF zfi_po_report_det,
      gv_wa_disp   LIKE LINE OF  gv_it_disp,
      ok_code      LIKE          sy-ucomm.

DATA: gv_grid_data TYPE REF TO   cl_gui_alv_grid,
      gv_cc_data   TYPE REF TO   cl_gui_custom_container,
      gv_grid_disp TYPE REF TO   cl_gui_alv_grid,
      gv_cc_disp   TYPE REF TO   cl_gui_custom_container.


CLASS: lcl_alv_class   DEFINITION DEFERRED.

DATA : c_alv_class          TYPE REF TO lcl_alv_class,
       c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.


CLASS lcl_alv_class DEFINITION.
  PUBLIC SECTION.

*Constructor
    METHODS: constructor
      IMPORTING
        io_alv_grid TYPE REF TO cl_gui_alv_grid,

*Event for toolbar
      handle_double_click1 FOR EVENT double_click  OF  cl_gui_alv_grid  IMPORTING e_row.
ENDCLASS.

CLASS lcl_alv_class IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.                    "constructor

  METHOD handle_double_click1.
    PERFORM double_click1 USING e_row.
  ENDMETHOD.                    "handle_double_click


ENDCLASS.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME.
SELECT-OPTIONS : s_bukrs    FOR  ekko-bukrs OBLIGATORY NO INTERVALS NO-EXTENSION,
                 s_lifnr    FOR  lfa1-lifnr  NO INTERVALS NO-EXTENSION,
                 s_evrtn    FOR  rm06e-evrtn NO INTERVALS NO-EXTENSION,
                 s_ebeln    FOR  ekko-ebeln,
                 s_budat    for  ekbe-budat,
                 s_matnr    FOR  ekpo-matnr.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM check_company_code_auth.
  PERFORM get_data.
  PERFORM get_vendor_data.
  PERFORM create_output.

  CALL SCREEN 100.



*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .



  REFRESH :gv_it_data,gv_it_detail.

  SELECT ekko~konnr, ekko~ebeln, ekpo~ebelp, ekko~lifnr, ekpo~matnr, makt~maktx, ekpo~menge,
         ekpo~meins, ekbe~gjahr, ekbe~belnr, ekbe~buzei, ekbe~budat, ekbe~bwart,
         ekbe_2~gjahr AS gjahr_inv, ekbe_2~belnr  AS belnr_inv, ekbe_2~buzei  AS buzei_inv,
         rbkp_2~rmwwr,rbkp_2~waers,ekpo~netpr AS ekko_netpr,
         ekpo~netwr AS ekko_netwr,
         ekko~waers AS ekko_waers,
         CASE WHEN ekbe~shkzg   = 'S' THEN ekbe~menge   ELSE ekbe~menge   * -1 END AS menge_gr,
         CASE WHEN ekbe~shkzg   = 'S' THEN ekbe~dmbtr   ELSE ekbe~dmbtr   * -1 END AS dmbtr_gr,
         CASE WHEN ekbe~shkzg   = 'S' THEN ekbe~wrbtr   ELSE ekbe~wrbtr   * -1 END AS wrbtr_gr,
         ekbe~waers   AS waers_gr, ekbe_p~belnr AS belnr_pa, ekbe_p~buzei AS buzei_pa, ekbe_p~budat AS budat_pa,
         CASE WHEN ekbe_p~shkzg = 'S' THEN ekbe_p~menge ELSE ekbe_p~menge * -1 END AS menge_pa,
         CASE WHEN ekbe_p~shkzg = 'S' THEN ekbe_p~dmbtr ELSE ekbe_p~dmbtr * -1 END AS dmbtr_pa,
         CASE WHEN ekbe_p~shkzg = 'S' THEN ekbe_p~wrbtr ELSE ekbe_p~wrbtr * -1 END AS wrbtr_pa,
         ekbe_p~waers AS waers_pa, ekbe_2~belnr AS belnr_po, ekbe_2~buzei AS buzei_po, ekbe_2~budat AS budat_po,
         CASE WHEN ekbe_2~shkzg = 'S' THEN ekbe_2~menge ELSE ekbe_2~menge * -1 END AS menge_po,
         CASE WHEN ekbe_2~shkzg = 'S' THEN ekbe_2~dmbtr ELSE ekbe_2~dmbtr * -1 END AS dmbtr_po,
         CASE WHEN ekbe_2~shkzg = 'S' THEN ekbe_2~wrbtr ELSE ekbe_2~wrbtr * -1 END AS wrbtr_po,
         CASE WHEN ekbe_2~vgabe = '2' THEN 'Invoice'    ELSE 'Subseq.'         END AS stype_inv,
         CASE WHEN but000~type  = '1' THEN concat( but000~name_first , but000~name_last ) ELSE but000~name_org1 END AS name1,
         ekbe_2~waers AS waers_po,
         'IRR' AS waers_irr
    FROM ekko
    JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    JOIN ekbe ON ekpo~ebeln = ekbe~ebeln AND
                 ekpo~ebelp = ekbe~ebelp
    JOIN but000 ON ekko~lifnr = but000~partner
    JOIN makt ON ekpo~matnr = makt~matnr
    LEFT OUTER JOIN ekbe AS ekbe_p ON ekbe_p~lfgja =  ekbe~gjahr AND
                                      ekbe_p~lfbnr =  ekbe~belnr AND
                                      ekbe_p~lfpos =  ekbe~buzei AND
                                      ekbe_p~vgabe =  'P'
    LEFT OUTER JOIN ekbe AS ekbe_2 ON ekbe_2~lfgja =  ekbe~gjahr AND
                                      ekbe_2~lfbnr =  ekbe~belnr AND
                                      ekbe_2~lfpos =  ekbe~buzei AND
                                    ( ekbe_2~vgabe =  '2' OR ekbe_2~vgabe =  '3' )
    LEFT OUTER JOIN rbkp AS rbkp_2 ON ekbe_2~gjahr =  rbkp_2~gjahr AND
                                      ekbe_2~belnr =  rbkp_2~belnr AND rbkp_2~stblg = ''
    WHERE ekko~loekz =  ''               AND
          ekpo~loekz =  ''               AND
          ekbe~vgabe =  '1'              AND
          makt~spras =  'E'              AND
          "ekko~konnr <> ''              AND
          ekko~bukrs IN @s_bukrs         AND
          ekko~konnr IN @s_evrtn         AND
          ekko~lifnr IN @s_lifnr         AND
          ekko~ebeln IN @s_ebeln         AND
          ekbe~budat IN @s_budat         AND
          ekpo~matnr IN @s_matnr         AND

    NOT EXISTS ( SELECT * FROM ekbe AS e1 WHERE e1~lfgja =  ekbe~gjahr   AND
                                                e1~lfbnr =  ekbe~belnr   AND
                                                e1~lfpos =  ekbe~buzei   AND
                                                e1~belnr <> ekbe~belnr   AND
                                                e1~vgabe =  '1'          AND
                                                e1~menge =  ekbe~menge ) AND
    NOT EXISTS ( SELECT * FROM ekbe AS e2 WHERE e2~gjahr =  ekbe~lfgja   AND
                                                e2~belnr =  ekbe~lfbnr   AND
                                                e2~buzei =  ekbe~lfpos   AND
                                                e2~belnr <> ekbe~belnr   AND
                                                e2~vgabe =  '1'          AND
                                                e2~menge =  ekbe~menge ) AND
    NOT EXISTS ( SELECT * FROM rbkp AS r1 WHERE r1~gjahr =  ekbe_2~gjahr AND
                                                r1~belnr =  ekbe_2~belnr AND
                                                r1~stblg <> '')
    INTO CORRESPONDING FIELDS OF TABLE @gv_it_data.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_output .


  DATA: lv_it_tmp TYPE TABLE OF zfi_po_report,
        lv_wa_tmp LIKE LINE OF  lv_it_tmp.

  REFRESH gv_it_output.

  gv_it_output[] = gv_it_data[].

  SORT gv_it_output BY konnr ebeln ebelp gjahr belnr buzei gjahr_inv belnr_inv buzei_inv.
  DELETE ADJACENT DUPLICATES FROM gv_it_output COMPARING konnr ebeln ebelp gjahr belnr buzei gjahr_inv belnr_inv buzei_inv.


  LOOP AT gv_it_output INTO gv_wa_output.


    gv_wa_output-menge_inv = gv_wa_output-menge_po.
    gv_wa_output-dmbtr_inv = gv_wa_output-dmbtr_po.
    gv_wa_output-wrbtr_inv = gv_wa_output-wrbtr_po.
    gv_wa_output-waers_inv = gv_wa_output-waers_po.

    IF gv_wa_output-belnr_inv = ''.
      CLEAR gv_wa_output-stype_inv.
    ENDIF.

    IF gv_wa_output-stype_inv = 'Invoice' AND gv_wa_output-menge_inv < 0.
      gv_wa_output-stype_inv = 'Credit Memo'.
    ENDIF.

    IF gv_wa_output-stype_inv = 'Subseq.' AND gv_wa_output-menge_inv < 0.
      gv_wa_output-stype_inv = 'Subseq. Credit'.
      CLEAR: gv_wa_output-menge_inv,gv_wa_output-menge_gr.
    ENDIF.

    IF gv_wa_output-stype_inv = 'Subseq.' AND gv_wa_output-menge_inv > 0.
      gv_wa_output-stype_inv = 'Subseq. Debit'.
      CLEAR: gv_wa_output-menge_inv,gv_wa_output-menge_gr.
    ENDIF.

    PERFORM get_invoice_value USING    gv_wa_output-gjahr_inv gv_wa_output-belnr_inv
                              CHANGING gv_wa_output-dmbtr_pt  gv_wa_output-wrbtr_pt  gv_wa_output-waers_pt.


    REFRESH lv_it_tmp.
    lv_it_tmp[] = gv_it_data[].

    DELETE lv_it_tmp WHERE konnr     <> gv_wa_output-konnr     OR
                           ebeln     <> gv_wa_output-ebeln     OR
                           ebelp     <> gv_wa_output-ebelp     OR
                           gjahr     <> gv_wa_output-gjahr     OR
                           belnr     <> gv_wa_output-belnr     OR
                           buzei     <> gv_wa_output-buzei     OR
                           gjahr_inv <> gv_wa_output-gjahr_inv OR
                           belnr_inv <> gv_wa_output-belnr_inv OR
                           buzei_inv <> gv_wa_output-buzei_inv.

    SORT lv_it_tmp BY belnr_pa buzei_pa.
    DELETE ADJACENT DUPLICATES FROM lv_it_tmp COMPARING belnr_pa buzei_pa.

    CLEAR: gv_wa_output-belnr_pa,
           gv_wa_output-buzei_pa,
           gv_wa_output-budat_pa,
           gv_wa_output-menge_pa,
           gv_wa_output-dmbtr_pa,
           gv_wa_output-wrbtr_pa,
           gv_wa_output-waers_pa.

    LOOP AT lv_it_tmp INTO lv_wa_tmp.
      gv_wa_output-menge_pa = gv_wa_output-menge_pa + lv_wa_tmp-menge_pa.
      gv_wa_output-dmbtr_pa = gv_wa_output-dmbtr_pa + lv_wa_tmp-dmbtr_pa.
      gv_wa_output-wrbtr_pa = gv_wa_output-wrbtr_pa + lv_wa_tmp-wrbtr_pa.
      gv_wa_output-waers_pa = lv_wa_tmp-waers_pa.
    ENDLOOP.

    REFRESH lv_it_tmp.
    lv_it_tmp[] = gv_it_data[].

    DELETE lv_it_tmp WHERE ebeln <> gv_wa_output-ebeln OR
                           ebelp <> gv_wa_output-ebelp OR
                           gjahr <> gv_wa_output-gjahr OR
                           belnr <> gv_wa_output-belnr OR
                           buzei <> gv_wa_output-buzei.

    SORT lv_it_tmp BY belnr_po buzei_po.
    DELETE ADJACENT DUPLICATES FROM lv_it_tmp COMPARING belnr_po buzei_po.

    CLEAR: gv_wa_output-belnr_po,
           gv_wa_output-buzei_po,
           gv_wa_output-budat_po,
           gv_wa_output-menge_po,
           gv_wa_output-dmbtr_po,
           gv_wa_output-wrbtr_po,
           gv_wa_output-waers_po.

    LOOP AT lv_it_tmp INTO lv_wa_tmp.
      gv_wa_output-menge_po = gv_wa_output-menge_po + lv_wa_tmp-menge_po.
      gv_wa_output-dmbtr_po = gv_wa_output-dmbtr_po + lv_wa_tmp-dmbtr_po.
      gv_wa_output-wrbtr_po = gv_wa_output-wrbtr_po + lv_wa_tmp-wrbtr_po.
      gv_wa_output-waers_po = lv_wa_tmp-waers_po.
    ENDLOOP.


*    REFRESH lv_it_tmp.
*    lv_it_tmp[] = gv_it_data[].
*
*    DELETE lv_it_tmp WHERE ebeln <> gv_wa_output-ebeln OR
*                           ebelp <> gv_wa_output-ebelp OR
*                           gjahr <> gv_wa_output-gjahr OR
*                           belnr <> gv_wa_output-belnr OR
*                           buzei <> gv_wa_output-buzei.
*
*    SORT lv_it_tmp BY belnr_su buzei_su.
*    DELETE ADJACENT DUPLICATES FROM lv_it_tmp COMPARING belnr_su buzei_su.
*
*    CLEAR: gv_wa_output-belnr_su,
*           gv_wa_output-buzei_su,
*           gv_wa_output-dmbtr_su,
*           gv_wa_output-wrbtr_su,
*           gv_wa_output-waers_su.
*
*    LOOP AT lv_it_tmp INTO lv_wa_tmp.
*      gv_wa_output-dmbtr_su = gv_wa_output-dmbtr_su + lv_wa_tmp-dmbtr_su.
*      gv_wa_output-wrbtr_su = gv_wa_output-wrbtr_su + lv_wa_tmp-wrbtr_su.
*      gv_wa_output-waers_su = lv_wa_tmp-waers_su.
*    ENDLOOP.

    gv_wa_output-menge_pa_po = gv_wa_output-menge_pa + gv_wa_output-menge_po.
    gv_wa_output-menge_diff  = gv_wa_output-menge_gr - gv_wa_output-menge_pa_po.

    CLEAR gv_wa_vendor.
    READ TABLE gv_it_vendor INTO gv_wa_vendor WITH KEY lifnr = gv_wa_output-lifnr.
    gv_wa_output-taxnum1      = gv_wa_vendor-taxnum1  .
    gv_wa_output-taxnum2      = gv_wa_vendor-taxnum2  .
    gv_wa_output-taxnum3      = gv_wa_vendor-taxnum3  .
    gv_wa_output-taxnum4      = gv_wa_vendor-taxnum4  .
    gv_wa_output-taxnum5      = gv_wa_vendor-taxnum5  .
    gv_wa_output-taxnum6      = gv_wa_vendor-taxnum6  .
    gv_wa_output-region       = gv_wa_vendor-region  .
    gv_wa_output-city1        = gv_wa_vendor-city1  .
    gv_wa_output-post_code1   = gv_wa_vendor-post_code1  .
    gv_wa_output-street       = gv_wa_vendor-street  .
    gv_wa_output-house_num1   = gv_wa_vendor-house_num1  .
    gv_wa_output-tel_number   = gv_wa_vendor-tel_number  .
    gv_wa_output-fax_number   = gv_wa_vendor-fax_number  .
    gv_wa_output-iban         = gv_wa_vendor-iban  .
    gv_wa_output-bankl        = gv_wa_vendor-bankl  .



    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INVOICE_VALUE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_GV_WA_OUTPUT_GJAHR_INV  text
*      -->P_GV_WA_OUTPUT_BELNR_INV  text
*      -->P_GV_WA_OUTPUT_BUZEI_INV  text
*      <--P_GV_WA_OUTPUT_DMBTR_PT  text
*      <--P_GV_WA_OUTPUT_WRBTR_PT  text
*      <--P_GV_WA_OUTPUT_WAERS_PT  text
*&---------------------------------------------------------------------*
FORM get_invoice_value  USING    lv_gjahr
                                 lv_belnr
                        CHANGING lv_dmbtr
                                 lv_wrbtr
                                 lv_waers.

  DATA: lv_it_detail TYPE TABLE OF zfi_po_report_det,
        lv_wa_detail LIKE LINE OF  lv_it_detail.


  CLEAR: lv_dmbtr,lv_wrbtr,lv_waers.
  CHECK lv_belnr IS NOT INITIAL.

  LOOP AT gv_it_detail INTO lv_wa_detail WHERE gjahr_inv = lv_gjahr AND belnr_inv = lv_belnr.
    lv_dmbtr = lv_dmbtr + lv_wa_detail-dmbtr.
    lv_wrbtr = lv_wrbtr + lv_wa_detail-wrbtr.
    lv_waers = lv_wa_detail-waers.
  ENDLOOP.

  CHECK sy-subrc NE 0.

  REFRESH lv_it_detail.
*  SELECT rbkp~gjahr AS gjahr_inv , rbkp~belnr AS belnr_inv , rbkp~bukrs , bkpf~belnr , bseg~buzei ,
*         CASE WHEN bseg~shkzg   = 'S' THEN bseg~dmbtr   ELSE bseg~dmbtr   * -1 END AS dmbtr,
*         'IRR' AS waers_irr ,
*         CASE WHEN bseg~shkzg   = 'S' THEN bseg~wrbtr   ELSE bseg~wrbtr   * -1 END AS wrbtr,
*         bkpf~waers , bseg~augbl
*  FROM       rbkp
*  CROSS JOIN bkpf
*  JOIN       bseg AS bseg ON bkpf~bukrs  = bseg~bukrs AND
*                             bkpf~gjahr  = bseg~gjahr AND
*                             bkpf~belnr  = bseg~belnr
*  WHERE  rbkp~gjahr  =  @lv_gjahr AND
*         rbkp~belnr  =  @lv_belnr AND
*         rbkp~stblg  =  ''        AND
*         rbkp~rbstat = '5'        AND
*         rbkp~xrech  = 'X'        AND
*         bkpf~blart  = 'RE'       AND
*         bseg~koart  = 'K'        AND
*         bkpf~stblg  = ''         AND
*         concat( rbkp~belnr , rbkp~gjahr ) = bkpf~awkey
* INTO CORRESPONDING FIELDS OF TABLE @lv_it_detail.

  SELECT rbkp~gjahr AS gjahr_inv , rbkp~belnr AS belnr_inv , rbkp~bukrs , bkpf~belnr , bseg~buzei ,
*         CASE WHEN bseg~shkzg   = 'S' THEN bseg~dmbtr   ELSE bseg~dmbtr   * -1 END AS dmbtr,
          bseg~shkzg,
          bseg~dmbtr,
         'IRR' AS waers_irr ,
*         CASE WHEN bseg~shkzg   = 'S' THEN bseg~wrbtr   ELSE bseg~wrbtr   * -1 END AS wrbtr,
          bseg~wrbtr ,
         bkpf~waers , bseg~augbl
  FROM       rbkp
  CROSS JOIN bkpf
  JOIN       bseg AS bseg ON bkpf~bukrs  = bseg~bukrs AND
                             bkpf~gjahr  = bseg~gjahr AND
                             bkpf~belnr  = bseg~belnr
  WHERE  rbkp~gjahr  =  @lv_gjahr AND
         rbkp~belnr  =  @lv_belnr AND
         rbkp~stblg  =  ''        AND
         rbkp~rbstat = '5'        AND
         rbkp~xrech  = 'X'        AND
         bkpf~blart  = 'RE'       AND
         bseg~koart  = 'K'        AND
         bkpf~stblg  = ''         AND
         concat( rbkp~belnr , rbkp~gjahr ) = bkpf~awkey
 INTO CORRESPONDING FIELDS OF TABLE @lv_it_detail.


  LOOP AT lv_it_detail INTO lv_wa_detail WHERE augbl IS INITIAL.


*    SELECT bkpf~gjahr AS gjahr_inv , @lv_wa_detail-belnr_inv AS belnr_inv , bkpf~bukrs , bseg~belnr , bseg~buzei ,
*       CASE WHEN bseg~shkzg   = 'S' THEN bseg~dmbtr   ELSE bseg~dmbtr   * -1 END AS dmbtr,
*       'IRR' AS waers_irr ,
*       CASE WHEN bseg~shkzg   = 'S' THEN bseg~wrbtr   ELSE bseg~wrbtr   * -1 END AS wrbtr,
*       bkpf~waers,
*       '1111'AS augbl
*       FROM bkpf
*       JOIN bseg AS bseg ON bkpf~bukrs  = bseg~bukrs AND
*                            bkpf~gjahr  = bseg~gjahr AND
*                            bkpf~belnr  = bseg~belnr
*       WHERE  bseg~bukrs = @lv_wa_detail-bukrs     AND
*              bseg~rebzj = @lv_wa_detail-gjahr_inv AND
*              bseg~rebzg = @lv_wa_detail-belnr     AND
*              bseg~rebzz = @lv_wa_detail-buzei     AND
*              bseg~umskz = ''                      AND
*              bkpf~stblg = ''
*       APPENDING CORRESPONDING FIELDS OF TABLE @lv_it_detail.
SELECT bkpf~gjahr AS gjahr_inv , @lv_wa_detail-belnr_inv AS belnr_inv , bkpf~bukrs , bseg~belnr , bseg~buzei ,
*       CASE WHEN bseg~shkzg   = 'S' THEN bseg~dmbtr   ELSE bseg~dmbtr   * -1 END AS dmbtr,
       bseg~shkzg ,
       bseg~dmbtr ,
       'IRR' AS waers_irr ,
*       CASE WHEN bseg~shkzg   = 'S' THEN bseg~wrbtr   ELSE bseg~wrbtr   * -1 END AS wrbtr,
       bkpf~waers,
       '1111'AS augbl
       FROM bkpf
       JOIN bseg AS bseg ON bkpf~bukrs  = bseg~bukrs AND
                            bkpf~gjahr  = bseg~gjahr AND
                            bkpf~belnr  = bseg~belnr
       WHERE  bseg~bukrs = @lv_wa_detail-bukrs     AND
              bseg~rebzj = @lv_wa_detail-gjahr_inv AND
              bseg~rebzg = @lv_wa_detail-belnr     AND
              bseg~rebzz = @lv_wa_detail-buzei     AND
              bseg~umskz = ''                      AND
              bkpf~stblg = ''
       APPENDING CORRESPONDING FIELDS OF TABLE @lv_it_detail.

  ENDLOOP.
LOOP AT lv_it_detail INTO lv_wa_detail.
      if  lv_wa_detail-shkzg   = 'H' .
            lv_wa_detail-dmbtr = lv_wa_detail-dmbtr * -1 .
            lv_wa_detail-wrbtr = lv_wa_detail-wrbtr * -1 .
            MODIFY lv_it_detail FROM lv_wa_detail TRANSPORTING dmbtr wrbtr.
       endif.
 ENDLOOP.

  DELETE lv_it_detail WHERE augbl IS INITIAL.

  LOOP AT lv_it_detail INTO lv_wa_detail.
    lv_dmbtr = lv_dmbtr + lv_wa_detail-dmbtr.
    lv_wrbtr = lv_wrbtr + lv_wa_detail-wrbtr.
    lv_waers = lv_wa_detail-waers.
  ENDLOOP.

  APPEND LINES OF lv_it_detail TO gv_it_detail.

ENDFORM.


FORM double_click1  USING lv_e_row.


  LOOP AT gv_it_output INTO gv_wa_output WHERE color = 'C610'.
    CLEAR gv_wa_output-color.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.

  REFRESH: gv_it_disp.
  READ TABLE gv_it_output INTO gv_wa_output INDEX lv_e_row.
  IF sy-subrc EQ 0.



    REFRESH gv_it_disp.
    gv_it_disp[] = gv_it_detail[].

    DELETE gv_it_disp WHERE gjahr_inv <> gv_wa_output-gjahr_inv OR
                            belnr_inv <> gv_wa_output-belnr_inv.

    SORT gv_it_disp BY gjahr_inv belnr buzei.

    gv_wa_output-color = 'C610'.
    MODIFY gv_it_output FROM gv_wa_output INDEX lv_e_row.
  ENDIF.


  PERFORM refresh_alv USING ''.

ENDFORM.

FORM refresh_alv USING lv_item TYPE char1.

  IF lv_item IS INITIAL.
    IF gv_cc_data IS NOT INITIAL.
      CALL METHOD gv_grid_data->check_changed_data.
      CALL METHOD gv_grid_data->refresh_table_display.
    ENDIF.
  ENDIF.

  IF gv_cc_disp IS NOT INITIAL.
    CALL METHOD gv_grid_disp->check_changed_data.
    CALL METHOD gv_grid_disp->refresh_table_display.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM set_status.
  PERFORM refresh_alv USING ''.
  PERFORM display_data.
  PERFORM display_detail.

ENDMODULE.



MODULE exit_commands INPUT.

  PERFORM exit_commands_form CHANGING ok_code .

ENDMODULE.                 " EXIT_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMANDS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_commands INPUT.

  PERFORM exit_commands_form CHANGING ok_code .

ENDMODULE.


FORM exit_commands_form CHANGING ok_code.


  PERFORM refresh_alv USING ''.

  CASE ok_code.
    WHEN 'ENDE' OR 'ECAN' OR 'EXIT'.
      PERFORM exit.
  ENDCASE.


  CLEAR ok_code.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exit .
  SET SCREEN 0.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_STATUS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_status .

  SET TITLEBAR  '01'.
  SET PF-STATUS 'STATUS'.

ENDFORM.

FORM display_data .

  DATA: lv_it_fieldcatalog TYPE lvc_t_fcat,
        lv_it_exclude      TYPE ui_functions,
        lv_layout          TYPE lvc_s_layo,
        lv_variant         TYPE disvariant.


  IF gv_cc_data IS INITIAL.


    CREATE OBJECT gv_cc_data
      EXPORTING
        container_name = 'CN_DATA'.

    CREATE OBJECT gv_grid_data
      EXPORTING
        i_parent = gv_cc_data.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_PO_REPORT'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.



    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog.



    CREATE OBJECT c_alv_class
      EXPORTING
        io_alv_grid = gv_grid_data.

    SET HANDLER c_alv_class->handle_double_click1 FOR gv_grid_data.

    lv_layout-zebra      = 'X'.
    lv_layout-cwidth_opt = 'X'.
    lv_layout-info_fname = 'COLOR'.
    "lv_layout-no_toolbar = 'X'.

    lv_variant-report = sy-repid.

    CALL METHOD gv_grid_data->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFI_PO_REPORT'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
        i_save               = 'X'
        is_variant           = lv_variant
      CHANGING
        it_outtab            = gv_it_output[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.


ENDFORM.

FORM display_detail .



  DATA: lv_it_fieldcatalog TYPE lvc_t_fcat,
        lv_it_exclude      TYPE ui_functions,
        lv_layout          TYPE lvc_s_layo.


  IF gv_cc_disp IS INITIAL.


    CREATE OBJECT gv_cc_disp
      EXPORTING
        container_name = 'CN_DISP'.

    CREATE OBJECT gv_grid_disp
      EXPORTING
        i_parent = gv_cc_disp.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_PO_REPORT_DET'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.

    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog.


    lv_layout-zebra      = 'X'.
    lv_layout-cwidth_opt = 'X'.
    lv_layout-no_toolbar = 'X'.

    CALL METHOD gv_grid_disp->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFI_PO_REPORT_DET'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
      CHANGING
        it_outtab            = gv_it_disp[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_FIELDCATALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_LV_IT_FIELDCATALOG  text
*      -->P_       text
*&---------------------------------------------------------------------*
FORM modify_fieldcatalog TABLES lv_it_fieldcatalog TYPE lvc_t_fcat.

  DATA : lv_wa_fieldcatalog LIKE LINE OF lv_it_fieldcatalog.



  LOOP AT lv_it_fieldcatalog INTO lv_wa_fieldcatalog.

    CASE lv_wa_fieldcatalog-fieldname+6(2).
      WHEN 'GR'.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
      WHEN 'PA'.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
      WHEN 'PO'.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
      WHEN 'IN'.
        CONCATENATE 'Invoice'    lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Invoice'    lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Invoice'    lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Invoice'    lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
      WHEN 'PT'.
        CONCATENATE 'Peyment'    lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Peyment'    lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Peyment'    lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Peyment'    lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
      WHEN 'SU'.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
      WHEN OTHERS.
    ENDCASE.


    CASE lv_wa_fieldcatalog-fieldname.
      WHEN 'BELNR_PA' OR 'BUZEI_PA' OR 'BUDAT_PA' OR
           'BELNR_PO' OR 'BUZEI_PO' OR 'BUDAT_PO'.
        lv_wa_fieldcatalog-no_out = 'X'.
      WHEN OTHERS.
    ENDCASE.


    IF lv_wa_fieldcatalog-fieldname = 'MENGE_PA_PO'.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.
    ENDIF.

    IF lv_wa_fieldcatalog-fieldname = 'MENGE_DIFF'.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-reptext INTO lv_wa_fieldcatalog-reptext SEPARATED BY space.

    ENDIF.
    IF lv_wa_fieldcatalog-fieldname = 'STYPE_INV'.
      lv_wa_fieldcatalog-scrtext_l    = 'Type'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.

    IF lv_wa_fieldcatalog-fieldname = 'TAXNUM1'.
      lv_wa_fieldcatalog-scrtext_l    = 'شماره اقتصادي'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.
    IF lv_wa_fieldcatalog-fieldname = 'TAXNUM2'.
      lv_wa_fieldcatalog-scrtext_l    = 'شماره ثبت شرکت'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.
    IF lv_wa_fieldcatalog-fieldname = 'TAXNUM3'.
      lv_wa_fieldcatalog-scrtext_l    = 'شماره شناسنامه'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.
    IF lv_wa_fieldcatalog-fieldname = 'TAXNUM4'.
      lv_wa_fieldcatalog-scrtext_l    = 'شماره شناسه ملی'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.
    IF lv_wa_fieldcatalog-fieldname = 'TAXNUM5'.
      lv_wa_fieldcatalog-scrtext_l    = 'کد ملی'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.
    IF lv_wa_fieldcatalog-fieldname = 'TAXNUM6'.
      lv_wa_fieldcatalog-scrtext_l    = 'شماره پاسپورت'.
      lv_wa_fieldcatalog-scrtext_m    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-scrtext_s    = lv_wa_fieldcatalog-scrtext_l.
      lv_wa_fieldcatalog-reptext      = lv_wa_fieldcatalog-scrtext_l.
    ENDIF.

    MODIFY lv_it_fieldcatalog FROM lv_wa_fieldcatalog.
  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_VENDOR_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_vendor_data .

  REFRESH gv_it_vendor.

  gv_it_vendor[] = gv_it_data[].
  SORT gv_it_vendor BY lifnr.
  DELETE ADJACENT DUPLICATES FROM gv_it_vendor COMPARING lifnr.

  CHECK gv_it_vendor[] IS NOT INITIAL.


  LOOP AT gv_it_vendor INTO gv_wa_vendor.

    SELECT SINGLE taxnum INTO gv_wa_vendor-taxnum1 FROM dfkkbptaxnum WHERE partner = gv_wa_vendor-lifnr AND taxtype = 'IR0'.
    SELECT SINGLE taxnum INTO gv_wa_vendor-taxnum2 FROM dfkkbptaxnum WHERE partner = gv_wa_vendor-lifnr AND taxtype = 'IR1'.
    SELECT SINGLE taxnum INTO gv_wa_vendor-taxnum3 FROM dfkkbptaxnum WHERE partner = gv_wa_vendor-lifnr AND taxtype = 'IR2'.
    SELECT SINGLE taxnum INTO gv_wa_vendor-taxnum4 FROM dfkkbptaxnum WHERE partner = gv_wa_vendor-lifnr AND taxtype = 'IR3'.
    SELECT SINGLE taxnum INTO gv_wa_vendor-taxnum5 FROM dfkkbptaxnum WHERE partner = gv_wa_vendor-lifnr AND taxtype = 'IR4'.
    SELECT SINGLE taxnum INTO gv_wa_vendor-taxnum6 FROM dfkkbptaxnum WHERE partner = gv_wa_vendor-lifnr AND taxtype = 'IR5'.

    SELECT * UP TO 1 ROWS INTO CORRESPONDING FIELDS OF gv_wa_vendor
      FROM adrc
      JOIN but020 ON adrc~addrnumber = but020~addrnumber
      WHERE adrc~langu = 'EN' AND
            but020~partner = gv_wa_vendor-lifnr
      ORDER BY but020~addr_valid_to DESCENDING.
    ENDSELECT.

    SELECT SINGLE tiban~iban tiban~bankl INTO CORRESPONDING FIELDS OF gv_wa_vendor
     FROM tiban
     JOIN lfbk ON tiban~bankn = lfbk~bankn
     WHERE lfbk~lifnr = gv_wa_vendor-lifnr.

    MODIFY gv_it_vendor FROM gv_wa_vendor.

  ENDLOOP.



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
