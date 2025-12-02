*&---------------------------------------------------------------------*
*& Report ZFI_GR_IR_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfi_gr_ir_report.




TABLES : ekko.

TYPE-POOLS:slis.


DATA: gv_it_output TYPE TABLE OF zfi_gr_ir_report,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_it_data   TYPE TABLE OF zfi_gr_ir_report,
      gv_wa_data   LIKE LINE OF  gv_it_data,
      gv_it_post   TYPE TABLE OF zfi_gr_ir_report,
      gv_wa_post   LIKE LINE OF  gv_it_post,
      gv_it_park   TYPE TABLE OF zfi_gr_ir_report,
      gv_wa_park   LIKE LINE OF  gv_it_park,
      ok_code      LIKE          sy-ucomm.

DATA: gv_grid_data TYPE REF TO   cl_gui_alv_grid,
      gv_cc_data   TYPE REF TO   cl_gui_custom_container,
      gv_grid_post TYPE REF TO   cl_gui_alv_grid,
      gv_cc_post   TYPE REF TO   cl_gui_custom_container,
      gv_grid_park TYPE REF TO   cl_gui_alv_grid,
      gv_cc_park   TYPE REF TO   cl_gui_custom_container.


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


SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS : s_lifnr    FOR  ekko-lifnr,
                 s_ebeln    FOR  ekko-ebeln,
                 s_budat    FOR  ekko-aedat.
SELECTION-SCREEN: END   OF BLOCK blk1.



START-OF-SELECTION.

  PERFORM get_data.
  PERFORM create_output.

  CALL SCREEN 100.







*&---------------------------------------------------------------------*
*& Module PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  PERFORM set_status.
  PERFORM refresh_alv USING ''.
  PERFORM display_data.
  PERFORM display_post.
  PERFORM display_park.

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
*&---------------------------------------------------------------------*
*& Form REFRESH_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_alv USING lv_item TYPE char1.

  IF lv_item IS INITIAL.
    IF gv_cc_data IS NOT INITIAL.
      CALL METHOD gv_grid_data->check_changed_data.
      CALL METHOD gv_grid_data->refresh_table_display.
    ENDIF.
  ENDIF.

  IF gv_cc_post IS NOT INITIAL.
    CALL METHOD gv_grid_post->check_changed_data.
    CALL METHOD gv_grid_post->refresh_table_display.
  ENDIF.

  IF gv_cc_park IS NOT INITIAL.
    CALL METHOD gv_grid_park->check_changed_data.
    CALL METHOD gv_grid_park->refresh_table_display.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
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
        i_structure_name = 'ZFI_GR_IR_REPORT'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.



    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog USING ''.



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
        i_structure_name     = 'ZFI_GR_IR_REPORT'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
        i_save               = 'X'
        is_variant           = lv_variant
      CHANGING
        it_outtab            = gv_it_output[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_POST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_post .

  DATA: lv_it_fieldcatalog TYPE lvc_t_fcat,
        lv_it_exclude      TYPE ui_functions,
        lv_layout          TYPE lvc_s_layo.


  IF gv_cc_post IS INITIAL.


    CREATE OBJECT gv_cc_post
      EXPORTING
        container_name = 'CN_POST'.

    CREATE OBJECT gv_grid_post
      EXPORTING
        i_parent = gv_cc_post.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_GR_IR_REPORT'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.

    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog USING 'PO'.

    lv_layout-zebra      = 'X'.
    lv_layout-cwidth_opt = 'X'.
    lv_layout-no_toolbar = 'X'.

    CALL METHOD gv_grid_post->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFI_GR_IR_REPORT'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
      CHANGING
        it_outtab            = gv_it_post[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_PARK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_park .



  DATA: lv_it_fieldcatalog TYPE lvc_t_fcat,
        lv_it_exclude      TYPE ui_functions,
        lv_layout          TYPE lvc_s_layo.


  IF gv_cc_park IS INITIAL.


    CREATE OBJECT gv_cc_park
      EXPORTING
        container_name = 'CN_PARK'.

    CREATE OBJECT gv_grid_park
      EXPORTING
        i_parent = gv_cc_park.


    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ZFI_GR_IR_REPORT'
      CHANGING
        ct_fieldcat      = lv_it_fieldcatalog.

    PERFORM modify_fieldcatalog TABLES lv_it_fieldcatalog USING 'PA'.


    lv_layout-zebra      = 'X'.
    lv_layout-cwidth_opt = 'X'.
    lv_layout-no_toolbar = 'X'.

    CALL METHOD gv_grid_park->set_table_for_first_display
      EXPORTING
        i_structure_name     = 'ZFI_GR_IR_REPORT'
        it_toolbar_excluding = lv_it_exclude
        is_layout            = lv_layout
      CHANGING
        it_outtab            = gv_it_park[]
        it_fieldcatalog      = lv_it_fieldcatalog[].


  ENDIF.


ENDFORM.



FORM modify_fieldcatalog TABLES lv_it_fieldcatalog TYPE lvc_t_fcat
                         USING  lv_type            TYPE char2.

  DATA : lv_wa_fieldcatalog LIKE LINE OF lv_it_fieldcatalog.


  LOOP AT lv_it_fieldcatalog INTO lv_wa_fieldcatalog.

    CASE lv_wa_fieldcatalog-fieldname+6(2).
      WHEN 'GR'.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'GR'         lv_wa_fieldcatalog-reptext      INTO lv_wa_fieldcatalog-reptext      SEPARATED BY space.
      WHEN 'PA'.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Parked'     lv_wa_fieldcatalog-reptext      INTO lv_wa_fieldcatalog-reptext      SEPARATED BY space.
      WHEN 'PO'.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Posted'     lv_wa_fieldcatalog-reptext      INTO lv_wa_fieldcatalog-reptext      SEPARATED BY space.
      WHEN 'SU'.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
        CONCATENATE 'Subsequent' lv_wa_fieldcatalog-reptext      INTO lv_wa_fieldcatalog-reptext      SEPARATED BY space.

      WHEN OTHERS.
    ENDCASE.

    IF lv_type IS INITIAL.
      CASE lv_wa_fieldcatalog-fieldname.
        WHEN 'BELNR_PA' OR 'BUZEI_PA' OR 'BUDAT_PA' OR
             'BELNR_PO' OR 'BUZEI_PO' OR 'BUDAT_PO' OR
             'BELNR_SU' OR 'BUZEI_SU'.
          lv_wa_fieldcatalog-no_out = 'X'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

    IF lv_wa_fieldcatalog-fieldname = 'MENGE_PA_PO'.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
      CONCATENATE 'SUM Park&Post'  lv_wa_fieldcatalog-reptext      INTO lv_wa_fieldcatalog-reptext      SEPARATED BY space.
    ENDIF.

    IF lv_wa_fieldcatalog-fieldname = 'MENGE_DIFF'.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-scrtext_l    INTO lv_wa_fieldcatalog-scrtext_l    SEPARATED BY space.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-scrtext_m    INTO lv_wa_fieldcatalog-scrtext_m    SEPARATED BY space.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-scrtext_s    INTO lv_wa_fieldcatalog-scrtext_s    SEPARATED BY space.
      CONCATENATE 'GR Diff'        lv_wa_fieldcatalog-reptext      INTO lv_wa_fieldcatalog-reptext      SEPARATED BY space.

    ENDIF.

    MODIFY lv_it_fieldcatalog FROM lv_wa_fieldcatalog.
  ENDLOOP.


  IF lv_type IS NOT INITIAL.
    LOOP AT lv_it_fieldcatalog INTO lv_wa_fieldcatalog WHERE fieldname+6(2) <> lv_type.
      lv_wa_fieldcatalog-no_out = 'X'.
      MODIFY lv_it_fieldcatalog FROM lv_wa_fieldcatalog.
    ENDLOOP.
  ENDIF.



ENDFORM.

FORM double_click1  USING lv_e_row.


  LOOP AT gv_it_output INTO gv_wa_output WHERE color = 'C610'.
    CLEAR gv_wa_output-color.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.

  REFRESH: gv_it_post,gv_it_park.
  READ TABLE gv_it_output INTO gv_wa_output INDEX lv_e_row.
  IF sy-subrc EQ 0.



    REFRESH gv_it_park.
    gv_it_park[] = gv_it_data[].

    DELETE gv_it_park WHERE ebeln <> gv_wa_output-ebeln OR
                            ebelp <> gv_wa_output-ebelp OR
                            gjahr <> gv_wa_output-gjahr OR
                            belnr <> gv_wa_output-belnr OR
                            buzei <> gv_wa_output-buzei.

    SORT gv_it_park BY belnr_pa buzei_pa.
    DELETE ADJACENT DUPLICATES FROM gv_it_park COMPARING belnr_pa buzei_pa.

    REFRESH gv_it_post.
    gv_it_post[] = gv_it_data[].

    DELETE gv_it_post WHERE ebeln <> gv_wa_output-ebeln OR
                            ebelp <> gv_wa_output-ebelp OR
                            gjahr <> gv_wa_output-gjahr OR
                            belnr <> gv_wa_output-belnr OR
                            buzei <> gv_wa_output-buzei.

    SORT gv_it_post BY belnr_po buzei_po.
    DELETE ADJACENT DUPLICATES FROM gv_it_post COMPARING belnr_po buzei_po.


    gv_wa_output-color = 'C610'.
    MODIFY gv_it_output FROM gv_wa_output INDEX lv_e_row.
  ENDIF.


  PERFORM refresh_alv USING ''.

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


  REFRESH gv_it_data.
  SELECT ekko~ebeln, ekpo~ebelp, ekko~lifnr, lfa1~name1, ekpo~matnr, makt~maktx, ekpo~menge, ekpo~meins, ekbe~gjahr, ekbe~belnr, ekbe~buzei, ekbe~budat, ekbe~bwart,
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
         ekbe_2~waers AS waers_po, ekbe_3~belnr AS belnr_su, ekbe_3~buzei AS buzei_su,
         CASE WHEN ekbe_3~shkzg = 'S' THEN ekbe_3~dmbtr ELSE ekbe_3~dmbtr * -1 END AS dmbtr_su,
         CASE WHEN ekbe_3~shkzg = 'S' THEN ekbe_3~wrbtr ELSE ekbe_3~wrbtr * -1 END AS wrbtr_su,
         ekbe_3~waers AS waers_su,
         'IRR' AS waers_irr
    INTO CORRESPONDING FIELDS OF TABLE @gv_it_data
    FROM ekko
    JOIN ekpo ON ekko~ebeln = ekpo~ebeln
    JOIN ekbe ON ekpo~ebeln = ekbe~ebeln AND
                 ekpo~ebelp = ekbe~ebelp
    JOIN lfa1 ON ekko~lifnr = lfa1~lifnr
    JOIN makt ON ekpo~matnr = makt~matnr
    LEFT OUTER JOIN ekbe AS ekbe_p ON ekbe_p~lfgja =  ekbe~gjahr AND
                                      ekbe_p~lfbnr =  ekbe~belnr AND
                                      ekbe_p~lfpos =  ekbe~buzei AND
                                      ekbe_p~vgabe =  'P'
    LEFT OUTER JOIN ekbe AS ekbe_2 ON ekbe_2~lfgja =  ekbe~gjahr AND
                                      ekbe_2~lfbnr =  ekbe~belnr AND
                                      ekbe_2~lfpos =  ekbe~buzei AND
                                      ekbe_2~vgabe =  '2'
    LEFT OUTER JOIN ekbe AS ekbe_3 ON ekbe_3~ebeln =  ekpo~ebeln AND
                                      ekbe_3~ebelp =  ekpo~ebelp AND
                                      ekbe_3~vgabe =  '3'
    WHERE ekko~loekz =  ''               AND
          ekpo~loekz =  ''               AND
          ekbe~vgabe =  '1'              AND
          makt~spras =  'E'              AND
          ekko~lifnr IN @s_lifnr         AND
          ekko~ebeln IN @s_ebeln         AND
          ekbe~budat IN @s_budat         AND
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
                                                e2~menge =  ekbe~menge ).





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

  DATA: lv_it_tmp TYPE TABLE OF zfi_gr_ir_report,
        lv_wa_tmp LIKE LINE OF  lv_it_tmp.

  REFRESH gv_it_output.

  gv_it_output[] = gv_it_data[].

  SORT gv_it_output BY ebeln ebelp gjahr belnr buzei.
  DELETE ADJACENT DUPLICATES FROM gv_it_output COMPARING ebeln ebelp gjahr belnr buzei.


  LOOP AT gv_it_output INTO gv_wa_output.

    REFRESH lv_it_tmp.
    lv_it_tmp[] = gv_it_data[].

    DELETE lv_it_tmp WHERE ebeln <> gv_wa_output-ebeln OR
                           ebelp <> gv_wa_output-ebelp OR
                           gjahr <> gv_wa_output-gjahr OR
                           belnr <> gv_wa_output-belnr OR
                           buzei <> gv_wa_output-buzei.

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


    REFRESH lv_it_tmp.
    lv_it_tmp[] = gv_it_data[].

    DELETE lv_it_tmp WHERE ebeln <> gv_wa_output-ebeln OR
                           ebelp <> gv_wa_output-ebelp OR
                           gjahr <> gv_wa_output-gjahr OR
                           belnr <> gv_wa_output-belnr OR
                           buzei <> gv_wa_output-buzei.

    SORT lv_it_tmp BY belnr_su buzei_su.
    DELETE ADJACENT DUPLICATES FROM lv_it_tmp COMPARING belnr_su buzei_su.

    CLEAR: gv_wa_output-belnr_su,
           gv_wa_output-buzei_su,
           gv_wa_output-dmbtr_su,
           gv_wa_output-wrbtr_su,
           gv_wa_output-waers_su.

    LOOP AT lv_it_tmp INTO lv_wa_tmp.
      gv_wa_output-dmbtr_su = gv_wa_output-dmbtr_su + lv_wa_tmp-dmbtr_su.
      gv_wa_output-wrbtr_su = gv_wa_output-wrbtr_su + lv_wa_tmp-wrbtr_su.
      gv_wa_output-waers_su = lv_wa_tmp-waers_su.
    ENDLOOP.

    gv_wa_output-menge_pa_po = gv_wa_output-menge_pa + gv_wa_output-menge_po.
    gv_wa_output-menge_diff  = gv_wa_output-menge_gr - gv_wa_output-menge_pa_po.
    MODIFY gv_it_output FROM gv_wa_output.
  ENDLOOP.


  PERFORM double_click1 USING 1.

ENDFORM.
