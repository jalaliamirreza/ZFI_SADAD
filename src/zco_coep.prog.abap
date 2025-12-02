
*&---------------------------------------------------------------------*
*& Report ZCO_COEP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCO_COEP.

tables : COEP.

type-pools:slis.


DATA: gv_it_output  TYPE TABLE OF ZCO22,
      gv_wa_output  LIKE LINE OF  gv_it_output,
      fieldcatalog  TYPE          slis_t_fieldcat_alv WITH HEADER LINE.



SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .
SELECT-OPTIONS :  S_BUKRS FOR coep-BUKRS DEFAULT 1000 OBLIGATORY,
                  S_PERIO FOR coep-PERIO OBLIGATORY,
                  S_OBJNR FOR coep-OBJNR,
                  s_GJAHR FOR coep-GJAHR OBLIGATORY,
                  s_WRTTP FOR coep-WRTTP DEFAULT 4 OBLIGATORY ,
                  s_AUFNR FOR coep-AUFNR,
                  s_AUTYP FOR coep-AUTYP.
SELECTION-SCREEN: END   OF BLOCK blk1.

START-OF-SELECTION.

PERFORM get_data.
PERFORM build_fieldcatalog.
PERFORM calc_data.
PERFORM display_grid.

FORM build_fieldcatalog .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZCO22'
    CHANGING
      ct_fieldcat      = fieldcatalog[].

  LOOP AT fieldcatalog.

    CASE fieldcatalog-fieldname.


      WHEN 'BUKRS' .PERFORM set_catalog_text USING 'Company Code' CHANGING fieldcatalog.
      WHEN 'GJAHR' .PERFORM set_catalog_text USING 'Fiscal Year' CHANGING fieldcatalog.
      WHEN 'PERIO' .PERFORM set_catalog_text USING 'Period' CHANGING fieldcatalog.
      WHEN 'WOGBTR'.PERFORM set_catalog_text USING 'Value In Object Currency' CHANGING fieldcatalog.
      WHEN 'OBJNR' .PERFORM set_catalog_text USING 'Object Number' CHANGING fieldcatalog.
      WHEN 'AUFNR' .PERFORM set_catalog_text USING 'Order' CHANGING fieldcatalog.
      WHEN 'AUTYP' .PERFORM set_catalog_text USING 'Order Category' CHANGING fieldcatalog.



      WHEN OTHERS.
    ENDCASE.

    MODIFY fieldcatalog INDEX sy-tabix.
  ENDLOOP.

ENDFORM.

FORM display_grid .

  DATA: is_layout TYPE slis_layout_alv,
        i_save(1).

  is_layout-zebra = 'X'.
  is_layout-colwidth_optimize = 'X'.

  IF sy-uname = '10004939' or  sy-uname =  '10003411'.
    i_save = 'A'.
  ELSE.
    i_save = 'U'.
  ENDIF.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fieldcatalog[]
      i_save             = i_save
      i_default          = ''
      is_layout          = is_layout
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
    TABLES
      t_outtab           = gv_it_output
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.

FORM set_catalog_text  USING    lv_text            TYPE char100
                       CHANGING lv_wa_fieldcatalog TYPE slis_fieldcat_alv.

  lv_wa_fieldcatalog-seltext_l     = lv_text.
  lv_wa_fieldcatalog-seltext_m     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-seltext_s     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-reptext_ddic  = lv_wa_fieldcatalog-seltext_l.

ENDFORM.


FORM get_data.
REFRESH: gv_it_output.
 select
   coep~bukrs,
   coep~perio,
   coep~GJAHR,
   coep~OBJNR,
   coep~WRTTP,
   sum( coep~WOGBTR ) as WOGBTR,
   coep~AUFNR ,
   coep~AUTYP

 FROM coep
 where
  coep~PERIO  IN  @s_perio AND
  coep~objnr  IN  @s_objnr AND
  coep~gjahr  IN  @s_gjahr AND
  coep~bukrs  IN  @s_bukrs AND
  coep~wrttp  IN  @s_WRTTP AND
  coep~AUFNR  IN  @s_AUFNR AND
  coep~AUTYP  IN  @s_AUTYP
  GROUP BY  coep~OBJNR    ,  coep~perio ,  coep~GJAHR,    coep~WRTTP ,coep~bukrs ,  coep~AUFNR ,   coep~AUTYP

INTO CORRESPONDING FIELDS of table @gv_it_output.
ENDFORM.

 FORM add_catalog_text  USING    lv_text            TYPE char100
                       CHANGING lv_wa_fieldcatalog TYPE slis_fieldcat_alv.

  CONCATENATE lv_text lv_wa_fieldcatalog-seltext_l    INTO lv_wa_fieldcatalog-seltext_l    SEPARATED BY space.
  CONCATENATE lv_text lv_wa_fieldcatalog-seltext_m    INTO lv_wa_fieldcatalog-seltext_m    SEPARATED BY space.
  CONCATENATE lv_text lv_wa_fieldcatalog-seltext_s    INTO lv_wa_fieldcatalog-seltext_s    SEPARATED BY space.
  CONCATENATE lv_text lv_wa_fieldcatalog-reptext_ddic INTO lv_wa_fieldcatalog-reptext_ddic SEPARATED BY space.

ENDFORM.

FORM calc_data.

  LOOP AT gv_it_output INTO gv_wa_output.
    gv_wa_output-WOGBTR = gv_wa_output-WOGBTR * 100.
  MODIFY gv_it_output FROM gv_wa_output.
endloop.
endform.
