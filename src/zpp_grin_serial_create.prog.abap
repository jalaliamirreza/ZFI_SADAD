*&---------------------------------------------------------------------*
*& Report ZPP_GRIN_SERIAL_CREATE
*&---------------------------------------------------------------------*
*& create serial  for given range and matnr - rostami - called from zpp tcode
*&---------------------------------------------------------------------*
REPORT ZPP_GRIN_SERIAL_CREATE.


INCLUDE ZPP_PLANORDER_CONFIRMATION_DAT .

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME    .
  SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) TEXT-001.
PARAMETERS: p_werks TYPE marc-werks  OBLIGATORY MEMORY ID wrk.
SELECTION-SCREEN END OF LINE.


  SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) TEXT-007.
PARAMETERS: p_matnr TYPE mara-matnr  .
SELECTION-SCREEN COMMENT 75(40) t_bismt FOR FIELD p_matnr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (17) TEXT-003.
 SELECT-OPTIONS :s_sernr for equi-sernr no-EXTENSION.
SELECTION-SCREEN COMMENT 75(40) t_bismt1 FOR FIELD s_sernr.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN: END   OF BLOCK blk1.
SELECTION-SCREEN: BEGIN OF BLOCK blk2  WITH FRAME TITLE TEXT-002   .
PARAMETERS : ZREPORT AS CHECKBOX .
SELECTION-SCREEN: END   OF BLOCK blk2 .




START-OF-SELECTION.
if ZREPORT EQ 'X' .
  call TRANSACTION 'IQ02' .
ELSE .
 PERFORM gen_serial_number TABLES
       gv_it_sernr   CHANGING gv_subrc.

 PERFORM serial_create_result TABLES gv_it_sernr .
ENDIF .
INCLUDE zpp_planorder_confirmation_frm.
