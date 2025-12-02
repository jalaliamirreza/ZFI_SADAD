*&---------------------------------------------------------------------*
*& Include          ZPP_PLANORDER_CONFIRMATION_DAT
*&---------------------------------------------------------------------*
TABLES: zpp_barcode_mfbf,zpp_user_wc,mara ,
        equi .


data: OK_CODE TYPE sy-ucomm  .
 data:
       R_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       R_GRID TYPE REF TO CL_GUI_ALV_GRID.

DATA: gv_subrc(1),
      disp_msg TYPE c ,
      gv_charg       TYPE          mch1-charg,
      gv_executed(1),
      gv_it_sernr    TYPE TABLE OF bapi_rm_datserial,
      gv_it_ret      TYPE TABLE OF bapiret2,
      gv_it_prod     TYPE TABLE OF zpp_planorder_confirmation,
      p_aufnr        TYPE          afko-aufnr,
      gv_count       TYPE          afko-gamng,
      gv_gamng       TYPE          afko-gamng,
      gv_matkl(1),
      gv_exit(1) ,
      batch_seq TYPE ZBARCODES-BATCH_SEQNO .


DATA: oref_dock TYPE REF TO   cl_gui_docking_container,
      oref_alv  TYPE REF TO   cl_gui_alv_grid,
      i_exclude TYPE TABLE OF syucomm.

DATA: t_bdcdata  LIKE TABLE OF bdcdata,
      fs_bdcdata LIKE LINE OF  t_bdcdata.
"---rostami
DATA: gv_it_output TYPE TABLE OF zbrz_barcode_output_mb,
      gv_wa_output LIKE LINE OF  gv_it_output,
      gv_wa_output_n LIKE LINE OF  gv_it_output,
      adrc TYPE BAPIADDR3 ,
      it_ret TYPE TABLE OF BAPIRET2 .
*DATA : ZCHECK TYPE C .
