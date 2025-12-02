*&---------------------------------------------------------------------*
*& Include          ZFI_SADAD_SEL
*&---------------------------------------------------------------------*

TABLES : mara,vbrk,vbap,vbak,vbkd.

INITIALIZATION.
perFORM initialization .


SELECTION-SCREEN: BEGIN     OF BLOCK blk1 WITH FRAME .

  SELECT-OPTIONS :  s_bukrs1   FOR  vbrk-bukrs OBLIGATORY,
                    s_bukrs    FOR  vbrk-bukrs NO-DISPLAY,
                    s_buchk    FOR  vbrk-buchk,
                    s_kunrg    FOR  vbrk-kunrg,
                    s_kdgrp    FOR  vbrk-kdgrp,
                    s_fkart    FOR  vbrk-fkart,
                    s_spart    FOR  vbrk-spart,
                    s_vkgrp    FOR  vbak-vkgrp,
                    s_vkorg    FOR  vbrk-vkorg,
                    s_mvgr5    FOR  vbap-mvgr5,
                    s_regio    FOR  vbrk-regio,
                    s_vbel_s   FOR  vbak-vbeln,
                    s_vbel_b   FOR  vbrk-vbeln,
                    s_fkdat    FOR  vbrk-fkdat OBLIGATORY,
                    s_werks    FOR  vbap-werks,
                    s_matnr    FOR  mara-matnr,
                    s_matkl    FOR  mara-matkl,
                    s_rfbsk    FOR  vbrk-rfbsk,
                    s_bstkd    FOR  vbkd-bstkd,
                    s_xblnr    for  vbrk-xblnr.  "شماره صورتحساب
SELECTION-SCREEN: END   OF BLOCK blk1.


PARAMETERS: p_time as CHECKBOX,
            p_ex AS CHECKBOX,
            p_test as CHECKBOX.

****************************
****************************
****************************


START-OF-SELECTION.
**********
  PERFORM check_company_code_auth.

  PERFORM get_data.
  PERFORM check_auth.
  PERFORM calc_data.
**********
  PERFORM create_api_data_and_display.  "کد اصلي اينجاست
