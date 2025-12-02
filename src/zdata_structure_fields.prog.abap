*&---------------------------------------------------------------------*
*& Include          ZDATA_STRUCTURE_FIELDS
*&---------------------------------------------------------------------*
tables : mara,vbrk,vbap,vbak,likp,vbkd.

type-pools:slis.


data: gv_it_output  type table of zfi_sales_report,
      gv_wa_output  like line of  gv_it_output,
      gv_it_billing type table of zfi_sales_report,
      gv_wa_billing like line of  gv_it_billing,
      fieldcatalog  type          slis_t_fieldcat_alv with header line,
      fieldcatalog1  type          slis_t_fieldcat_alv with header line,
      fieldcatalog2  type          slis_t_fieldcat_alv with header line,
      fieldcatalog3  type          slis_t_fieldcat_alv with header line,
      fieldcatalog4  type          slis_t_fieldcat_alv with header line,
      fieldcatalog5  type          slis_t_fieldcat_alv with header line,
      fieldcatalog6  type          slis_t_fieldcat_alv with header line.



selection-screen: begin     of block blk1 with frame .
PARAMETERS: p_moad TYPE ZMOADIAN.
select-options :  s_bukrs1   for  vbrk-bukrs obligatory,
                  s_bukrs    for  vbrk-bukrs no-display,
                  s_buchk    for  vbrk-buchk,
                  s_kunrg    for  vbrk-kunrg,
                  s_kdgrp    for  vbrk-kdgrp,
                  s_fkart    for  vbrk-fkart,
                  s_spart    for  vbrk-spart,
                  s_vkgrp    for  vbak-vkgrp,
                  s_vkorg    for  vbrk-vkorg,
                  s_mvgr5    for  vbap-mvgr5,
                  s_regio    for  vbrk-regio,
                  s_vbel_s   for  vbak-vbeln,
                  s_vbel_b   for  vbrk-vbeln,
                  s_fkdat    for  vbrk-fkdat,
                  s_werks    for  vbap-werks,
                  s_matnr    FOR  mara-matnr,
                  s_matkl    for  mara-matkl,
                  s_rfbsk    for  vbrk-rfbsk,
                  s_bstkd    FOR  vbkd-bstkd.
selection-screen: end   of block blk1.






DATA: GT_MOADIAN1 TYPE TABLE OF ZST_MOADIAN1, " مودیان هدر 1
      LS_MOADIAN1 TYPE ZST_MOADIAN1,
      GT_MOADIAN2 TYPE TABLE OF ZST_MOADIAN2, " مودیان فایل 2
      LS_MOADIAN2 TYPE ZST_MOADIAN2,
      GT_MOADIAN3 TYPE TABLE OF ZST_MOADIAN3, " مودیان فایل 3
      LS_MOADIAN3 TYPE ZST_MOADIAN3,
      GT_MOADIAN4 TYPE TABLE OF ZST_MOADIAN4, " مودیان فایل 4
      LS_MOADIAN4 TYPE ZST_MOADIAN4,
      GT_MOADIAN5 TYPE TABLE OF ZST_MOADIAN5, " مودیان خرده فروشی 5
      LS_MOADIAN5 TYPE ZST_MOADIAN5,
      GT_MOADIAN6 TYPE TABLE OF ZST_MOADIAN6, " مودیان صادرات 6
      LS_MOADIAN6 TYPE ZST_MOADIAN6.
