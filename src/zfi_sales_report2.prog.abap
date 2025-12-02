*&---------------------------------------------------------------------*
*& Report ZFI_SALES_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zfi_sales_report2.

INCLUDE ZDATA_STRUCTURE_FIELDS.
INCLUDE ZFI_SALES_REPORT2_API.

initialization.
  perform initialization.

start-of-selection.

  perform check_company_code_auth.
  perform get_data.
  perform check_auth.
  perform calc_data.
  perform build_fieldcatalog.
  perform display_grid.







*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_fieldcatalog .


  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZFI_SALES_REPORT'
    changing
      ct_fieldcat      = fieldcatalog[].


  loop at fieldcatalog.


    case fieldcatalog-fieldname(3).
      when 'BU_'.
        perform add_catalog_text using 'Co.' changing fieldcatalog.
      when 'AG_'.
        perform add_catalog_text using 'SO.' changing fieldcatalog.
      when 'WE_'.
        perform add_catalog_text using 'SH.' changing fieldcatalog.
      when 'Z7_'.
        perform add_catalog_text using 'Z7.' changing fieldcatalog.
    endcase.

    case fieldcatalog-fieldname.


      when 'BU_SORT2'.
        perform set_catalog_text using 'Co. National ID' changing fieldcatalog.

      when 'KUNRG'.
        perform set_catalog_text using 'Sold to Party' changing fieldcatalog.

      when 'WE_KUNRG'.
        perform set_catalog_text using 'Ship to Party' changing fieldcatalog.

      when 'IRANCODE'.
        perform set_catalog_text using 'ايران کد' changing fieldcatalog.

      when 'SHENASEH'.
        perform set_catalog_text using 'شناسه کالا' changing fieldcatalog.

      when 'EAN11'.
        perform set_catalog_text using 'بارکد' changing fieldcatalog.

      when 'EXTI2'.
        perform set_catalog_text using 'شماره بارنامه' changing fieldcatalog.

      when 'AG_IR0'.
        perform set_catalog_text using 'کد اقتصادي' changing fieldcatalog.

      when 'AG_IR1'.
        perform set_catalog_text using 'شماره ثبت' changing fieldcatalog.

      when 'AG_IR2'.
        perform set_catalog_text using 'شماره شناسنامه' changing fieldcatalog.

      when 'AG_IR3'.
        perform set_catalog_text using 'شناسه ملي' changing fieldcatalog.

      when 'AG_IR4'.
        perform set_catalog_text using 'کد ملي' changing fieldcatalog.

      when 'AG_IR5'.
        perform set_catalog_text using 'شماره پاسپورت' changing fieldcatalog.

      when 'AMOUNT1'.
        perform set_catalog_text using 'Amount' changing fieldcatalog.

      when 'AMOUNT2'.
        perform set_catalog_text using 'Condition Value' changing fieldcatalog.

      when 'AMOUNT3'.
        perform set_catalog_text using 'Cleared Downpayment' changing fieldcatalog.

      when 'AMOUNT4'.
        perform set_catalog_text using 'Tax' changing fieldcatalog.

      when 'AMOUNT5'.
        perform set_catalog_text using 'Discount' changing fieldcatalog.

      when 'AMOUNT6'.
        perform set_catalog_text using 'Delivery Cost' changing fieldcatalog.


      when 'AMOUNT7'.
        perform set_catalog_text using 'ساير' changing fieldcatalog.

         when 'AMOUNT_BURS'.
        perform set_catalog_text using 'مازاد بورس' changing fieldcatalog.

      when 'AMOUNT8'.
        perform set_catalog_text using 'Total Delivery Cost' changing fieldcatalog.

      when 'AMOUNT9'.
        perform set_catalog_text using 'Total Packing Cost' changing fieldcatalog.

      when 'AMOUNT10'.
        perform set_catalog_text using 'Total Net Amount' changing fieldcatalog.

      when 'AMOUNT11'.
        perform set_catalog_text using 'Total Cleared Downpayment' changing fieldcatalog.

      when 'AMOUNT12'.
        perform set_catalog_text using 'Payable Amount' changing fieldcatalog.

      when 'AMOUNT13'.
        perform set_catalog_text using 'Net Amount' changing fieldcatalog.

      when 'AMOUNT14'.
        perform set_catalog_text using 'Net Value VF05' changing fieldcatalog.

      when 'AMOUNT15'.
        perform set_catalog_text using 'Net' changing fieldcatalog.

      when 'AMOUNT16'.
        perform set_catalog_text using 'هزينه کارشناسي خسارت' changing fieldcatalog.

      when 'AMOUNT17'.
        perform set_catalog_text using 'هزينه خسارت فروش' changing fieldcatalog.

      when 'AMOUNT18'.
        perform set_catalog_text using 'Post Delivery' changing fieldcatalog.

      when 'AMOUNT19'.
        perform set_catalog_text using 'هزينه خدمات نصب' changing fieldcatalog.

      when 'AMOUNT20'.
        perform set_catalog_text using 'ساير خدمات' changing fieldcatalog.

      when 'FROM_DATE'.
        perform set_catalog_text using 'BillingDate From' changing fieldcatalog.

      when 'TO_DATE'.
        perform set_catalog_text using 'BillingDate to' changing fieldcatalog.

      when 'CURRENT_DATE'.
        perform set_catalog_text using 'Current Date' changing fieldcatalog.

      when 'VBELN_Q'.
        perform set_catalog_text using 'Quotation' changing fieldcatalog.

      when 'AUDAT_Q'.
        perform set_catalog_text using 'Quotation Date' changing fieldcatalog.

      when 'KURRF_DAT'.
        perform set_catalog_text using 'Reference Date' changing fieldcatalog.

      when 'AUDAT'.
        perform set_catalog_text using 'Sales Doc Date' changing fieldcatalog.

      when 'PRSDT'.
        perform set_catalog_text using 'Pricing Date' changing fieldcatalog.

      when 'DEGREE'.
        perform set_catalog_text using 'درجه' changing fieldcatalog.

      when 'KURRF'.
        perform set_catalog_text using 'EXHANGERATE' changing fieldcatalog.

      when 'BILLREF'.
        perform set_catalog_text using 'Refrence Billing' changing fieldcatalog.

      when 'IHREZ'.
        perform set_catalog_text using 'Your Refrence' changing fieldcatalog.

      when 'LGORT'.
        perform set_catalog_text using 'Storage Location' changing fieldcatalog.

      when 'BSTKD'.
        perform set_catalog_text using 'National Code' changing fieldcatalog.
      when others.
    endcase.



    modify fieldcatalog index sy-tabix.
  endloop.


endform.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_grid .

  data: is_layout type slis_layout_alv,
        ls_data LIKE LINE OF gv_it_output .
**ls_data-ag_city = '12'.
**ls_data-werks = '1'.



*APPEND ls_data to gv_it_output.

PERFORM create_API_DATA.

**  is_layout-zebra = 'X'.
**  is_layout-colwidth_optimize = 'X'.
**
**  call function 'REUSE_ALV_GRID_DISPLAY'
**    exporting
**      i_callback_program = sy-repid
**      it_fieldcat        = fieldcatalog[]
**      i_save             = 'X'
**      is_layout          = is_layout
**    tables
**      t_outtab           = gv_it_output
**    exceptions
**      program_error      = 1
**      others             = 2.

endform.                    " DISPLAY_GRID


*&---------------------------------------------------------------------*
*& Form SET_CATALOG_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      <--P_LV_WA_FIELDCATALOG  text
*&---------------------------------------------------------------------*
form set_catalog_text  using    lv_text            type char100
                       changing lv_wa_fieldcatalog type slis_fieldcat_alv.


  lv_wa_fieldcatalog-seltext_l     = lv_text.
  lv_wa_fieldcatalog-seltext_m     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-seltext_s     = lv_wa_fieldcatalog-seltext_l.
  lv_wa_fieldcatalog-reptext_ddic  = lv_wa_fieldcatalog-seltext_l.

endform.
*&---------------------------------------------------------------------*
*& Form CHECK_COMPANY_CODE_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form check_company_code_auth.

  data: lv_t001 type t001.

  refresh s_bukrs.

  select * into lv_t001 from t001 where bukrs in s_bukrs1.

    authority-check object 'F_PAYR_BUK'
        id 'ACTVT' field '03'
        id 'BUKRS' field lv_t001-bukrs.
    if sy-subrc eq 0.
      clear s_bukrs.
      s_bukrs-low    = lv_t001-bukrs.
      s_bukrs-sign   = 'I'.
      s_bukrs-option = 'EQ'.
      append s_bukrs.
    endif.
  endselect.

  if s_bukrs[] is initial.
    message e002(zfi).
    exit.
  endif.


endform.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form get_data .

  refresh: gv_it_output,gv_it_billing.


  select

    vbrk~bukrs,
    vbrk~vkorg,
    vbrk~vtweg,
    vbrk~spart,
    vbak~vbeln   as vbeln_s,
    vbap~posnr   as posnr_s,
    likp~vbeln   as vbeln_d,
    lips~posnr   as posnr_d,
    likp~vsart,
    likp~asn_num,
    likp~asn_dat,
    vbrk~vbeln   as vbeln_b,
    vbrk~xblnr   as billref,
    vbrp~posnr   as posnr_b,
    vbrk~sfakn   as sfakn1,
    vbfa~vbeln   as sfakn2,
    vbrp~werks,
    vbrk~kdgrp,
    vbrk~buchk,
    vbrk~fkart,
    tvfkt~vtext  as fkart_t,
    vbrk~vbtyp,
    vbrk~fkdat,
    vbrk~cmwae,
    vbrk~zterm,
    vbrk~regio,
    t005u~bezei  as regio_t,
    vbrk~kunrg,
    vbrp~matnr,
    mchar2~atwrt as shenaseh,
    vbrp~fkimg,
    vbrp~vrkme,
    vbrp~ntgew,
    vbrp~brgew,
    vbrp~gewei,
    vbrp~spart   as spart_b_i,
    vbrp~batch_ntgew,
    vbrp~batch_gewei,
    vbrp~netwr   as amount15,
    bkpf~xblnr,
    bkpf~belnr,
    lips~charg,
    mara~matkl,
    t023t~wgbez  as matkl_t,
    mara~groes,
    vbak~audat,
    vbak~erdat,
    vbak~kvgr1,
    vbak~kvgr2,
    vbak~kvgr3,
    vbak~kvgr4,
    vbak~kvgr5,
    tvv1t~bezei  as kvgr1_t,
    vbap~mvgr5,
    tvm5t~bezei  as mvgr5_t,
    adr_bu~butxt,
    adr_bu~stceg,
    adr_bu~country    as bu_country,
    adr_bu~country_t  as bu_country_t,
    adr_bu~region     as bu_region,
    adr_bu~region_t   as bu_region_t,
    adr_bu~city       as bu_city,
    adr_bu~post_code  as bu_post_code,
    adr_bu~street     as bu_street,
    adr_bu~house_num  as bu_house_num,
    adr_bu~tel        as bu_tel,
    adr_bu~fax        as bu_fax,
    adr_bu~sort2      as bu_sort2,
    adr_ag~org        as ag_org,
    adr_ag~name_last  as ag_last,
    adr_ag~name_first as ag_first,
    adr_ag~ktokd      as ag_ktokd,
    adr_ag~ktokd_t    as ag_ktokd_t,
    adr_ag~rpmkr      as ag_rpmkr,
    adr_ag~country    as ag_country,
    adr_ag~country_t  as ag_country_t,
    adr_ag~region     as ag_region,
    adr_ag~region_t   as ag_region_t,
    adr_ag~city       as ag_city,
    adr_ag~post_code  as ag_post_code,
    adr_ag~street     as ag_street,
    adr_ag~house_num  as ag_house_num,
    ad2_ag~tel_number as ag_tel,
    ad3_ag~fax_number as ag_fax,
    ir0~taxnum        as ag_ir0,
    ir1~taxnum        as ag_ir1,
    ir2~taxnum        as ag_ir2,
    ir3~taxnum        as ag_ir3,
    ir4~taxnum        as ag_ir4,
    ir5~taxnum        as ag_ir5,
*    vbkd~prsdt,
    case when vbrk~fkart = 'ZB01' then vbrk~kurrf_dat else vbrk~fkdat end as kurrf_dat,
    case when substring( vbrp~matnr,13,1 ) = '1' and ( vbrk~kdgrp =  'CM' or  vbrk~kdgrp =  'CA' or  vbrk~kdgrp =  'CT' ) then 'OE'
         when substring( vbrp~matnr,13,1 ) = '1' and ( vbrk~kdgrp <> 'CM' and vbrk~kdgrp <> 'CA' and vbrk~kdgrp <> 'CT' ) then 'درجه1'
         when substring( vbrp~matnr,13,1 ) = '2'                                                                          then 'درجه2' end as degree,

    case when ( adr_ag~ktokd = 'C006' and mara~mtart = 'ZFER' ) then concat( 'OE-',vbrp~arktx ) else vbrp~arktx end as arktx,

    prc1~waerk        as waers,
    case when vbrp~fkimg > 0 then division( prc1~kwert , vbrp~fkimg , 2 )  else 0 end as amount1,
    prc1~kwert        as amount2,
    prc3~kwert        as amount3,
    prc9~kwert        as amount_burs,
    sum( distinct prc2~kwert ) as amount4,
    sum( distinct prc4~kwert ) as amount5,
    sum( distinct prc5~kwert ) as amount6,
    sum( distinct prc6~kwert ) as amount7,
    sum( distinct prc7~kwert ) as amount16,
    sum( distinct prc8~kwert )  as amount17,
    sum( distinct prc10~kwert ) as amount18,
    sum( distinct prc11~kwert ) as amount19,
    sum( distinct prc12~kwert ) as amount20,
    vbrk~kurrf,
    vbkd~ihrez,
    lips~LGORT,
    vbkd~bstkd,
    @s_fkdat-low      as from_date,
    @s_fkdat-high     as to_date,
    @sy-datum         as current_date


  from vbrk
  join vbrp on           vbrk~vbeln   = vbrp~vbeln
  join vbap on           vbap~vbeln   = vbrp~aubel and
                         vbap~posnr   = vbrp~aupos

  join vbak           on vbap~vbeln   = vbak~vbeln
  left join vbkd      on vbkd~vbeln = vbrk~zuonr
  join mara           on vbrp~matnr   = mara~matnr
  join zvadrc as adr_bu on vbrk~bukrs   = adr_bu~bukrs
  left join lips      on lips~vbeln   = vbrp~vgbel and
                         lips~posnr   = vbrp~vgpos
  left join likp      on lips~vbeln   = likp~vbeln
  left join bkpf      on vbrk~vbeln   = bkpf~awkey
  left join tvfkt     on vbrk~fkart   = tvfkt~fkart  and
                         tvfkt~spras  = 'E'
  left join t005u     on vbrk~regio   = t005u~bland  and
                         vbrk~land1   = t005u~land1  and
                         t005u~spras  = 'E'
  left join t023t     on mara~matkl   = t023t~matkl  and
                         t023t~spras  = 'E'
  left join tvv1t     on vbak~kvgr1   = tvv1t~kvgr1  and
                         tvv1t~spras  = 'E'
  left join tvm5t     on vbap~mvgr5   = tvm5t~mvgr5  and
                         tvm5t~spras  = 'E'
  left join zvadrc_bp    as adr_ag on vbak~vbeln        = adr_ag~vbeln      and
                                      adr_ag~parvw      = 'AG'
  left join zvmchar      as mchar2 on mara~matnr        = mchar2~objek      and
                                      mchar2~atnam      = 'Z_SHENASEH'
  left join adr2         as ad2_ag on adr_ag~addrnumber = ad2_ag~addrnumber and
                                      ad2_ag~home_flag  = 'X'               and
                                      ad2_ag~flgdefault = 'X'
  left join adr3         as ad3_ag on adr_ag~addrnumber = ad3_ag~addrnumber and
                                      ad3_ag~home_flag  = 'X'               and
                                      ad3_ag~flgdefault = 'X'
  left join dfkkbptaxnum as ir0    on vbrk~kunrg        = ir0~partner       and
                                      ir0~taxtype       = 'IR0'
  left join dfkkbptaxnum as ir1    on vbrk~kunrg        = ir1~partner       and
                                      ir1~taxtype       = 'IR1'
  left join dfkkbptaxnum as ir2    on vbrk~kunrg        = ir2~partner       and
                                      ir2~taxtype       = 'IR2'
  left join dfkkbptaxnum as ir3    on vbrk~kunrg        = ir3~partner       and
                                      ir3~taxtype       = 'IR3'
  left join dfkkbptaxnum as ir4    on vbrk~kunrg        = ir4~partner       and
                                      ir4~taxtype       = 'IR4'
  left join dfkkbptaxnum as ir5    on vbrk~kunrg        = ir5~partner       and
                                      ir5~taxtype       = 'IR5'
  join      prcd_elements     as prc1   on prc1~knumv = vbrk~knumv and
                                           prc1~kposn = vbrp~posnr
  join      prcd_elements     as prc2   on prc2~knumv = vbrk~knumv and
                                           prc2~kposn = vbrp~posnr
  left join prcd_elements     as prc3   on prc3~knumv = vbrk~knumv and
                                           prc3~kposn = vbrp~posnr and
                                           prc3~kschl = 'AZWB'
  left join prcd_elements     as prc4   on prc4~knumv = vbrk~knumv and
                                           prc4~kposn = vbrp~posnr and
                                           prc4~kstat = ''         and
                                           prc4~koaid = 'A'        and
                                         ( prc4~kschl like 'ZD%'   or
                                           prc4~kschl =    'R100')
  left join prcd_elements     as prc5   on prc5~knumv = vbrk~knumv and
                                           prc5~kposn = vbrp~posnr and
                                           prc5~kstat = ''         and
                                           prc5~kinak = ''         and
                                           prc5~kschl = 'ZH00'
  left join prcd_elements     as prc6   on prc6~knumv = vbrk~knumv and
                                           prc6~kposn = vbrp~posnr and
                                           prc6~kstat = ''         and
                                           prc6~kinak = ''         and
                                           prc6~kschl like 'ZK%'
  left join prcd_elements     as prc7   on prc7~knumv = vbrk~knumv and
                                           prc7~kposn = vbrp~posnr and
                                           prc7~kinak = ''         and
                                           prc7~kschl = 'ZP04'
  left join prcd_elements     as prc8   on prc8~knumv = vbrk~knumv and
                                           prc8~kposn = vbrp~posnr and
                                           prc8~kinak = ''         and
                                           prc8~kstat = ''         and
                                           prc8~kschl = 'ZP03'
  left join prcd_elements     as prc9   on prc9~knumv = vbrk~knumv and
                                           prc9~kposn = vbrp~posnr and
                                           prc9~kstat = ''         and
                                           prc9~koaid = 'A'        and
                                           prc9~kschl = 'ZD05'

  left join prcd_elements     as prc10  on prc10~knumv = vbrk~knumv and
                                           prc10~kposn = vbrp~posnr and
                                           prc10~kstat = ''         and
                                           prc10~kinak = ''         and
                                           prc10~kschl = 'ZH01'
  left join prcd_elements     as prc11  on prc11~knumv = vbrk~knumv and
                                           prc11~kposn = vbrp~posnr and
                                           prc11~kstat = ''         and
                                           prc11~kinak = ''         and
                                           prc11~kschl = 'ZH02'
  left join prcd_elements     as prc12  on prc12~knumv = vbrk~knumv and
                                           prc12~kposn = vbrp~posnr and
                                           prc12~kstat = ''         and
                                           prc12~kinak = ''         and
                                           prc12~kschl = 'ZA01'
  left join vbfa              on vbfa~vbeln   = vbrk~vbeln and
                                 vbfa~posnv   = vbrp~posnr and
                             ( ( vbfa~vbtyp_n = 'N'        and
                                 vbfa~vbtyp_v = 'M' )      or
                               ( vbfa~vbtyp_n = 'S'        and
                                 vbfa~vbtyp_v = 'O' ) )

  where vbrk~fkdat     in @s_fkdat   and
        vbrk~kunrg     in @s_kunrg   and
        vbak~vbeln     in @s_vbel_s  and
        vbrk~vbeln     in @s_vbel_b  and
        vbrk~buchk     in @s_buchk   and
        vbrk~fkart     in @s_fkart   and
        vbrk~spart     in @s_spart   and
        vbrk~vkorg     in @s_vkorg   and
        vbrk~regio     in @s_regio   and
        vbak~vkgrp     in @s_vkgrp   and
        vbrk~kdgrp     in @s_kdgrp   and
        vbrp~werks     in @s_werks   and
        vbrp~matnr     in @s_matnr   and
        vbrk~bukrs     in @s_bukrs   and
        mara~matkl     in @s_matkl   and
        vbap~mvgr5     in @s_mvgr5   and
        vbrk~rfbsk     in @s_rfbsk   and
        vbkd~bstkd     in @s_bstkd   and
        prc1~kstat = ''  and   prc1~koaid = 'B' and prc1~kinak = '' and
        prc2~kstat = ''  and ( prc2~kschl = 'ZVAT' or prc2~kschl = 'ZVAD' )

  group by

    vbrk~bukrs,
    vbrk~vkorg,
    vbrk~vtweg,
    vbrk~spart,
    vbak~vbeln,
    vbap~posnr,
    likp~vbeln,
    lips~posnr,
    likp~vsart,
    likp~asn_num,
    likp~asn_dat,
    vbrk~vbeln,
    vbrk~xblnr,
    vbrp~posnr,
    vbrk~sfakn,
    vbfa~vbeln,
    vbrp~werks,
    vbrk~kdgrp,
    vbrk~buchk,
    vbrk~fkart,
    tvfkt~vtext,
    vbrk~vbtyp,
    vbrk~fkdat,
    vbrk~cmwae,
    vbrk~zterm,
    vbrk~regio,
    t005u~bezei,
    vbrk~kunrg,
    vbrp~matnr,
    mchar2~atwrt,
    vbrp~fkimg,
    vbrp~vrkme,
    vbrp~ntgew,
    vbrp~brgew,
    vbrp~gewei,
    vbrp~spart,
    vbrp~batch_ntgew,
    vbrp~batch_gewei,
    vbrp~netwr,
    bkpf~xblnr,
    bkpf~belnr,
    lips~charg,
    mara~matkl,
    t023t~wgbez,
    mara~groes,
    vbak~erdat,
    vbak~kvgr1,
    vbak~kvgr2,
    vbak~kvgr3,
    vbak~kvgr4,
    vbak~kvgr5,
    tvv1t~bezei,
    vbap~mvgr5,
    tvm5t~bezei,
    adr_bu~butxt,
    adr_bu~stceg,
    adr_bu~country,
    adr_bu~country_t,
    adr_bu~region,
    adr_bu~region_t,
    adr_bu~city,
    adr_bu~post_code,
    adr_bu~street,
    adr_bu~house_num,
    adr_bu~tel,
    adr_bu~fax,
    adr_bu~sort2,
    adr_ag~org,
    adr_ag~name_last,
    adr_ag~name_first,
    adr_ag~ktokd,
    adr_ag~ktokd_t,
    adr_ag~rpmkr,
    adr_ag~country,
    adr_ag~country_t,
    adr_ag~region,
    adr_ag~region_t,
    adr_ag~city,
    adr_ag~post_code,
    adr_ag~street,
    adr_ag~house_num,
    ad2_ag~tel_number,
    ad3_ag~fax_number,
    ir0~taxnum,
    ir1~taxnum,
    ir3~taxnum,
    ir2~taxnum,
    ir4~taxnum,
    ir5~taxnum,
    mara~mtart,
    vbrp~arktx,
    prc1~waerk,
    prc1~kbetr,
    prc1~kwert,
    prc3~kwert,
    vbrk~kurrf_dat, vbak~audat , prc9~kwert, vbrk~kurrf,
    vbkd~ihrez,
    lips~LGORT,
    vbkd~bstkd
*    , VBKD~PRSDT

  into corresponding fields of table @gv_it_output.

  check sy-subrc is initial.

  sort gv_it_output.
  delete adjacent duplicates from gv_it_output comparing all fields.

  select
    vbrk~vbeln as vbeln_b,
    prc7~kwert  as amount_burs,
    sum( distinct prc1~kwert ) as amount2,
    sum( distinct prc3~kwert ) as amount3,
    sum( distinct prc2~kwert ) as amount4,
    sum( distinct prc4~kwert ) as amount5,
    sum( distinct prc5~kwert ) as amount6,
    sum( distinct prc7~kwert ) as amount7,
    sum( distinct prc8~kwert ) as amount18,
    sum( distinct prc9~kwert ) as amount19,
    sum( distinct prc10~kwert ) as amount20


   " SUM( DISTINCT prc7~kwert ) AS AMOUNT_BURS
  from vbrk
  join vbrp on           vbrk~vbeln   = vbrp~vbeln
  join vbap on           vbap~vbeln   = vbrp~aubel and
                         vbap~posnr   = vbrp~aupos
  join vbak           on vbap~vbeln   = vbak~vbeln
  join mara           on vbrp~matnr   = mara~matnr
  join t001           on vbrk~bukrs   = t001~bukrs
  join      prcd_elements     as prc1   on prc1~knumv = vbrk~knumv and
                                           prc1~kposn = vbrp~posnr
  join      prcd_elements     as prc2   on prc2~knumv = vbrk~knumv and
                                           prc2~kposn = vbrp~posnr
  left join prcd_elements     as prc3   on prc3~knumv = vbrk~knumv and
                                           prc3~kposn = vbrp~posnr and
                                           prc3~kschl = 'AZWB'
  left join prcd_elements     as prc4   on prc4~knumv = vbrk~knumv and
                                           prc4~kposn = vbrp~posnr and
                                           prc4~kstat = ''         and
                                           prc4~koaid = 'A'        and
                                         ( prc4~kschl like 'ZD%'   or
                                           prc4~kschl =    'R100')

  left join prcd_elements     as prc5   on prc5~knumv = vbrk~knumv and
                                           prc5~kposn = vbrp~posnr and
                                           prc5~kstat = ''         and
                                           prc5~kinak = ''         and
                                           prc5~kschl = 'ZH00'
  left join prcd_elements     as prc6   on prc6~knumv = vbrk~knumv and
                                           prc6~kposn = vbrp~posnr and
                                           prc6~kstat = ''         and
                                           prc6~kinak = ''         and
                                           prc6~kschl like 'ZK%'
  left join prcd_elements     as prc7   on prc7~knumv = vbrk~knumv and
                                           prc7~kposn = vbrp~posnr and
                                           prc7~kstat = ''         and
                                           prc7~koaid = 'A'        and
                                           prc7~kschl = 'ZD05'

   left join prcd_elements     as prc8  on prc8~knumv = vbrk~knumv and
                                           prc8~kposn = vbrp~posnr and
                                           prc8~kstat = ''         and
                                           prc8~kinak = ''         and
                                           prc8~kschl = 'ZH01'
    left join prcd_elements    as prc9  on prc9~knumv = vbrk~knumv and
                                           prc9~kposn = vbrp~posnr and
                                           prc9~kstat = ''         and
                                           prc9~kinak = ''         and
                                           prc9~kschl = 'ZH02'
  left join prcd_elements     as prc10  on prc10~knumv = vbrk~knumv and
                                           prc10~kposn = vbrp~posnr and
                                           prc10~kstat = ''         and
                                           prc10~kinak = ''         and
                                           prc10~kschl = 'ZA01'

  where vbrk~fkdat     in @s_fkdat   and
        vbrk~kunrg     in @s_kunrg   and
        vbak~vbeln     in @s_vbel_s  and
        vbrk~vbeln     in @s_vbel_b  and
        vbrk~buchk     in @s_buchk   and
        vbrk~fkart     in @s_fkart   and
        vbrk~spart     in @s_spart   and
        vbrk~vkorg     in @s_vkorg   and
        vbrk~regio     in @s_regio   and
        vbak~vkgrp     in @s_vkgrp   and
        vbrk~kdgrp     in @s_kdgrp   and
        vbrp~werks     in @s_werks   and
        vbrk~bukrs     in @s_bukrs   and
        mara~matkl     in @s_matkl   and
        vbap~mvgr5     in @s_mvgr5   and
        prc1~kstat = ''  and   prc1~koaid = 'B' and prc1~kinak = '' and
        prc2~kstat = ''  and ( prc2~kschl = 'ZVAT' or prc2~kschl = 'ZVAD' )

  group by
    vbrk~vbeln , prc7~kwert
  into corresponding fields of table @gv_it_billing.



endform.
*&---------------------------------------------------------------------*
*& Form ADD_CATALOG_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      -->P_       text
*      <--P_FIELDCATALOG  text
*&---------------------------------------------------------------------*
form add_catalog_text  using    lv_text            type char100
                       changing lv_wa_fieldcatalog type slis_fieldcat_alv.

  concatenate lv_text lv_wa_fieldcatalog-seltext_l    into lv_wa_fieldcatalog-seltext_l    separated by space.
  concatenate lv_text lv_wa_fieldcatalog-seltext_m    into lv_wa_fieldcatalog-seltext_m    separated by space.
  concatenate lv_text lv_wa_fieldcatalog-seltext_s    into lv_wa_fieldcatalog-seltext_s    separated by space.
  concatenate lv_text lv_wa_fieldcatalog-reptext_ddic into lv_wa_fieldcatalog-reptext_ddic separated by space.

endform.
*&---------------------------------------------------------------------*
*& Form CALC_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form calc_data .


  loop at gv_it_output into gv_wa_output.

    clear gv_wa_billing.
    read table gv_it_billing into gv_wa_billing with key vbeln_b = gv_wa_output-vbeln_b.

if gv_wa_output-amount_burs > '0' .
gv_wa_output-amount7  = gv_wa_output-amount7.

elseif gv_wa_output-amount7 < '0'.
 gv_wa_output-amount7  =  gv_wa_output-amount7 + gv_wa_output-amount_burs.
endif.


if gv_wa_billing-amount7 > '0' .
gv_wa_billing-amount7  = gv_wa_billing-amount7.

elseif gv_wa_billing-amount7 < '0'.
 gv_wa_billing-amount7  = gv_wa_billing-amount7 + gv_wa_billing-amount_burs.
endif.


    gv_wa_output-amount5  = gv_wa_output-amount5 - gv_wa_output-amount_burs.
    gv_wa_billing-amount5  = gv_wa_billing-amount5 - gv_wa_billing-amount_burs.

    gv_wa_output-amount8  = gv_wa_billing-amount6.
    gv_wa_output-amount9  = gv_wa_billing-amount7.
    gv_wa_output-amount10 = gv_wa_billing-amount2 +
                            gv_wa_billing-amount5.
    gv_wa_output-amount11 = gv_wa_billing-amount3.
    gv_wa_output-amount12 = gv_wa_output-amount8  +
                            gv_wa_output-amount9  +
                            gv_wa_output-amount10 -
                            gv_wa_output-amount11 +
                            gv_wa_billing-amount4.
    gv_wa_output-amount13 = gv_wa_output-amount2  +
                            gv_wa_output-amount5.

    if gv_wa_output-fkart = 'ZT01' or
       gv_wa_output-fkart = 'ZT00' or
       gv_wa_output-fkart = 'ZB01' or
       gv_wa_output-fkart = 'ZB00' or
       gv_wa_output-fkart = 'YB00' or
       gv_wa_output-fkart = 'YT00' or
       gv_wa_output-fkart = 'YT01'.

      gv_wa_output-fkimg       = 0.
      gv_wa_output-batch_ntgew = 0.
      gv_wa_output-ntgew       = 0.
      gv_wa_output-brgew       = 0.

    endif.


    if ( gv_wa_output-vbtyp = 'O' and gv_wa_output-fkart <> 'ZT01' and gv_wa_output-fkart <> 'ZT00') or
       ( gv_wa_output-vbtyp = 'N' and gv_wa_output-fkart <> 'ZT01' and gv_wa_output-fkart <> 'ZT00').

      gv_wa_output-fkimg       = gv_wa_output-fkimg       * -1.
      gv_wa_output-batch_ntgew = gv_wa_output-batch_ntgew * -1.
      gv_wa_output-ntgew       = gv_wa_output-ntgew       * -1.
      gv_wa_output-brgew       = gv_wa_output-brgew       * -1.

      gv_wa_output-amount1     = gv_wa_output-amount1     * -1.
      gv_wa_output-amount2     = gv_wa_output-amount2     * -1.
      gv_wa_output-amount3     = gv_wa_output-amount3     * -1.
      gv_wa_output-amount4     = gv_wa_output-amount4     * -1.
      gv_wa_output-amount5     = gv_wa_output-amount5     * -1.
      gv_wa_output-amount6     = gv_wa_output-amount6     * -1.
      gv_wa_output-amount7     = gv_wa_output-amount7     * -1.
      gv_wa_output-amount8     = gv_wa_output-amount8     * -1.
      gv_wa_output-amount9     = gv_wa_output-amount9     * -1.
      gv_wa_output-amount10    = gv_wa_output-amount10    * -1.
      gv_wa_output-amount11    = gv_wa_output-amount11    * -1.
      gv_wa_output-amount12    = gv_wa_output-amount12    * -1.
      gv_wa_output-amount13    = gv_wa_output-amount13    * -1.
      gv_wa_output-amount17    = gv_wa_output-amount17    * -1.
      gv_wa_output-AMOUNT_BURS = gv_wa_output-AMOUNT_BURS * -1.
      gv_wa_output-amount18     = gv_wa_output-amount18   * -1.
      gv_wa_output-amount19     = gv_wa_output-amount19   * -1.
      gv_wa_output-AMOUNT20    =  gv_wa_output-AMOUNT20   * -1.

    elseif  gv_wa_output-fkart = 'ZT01' or gv_wa_output-fkart = 'ZT00'.

      gv_wa_output-amount1     = gv_wa_output-amount1     * -1.
      gv_wa_output-amount2     = gv_wa_output-amount2     * -1.
      gv_wa_output-amount3     = gv_wa_output-amount3     * -1.
      gv_wa_output-amount4     = gv_wa_output-amount4     * -1.
      gv_wa_output-amount5     = gv_wa_output-amount5     * -1.
      gv_wa_output-amount6     = gv_wa_output-amount6     * -1.
      gv_wa_output-amount7     = gv_wa_output-amount7     * -1.
      gv_wa_output-amount8     = gv_wa_output-amount8     * -1.
      gv_wa_output-amount9     = gv_wa_output-amount9     * -1.
      gv_wa_output-amount10    = gv_wa_output-amount10    * -1.
      gv_wa_output-amount11    = gv_wa_output-amount11    * -1.
      gv_wa_output-amount12    = gv_wa_output-amount12    * -1.
      gv_wa_output-amount13    = gv_wa_output-amount13    * -1.
      gv_wa_output-amount17    = gv_wa_output-amount17    * -1.
      gv_wa_output-AMOUNT_BURS = gv_wa_output-AMOUNT_BURS * -1.
      gv_wa_output-amount18     = gv_wa_output-amount18   * -1.
      gv_wa_output-amount19     = gv_wa_output-amount19   * -1.
      gv_wa_output-AMOUNT20    =  gv_wa_output-AMOUNT20   * -1.
    endif.

    gv_wa_output-amount14 = gv_wa_output-amount2  + gv_wa_output-amount5 + gv_wa_output-amount6.
    gv_wa_output-KURRF_RATE = gv_wa_output-kurrf * 1000 .
    modify gv_it_output from gv_wa_output.
  endloop.




endform.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form check_auth .

  data: lv_it_tmp type table of zfi_sales_report.

  refresh lv_it_tmp.
  lv_it_tmp[] = gv_it_output[].

  loop at gv_it_output into gv_wa_output group by gv_wa_output-spart.
    authority-check object  'V_VBAK_VKO'
      id 'ACTVT' field '03'
      id 'VKORG' field gv_wa_output-vkorg
      id 'SPART' field gv_wa_output-spart.
    if sy-subrc is not initial.
      message i008(zfi) with gv_wa_output-spart.
      delete lv_it_tmp where vkorg = gv_wa_output-vkorg and spart = gv_wa_output-spart.
    endif.
  endloop.

  loop at gv_it_output into gv_wa_output group by gv_wa_output-kdgrp.
    authority-check object  'Z_CUST_GRP'
       id 'ACTVT' field '03'
       id 'KDGRP' field gv_wa_output-kdgrp.
    if sy-subrc is not initial.
      message i009(zfi) with gv_wa_output-kdgrp.
      delete lv_it_tmp where kdgrp = gv_wa_output-kdgrp.
    endif.
  endloop.

  loop at gv_it_output into gv_wa_output group by gv_wa_output-fkart.
    authority-check object  'V_VBRK_FKA'
      id 'ACTVT' field '03'
      id 'FKART' field gv_wa_output-fkart.
    if sy-subrc is not initial.
      message i006(zfi) with gv_wa_output-fkart.
      delete lv_it_tmp where fkart = gv_wa_output-fkart.
    endif.
  endloop.

  refresh gv_it_output.
  gv_it_output[] = lv_it_tmp[].

endform.

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form initialization .

  clear s_rfbsk.

  s_rfbsk-sign   = 'I'.
  s_rfbsk-option = 'NE'.
  s_rfbsk-low    = 'E'.

  append s_rfbsk.

endform.
