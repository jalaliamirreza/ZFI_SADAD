*&---------------------------------------------------------------------*
*& Include          ZDATA_STRUCTURE_FIELDS3
*&---------------------------------------------------------------------*

  CONSTANTS: gc_mobile_number type string value '09210171978',  "شماره خانم درويش فرد
             gc_password type string value '123456789#Mm'.


TABLES : mara,vbrk,vbap,vbak,vbkd.

TYPE-POOLS:slis.
DATA lt_mapping TYPE /ui2/cl_json=>name_mappings.
DATA: gv_it_output  TYPE TABLE OF zfi_sales_report,
      gv_wa_output  LIKE LINE OF  gv_it_output,
      gv_it_billing TYPE TABLE OF zfi_sales_report,
      gv_wa_billing LIKE LINE OF  gv_it_billing,
      fieldcatalog1 TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
      flg_token(1)  TYPE c,
      ls_out_put    TYPE zfi_sales_report.

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
                    s_fkdat    FOR  vbrk-fkdat,
                    s_werks    FOR  vbap-werks,
                    s_matnr    FOR  mara-matnr,
                    s_matkl    FOR  mara-matkl,
                    s_rfbsk    FOR  vbrk-rfbsk,
                    s_bstkd    FOR  vbkd-bstkd,
                    s_xblnr    for  vbrk-xblnr.  "شماره صورتحساب
SELECTION-SCREEN: END   OF BLOCK blk1.

DATA: gt_moadian1  TYPE TABLE OF zst_sadad, " صورتحساب
      ls_moadian1  TYPE zst_sadad,
      lv_json      TYPE string,
      flg_table(1) TYPE c.

*****    ----------------------------------------------  -------------------------------------------- structures Send Sadad API

DATA: zktokd          TYPE zktokd,
      zcmwae          TYPE zcmwae,
      ycmwae          TYPE zcmwae,
      zag_org1        TYPE zag_org1,
      zvrkme          TYPE zvrkme,
      zg_org2         TYPE zag_org1,
      zfkart(1)       TYPE c,
      ag_org2         TYPE char20,
      lv_timestamp_ms TYPE int8. " براي تبديل زمان و تاريخ
*      invoicetype(1)  TYPE c. " براي مشخص شدن نوع صورت حساب

types: BEGIN OF gtyp_result,
    internalserialnumber type c LENGTH 20,
    taxid type c LENGTH 22,
    invoiceType type ZST_SADAD-invoicetype,
    invoicePattern type zst_sadad-invoicePattern,
    invoiceSubject type zst_sadad-invoiceSubject,
    settlementMethod type zst_sadad-settlementMethod,
    typeOfBuyer type zst_sadad-typeOfBuyer,
    buyerEconomicNumber type zst_sadad-buyerEconomicNumber,
    buyerNationalIdentity type zst_sadad-BUYERNATIONALIDENTITY,
    buyerPostalCode type zst_sadad-buyerPostalCode,
    buyerBranchCode type zst_sadad-buyerBranchCode,
    buyerName type zst_sadad-buyerName,
    asn_num type zst_sadad-ASN_NUM,
    asn_dat type zst_sadad-ASN_DAT,

    error1 type char70,
    error2 type char70,
    error3 type char70,
    error4 type char70,
    error5 type char70,
  END OF gtyp_result.

  types: gtype_t_result type table of gtyp_result.


TYPES: BEGIN OF zmoad_shipping, "------------------------------------ shipping1
         stuffServiceId          TYPE matnr,
         stuffServiceDescription TYPE arktx,
         invoiceHeaderId         TYPE c,
       END OF zmoad_shipping,
       zmoad_t_shipping TYPE STANDARD TABLE OF zmoad_shipping WITH EMPTY KEY.
DATA: ls_shipping TYPE zmoad_shipping.

TYPES: BEGIN OF zmoad_bodies, "------------------------------------ Bodies1
         id                      TYPE n LENGTH 13,
         stuffServiceId          TYPE matnr,
         stuffServiceDescription TYPE arktx,
         quantity                TYPE char10,
         measurementUnit         TYPE char5,
         measurementUnitName(20) TYPE c,
         fee                     TYPE zznetwr,
         discount                TYPE netwr,
         netweight               type NTGEW_15,
         currencyType            type ZZCMWAE,
         sourceVat               type char02,
       END OF zmoad_bodies,
       zmoad_t_bodies TYPE STANDARD TABLE OF zmoad_bodies WITH EMPTY KEY.

TYPES: BEGIN OF zmoad_payments, "------------------------------------ Payments1
         paymentSwitchNumber       TYPE c,
         acceptanceNumber          TYPE c,
         terminal                  TYPE c,
         paymentType(1)            TYPE c,
         trackPaymentNumber        TYPE c,
         payerCardNumber           TYPE c,
         payerNationalIdentityCode TYPE c,
         paymentDate               TYPE dats,
         paymentValue              TYPE c,
       END OF zmoad_payments,
       zmoad_t_payments TYPE STANDARD TABLE OF zmoad_payments WITH EMPTY KEY.

TYPES: BEGIN OF zmoad_header, "------------------------------------ Header1
         invoiceType          TYPE i,
         invoicePattern       TYPE i,
         invoiceSubject       TYPE i,
         issuingDateTime      TYPE int8,
         settlementMethod     TYPE i,
         invoiceShippingGoods TYPE zmoad_t_shipping,
         typeOfBuyer          TYPE i,
         buyerEconomicNumber  TYPE char20,
         buyerNationalIdentityCode type char20,
         buyerPostalCode      type char20,
         buyerBranchCode      type char20,
         buyerId              TYPE char35,
         buyerName            type char60,
         cottageDeclarationCustomsNu   TYPE char30,
         cottageDeclarationCustomsDate TYPE char14,
       END OF zmoad_header,
       zmoad_t_header TYPE STANDARD TABLE OF zmoad_header WITH EMPTY KEY.

TYPES: BEGIN OF zmoad_main,
         clientid                 TYPE char12,
         invoiceheader            TYPE zmoad_header,
         internalserialnumber(20) TYPE c,
         invoicebodies            TYPE zmoad_t_bodies,
         invoicepayments          TYPE zmoad_t_payments,
       END OF zmoad_main.

*TYPES: BEGIN OF zmoad_bodies2, "------------------------------------ Bodies2
*         stuffServiceId                 TYPE matnr,
*         stuffServiceDescription        TYPE arktx,
*         quantity                       TYPE char10,
*         measurementUnit                TYPE char5,
*         netWeight                      TYPE c,
*         fee                            TYPE zznetwr,
*         currencyUnitAmount             TYPE c,
*         currencyType                   TYPE c,
*         exchangeRate                   TYPE c,
*         stuffServiceRialValue          TYPE c,
*         stuffServiceCurrencyValue      TYPE c,
*         discount                       TYPE netwr,
*         afterDiscount                  TYPE c,
*         otherDutiesTax                 TYPE c,
*         otherDutiesRate                TYPE c,
*         otherDutiesAmount              TYPE c,
*         otherLegalTax                  TYPE c,
*         otherLegalRate                 TYPE c,
*         otherLegalAmount               TYPE c,
*         constructionFee                TYPE c,
*         sellerProfit                   TYPE c,
*         operationStipend               TYPE c,
*         operationStipendRegisterNumber TYPE c,
*         cutie                          TYPE c,
*         currencyPrice                  TYPE c,
*         sourceVat                      TYPE c,
*       END OF zmoad_bodies2,
*       zmoad_t_bodies2 TYPE STANDARD TABLE OF zmoad_bodies2 WITH EMPTY KEY.
*
*TYPES: BEGIN OF zmoad_header2, "------------------------------------ Header2
*         issuingDateTime               TYPE int8,
*         invoiceType                   TYPE c,
*         invoicePattern                TYPE c,
*         typeOfBuyer                   TYPE c,
*         buyerName                     TYPE c,
*         buyerLandlineNumber           TYPE c,
*         buyerAddress                  TYPE c,
*         buyerNationalIdentityCode     TYPE c,
*         buyerEconomicNumber           TYPE c,
*         buyerPostalCode               TYPE c,
*         buyerBranchCode               TYPE c,
*         flightType                    TYPE c,
*         buyerPassportNumber           TYPE c,
*         serialCustomsLicenseNumber    TYPE c,
*         sellerCustomsCode             TYPE c,
*         cottageDeclarationCustomsNu   TYPE c,
*         cottageDeclarationCustomsDate TYPE c,
*         contactRegistrationNumber     TYPE c,
*         billId                        TYPE c,
*         settlementMethod              TYPE c,
*         cashPaymentValue              TYPE c,
*         tax17                         TYPE c,
*         agencyEconomicNumber          TYPE c,
*         totalNetWeight                TYPE c,
*         ladingNumber                  TYPE c,
*         ladingReferenceNumber         TYPE c,
*         originCountry                 TYPE c,
*         originCity                    TYPE c,
*         destinationCountry            TYPE c,
*         destinationCity               TYPE c,
*         transmitterNationalIdentity   TYPE c,
*         receiverNationalIdentityCode  TYPE c,
*         ladingType                    TYPE c,
*         carrierNumber                 TYPE c,
*         driverNationalIdentityCode    TYPE c,
*         announcementSalesNumber       TYPE c,
*         announcementSalesDate         TYPE c,
*         invoiceShippingGoods          TYPE zmoad_t_shipping,
*         buyerId                       TYPE char11,
*       END OF zmoad_header2.
*TYPES: BEGIN OF zmoad_main2, "*************** moadian 2
*         clientid                 TYPE char12,
*         isFake(1)                TYPE c,
*         isProformaInvoice(1)     TYPE c,
*         isUpdate(1)              TYPE c,
*         description(1)           TYPE c,
*         internalSerialNumber(20) TYPE c,
*         invoiceheader            TYPE zmoad_header2,
*         invoicebodies            TYPE zmoad_t_bodies2,
*         invoicepayments          TYPE zmoad_t_payments,
*       END OF zmoad_main2.

DATA:     ls_request TYPE zmoad_main. " براي نوع 1
*DATA:     ls_request2 TYPE zmoad_main2. " براي نوع 2
