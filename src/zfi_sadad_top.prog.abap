*&---------------------------------------------------------------------*
*& Include          ZFI_SADAD_TOP
*&---------------------------------------------------------------------*

CONSTANTS: gc_mobile_number type string value '09103071164',  "تلفن آقاي ميرزايي
           gc_password type string value 'JBMTI4Q0JDLUhTMj',
           gc_shenaseh_client type string value 'JAVZ8DT6PK3Q',   " شناسه کلاينت اصلي
           gc_destination_api type string value 'SADAD_API',
           gc_dest_token      type string value 'SADAD_GET',


           "for test
           gc_mobile_number_test type string value '09210171978',  "شماره خانم درويش فرد
           gc_password_test type string value '123456789#Mm',
           gc_shenaseh_client_test type string value '1E8FO4SD3ANB',   " شناسه کلاينت تستي
           gc_destination_api_test type string value 'SADAD_API_TEST',
           gc_dest_token_test      type string value 'SADAD_GET_TEST'.

types: gtyp_moadian type table of zst_sadad.

types: BEGIN OF gtyp_result,
    internalserialnumber type c LENGTH 20,
    taxid type c LENGTH 22,
    guid type c LENGTH 22,
    invoiceType type ZST_SADAD-invoicetype,
    invoicePattern type zst_sadad-invoicePattern,
    invoiceSubject type zst_sadad-invoiceSubject,
    settlementMethod type zst_sadad-settlementMethod,
    ISSUINGDATETIME type zst_sadad-ISSUINGDATETIME,
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
         currencyType            type char04,
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


DATA: gv_it_output  TYPE TABLE OF zfi_sales_report,
      gv_it_billing TYPE TABLE OF zfi_sales_report,
      fieldcatalog TYPE          slis_t_fieldcat_alv WITH HEADER LINE,
      gv_flg_token(1)  TYPE c,
      gt_mapping TYPE /ui2/cl_json=>name_mappings,
      gt_moadian type table of zst_sadad, "تيبل اصلي اين است که در خروجي نمايش داده ميشود
      lv_mobile_number type string,
      lv_password type string,
      lv_shenaseh_client type string,
      lv_destination_api type char60,
      lv_dest_token type char60.

*****نتايج در تيبل
*****zsadad_log
*****ذخيره ميشود
