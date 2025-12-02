"Name: \PR:SAPMFCJ0\FO:STORE_COBL_E\SE:BEGIN\EI
ENHANCEMENT 0 ZMFCJ0F01.
*

itcj_e_postings-iban         = iscj_e_postings-iban.
itcj_e_postings-ebeln        = iscj_e_postings-ebeln.
itcj_e_postings-belnr_gjahr  = iscj_e_postings-belnr_gjahr.
itcj_e_postings-belnr_ZSCJ   = iscj_e_postings-belnr_ZSCJ.


IF iscj_e_postings-iban IS NOT INITIAL.
  CALL FUNCTION 'CHECK_IBAN'
    EXPORTING
      i_iban    = iscj_e_postings-iban
    EXCEPTIONS
      not_valid = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE 'IBAN IS Incorrect' TYPE 'E'.
  ENDIF.
ENDIF.


ENDENHANCEMENT.
