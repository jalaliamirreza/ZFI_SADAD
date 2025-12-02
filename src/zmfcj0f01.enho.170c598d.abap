"Name: \PR:SAPMFCJ0\FO:STORE_COBL_R\SE:BEGIN\EI
ENHANCEMENT 0 ZMFCJ0F01.
*

itcj_r_postings-iban         = iscj_r_postings-iban.
itcj_r_postings-ebeln        = iscj_r_postings-ebeln.
itcj_r_postings-belnr_gjahr  = iscj_r_postings-belnr_gjahr.
itcj_r_postings-belnr_ZSCJ   = iscj_r_postings-belnr_ZSCJ.


IF iscj_r_postings-iban IS NOT INITIAL.

  CALL FUNCTION 'CHECK_IBAN'
    EXPORTING
      i_iban    = iscj_r_postings-iban
    EXCEPTIONS
      not_valid = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE 'IBAN IS Incorrect' TYPE 'E'.
  ENDIF.
ENDIF.



ENDENHANCEMENT.
