"Name: \TY:CL_FINS_ACDOC_VALUATION_UTIL\ME:ADD_CURTP_LD_CMP_TO_ACCCR\SE:BEGIN\EI
ENHANCEMENT 0 ZADD_CURTP_LD_CMP_TO_ACCCR.
*


break omrani.

DATA: lv_data  LIKE LINE OF ct_acccr,
      lv_data2 LIKE LINE OF ct_acccr.




LOOP AT ct_acccr INTO lv_data WHERE curtp = '10'.
  READ TABLE ct_acccr INTO lv_data2 WITH KEY posnr = lv_data-posnr curtp = '30'.
  IF sy-subrc NE 0.
    lv_data-curtp = '30'.
    APPEND lv_data TO ct_acccr.
  ENDIF.
ENDLOOP.



ENDENHANCEMENT.
