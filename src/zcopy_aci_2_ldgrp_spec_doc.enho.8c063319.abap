"Name: \TY:CL_FAA_DOCUMENT\ME:COPY_ACI_2_LDGRP_SPEC_DOC\SE:BEGIN\EI
ENHANCEMENT 0 ZCOPY_ACI_2_LDGRP_SPEC_DOC.
*


break omrani.

DATA: lv_data  LIKE LINE OF ms_accounting_document-item_amounts,
      lv_data2 LIKE LINE OF ms_accounting_document-item_amounts.




LOOP AT ms_accounting_document-item_amounts INTO lv_data WHERE curtp = '10'.
  READ TABLE ms_accounting_document-item_amounts INTO lv_data2 WITH KEY posnr = lv_data-posnr curtp = '30'.
  IF sy-subrc NE 0.
    lv_data-curtp = '30'.
    APPEND lv_data TO ms_accounting_document-item_amounts.
  ENDIF.
ENDLOOP.


ENDENHANCEMENT.
