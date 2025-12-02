class ZCL_FI_IMP_FAGL_FSV definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_FINS_FCV_BSTAT .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_FI_IMP_FAGL_FSV IMPLEMENTATION.


  method IF_BADI_FINS_FCV_BSTAT~SET_BSTAT.

    BREAK-POINT.

    IF cl_fins_acdoc_util=>is_currency_type_integrated(
       EXPORTING
         iv_company_code  =     iv_bukrs
         iv_currency_type =     iv_currency_type ).

      RAISE ex_create_bseg.
    ENDIF.



     RAISE ex_create_bseg.

  endmethod.
ENDCLASS.
