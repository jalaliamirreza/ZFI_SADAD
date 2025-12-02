class ZCLFINS_FCV_BSTAT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_FINS_FCV_BSTAT .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCLFINS_FCV_BSTAT IMPLEMENTATION.


  method IF_BADI_FINS_FCV_BSTAT~SET_BSTAT.
*    BREAK-POINT.
    RAISE ex_create_bseg.


  endmethod.
ENDCLASS.
