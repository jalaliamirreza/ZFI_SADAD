class ZCL_IM_UKM_R3_ACTIVATE definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_UKM_R3_ACTIVATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_UKM_R3_ACTIVATE IMPLEMENTATION.


  method IF_EX_UKM_R3_ACTIVATE~DCD_ACTIVE.
  endmethod.


  method IF_EX_UKM_R3_ACTIVATE~FI_AR_UPDATE_MODE.
  endmethod.


  method IF_EX_UKM_R3_ACTIVATE~GET_RFCDEST_FSCM.
  endmethod.


  method IF_EX_UKM_R3_ACTIVATE~NO_SLD.
  endmethod.


  METHOD if_ex_ukm_r3_activate~set_active.

    e_active_flag = 'X'.
    e_erp2005     = 'X'.


  ENDMETHOD.
ENDCLASS.
