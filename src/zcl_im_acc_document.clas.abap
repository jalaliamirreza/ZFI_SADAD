class ZCL_IM_ACC_DOCUMENT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_ACC_DOCUMENT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_ACC_DOCUMENT IMPLEMENTATION.


  METHOD if_ex_acc_document~change.

    DATA: wa_extension TYPE  bapiparex,
          wa_accit     TYPE  accit.

    BREAK omrani.


    LOOP AT c_extension2 INTO wa_extension.
      IF wa_extension-structure = 'ACCOUNTRECEIVABLE' AND wa_extension-valuepart2 = 'VBEL2'.
        CLEAR wa_accit.
        READ TABLE c_accit INTO wa_accit WITH KEY posnr = wa_extension-valuepart1.
        IF sy-subrc = 0.
          wa_accit-vbel2 = wa_extension-valuepart3.
          wa_accit-posn2 = wa_extension-valuepart4.
          MODIFY c_accit FROM wa_accit INDEX sy-tabix TRANSPORTING vbel2 posn2.
        ENDIF.
      ENDIF.
    ENDLOOP.


    LOOP AT c_extension2 INTO wa_extension.
      IF wa_extension-structure = 'ZSD_POS'.
        CLEAR wa_accit.
        READ TABLE c_accit INTO wa_accit WITH KEY posnr = wa_extension-valuepart1.
        IF sy-subrc = 0.
          CASE wa_extension-valuepart2.
            WHEN 'VBEL2'.
              wa_accit-vbel2         = wa_extension-valuepart3.
              wa_accit-posn2         = '000010'.
              MODIFY c_accit FROM wa_accit INDEX sy-tabix TRANSPORTING vbel2.
          ENDCASE.
        ENDIF.
      ENDIF.
    ENDLOOP.



  ENDMETHOD.


  method IF_EX_ACC_DOCUMENT~FILL_ACCIT.
  endmethod.
ENDCLASS.
