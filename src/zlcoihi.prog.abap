*----------------------------------------------------------------------*
***INCLUDE ZLCOIHI.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_FKBER  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_fkber INPUT.
  IF caufvd-equnr IS NOT INITIAL.
    data fkbwr TYPE fkber.
    select SINGLE FUNC_AREA FROM itob
                            INNER JOIN csks on csks~kostl = itob~kostl
                            INTO CAUFVD-FUNC_AREA WHERE equnr = caufvd-equnr.
    endif.



ENDMODULE.
