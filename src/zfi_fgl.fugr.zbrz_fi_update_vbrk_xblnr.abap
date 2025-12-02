FUNCTION ZBRZ_FI_UPDATE_VBRK_XBLNR.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(VBELN) TYPE  VBELN
*"     VALUE(XBLNR) TYPE  XBLNR
*"----------------------------------------------------------------------


  update vbrk SET xblnr = xblnr WHERE vbeln = vbeln.
  commit WORK.


ENDFUNCTION.
