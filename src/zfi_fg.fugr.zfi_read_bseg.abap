FUNCTION ZFI_READ_BSEG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"     REFERENCE(GJAHR) TYPE  GJAHR
*"  TABLES
*"      IT_BSEG STRUCTURE  ZBSEG_DATA OPTIONAL
*"----------------------------------------------------------------------


  REFRESH IT_BSEG.
  SELECT
      BUKRS
      BELNR
      GJAHR
      EBELN
      EBELP
      BSCHL
      WRBTR
      UMSKZ
      DMBTR
      ZUONR
      PSWBT
      PSWSL
      PYAMT
      PYCUR
      VORGN
      VERTN
      SHKZG
      XREF1
  FROM BSEG INTO TABLE IT_BSEG WHERE BUKRS = BUKRS AND GJAHR = GJAHR.


ENDFUNCTION.
