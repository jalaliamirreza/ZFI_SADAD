*&---------------------------------------------------------------------*
*& Report ZSD_FI_SALES_POS_JOB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSD_FI_SALES_POS_JOB.
TABLES : vbak .
DATA: wposreq TYPE ZFISD_POS_REQ ,
      it_pos_req TYPE TABLE OF ZFISD_POS_REQ.
PARAMETERS :
  p_bukrs  TYPE t001-bukrs OBLIGATORY MEMORY ID BUK.


SELECT-OPTIONS :

    s_vbeln    FOR  vbak-VBELN MEMORY ID AUN  .
INITIALIZATION .
if p_bukrs is INITIAL .
get PARAMETER ID 'BUK' FIELD p_bukrs .
ENDIF.

 START-OF-SELECTION.


SELECT * INTO TABLE   it_pos_req
    from  ZFISD_POS_REQ WHERE
  VBELN in  s_vbeln AND PROC_STAT ne 'X' .

LOOP AT it_pos_req INTO wposreq.

  CALL FUNCTION 'ZFI_POS_RESPONSE_READ_N'
  EXPORTING
    SO_NO         = wposreq-VBELN
    SEQ_NO        = wposreq-SEQ_NO
 EXCEPTIONS
   FAILED        = 1
   OTHERS        = 2
          .




ENDLOOP.
