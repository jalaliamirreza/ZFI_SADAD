*&---------------------------------------------------------------------*
*& Report ZBRZ_SD_SIMULA_VF01
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbrz_sd_simula_vf01.

TYPE-POOLS: slis.

TYPES: BEGIN OF ty_bseg.

    INCLUDE STRUCTURE bseg.

TYPES: ktext TYPE rfpsd-ktext,

       kursf TYPE acccr-kursf,

       END OF ty_bseg,

       ty_t_bseg TYPE TABLE OF ty_bseg.

CONSTANTS: c_c(1) TYPE c VALUE 'C',

           c_j(1) VALUE 'J',

           c_pcmt TYPE skat-ktopl VALUE 'BARZ',

           c_brl  TYPE acccr-waers VALUE 'IRR'.

PARAMETERS: p_vbeln TYPE likp-vbeln.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

PARAMETERS:

  p_vbelnv TYPE vbap-vbeln,

  p_posnr  TYPE vbap-posnr.

SELECTION-SCREEN END OF BLOCK b1.

DATA:

  t_billing_data_in   TYPE TABLE OF bapivbrk,

  w_billind_data_in   LIKE LINE OF t_billing_data_in,

  t_condition_data_in TYPE TABLE OF bapikomv,

  t_returnlog_out     TYPE TABLE OF bapireturn1,

  t_success_doc_out   TYPE TABLE OF bapisucc,

  t_xaccit            TYPE TABLE OF accit,

  t_xacccr            TYPE TABLE OF acccr,

  lv_simulate(1)      TYPE c,

  xbseg               TYPE ty_t_bseg,

  w_bseg              LIKE LINE OF xbseg.

DATA:

  w_layout   TYPE slis_layout_alv,

  w_gs_print TYPE slis_print_alv,

  t_fieldcat TYPE slis_t_fieldcat_alv,

  w_fieldcat LIKE LINE OF t_fieldcat.

START-OF-SELECTION.

  IF p_vbeln IS INITIAL AND

  p_vbelnv IS INITIAL AND

  p_posnr IS INITIAL.

* Fill in the Supplier or OV / item.

    MESSAGE i208(00) WITH TEXT-e03.

    EXIT.

  ENDIF.

  IF NOT p_vbeln IS INITIAL AND ( NOT p_vbelnv IS INITIAL OR NOT p_posnr IS INITIAL ).

* Can only be filled Supply or OV.

    MESSAGE i208(00) WITH TEXT-e01.

    EXIT.

  ENDIF.

* If delivery is not filled order and item are required.

  IF p_vbeln IS INITIAL.

    IF p_vbelnv IS INITIAL OR

    p_posnr IS INITIAL.

* Document or Sales Item was not filled.

      MESSAGE i208(00) WITH TEXT-e02.

      EXIT.

    ENDIF.

  ENDIF.

* Supply / Picking

  IF NOT p_vbeln IS INITIAL.

    CLEAR w_billind_data_in.

    w_billind_data_in-ref_doc = p_vbeln.

    w_billind_data_in-ref_doc_ca = c_j.

    APPEND w_billind_data_in TO t_billing_data_in.

  ELSE.

    CLEAR w_billind_data_in.

    w_billind_data_in-ref_doc = p_vbelnv.

    w_billind_data_in-ref_item = p_posnr.

    w_billind_data_in-ref_doc_ca = c_c.

    APPEND w_billind_data_in TO t_billing_data_in.

  ENDIF.

* Exports to EXIT_SAPLV60B_008

  lv_simulate = 'X'.

  EXPORT p1 = lv_simulate

  TO MEMORY ID 'ZBILLINGDOC_SIMULATE'.

  CALL FUNCTION 'BAPI_BILLINGDOC_SIMULATE'
    TABLES
      billing_data_in   = t_billing_data_in
      condition_data_in = t_condition_data_in
      returnlog_out     = t_returnlog_out
      success_doc_out   = t_success_doc_out
    EXCEPTIONS
      error_message     = 1
      OTHERS            = 2.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno

    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

  IMPORT p1 = t_xaccit[]

  p2 = t_xacccr[]

  FROM MEMORY ID 'ZBILLINGDOC_SIMULATE_TAB'.

  PERFORM f_preenche_alv.

  PERFORM f_alv.

  CLEAR t_xaccit[].

  CLEAR t_xacccr[].


* & ----------------------- *

* & Form f_alv

* & ----------------------- *

* text

* ------------------------ *

* -> p1 text

* <- p2 text

* ------------------------ *

FORM f_alv.

* ALV list equal to FB03

* Layout

  w_layout-zebra = 'X'.

  w_layout-min_linesize = 100.

* Print

  w_gs_print-no_print_listinfos = 'X'.

* Fielcat

  PERFORM f_fieldcat.

* Free the document

  ROLLBACK WORK.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_interface_check  = ''
*     i_buffer_active    = 'X' "ALRK241034
      i_callback_program = 'ZSD_SIMULA_VF01'
*     i_callback_pf_status_set = 'PF_STATUS_SET'
*     i_callback_user_command = 'HANDLE_USER_COMMAND'
*     I_STRUCTURE_NAME   = 'BSEG'
      is_layout          = w_layout
      it_fieldcat        = t_fieldcat
      i_default          = 'X'
      i_save             = 'A' "Note 319936
      is_print           = w_gs_print
    TABLES
      t_outtab           = xbseg
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno

    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.

* & ----------------------- *

* & Form f_fieldcat

* & ----------------------- *

* text

* ------------------------ *

* -> p1 text

* <- p2 text

* ------------------------ *

FORM f_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'BUZEI'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'BUZEI'.

  w_fieldcat-ref_tabname = 'BSEG'.

  w_fieldcat-key = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'BSCHL'.

  w_fieldcat-emphasize = '$'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'BSCHL'.

  w_fieldcat-ref_tabname = 'BSEG'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'HKONT'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'KONTO'.

  w_fieldcat-ref_tabname = 'RFPSD'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'KTEXT'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'KTEXT'.

  w_fieldcat-ref_tabname = 'RFPSD'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'ZUONR'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'ZUONR'.

  w_fieldcat-ref_tabname = 'BSEG'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'MWSKZ'.

  w_fieldcat-emphasize = '$'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'MWSKZ'.

  w_fieldcat-ref_tabname = 'BSEG'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'WRBTR'.

  w_fieldcat-tabname = 'XBSEG'.
  w_fieldcat-currency = 'IRR'.
  w_fieldcat-ref_tabname = 'BSEG'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'DMBTR'.
  w_fieldcat-currency = 'IRR'.

  w_fieldcat-tabname = 'XBSEG'.

  APPEND w_fieldcat TO t_fieldcat.

  CLEAR w_fieldcat.

  w_fieldcat-fieldname = 'KURSF'.

  w_fieldcat-tabname = 'XBSEG'.

  w_fieldcat-ref_fieldname = 'KURSF'.

  w_fieldcat-ref_tabname = 'ACCCR'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM. "F_fieldcat

* & ----------------------- *

* & Form f_preenche_alv

* & ----------------------- *

* text

* ------------------------ *

* -> p1 text

* <- p2 text

* ------------------------ *

FORM f_preenche_alv.

  CLEAR xbseg[].

  FIELD-SYMBOLS: <lf_xaccit> LIKE LINE OF t_xaccit,

                 <lf_xacccr> LIKE LINE OF t_xacccr,

                 <lf_bseg>   LIKE LINE OF xbseg.

  DATA: lv_buzei TYPE bseg-buzei.

* First find the customers

  LOOP AT t_xaccit ASSIGNING <lf_xaccit>

  WHERE kunnr <> space.

    CLEAR w_bseg.

    w_bseg-bschl = <lf_xaccit>-bschl.

    w_bseg-hkont = <lf_xaccit>-kunnr.

    w_bseg-zuonr = <lf_xaccit>-zuonr.

    w_bseg-mwskz = <lf_xaccit>-mwskz.

    SELECT SINGLE name1 INTO w_bseg-ktext

    FROM kna1

    WHERE kunnr = <lf_xaccit>-kunnr.

    READ TABLE t_xacccr ASSIGNING <lf_xacccr>

    WITH KEY posnr = <lf_xaccit>-posnr.

    IF sy-subrc = 0.

      w_bseg-wrbtr = <lf_xacccr>-wrbtr.

      w_bseg-kursf = <lf_xacccr>-kursf.

* Checks if it is different from BRL

      IF <lf_xacccr>-waers <> c_brl.

        UNASSIGN <lf_xacccr>.

        READ TABLE t_xacccr ASSIGNING <lf_xacccr>

        WITH KEY posnr = <lf_xaccit>-posnr

        waers = c_brl.

        IF sy-subrc = 0.

          w_bseg-dmbtr = <lf_xacccr>-wrbtr.

        ENDIF.

      ENDIF.

    ENDIF.

    IF NOT w_bseg-wrbtr IS INITIAL.

      COLLECT w_bseg INTO xbseg.

    ENDIF.

  ENDLOOP.

* Accounts only without client

  LOOP AT t_xaccit ASSIGNING <lf_xaccit>

  WHERE hkont <> space AND

  kunnr = space.

* You can only enter

    CLEAR w_bseg.

    w_bseg-bschl = <lf_xaccit>-bschl.

    w_bseg-hkont = <lf_xaccit>-hkont.

    w_bseg-zuonr = <lf_xaccit>-bldat.

    w_bseg-mwskz = <lf_xaccit>-mwskz.

    READ TABLE t_xacccr ASSIGNING <lf_xacccr>

    WITH KEY posnr = <lf_xaccit>-posnr.

    IF sy-subrc = 0.

      w_bseg-wrbtr = <lf_xacccr>-wrbtr.

      w_bseg-kursf = <lf_xacccr>-kursf.

* Checks if it is different from BRL

      IF <lf_xacccr>-waers <> c_brl.

        UNASSIGN <lf_xacccr>.

        READ TABLE t_xacccr ASSIGNING <lf_xacccr>

        WITH KEY posnr = <lf_xaccit>-posnr

        waers = c_brl.

        IF sy-subrc = 0.

          w_bseg-dmbtr = <lf_xacccr>-wrbtr.

        ENDIF.

      ENDIF.

    ENDIF.

* TABLE BUFFER !!!

    SELECT SINGLE txt50 INTO w_bseg-ktext

    FROM skat

    WHERE spras = sy-langu

    AND ktopl = c_pcmt

    AND saknr = <lf_xaccit>-hkont.

    IF NOT w_bseg-wrbtr IS INITIAL.

      COLLECT w_bseg INTO xbseg.

    ENDIF.

  ENDLOOP.

* Arrange item numbering

  LOOP AT xbseg ASSIGNING <lf_bseg>.

    ADD 1 TO lv_buzei.

    <lf_bseg>-buzei = lv_buzei.

  ENDLOOP.

ENDFORM.
