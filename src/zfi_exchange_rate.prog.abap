*&---------------------------------------------------------------------*
*& Report  ZFI_EXCH_RATES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZFI_EXCHANGE_RATE.

TYPE-POOLS: TRUXS.
DATA: TAB_RAW_DATA TYPE TRUXS_XML_TABLE,
TOT_SIZE TYPE I ,
TOT_SIZE1 TYPE I.

DATA: BEGIN OF TAB_CONVERTED_DATA OCCURS 0,
DATA1(250) TYPE C,
END OF TAB_CONVERTED_DATA.

PARAMETERS FILEPATH LIKE RLGRAP-FILENAME DEFAULT 'C:\20150608.xml' NO-DISPLAY.
PARAMETER FILELOC TYPE STRING DEFAULT 'D:\Exch-Rate\' NO-DISPLAY.
*selection-screen comment /35(65) text-010.

DATA SIZE TYPE I.
DATA IN_FILENAME TYPE STRING.

PARAMETER IN_DATE TYPE SY-DATUM DEFAULT SY-DATUM.
*concatenate sy-datum '.xml' into in_filename.
CONCATENATE IN_DATE '.xml' INTO IN_FILENAME.

DATA: IT_TAB TYPE FILETABLE,
      GD_SUBRC TYPE I.

*at selection-screen on value-request for filepath.
*  refresh: it_tab.
*  call method cl_gui_frontend_services=>file_open_dialog
*    exporting
*      window_title     = 'Select File'
*      default_filename = '*.xml'
*      multiselection   = ' '
*    changing
*      file_table       = it_tab
*      rc               = gd_subrc.
*
*  loop at it_tab into filepath.
*  endloop.

* BAD FUNCTION!!!!!!!!!
*CALL FUNCTION 'TEXT_CONVERT_XML_TO_SAP'
*  EXPORTING
*    I_FIELD_SEPERATOR = ';'
*    I_TAB_RAW_DATA = TAB_RAW_DATA
*    I_FILENAME = FILENAME1
*    I_TOTALSIZE = TOT_SIZE
*  TABLES
*    I_TAB_CONVERTED_DATA = TAB_CONVERTED_DATA.
*

*at selection-screen on value-request for fileloc.
*  call method cl_gui_frontend_services=>directory_browse
**    exporting
**      window_title         =
**      initial_folder       =
*    changing
*      selected_folder      = fileloc
**    exceptions
**      cntl_error           = 1
**      error_no_gui         = 2
**      not_supported_by_gui = 3
**      others               = 4
*          .
*  if sy-subrc <> 0.
**   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  endif.


START-OF-SELECTION.
  DATA FULLNAME TYPE STRING.
  CONCATENATE FILELOC IN_FILENAME INTO FULLNAME.

*  filename = filepath.
**&-----------------------------------------------------------&
**   GUI_UPLOAD cannot be run in backgound!!!
**&-----------------------------------------------------------&
*  call function 'GUI_UPLOAD'
*    exporting
*      filename                = filename
*      filetype                = 'BIN'
**     HAS_FIELD_SEPARATOR     = ' '
**     HEADER_LENGTH           = 0
**     READ_BY_LINE            = 'X'
**     DAT_MODE                = ' '
**     CODEPAGE                = ' '
**     IGNORE_CERR             = ABAP_TRUE
**     REPLACEMENT             = '#'
**     CHECK_BOM               = ' '
**     VIRUS_SCAN_PROFILE      =
**     NO_AUTH_CHECK           = ' '
*    importing
*      filelength              = size
**     HEADER                  =
*    tables
*      data_tab                = tab_raw_data
*    exceptions
*      file_open_error         = 1
*      file_read_error         = 2
*      no_batch                = 3
*      gui_refuse_filetransfer = 4
*      invalid_type            = 5
*      no_authority            = 6
*      unknown_error           = 7
*      bad_data_format         = 8
*      header_not_allowed      = 9
*      separator_not_allowed   = 10
*      header_too_long         = 11
*      unknown_dp_error        = 12
*      access_denied           = 13
*      dp_out_of_memory        = 14
*      disk_full               = 15
*      dp_timeout              = 16
*      others                  = 17.
*  if sy-subrc <> 0.
*    write:/ sy-subrc.
*    write:/ 'File Not Found!'.
*    return.
*    "LEAVE PROGRAM.
** Implement suitable error handling here
*  endif.

  OPEN DATASET FULLNAME FOR INPUT IN BINARY MODE.
  IF SY-SUBRC NE 0.
    WRITE:/ 'File Not Found!'.
    LEAVE LIST-PROCESSING.
    RETURN.
  ELSE.
    WRITE:/ 'File Found!'.
    "return.
  ENDIF.

  DATA: P_FILENAME TYPE EPSF-EPSFILNAM,
        P_DIR TYPE EPSF-EPSDIRNAM.

  P_FILENAME = IN_FILENAME.
  P_DIR = FILELOC.
  CALL FUNCTION 'EPS_GET_FILE_ATTRIBUTES'
    EXPORTING
      FILE_NAME        = P_FILENAME
      DIR_NAME         = P_DIR
*     IV_LONG_DIR_NAME = fullpath
    IMPORTING
      FILE_SIZE        = SIZE.

  DATA: BEGIN OF TAB OCCURS 0,
"        text(100),
        DATA(1024) TYPE X,
        END OF TAB.

  WHILE SY-SUBRC EQ 0.
    READ DATASET FULLNAME INTO TAB.
    APPEND TAB.
    CLEAR TAB.
  ENDWHILE.
  CLOSE DATASET FULLNAME.

  DATA XMLDATA TYPE XSTRING.
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      INPUT_LENGTH       = SIZE
*   FIRST_LINE         = 0
*   LAST_LINE          = 0
   IMPORTING
     BUFFER             = XMLDATA
    TABLES
      BINARY_TAB         = TAB
      "binary_tab         = tab_raw_data
* EXCEPTIONS
*   FAILED             = 1
*   OTHERS             = 2
            .
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.

  DATA: RESULT_XML TYPE STANDARD TABLE OF SMUM_XMLTB,
        WA_XML LIKE SMUM_XMLTB,
        RETURN LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'SMUM_XML_PARSE'
    EXPORTING
      XML_INPUT = XMLDATA
    TABLES
      XML_TABLE = RESULT_XML
      RETURN    = RETURN.

  DELETE RESULT_XML WHERE TYPE <> 'V'.
  DELETE RESULT_XML WHERE CNAME = 'CLIENT_ID'.

  "Correct date values. Input format is mm.dd.yyyy
  DATA: DD TYPE CHAR2,
        MM TYPE CHAR2,
        YYYY TYPE CHAR4.

  LOOP AT RESULT_XML INTO WA_XML WHERE CNAME = 'DATE'.
    "concatenate wa_xml-cvalue+6(4) wa_xml-cvalue(2) wa_xml-cvalue+3(2) into wa_xml-cvalue.
    CLEAR: MM, DD, YYYY.
    SPLIT WA_XML-CVALUE AT '/' INTO MM DD YYYY.

    IF STRLEN( DD ) = 1.
      CONCATENATE '0' DD INTO DD.
    ENDIF.

    IF STRLEN( MM ) = 1.
      CONCATENATE '0' MM INTO MM.
    ENDIF.

    CONCATENATE YYYY MM DD INTO WA_XML-CVALUE.

    "Validating date
    DATA TMP_DATE TYPE SY-DATUM.
    TMP_DATE = WA_XML-CVALUE.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        DATE                      = TMP_DATE
      EXCEPTIONS
        PLAUSIBILITY_CHECK_FAILED = 1
        OTHERS                    = 2.

    IF SY-SUBRC <> 0.
      MESSAGE E002(0S) WITH TMP_DATE.
    ENDIF.

    MODIFY RESULT_XML FROM WA_XML.
  ENDLOOP.


  TYPES: BEGIN OF ST_CURR,
            KURST TYPE TCURR-KURST,
            FCURR TYPE TCURR-FCURR,
            TCURR TYPE TCURR-TCURR,
            GDATU TYPE SY-DATUM,
            "gdatu type tcurr-gdatu,
            "ukurs type tcurr-ukurs, SHORT DECIMALS!
            UKURS TYPE P DECIMALS 6,
            FROM_FACTOR TYPE P DECIMALS 0,
            TO_FACTOR TYPE P DECIMALS 0,
         END OF ST_CURR.

  DATA: IT_CURR TYPE TABLE OF ST_CURR,
        WA_CURR LIKE LINE OF IT_CURR.

  DATA : LD_INDEX TYPE I VALUE 1,
         LD_OFFSET TYPE I VALUE 5.

  CONSTANTS: EXRATE_TYPE TYPE TCURR-KURST VALUE 'M',
             EXRATE_TYPE_IRR TYPE TCURR-KURST VALUE 'CBMI'.
  FIELD-SYMBOLS : <FS>.

  "Convert xml table into target table(TCURR)
  LOOP AT RESULT_XML INTO WA_XML.
    LD_INDEX = LD_INDEX + 1.
    ASSIGN COMPONENT LD_INDEX OF STRUCTURE WA_CURR TO <FS>.
    MOVE WA_XML-CVALUE TO <FS>.
    IF LD_INDEX MOD LD_OFFSET = 0.
      IF WA_CURR-FCURR = 'IRR' OR WA_CURR-TCURR = 'IRR'.
        WA_CURR-KURST = EXRATE_TYPE_IRR.
      ELSE.
        WA_CURR-KURST = EXRATE_TYPE.
      ENDIF.
      APPEND WA_CURR TO IT_CURR.
      CLEAR WA_CURR.
      LD_INDEX = 1.
    ENDIF.
  ENDLOOP.

  "Set exchange rates considering Ratios(TCURF)
  "-------------------- NOTE ------------------
  "  Date values in table TCURF are converted to specific format INVDAT(Char8)
  "  We should convert it back to normal date mode to compare dates
  "--------------------------------------------
  DATA: IT_RATIO TYPE TABLE OF TCURF,
        WA_RATIO TYPE TCURF,
        WA_RATIO_TEMP LIKE WA_RATIO.

*&----------------------------------
*&    TEST
*&----------------------------------
*  data noofrec TYPE i.
*  noofrec = lines( it_curr ).
*  write:/ noofrec.
*  return.
*&----------------------------------
*&    END TEST
*&----------------------------------

  LOOP AT IT_CURR INTO WA_CURR.
    CLEAR IT_RATIO.
    SELECT * FROM TCURF INTO TABLE IT_RATIO
      WHERE KURST = WA_CURR-KURST AND FCURR = WA_CURR-FCURR AND TCURR = WA_CURR-TCURR.

    "Find latest Ratio
    DATA: NORMALDATE TYPE CHAR10, "dd.mm.yyyy
          MAXDATE TYPE CHAR10. "dd.mm.yyyy
    CLEAR MAXDATE.
    LOOP AT IT_RATIO INTO WA_RATIO_TEMP.
      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          INPUT  = WA_RATIO_TEMP-GDATU
        IMPORTING
          OUTPUT = NORMALDATE.
      CONCATENATE NORMALDATE+6(4) '.' NORMALDATE+3(2) '.' NORMALDATE(2) INTO NORMALDATE.
      IF NORMALDATE > MAXDATE.
        MOVE-CORRESPONDING WA_RATIO_TEMP TO WA_RATIO.
        MAXDATE = NORMALDATE.
      ENDIF.
    ENDLOOP.

    CHECK WA_RATIO-TFACT <> 0.

    WA_CURR-UKURS = WA_CURR-UKURS * WA_RATIO-FFACT / WA_RATIO-TFACT.
    WA_CURR-FROM_FACTOR = WA_RATIO-FFACT.
    WA_CURR-TO_FACTOR = WA_RATIO-TFACT.
    MODIFY IT_CURR FROM WA_CURR.
    CLEAR WA_RATIO.
  ENDLOOP.

  "Convert Normal Date to INVDate
*loop at it_curr into wa_curr.
*  call function 'CONVERSION_EXIT_INVDT_INPUT'
*    exporting
*      input  = wa_curr-gdatu
*    importing
*      output = wa_curr-gdatu.
*  modify it_curr from wa_curr.
*endloop.


  "Update Table TCURR from it_curr.
  DATA: EXCH_RATE LIKE BAPI1093_0,
       RET LIKE BAPIRET2,
       MSG TYPE STRING.
  LOOP AT IT_CURR INTO WA_CURR.
    IF WA_CURR-FROM_FACTOR = 0 OR WA_CURR-TO_FACTOR = 0.
      CONCATENATE 'Exchange rate ratio not defined for:'  WA_CURR-FCURR 'To'  WA_CURR-TCURR INTO MSG SEPARATED BY SPACE.
      WRITE:/ MSG COLOR COL_NEGATIVE.
      CONTINUE.
    ENDIF.

    EXCH_RATE-RATE_TYPE = WA_CURR-KURST.
    EXCH_RATE-FROM_CURR = WA_CURR-FCURR.
    EXCH_RATE-TO_CURRNCY = WA_CURR-TCURR.
    EXCH_RATE-VALID_FROM = WA_CURR-GDATU.
    EXCH_RATE-EXCH_RATE = WA_CURR-UKURS.
    EXCH_RATE-FROM_FACTOR = WA_CURR-FROM_FACTOR.
    EXCH_RATE-TO_FACTOR = WA_CURR-TO_FACTOR.

    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATE'
      EXPORTING
        EXCH_RATE  = EXCH_RATE
        UPD_ALLOW  = 'X'
*       CHG_FIXED  = ' '
*       DEV_ALLOW  = '000'
      IMPORTING
        RETURN     = RET
*       RATE_TYPE  =
*       FROM_CURR  =
*       TO_CURRNCY =
      .
    IF RET-TYPE CA 'EA'.
      ROLLBACK WORK.
      CONCATENATE 'Exchange rate not saved for:' EXCH_RATE-FROM_CURR 'To' EXCH_RATE-TO_CURRNCY ', Error:' RET-MESSAGE INTO MSG SEPARATED BY SPACE.
      MESSAGE MSG TYPE 'E'.
    ELSE.
      DATA: FROM_FACT_STR TYPE STRING,
            TO_FACT_STR TYPE STRING,
            EXCH_RATE_STR TYPE STRING.

      FROM_FACT_STR = EXCH_RATE-FROM_FACTOR.
      TO_FACT_STR = EXCH_RATE-TO_FACTOR.
      EXCH_RATE_STR = EXCH_RATE-EXCH_RATE.
      COMMIT WORK.
      CONCATENATE 'Exchange rate saved for:' FROM_FACT_STR EXCH_RATE-FROM_CURR 'To' TO_FACT_STR EXCH_RATE-TO_CURRNCY 'with rate:' EXCH_RATE_STR INTO MSG SEPARATED BY SPACE.
      WRITE:/ MSG.
    ENDIF.

    CLEAR EXCH_RATE.
  ENDLOOP.
