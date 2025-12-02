************************************************************************
* Copy The Check Number In A Reference Document Field                  *
*----------------------------------------------------------------------*
* -> If a further field of the document is to be filled with           *
*    the check number, additional lines, which correspond              *
*    with ** (double star) characterized lines, must are inserted.     *
************************************************************************
* Title       : Create Reference for Check from Payment Document
*----------------------------------------------------------------------*
* Change history of ALV Development
* Program description: Display the list of Reference for Check from
*                      Payment Document
* Short description of the program:This program Converted into ALV List
*          Using Function Modules REUSE_ALV_BLOCK_LIST_INIT,
*          REUSE_ALV_BLOCK_LIST_APPEND,REUSE_ALV_BLOCK_LIST_DISPLAY,
*          The ALV output contains Classical append ALV list
*----------------------------------------------------------------------*

REPORT RFCHKU01
       MESSAGE-ID FS
       NO STANDARD PAGE HEADING
       LINE-SIZE 132.

*----------------------------------------------------------------------*
* Data Declarations                                                    *
*----------------------------------------------------------------------*
INCLUDE FF05LCDV.                "function module BELEG_WRITE_DOCUMENT
INCLUDE FF05LCDF.                "form CD_CALL_BELEG

TABLES:
* -> If further fields of a document are to be filled with
*    the check number, additional reference flags must be maintained
*    in the value table of the domain of PAYR-IREFE.
  PAYR,                          "Payment transfer medium file
  T001,                          "Company Codes
  BSEG,                          "Accounting document segment
  RFSDO,                         "Data elements for selection
  BSEC, BSED, BSET.              "Dummy Tables for
                                 "function module CHANGE_DOCUMENT

TYPES:
  BEGIN OF PROTOCOL_LINE,        "Protocol line
    ZBUKR LIKE PAYR-ZBUKR,
    VBLNR LIKE PAYR-VBLNR,
    GJAHR LIKE PAYR-GJAHR,
    HBKID LIKE PAYR-HBKID,
    HKTID LIKE PAYR-HKTID,
    RZAWE LIKE PAYR-RZAWE,
    CHECT LIKE PAYR-CHECT,
  END OF PROTOCOL_LINE.

RANGES:
  R_LAUFD FOR PAYR-LAUFD,        "Date on which the program is to be run
  R_LAUFI FOR PAYR-LAUFI.        "Additional identification

DATA:
  T_PAYR        LIKE PAYR   OCCURS 0 WITH HEADER LINE,
  T_BKPF        LIKE BKPF   OCCURS 1 WITH HEADER LINE,
  T_BSEG        LIKE BSEG   OCCURS 0 WITH HEADER LINE,
  T_BKDF        LIKE BKDF   OCCURS 0 WITH HEADER LINE,
  T_BSEC        LIKE BSEC   OCCURS 0 WITH HEADER LINE,
  T_BSED        LIKE BSED   OCCURS 0 WITH HEADER LINE,
  T_BSET        LIKE BSET   OCCURS 0 WITH HEADER LINE,
  T_LAUFK       LIKE ILAUFK OCCURS 0 WITH HEADER LINE,
  T_UPDATE      TYPE PROTOCOL_LINE OCCURS 0 WITH HEADER LINE,
  T_QUEUE       TYPE PROTOCOL_LINE OCCURS 0 WITH HEADER LINE,
  T_ERROR       TYPE PROTOCOL_LINE OCCURS 0 WITH HEADER LINE,
  t_multi       TYPE protocol_line OCCURS 0 WITH HEADER LINE,
  W_XBSEG       LIKE FBSEG,
  W_YBSEG       LIKE FBSEG,
  I_REFKZ       LIKE PAYR-IREFE,
  I_NOPROT      TYPE C VALUE 'X',
  I_PTYPE       TYPE C,
*  I_PAGEEND     TYPE C VALUE 'X', "removed Retrofit
*  I_INTENSIFIED TYPE C,           "removed Retrofit
  IS_ZBUKR(10)  TYPE C,
  IS_VBLNR(10)  TYPE C,
  IS_GJAHR(10)  TYPE C,
  IS_HBKID(10)  TYPE C,
  IS_HKTID(10)  TYPE C,
  IS_RZAWE(10)  TYPE C,
  IM_CHECT(20)  TYPE C.
************************************************************************
***    Begin of ALV Conversion                   Retrofit
************************************************************************
***Data Declaration
TYPE-POOLS: SLIS. "ALV def. type-pool
DATA: GT_UPDATE TYPE STANDARD TABLE OF FOAP_S_RFCHKU00_LIST.
"Update table
DATA: GT_QUEUE TYPE STANDARD TABLE OF FOAP_S_RFCHKU00_LIST.
" Queue table
DATA: GT_ERROR TYPE STANDARD TABLE OF FOAP_S_RFCHKU00_LIST.
"Error table
DATA: GT_MULTI TYPE STANDARD TABLE OF FOAP_S_RFCHKU00_LIST.
"Multiple check table
DATA: GV_REPID TYPE SY-REPID.               "Report field
DATA: GT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALv.  " ALV Field Catalog Table
DATA: GS_LAYO TYPE SLIS_LAYOUT_ALV.     "layout table
DATA:GC_TOL_FORM1 TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST_ONE'.
"top of page 1
DATA:GC_TOL_FORM2 TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST_TWO'.
"top of page 2
DATA:GC_TOL_FORM3 TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST_THREE'.
"top of page 3
DATA:GC_TOL_FORM4 TYPE SLIS_FORMNAME VALUE 'TOP_OF_LIST_FOUR'.
"top of page 4
DATA: GV_COUNT TYPE I. "Counter field
CONSTANTS: gc_str   TYPE dd02l-tabname VALUE
                        'FOAP_S_RFCHKU00_LIST',"Structure
           GC_COLOR TYPE CHAR05 VALUE 'COLOR', "TO MAINTAIN COLOR VALUE
           GC_CHK   TYPE CHAR01 VALUE 'X', "Check field
           GC_PTU   TYPE CHAR01 VALUE 'U', "update type field
           GC_PTQ   TYPE CHAR01 VALUE 'Q', "Queue type field
           GC_PTM   TYPE CHAR01 VALUE 'M', "multiple type field
           GC_PTE   TYPE CHAR01 VALUE 'E', "Error type field
           GC_LTYA  TYPE CHAR01 VALUE 'A', "Layout type field
           GC_S     TYPE CHAR01 VALUE 'S', "short text field
           GC_OUTTAB TYPE SLIS_TABNAME VALUE 'OUTTAB', "Table name
           GC_GJAHR TYPE SLIS_FIELDNAME VALUE 'GJAHR', "field name
           GC_HBKID TYPE SLIS_FIELDNAME VALUE 'HBKID', "field name
           GC_RZAWE TYPE SLIS_FIELDNAME VALUE 'RZAWE'. "field name
************************************************************************
***    End of ALV Conversion                     Retrofit
************************************************************************


*----------------------------------------------------------------------*
* Selections                                                           *
*----------------------------------------------------------------------*
  SELECT-OPTIONS:
    S_ZBUK FOR PAYR-ZBUKR,       "Paying company code
    S_BANK FOR PAYR-HBKID,       "Short key for a house bank
    S_ACCO FOR PAYR-HKTID.       "ID for account details

SELECTION-SCREEN:
SKIP 1,
BEGIN OF BLOCK BL0 WITH FRAME TITLE TEXT-001.
* TEXT-001: 'General selections'
  SELECT-OPTIONS:
    S_CHEC FOR PAYR-CHECT,       "Check number
    S_VBLN FOR PAYR-VBLNR,       "Document number
    S_ZALD FOR RFSDO-CHKLADAT,   "Check date
    S_CPUD FOR RFSDO-CHKLEDAT,   "Print date
    S_USER FOR RFSDO-CHKLUSER,   "Print user
    S_ZWEG FOR PAYR-RZAWE.       "Payment method
  SELECTION-SCREEN:
  BEGIN OF LINE,
    COMMENT 01(31) TEXT-002 FOR FIELD ZW_LAUFD.
*   TEXT-002: 'Checks from a pay run'
    PARAMETERS:
      ZW_LAUFD LIKE PAYR-LAUFD.  "Date on which the program is to be run
    SELECTION-SCREEN POSITION 46.
    PARAMETERS:
      ZW_LAUFI LIKE PAYR-LAUFI,  "Additional identification
      ZW_XVORL LIKE REGUH-XVORL NO-DISPLAY.
    SELECTION-SCREEN:
  END OF LINE,
END OF BLOCK BL0.

SELECTION-SCREEN:
BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-003,
* TEXT-003: 'Selection of a target field for the check number'
  BEGIN OF LINE,
    POSITION 1.
*   Fill the reference document number with the check number
    PARAMETERS: P_XBLNR RADIOBUTTON GROUP FUNC DEFAULT 'X'.
    SELECTION-SCREEN: COMMENT 3(40) IL_XBLNR FOR FIELD P_XBLNR,
  END OF LINE,
  BEGIN OF LINE,
    POSITION 1.
*   Fill the allocation number with the check number
    PARAMETERS: P_ZUONR RADIOBUTTON GROUP FUNC.
    SELECTION-SCREEN: COMMENT 3(40) IL_ZUONR FOR FIELD P_ZUONR,
  END OF LINE,
  BEGIN OF LINE,
    POSITION 1.
*   Fill the reference key for line item with the check number
    PARAMETERS: P_XREF3 RADIOBUTTON GROUP FUNC.
    SELECTION-SCREEN: COMMENT 3(40) IL_XREF3 FOR FIELD P_XREF3,
  END OF LINE,
**BEGIN OF LINE, POSITION 1.
**  PARAMETERS: Further_Parameter RADIOBUTTON GROUP FUNC.
**  SELECTION-SCREEN: COMMENT 3(50) Label_Variable.
**END OF LINE,
END OF BLOCK BL1.

*----------------------------------------------------------------------*
*  Initialization                                                      *
*----------------------------------------------------------------------*
INITIALIZATION.
  REFRESH T_LAUFK.
  T_LAUFK-LAUFK = 'W'.
  T_LAUFK-SIGN  = 'E'.
  APPEND T_LAUFK.
  PERFORM GET_LABEL USING 'PAYR' 'S': 'ZBUKR' IS_ZBUKR,
                                      'VBLNR' IS_VBLNR,
                                      'GJAHR' IS_GJAHR,
                                      'HBKID' IS_HBKID,
                                      'HKTID' IS_HKTID,
                                      'RZAWE' IS_RZAWE.
  PERFORM GET_LABEL USING 'PAYR' 'M': 'CHECT' IM_CHECT.
  PERFORM GET_LABEL USING 'BKPF' 'L': 'XBLNR' IL_XBLNR.
  PERFORM GET_LABEL USING 'BSEG' 'L': 'ZUONR' IL_ZUONR,
                                      'XREF3' IL_XREF3.
************************************************************************
***    Begin of ALV Conversion                   Retrofit
************************************************************************
  gv_repid = sy-repid.
************************************************************************
***    End of ALV Conversion                     Retrofit
************************************************************************
*----------------------------------------------------------------------*
* Check Of The Selections                                              *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON S_ZBUK.
  SELECT * FROM T001 WHERE BUKRS IN S_ZBUK.
    AUTHORITY-CHECK OBJECT 'F_PAYR_BUK' ID 'BUKRS' FIELD T001-BUKRS
                                        ID 'ACTVT' FIELD '02'.
*   Authority check
    IF SY-SUBRC <> 0.
      SET CURSOR FIELD 'S_ZBUK'.
      MESSAGE E515 WITH S_ZBUK.
     ENDIF.
  ENDSELECT.
* Company code check
  IF SY-SUBRC <> 0.
    MESSAGE E511 WITH S_ZBUK.
  ENDIF.

*---------------------------------------------------------------------*
* F1-Help For Radiobuttons                                            *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON HELP-REQUEST FOR P_XBLNR.
  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
       EXPORTING CALLED_FOR_TAB   = 'BKPF'
                 CALLED_FOR_FIELD = 'XBLNR'.
AT SELECTION-SCREEN ON HELP-REQUEST FOR P_ZUONR.
  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
       EXPORTING CALLED_FOR_TAB   = 'BSEG'
                 CALLED_FOR_FIELD = 'ZUONR'.
AT SELECTION-SCREEN ON HELP-REQUEST FOR P_XREF3.
  CALL FUNCTION 'HELP_OBJECT_SHOW_FOR_FIELD'
       EXPORTING CALLED_FOR_TAB   = 'BSEG'
                 CALLED_FOR_FIELD = 'XREF3'.


*---------------------------------------------------------------------*
* F4 For Payment Method                                               *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR ZW_LAUFD.
  CALL FUNCTION 'F4_ZAHLLAUF'
       EXPORTING F1TYP = 'D'
                 F1NME = 'ZW_LAUFD'
                 F2NME = 'ZW_LAUFI'
       IMPORTING LAUFD = ZW_LAUFD
                 LAUFI = ZW_LAUFI
       TABLES    LAUFK = T_LAUFK.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR ZW_LAUFI.
  CALL FUNCTION 'F4_ZAHLLAUF'
       EXPORTING F1TYP   = 'I'
                 F1NME   = 'ZW_LAUFI'
                 F2NME   = 'ZW_LAUFD'
       IMPORTING LAUFD   = ZW_LAUFD
                 LAUFI   = ZW_LAUFI
       TABLES    LAUFK   = T_LAUFK.

*----------------------------------------------------------------------*
* List Title                                                           *
*----------------------------------------------------------------------*
*>>>>>commented Retrofit
*TOP-OF-PAGE.
*  FORMAT COLOR COL_BACKGROUND INTENSIFIED ON.
*  CASE I_PTYPE.
*    WHEN 'U'.
*      IF     P_XBLNR = 'X'.
*        WRITE TEXT-004.
**       TEXT-004: 'Reference document numbers with check numbers'
*      ELSEIF P_ZUONR = 'X'.
*        WRITE TEXT-005.
**       TEXT-005: 'Allocation numbers with check numbers'
*      ELSEIF P_XREF3 = 'X'.
*        WRITE TEXT-006.
**       TEXT-006: 'Reference keys for line items with check numbers'
***    ELSEIF Further_Parameter = 'X'.
***      WRITE Text_element_with_list_header_text.
*      ENDIF.
*    WHEN 'Q'.
*      WRITE TEXT-007.
**     TEXT-007: 'By other users closed documents'
*    WHEN 'E'.
*      WRITE TEXT-008.
**     TEXT-008: 'Error with the document update'
*    WHEN 'M'.
*      WRITE text-009.
**     TEXT-009: 'Maintain the check reference manually'
*  ENDCASE.
*  WRITE 84 SY-PAGNO.
*  FORMAT COLOR COL_HEADING INTENSIFIED ON.
*  WRITE: / SY-ULINE(88),
*         /01 SY-VLINE NO-GAP, IS_ZBUKR,   "Company code
*          12 SY-VLINE NO-GAP, IS_VBLNR,   "Document number
*          23 SY-VLINE NO-GAP, IS_GJAHR,   "Fiscal Year
*          34 SY-VLINE NO-GAP, IS_HBKID,   "Bank key
*          45 SY-VLINE NO-GAP, IS_HKTID,   "Bank account key
*          56 SY-VLINE NO-GAP, IS_RZAWE,   "payment method
*          67 SY-VLINE NO-GAP, IM_CHECT,   "Check number
*          88 SY-VLINE NO-GAP,
*         /   SY-ULINE(88).
*
*END-OF-PAGE.
*  IF I_PAGEEND = 'X'.
*    WRITE: / SY-ULINE(88).
*  ENDIF.
*<<<<<<commented Retrofit
*----------------------------------------------------------------------*
* Main Program                                                        *
*----------------------------------------------------------------------*
START-OF-SELECTION.
************************************************************************
***    Begin of ALV Conversion                   Retrofit
************************************************************************
  PERFORM CREATE_BLOCK_LIST_ALV USING gv_repid.
************************************************************************
***    End of ALV Conversion                     Retrofit
************************************************************************
* Conversion of the payment method Parameters into Select-Options
  IF NOT ZW_LAUFD IS INITIAL.
    CLEAR R_LAUFD.
    R_LAUFD-LOW    = ZW_LAUFD.
    R_LAUFD-OPTION = 'EQ'.
    R_LAUFD-SIGN   = 'I'.
    APPEND R_LAUFD.
  ENDIF.
  IF NOT ZW_LAUFI IS INITIAL.
    CLEAR R_LAUFI.
    R_LAUFI-LOW    = ZW_LAUFI.
    R_LAUFI-OPTION = 'EQ'.
    R_LAUFI-SIGN   = 'I'.
    APPEND R_LAUFI.
  ENDIF.

* Reading of the payment transfer mediums, whose numbers are to filled
* in the selected field of document
  SELECT * FROM PAYR INTO TABLE T_PAYR
         WHERE IREFE =  SPACE
           AND VOIDR =  0
           AND LAUFD IN R_LAUFD
           AND LAUFI IN R_LAUFI
           AND ZBUKR IN S_ZBUK
           AND HBKID IN S_BANK
           AND HKTID IN S_ACCO
           AND RZAWE IN S_ZWEG
           AND CHECT IN S_CHEC
           AND ZALDT IN S_ZALD
           AND PRIDT IN S_CPUD
           AND PRIUS IN S_USER
           AND VBLNR IN S_VBLN
           AND VBLNR <> SPACE.

  LOOP AT T_PAYR.
    CLEAR: T_BKPF, T_BSEG.
    CLEAR: xbseg, ybseg, *bkpf, bkpf.
    REFRESH: xbseg, ybseg.
    REFRESH  t_bkpf.
*   if a payment document is referenced more than once,
*   it is not possible to determine the check number
    SELECT * FROM payr UP TO 2 ROWS
       WHERE zbukr = t_payr-zbukr
         AND vblnr = t_payr-vblnr
         AND gjahr = t_payr-gjahr
        AND voidr = 0.
    ENDSELECT.
    IF sy-dbcnt > 1.
      MOVE-CORRESPONDING t_payr TO t_multi.
      APPEND t_multi.
      CONTINUE.
    ENDIF.
*   Reading of the document header
    SELECT SINGLE * FROM BKPF INTO T_BKPF
           WHERE BUKRS = T_PAYR-ZBUKR
             AND BELNR = T_PAYR-VBLNR
             AND GJAHR = T_PAYR-GJAHR.
    CHECK SY-SUBRC = 0.
    *BKPF = T_BKPF.
*   Fill a field of the BKPF table with the check number
    IF P_XBLNR = 'X'.
*     Fill the reference document number with the check number
      T_BKPF-XBLNR = T_PAYR-CHECT.
      I_REFKZ = '1'.
**  ELSEIF Further_Parameter_Of_A_BKPF_Field = 'X'.
**    t_bkpf-Document_Field_Name = t_payr-chect.
**    i_refkz = Defined_Reference_Item.
    ELSE.
*   Fill a field of the BSEG table with the check number
*     Reading of the document items
      SELECT * FROM BSEG INTO TABLE T_BSEG
             WHERE BUKRS = T_PAYR-ZBUKR
               AND BELNR = T_PAYR-VBLNR
               AND GJAHR = T_PAYR-GJAHR
               AND ( HKONT = T_PAYR-UBHKT or
                     UMSKZ = 'W' ).
      CHECK SY-SUBRC = 0.
      LOOP AT T_BSEG.
        MOVE-CORRESPONDING T_BSEG TO W_YBSEG.
        APPEND W_YBSEG TO YBSEG.
        IF     P_ZUONR = 'X'.
*         Fill the allocation number with the check number
          T_BSEG-ZUONR = T_PAYR-CHECT.
          I_REFKZ = '2'.
        ELSEIF P_XREF3 = 'X'.
*         Fill the reference key for line item with the check number
          T_BSEG-XREF3 = T_PAYR-CHECT.
          I_REFKZ = '3'.
**      ELSEIF Further_Parameter_Of_A_BSEG_Field = 'X'.
**        t_bseg-Document_Field_Name = t_payr-chect.
**        i_refkz = Defined_Reference_Item.
        ENDIF.
        MODIFY T_BSEG.
        MOVE-CORRESPONDING T_BSEG TO W_XBSEG.
        APPEND W_XBSEG TO XBSEG.
      ENDLOOP.
      UPD_BSEG = 'U'.
    ENDIF.
    BKPF = T_BKPF.
    APPEND T_BKPF.
    UPD_BKPF = 'U'.
*   Enqueue document for update
    CALL FUNCTION 'ENQUEUE_EFBKPF'
         EXPORTING  MODE_BKPF      = 'E'
                    MANDT          = SY-MANDT
                    BUKRS          = T_BKPF-BUKRS
                    BELNR          = T_BKPF-BELNR
                    GJAHR          = T_BKPF-GJAHR
         EXCEPTIONS FOREIGN_LOCK   = 1
                    SYSTEM_FAILURE = 2
                    OTHERS         = 3.
    IF SY-SUBRC = 0.
*      Enqueque payment transfer medium for update
       CALL FUNCTION 'ENQUEUE_EFPAYR'
            EXPORTING  MODE_PAYR      = 'E'
                       MANDT          = SY-MANDT
                       ZBUKR          = T_PAYR-ZBUKR
                       HBKID          = T_PAYR-HBKID
                       HKTID          = T_PAYR-HKTID
                       RZAWE          = T_PAYR-RZAWE
                       CHECT          = T_PAYR-CHECT
            EXCEPTIONS FOREIGN_LOCK   = 1
                       SYSTEM_FAILURE = 2
                       OTHERS         = 3.
       IF SY-SUBRC = 0.
*        Document update
         CALL FUNCTION 'CHANGE_DOCUMENT'
              TABLES     T_BKDF = T_BKDF
                         T_BKPF = T_BKPF
                         T_BSEC = T_BSEC
                         T_BSED = T_BSED
                         T_BSEG = T_BSEG
                         T_BSET = T_BSET
              EXCEPTIONS OTHERS = 4.
         IF SY-SUBRC = 0.
*          Update of payment transfer medium
           UPDATE PAYR SET IREFE = I_REFKZ
                  WHERE ZBUKR = T_PAYR-ZBUKR
                    AND HBKID = T_PAYR-HBKID
                    AND HKTID = T_PAYR-HKTID
                    AND RZAWE = T_PAYR-RZAWE
                    AND CHECT = T_PAYR-CHECT.
           IF SY-SUBRC = 0.
*            Fill the update protocol item table
             MOVE-CORRESPONDING T_PAYR TO T_UPDATE.
             APPEND T_UPDATE.
*            Create a change document for the document modifications
             OBJECTID = BKPF(21).
             TCODE    = 'FCHU'.
             UTIME    = SY-UZEIT.
             UDATE    = SY-DATUM.
             USERNAME = SY-UNAME.
             SET UPDATE TASK LOCAL.
             PERFORM CD_CALL_BELEG.
             COMMIT WORK.
           ELSE.
             ROLLBACK WORK.
             MOVE-CORRESPONDING T_PAYR TO T_ERROR.
             APPEND T_ERROR.
           ENDIF.
         ELSE.
           ROLLBACK WORK.
           MOVE-CORRESPONDING T_PAYR TO T_ERROR.
           APPEND T_ERROR.
         ENDIF.
*        Dequeque payment transfer medium after update
         CALL FUNCTION 'DEQUEUE_EFPAYR'
              EXPORTING MODE_PAYR = 'E'
                        MANDT     = SY-MANDT
                        ZBUKR     = T_PAYR-ZBUKR
                        HBKID     = T_PAYR-HBKID
                        HKTID     = T_PAYR-HKTID
                        RZAWE     = T_PAYR-RZAWE
                        CHECT     = T_PAYR-CHECT.
      ELSE.
        MOVE-CORRESPONDING T_PAYR TO T_QUEUE.
        APPEND T_QUEUE.
      ENDIF.
*     Dequeue document after update
      CALL FUNCTION 'DEQUEUE_EFBKPF'
           EXPORTING MODE_BKPF = 'E'
                     MANDT     = SY-MANDT
                     BUKRS     = T_BKPF-BUKRS
                     BELNR     = T_BKPF-BELNR
                     GJAHR     = T_BKPF-GJAHR.
    ELSE.
      MOVE-CORRESPONDING T_PAYR TO T_QUEUE.
      APPEND T_QUEUE.
    ENDIF.
  ENDLOOP.
*>>>>>>>>>>>>>commented Retrofit
** Protocol output of updated documents
*  LOOP AT T_UPDATE.
*    AT FIRST.
*      CLEAR I_NOPROT.
*      I_PTYPE   = 'U'.
*      NEW-PAGE.
*    ENDAT.
*    PERFORM PROTOCOL_OUTPUT USING T_UPDATE.
*    AT LAST.
*      WRITE: / SY-ULINE(88).
*      CLEAR I_PAGEEND.
*    ENDAT.
*  ENDLOOP.
** Protocol output of closed documents
*  LOOP AT T_QUEUE.
*    AT FIRST.
*      I_PAGEEND = 'X'.
*      I_PTYPE   = 'Q'.
*      NEW-PAGE.
*    ENDAT.
*    PERFORM PROTOCOL_OUTPUT USING T_QUEUE.
*    AT LAST.
*      WRITE: / sy-uline(88).
*      CLEAR i_pageend.
*    ENDAT.
*  ENDLOOP.
** Protocol output of multiple checks for
** one payment document.
*  LOOP AT t_multi.
*    AT FIRST.
*      i_pageend = 'X'.
*      i_ptype   = 'M'.
*      NEW-PAGE.
*    ENDAT.
*    PERFORM protocol_output USING t_multi.
*    AT LAST.
*      WRITE: / SY-ULINE(88).
*      CLEAR I_PAGEEND.
*    ENDAT.
*  ENDLOOP.
** Protocol output of update errors
*  LOOP AT T_ERROR.
*    AT FIRST.
*      I_PAGEEND = 'X'.
*      I_PTYPE   = 'E'.
*      NEW-PAGE.
*    ENDAT.
*    PERFORM PROTOCOL_OUTPUT USING T_ERROR.
*    AT LAST.
*      WRITE: / SY-ULINE(88).
*      CLEAR I_PAGEEND.
*    ENDAT.
*  ENDLOOP.
** Check whether a document field was filled with a check number
*  IF I_NOPROT = 'X'.
*    MESSAGE I321.
*  ENDIF.
*<<<<<<<<<<<<<commented Retrofit
************************************************************************
***    Begin of ALV Conversion                   Retrofit
************************************************************************
  PERFORM OUTPUT_DOCUMENTS_ALV.
***Display all available lists
  IF GV_COUNT GT 0.
    PERFORM DISPLAY_BLOCK_LIST_ALV.
  ENDIF.
************************************************************************
***    End of ALV Conversion                     Retrofit
************************************************************************

* 'form cd_call_beleg'
INCLUDE FF05LCDC.

*----------------------------------------------------------------------*
* Write A Protocol Item                                                *
*----------------------------------------------------------------------*
*>>>>>>>>>>>>>commented Retrofit
*FORM PROTOCOL_OUTPUT USING PLINE TYPE PROTOCOL_LINE.
*  IF I_INTENSIFIED = SPACE.
*    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*    I_INTENSIFIED = 'X'.
*  ELSE.
*    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*    I_INTENSIFIED = ' '.
*  ENDIF.
*  WRITE: /01 SY-VLINE NO-GAP,
*             PLINE-ZBUKR NO-GAP, 12 SY-VLINE NO-GAP,
*             PLINE-VBLNR NO-GAP, 23 SY-VLINE NO-GAP,
*             PLINE-GJAHR NO-GAP, 34 SY-VLINE NO-GAP,
*             PLINE-HBKID NO-GAP, 45 SY-VLINE NO-GAP,
*             PLINE-HKTID NO-GAP, 56 SY-VLINE NO-GAP,
*             PLINE-RZAWE NO-GAP, 67 SY-VLINE NO-GAP,
*             PLINE-CHECT NO-GAP, 88 SY-VLINE NO-GAP.
*ENDFORM.
*<<<<<<<<<<<commented Retrofit

*----------------------------------------------------------------------*
* Get The Data Dictionary Label Of A Table Field                       *
*----------------------------------------------------------------------*
FORM GET_LABEL USING    TABLE_NAME TYPE DDOBJNAME
                        LABEL_TYPE TYPE CLIKE
                        FIELD_NAME TYPE DFIES-FIELDNAME
               CHANGING LABEL      TYPE CLIKE.
  DATA: lt_dfies like dfies occurs 0 with header line.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
       EXPORTING  TABNAME        = TABLE_NAME
                  FIELDNAME      = FIELD_NAME
                  LANGU          = SY-LANGU
       tables     DFIES_tab      = lt_dfies
       EXCEPTIONS NOT_FOUND      = 1
                  INTERNAL_ERROR = 2
                  OTHERS         = 3.
  IF SY-SUBRC = 0.
    read table lt_dfies index 1.
    CASE LABEL_TYPE.
      WHEN 'S'. LABEL = lt_dfies-SCRTEXT_S.
      WHEN 'M'. LABEL = lt_dfies-SCRTEXT_M.
      WHEN 'L'. LABEL = lt_dfies-SCRTEXT_L.
    ENDCASE.
  ENDIF.
ENDFORM.
************************************************************************
***    Begin of ALV Conversion                   Retrofit
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  CREATE_BLOCK_LIST_ALV
*&---------------------------------------------------------------------*
*       Create block list for ALV
*----------------------------------------------------------------------*
*      -->IV_REPID  Report name field
*----------------------------------------------------------------------*
FORM CREATE_BLOCK_LIST_ALV USING IV_REPID TYPE sy-repid.
***Create block list
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_INIT'
    EXPORTING
      I_CALLBACK_PROGRAM = IV_REPID.
*
ENDFORM.                    " CREATE_BLOCK_LIST_ALV
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_DOCUMENTS_ALV
*&---------------------------------------------------------------------*
*    Build and display the ALV data
*----------------------------------------------------------------------*
FORM OUTPUT_DOCUMENTS_ALV .
*
  DATA: wa_payr type protocol_line.
***Build field catalog and layout for ALV
  PERFORM BUILD_FIELDCAT_ALV CHANGING GT_FIELDCAT. "Build Field Catalog
  PERFORM SET_LAYOUT_ALV     CHANGING GS_LAYO.     "to set layout
* Protocol output of updated documents
  IF NOT t_update is INITIAL.
    LOOP AT T_UPDATE INTO WA_PAYR.
      AT FIRST.
        CLEAR I_NOPROT.
        I_PTYPE   = GC_PTU.
      ENDAT.
      PERFORM PROTOCOL_OUTPUT_ALV USING WA_PAYR
                                  CHANGING GT_UPDATE.
      CLEAR WA_PAYR.
    ENDLOOP.
    GV_COUNT = GV_COUNT + 1.
    PERFORM DISPLAY_ALV TABLES GT_UPDATE.
  ENDIF.
* Protocol output of closed documents
  IF NOT t_queue is INITIAL.
    LOOP AT T_QUEUE INTO WA_PAYR.
      AT FIRST.
        I_PTYPE   = GC_PTQ.
      ENDAT.
      PERFORM PROTOCOL_OUTPUT_ALV USING WA_PAYR
                                  CHANGING GT_QUEUE.
      CLEAR WA_PAYR.
    ENDLOOP.
    GV_COUNT = GV_COUNT + 1.
    PERFORM DISPLAY_ALV TABLES GT_QUEUE.
  ENDIF.
* Protocol output of multiple checks for
* one payment document.
  IF NOT t_multi is INITIAL.
    LOOP AT t_multi INTO WA_PAYR.
      AT FIRST.
        I_PTYPE   = GC_PTM.
      ENDAT.
      PERFORM PROTOCOL_OUTPUT_ALV USING WA_PAYR
                                  CHANGING GT_MULTI.
      CLEAR WA_PAYR.
    ENDLOOP.
    GV_COUNT = GV_COUNT + 1.
    PERFORM DISPLAY_ALV TABLES GT_MULTI.
  ENDIF.
* Protocol output of update errors
  IF NOT t_error is INITIAL.
    LOOP AT T_ERROR INTO WA_PAYR.
      AT FIRST.
        I_PTYPE   = GC_PTE.
      ENDAT.
      PERFORM PROTOCOL_OUTPUT_ALV USING WA_PAYR
                                  CHANGING GT_ERROR.
      CLEAR WA_PAYR.
    ENDLOOP.
    GV_COUNT = GV_COUNT + 1.
    PERFORM DISPLAY_ALV TABLES GT_ERROR.
  ENDIF.
* Check whether a document field was filled with a check number
  IF I_NOPROT = GC_CHK.
    MESSAGE I321.
  ENDIF.
*
ENDFORM.                    " OUTPUT_DOCUMENTS_ALV
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT_ALV
*&---------------------------------------------------------------------*
*       Build field catalog
*----------------------------------------------------------------------*
*      <--XT_FIELDCAT  Storet the field catolog details
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT_ALV  CHANGING XT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
*
  DATA: LS_FCAT TYPE SLIS_FIELDCAT_ALV.

  CLEAR: xt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = gc_str
    CHANGING
      ct_fieldcat            = XT_FIELDCAT
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT XT_FIELDCAT INTO LS_FCAT.
    CASE LS_FCAT-FIELDNAME.
      WHEN GC_GJAHR OR GC_HBKID OR GC_RZAWE.
        LS_FCAT-DDICTXT = GC_S.
    ENDCASE.
    LS_FCAT-TABNAME = GC_OUTTAB.
    MODIFY XT_FIELDCAT FROM LS_FCAT.
    CLEAR LS_FCAT.
  ENDLOOP.
*
ENDFORM.                    " BUILD_FIELDCAT_ALV
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_ALV
*&---------------------------------------------------------------------*
*       Build a layout
*----------------------------------------------------------------------*
*      <--XS_LAYO  Store the layout details
*----------------------------------------------------------------------*
FORM SET_LAYOUT_ALV  CHANGING XS_LAYO TYPE SLIS_LAYOUT_ALV.
*
  XS_LAYO-NO_HOTSPOT        = GC_CHK.
  XS_LAYO-COLWIDTH_OPTIMIZE = space.
  XS_LAYO-INFO_FIELDNAME    = GC_COLOR.
*
ENDFORM.                    " SET_LAYOUT_ALV
*&---------------------------------------------------------------------*
*&      Form  PROTOCOL_OUTPUT_ALV
*&---------------------------------------------------------------------*
*       Moving the data to ALV table
*----------------------------------------------------------------------*
*      -->IW_PLINE  work area
*      <--XT_OUTTAB store the ALV data
*----------------------------------------------------------------------*
FORM PROTOCOL_OUTPUT_ALV  USING IW_PLINE TYPE PROTOCOL_LINE
                          CHANGING XT_OUTTAB LIKE GT_UPDATE.
*
  DATA: LS_OUTTAB TYPE FOAP_S_RFCHKU00_LIST.
  CLEAR LS_OUTTAB.
  MOVE IW_PLINE TO LS_OUTTAB.
  APPEND LS_OUTTAB TO XT_OUTTAB.
*
ENDFORM.                    " PROTOCOL_OUTPUT_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       Display the ALV block list
*----------------------------------------------------------------------*
*      -->IT_OUTTAB  Passing table name
*----------------------------------------------------------------------*
FORM DISPLAY_ALV TABLES IT_OUTTAB LIKE GT_UPDATE.
*
  DATA: LT_EVTS TYPE SLIS_T_EVENT.        "events table
***Build the events
  PERFORM SET_EVENTS_ALV CHANGING LT_EVTS.  "to set events
***Display the ALV Block list
  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_APPEND'
    EXPORTING
      IS_LAYOUT                  = GS_LAYO
      IT_FIELDCAT                = GT_FIELDCAT
      I_TABNAME                  = GC_OUTTAB
      IT_EVENTS                  = LT_EVTS
    TABLES
      T_OUTTAB                   = IT_OUTTAB
    EXCEPTIONS
      PROGRAM_ERROR              = 1
      MAXIMUM_OF_APPENDS_REACHED = 2
      OTHERS                     = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
*
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  SET_EVENTS_ALV
*&---------------------------------------------------------------------*
*       Build the events
*----------------------------------------------------------------------*
*      <--XT_EVTS  To store the events details
*----------------------------------------------------------------------*
FORM SET_EVENTS_ALV  CHANGING XT_EVTS TYPE SLIS_T_EVENT.
*
  DATA: LS_EVENTS TYPE SLIS_ALV_EVENT.
*** To get the top of list based on the ptype
  CASE I_PTYPE.
    WHEN GC_PTU.
      LS_EVENTS-NAME = SLIS_EV_TOP_OF_LIST.
      LS_EVENTS-FORM = GC_TOL_FORM1.
      APPEND LS_EVENTS TO XT_EVTS.
    WHEN GC_PTQ.
      LS_EVENTS-NAME = SLIS_EV_TOP_OF_LIST.
      LS_EVENTS-FORM = GC_TOL_FORM2.
      APPEND LS_EVENTS TO XT_EVTS.
    WHEN GC_PTE.
      LS_EVENTS-NAME = SLIS_EV_TOP_OF_LIST.
      LS_EVENTS-FORM = GC_TOL_FORM3.
      APPEND LS_EVENTS TO XT_EVTS.
    WHEN GC_PTM.
      LS_EVENTS-NAME = SLIS_EV_TOP_OF_LIST.
      LS_EVENTS-FORM = GC_TOL_FORM4.
      APPEND LS_EVENTS TO XT_EVTS.
  ENDCASE.
*
ENDFORM.                    " SET_EVENTS_ALV
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_LIST_ONE
*&---------------------------------------------------------------------*
* This subroutine is dynamically called and handles event TOP-OF-LIST
*----------------------------------------------------------------------*
FORM TOP_OF_LIST_ONE .                                      "#EC CALLED

  DATA: LT_LINE TYPE SLIS_T_LISTHEADER,  "list header
        LS_LINE TYPE SLIS_LISTHEADER.

  IF P_XBLNR = GC_CHK.
    ls_line-info = TEXT-004.
  ELSEIF P_ZUONR = GC_CHK.
    ls_line-info = TEXT-005.
  ELSEIF P_XREF3 = GC_CHK.
    ls_line-info = TEXT-006.
  ENDIF.
  ls_line-typ = GC_LTYA.
  append ls_line to lt_line.
  clear ls_line.
***To display the list header details for ALV list
  PERFORM DISPLAY_LIST_HEADER_ALV USING LT_LINE.
ENDFORM.                    " top_of_list_ONE
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_LIST_TWO
*&---------------------------------------------------------------------*
* This subroutine is dynamically called and handles event TOP-OF-LIST
*----------------------------------------------------------------------*
FORM TOP_OF_LIST_TWO.                                       "#EC CALLED

  DATA: LT_LINE TYPE SLIS_T_LISTHEADER,  "list header
        LS_LINE TYPE SLIS_LISTHEADER.

  ls_line-typ = GC_LTYA.
  ls_line-info = TEXT-007.
  append ls_line to lt_line.
  clear ls_line.
***To display the list header details for ALV list
  PERFORM DISPLAY_LIST_HEADER_ALV USING LT_LINE.

ENDFORM.                    " top_of_list_TWO
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_LIST_THREE
*&---------------------------------------------------------------------*
* This subroutine is dynamically called and handles event TOP-OF-LIST
*----------------------------------------------------------------------*
FORM TOP_OF_LIST_THREE.                                     "#EC CALLED

  DATA: LT_LINE TYPE SLIS_T_LISTHEADER,  "list header
        LS_LINE TYPE SLIS_LISTHEADER.

  ls_line-typ = GC_LTYA.
  ls_line-info = TEXT-008.
  append ls_line to lt_line.
  clear ls_line.
***To display the list header details for ALV list
  PERFORM DISPLAY_LIST_HEADER_ALV USING LT_LINE.
ENDFORM.                    " top_of_list_THREE
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_LIST_FOUR
*&---------------------------------------------------------------------*
* This subroutine is dynamically called and handles event TOP-OF-LIST
*----------------------------------------------------------------------*
FORM TOP_OF_LIST_FOUR.                                      "#EC CALLED

  DATA: LT_LINE TYPE SLIS_T_LISTHEADER,  "list header
        LS_LINE TYPE SLIS_LISTHEADER.

  ls_line-typ = GC_LTYA.
  ls_line-info = TEXT-009.
  append ls_line to lt_line.
  clear ls_line.
***To display the list header details for ALV list
  PERFORM DISPLAY_LIST_HEADER_ALV USING LT_LINE.
ENDFORM.                    " top_of_list_FOUR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST_HEADER_ALV
*&---------------------------------------------------------------------*
* This subroutine prints the header details of list.
*----------------------------------------------------------------------*
*  -->  IT_LINE, is of Type slis_t_listheader, to store header details
*----------------------------------------------------------------------*
FORM DISPLAY_LIST_HEADER_ALV USING
                        IT_LINE TYPE SLIS_T_LISTHEADER.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_LINE.
  write at 65 sy-pagno.
ENDFORM.                    " DISPLAY_LIST_HEADER_ALV
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_BLOCK_LIST_ALV
*&---------------------------------------------------------------------*
*       Display the ALV block list
*----------------------------------------------------------------------*
FORM DISPLAY_BLOCK_LIST_ALV .

  CALL FUNCTION 'REUSE_ALV_BLOCK_LIST_DISPLAY'
    EXCEPTIONS
      PROGRAM_ERROR = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " DISPLAY_BLOCK_LIST_ALV
************************************************************************
***    End of ALV Conversion                     Retrofit
************************************************************************
