*----------------------------------------------------------------------*
* Check Reconsiliation Programm                                        *
*                                                                      *
*----------------------------------------------------------------------*
* ak220499 introduce secondary index for payr to solve performance-    *
*          issue  - see also note 0147810 -                            *
* ak020699 introduced currency in file-header definition & changed     *
* ak050500 Applied note 301177.                                        *
*          parameter transfer when called via submit by FCHR           *
* ak230899 Selection of items limited to BSEG-lines with cash-flow     *
*          relevant accounts.                                          *
* ak010999 Included country-code in protocol-header.                   *
*          Replaced form CURRENCY_CODE_ISO_TO_SAP with new function    *
*          EB_CURRENCY_CODE_ISO_TO_SAP.                                *
* ak281099 Introduced work-around to avoid cut-off of amount because   *
*          of too short field CKREC-AMOUNT ( only 11 characters,       *
*          should have 13 chars ).                                     *
* ak161199 Exceptions for function GET_BANK_ACCOUNT out-commented.     *
* ak290100 Applied update to note 147810.                              *
* mo271101 included changes for Web GUI compatibility (section 508)    *
*----------------------------------------------------------------------*
* Change history of ALV Development
* Program description: Conversion of Classic list into Hierarchy ,
*  Classic  and Append ALV List
* Author: Chenna Kiran Kumar (C5053249)
* Start date         : 16/02/2004
* End date           : 11/03/2004
* Short description of the program:This report displays an Classic ALV
* and Heirarchy ALV's appended to it and finally another classical ALV
* for displaying the statistics.
* FM's Used 'REUSE_ALV_LIST_DISPLAY',        CLASSIC alv
*           'REUSE_ALV_HIERSEQ_LIST_DISPLAY' HEIRARCHY ALV
* Strcutures Created: FAGL_s_rfebck00_list1,       "FILE DETAILS
*                     FAGL_s_rfebck00_list2,       "CHECK HEADER
*                     FAGL_s_rfebck00_list2_detail "CHECK DETAIL
*                     FAGL_s_rfebck00_stats        "STATISTICS
*                     FAGL_s_rfebck00_post         "POSTING HEADER
*                     FAGL_s_rfebck00_postdet      "POSTING DETAIL
*                     FAGL_s_rfebck00_hr           "HR CHECKS POSTING
*----------------------------------------------------------------------*
REPORT rfebck00 MESSAGE-ID fb
                LINE-SIZE 132
                NO STANDARD PAGE HEADING.


TABLES: bkpf,                          " Accounting document header
        bseg,                          " Accounting document segment
        skb1,              " G/L account master (company code)
        bhdgd,             " Common data area batch heading routin
        payr,                          " Payment transfer medium file
        tvoit,                         " Texts for void reasons
        t042i.                         " Verrechnungskonten Zahlprogramm

TABLES: rfdt.                    " Accounting data (INDX structure)

TABLES: t001,                          " Company codes
        t012,                          " House banks
        t012k,                         " House bank accounts
        t100.

CONSTANTS gc_filename                                       "n1517472
  TYPE fileintern                                           "n1517472
  VALUE 'FI_POSTING'.                                       "n1517472

****************** Konstanten **********************************
DATA: batchinput TYPE c VALUE 'B',
      calltrans  TYPE c VALUE 'C',
      mode       TYPE c VALUE 'N',
      true       TYPE c VALUE '0',
      false      TYPE c VALUE '8'.

****************** Arbeitsfelder *******************************
DATA: bkpf_komk_ok(1) TYPE c.                               "mp40a
DATA: tcode LIKE sy-tcode VALUE 'FB01'.                     "mp40a
DATA: total_prenums TYPE i.
DATA: total_voided_prenums TYPE i.
DATA: total_cashed_prenums TYPE i.
DATA: total_valid_prenums TYPE i.
DATA: total_hr_post TYPE i.
DATA: hr_total TYPE i.
DATA: hr_differing_amount TYPE i.
DATA: normal_prenums_total TYPE i.
DATA: oprenums_invalid_docno TYPE i.
DATA: lin_collect_tab TYPE i.
DATA: hr_total_post_fail TYPE i.
DATA: hr_total_post_success TYPE i.
DATA: hr_total_no_post TYPE i.
DATA: hr_post_proc TYPE i.
DATA: hr_post_man_check TYPE i.
DATA: hr_no_post_trans TYPE i.
DATA: hr_total_trans_success TYPE i.
DATA: hr_total_trans_fail TYPE i.
DATA: total_checks TYPE i.
DATA: total_fi_checks TYPE i.
DATA: total_unknown TYPE i.
DATA: total_multiple_prenums TYPE i.
DATA: oprenums_amount_ok TYPE i.
DATA: oprenums_amount_ne TYPE i.
DATA: valid_other_prenums TYPE i.
DATA: hr_cnt TYPE i.
DATA: prenums_no_valid_item TYPE i.
DATA: prenums_no_open_item TYPE i.
DATA: fi_no_valid_item TYPE i.
DATA: fi_no_open_item TYPE i.
DATA: valid_fi_op TYPE i.
DATA: fi_amount_ok TYPE i.
DATA: fi_amount_ne TYPE i.

DATA: sortkrit(35) TYPE c.
DATA: acc_ok(1) TYPE c.
DATA: amount_ok(1) TYPE c.

DATA: gd_cfr_flag TYPE c.                                   "ak230899

DATA: subrc LIKE sy-subrc,
      msgid LIKE sy-msgid,
      msgty LIKE sy-msgty,
      msgno LIKE sy-msgno,
      msgv1 LIKE sy-msgv1,
      msgv2 LIKE sy-msgv2,
      msgv3 LIKE sy-msgv3,
      msgv4 LIKE sy-msgv4.

DATA: agkon LIKE bseg-hkont.                       "30E
DATA: amount_chk LIKE bseg-wrbtr,
      amount_doc LIKE bseg-wrbtr,
      amount_sum LIKE bseg-wrbtr,
      gv_count   TYPE i.

DATA: belnr LIKE bkpf-belnr,
      bldat LIKE bkpf-bldat,
      budat LIKE bkpf-budat.
DATA: buzei LIKE bseg-buzei.                       "note 301177

DATA: char17(17)   TYPE c,
      char60(60)   TYPE c,
      char130(130) TYPE c.

DATA: err_cnt(5) TYPE n.

DATA: function   LIKE rfipi-funct,
      i_function LIKE rfipi-funct,
      fvalue     LIKE ftpost-fval.

DATA: gjahr     LIKE bkpf-gjahr,
      groupname LIKE apqi-groupid.

DATA: hbkid LIKE t012k-hbkid,
      hdcnt TYPE i,
      hktid LIKE t012k-hktid.

DATA: item_cnt TYPE i.

DATA: localcurr LIKE bkpf-waers.
DATA: xlocalcurr(1) TYPE c.                                 "mp45A

DATA: open(1) TYPE c.

DATA: payr_ok(1) TYPE c,
      pdate      LIKE payr-bancd.

DATA: status(1) TYPE c.

DATA: trans_cnt(5) TYPE n,
      tfill_blntab TYPE i,
      tfill_ftpost TYPE i.

DATA: valut LIKE sy-datum.

DATA: waers LIKE bkpf-waers.

DATA: xbdcc LIKE rfipi-xbdcc.
DATA: i_xbdcc LIKE rfipi-xbdcc.

****************** Interne Tabellen und Feldleisten ************
*---- checks       --------------------------------------------*
DATA: BEGIN OF checks OCCURS 100,
        rec(300) TYPE c,
      END OF checks.

*---- internal table for import from database -----------------*
DATA: BEGIN OF check_file OCCURS 50,
        record(128) TYPE c,
      END OF check_file.


*---- head record of check format -----------------------------*
DATA: BEGIN OF head.
    INCLUDE STRUCTURE check_head.
DATA: END OF head.

*----- single check record  -----------------------------------*
DATA: BEGIN OF ckrec.
    INCLUDE STRUCTURE check_rec.
DATA: END OF ckrec.

*----- trailer record  -----------------------------------------*
DATA: BEGIN OF trailer.
    INCLUDE STRUCTURE check_trl.
DATA: END OF trailer.

*------ Document header and items for internal posting interface
DATA:   BEGIN OF ftpost OCCURS 50.
    INCLUDE STRUCTURE ftpost.
DATA:   END OF ftpost.

*------ Clearing data for internal posting interface
DATA:   BEGIN OF ftclear OCCURS 100.
    INCLUDE STRUCTURE ftclear.
DATA:   END OF ftclear.

*------ Tax Data (not used but neccessary because its a table parameter)
DATA:   BEGIN OF fttax OCCURS 0.
    INCLUDE STRUCTURE fttax.
DATA:   END OF fttax.

*------ Financial Accounting document number table
DATA:   BEGIN OF blntab OCCURS 0.
    INCLUDE STRUCTURE blntab.
DATA:   END OF blntab.



DATA: BEGIN OF p_ftpost OCCURS 2,
        item(132) TYPE c,
      END OF p_ftpost.

DATA: BEGIN OF p_ftclear OCCURS 100,
        belnr  LIKE bkpf-belnr,
        waers  LIKE bkpf-waers,
        amount LIKE bseg-wrbtr,
*         ITEM(130) TYPE C,
      END OF p_ftclear.

DATA: BEGIN OF errors OCCURS 50,
        item(132) TYPE c,
      END OF errors.

DATA: BEGIN OF collect_tab OCCURS 0,
        bukrs LIKE t012-bukrs,
        hbkid LIKE t012-hbkid,
        hktid LIKE t012k-hktid,
        chect LIKE ckrec-cknum,
      END OF collect_tab.

********************Begin of Coding ALV C5053249*****************
TYPE-POOLS: slis.

TYPES: BEGIN OF yt_chkhdr,
         head_rec(3) TYPE n.                     "header record number
    INCLUDE STRUCTURE fagl_s_rfebck00_list2.
TYPES: END OF yt_chkhdr.

TYPES: BEGIN OF yt_post_info,
         status       LIKE apqi-groupid,         "Check Status
         char130(130) TYPE c,                    "message text
       END OF yt_post_info.

*** ALV list for displaying the Sequential file details**********
DATA:gt_outtab_file        TYPE STANDARD TABLE OF fagl_s_rfebck00_list1,
     "File list structure internal table
     gs_outtab_file        TYPE fagl_s_rfebck00_list1,
     "File structure work area
*** ALV list for displaying the Check processing details for
*** Company code, Bank Account and Bank Key - Header
     gt_outtab_chkhdr      TYPE STANDARD TABLE OF yt_chkhdr,
     "internal table for check header
     gs_outtab_chkhdr      TYPE yt_chkhdr,
     "work area for check header
*** ALV list for displaying the Check processing details for
*** Company code, Bank Account and Bank Key - Check Messages
     gt_outtab_chkdet      TYPE STANDARD TABLE OF
                          fagl_s_rfebck00_list2_detail,
     "internal table for check detail

     gs_outtab_chkdet      TYPE fagl_s_rfebck00_list2_detail,
     "work area for check detail
*** ALV list for displaying the statistics
     gt_outtab_stats       TYPE STANDARD TABLE OF fagl_s_rfebck00_stats,
     gs_outtab_stats       TYPE fagl_s_rfebck00_stats,
*** ALV list for displaying Posting Log Header (OK records)
     gt_outtab_post_ok     TYPE STANDARD TABLE OF fagl_s_rfebck00_post,
     gs_outtab_post_ok     TYPE fagl_s_rfebck00_post,
*** ALV list for displaying Posting Log Header (ERR records)
     gt_outtab_post_err    TYPE STANDARD TABLE OF fagl_s_rfebck00_post,
     gs_outtab_post_err    TYPE fagl_s_rfebck00_post,
*** ALV List for posting Log detail
     gt_outtab_postdet_ok  TYPE STANDARD TABLE OF
                          fagl_s_rfebck00_postdet,
     gs_outtab_postdet_ok  TYPE fagl_s_rfebck00_postdet,
*** ALV List for posting Log detail
     gt_outtab_postdet_err TYPE STANDARD TABLE OF
                           fagl_s_rfebck00_postdet,
     gs_outtab_postdet_err TYPE fagl_s_rfebck00_postdet,
*** ALV List for HR posting log
     gt_outtab_hrpost      TYPE STANDARD TABLE OF fagl_s_rfebck00_hr,
     gs_outtab_hrpost      TYPE fagl_s_rfebck00_hr,
*** END of page messages for HR posting Log
     gt_tab_end            TYPE STANDARD TABLE OF fagl_s_rfebck00_hr,
     gs_tab_end            TYPE fagl_s_rfebck00_hr.
*** Posting info headers
DATA:gt_tab_postinfo TYPE STANDARD TABLE OF yt_post_info,
     gs_tab_postinfo TYPE                   yt_post_info.

DATA: gt_header1 TYPE slis_t_listheader, "Top of page header
      gs_line1   TYPE slis_listheader.   "Header work area

DATA: gv_repid       TYPE sy-repid,  "program name
      gv_count_list1 TYPE i,         "Entries in List 1
      gv_count_a     TYPE i,         "List index to check for append
      gv_count_b     TYPE i,         "List index to check for append
      gv_flag        TYPE c.         "switch ON/OFF

DATA: gv_count_list2_hdr TYPE i,   "Entries in List 2 hdr"#EC NEEDED
      gv_count_list2_det TYPE i,   "Entries in List 2 det"#EC NEEDED
      gv_count_list3_hdr TYPE i,   "Entries in List 3 hdr"#EC NEEDED
      gv_count_list3_det TYPE i,   "Entries in List 3 det"#EC NEEDED
      gv_count_list4_hdr TYPE i,   "Entries in List 4 hdr"#EC NEEDED
      gv_count_list4_det TYPE i,   "Entries in List 4 det"#EC NEEDED
      gv_count_list5     TYPE i.   "Entries in List 5    "#EC NEEDED

DATA order TYPE i VALUE 0.                                  "857884

CONSTANTS:
  gc_struc1                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_LIST1',
  gc_struc2                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_LIST2',
  gc_struc3                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_LIST2_DETAIL',
  gc_struc4                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_STATS',
  gc_struc5                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_POST',
  gc_struc6                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_POSTDET',
  gc_struc7                 TYPE dd02l-tabname VALUE
                                       'FAGL_S_RFEBCK00_HR',

  gc_value_x                TYPE c VALUE 'X' ,        " Value X
  gc_value_y                TYPE c VALUE 'Y' ,        " Value Y
  gc_form_end_of_list(30)   TYPE c
                   VALUE 'END_OF_ALV_LIST', "End of List1
  gc_form_top_of_page1(30)  TYPE c
                  VALUE 'TOP_OF_PAGE1', "top of page -list1
  gc_form_top_of_page2(30)  TYPE c
                  VALUE 'TOP_OF_PAGE2', "top of page -list2
  gc_form_top_of_page3(30)  TYPE c
                  VALUE 'TOP_OF_PAGE3', "top of page -list3
  gc_form_top_of_page4(30)  TYPE c
                  VALUE 'TOP_OF_PAGE4', "top of page -list4
  gc_form_top_of_page5(30)  TYPE c
                  VALUE 'TOP_OF_PAGE5', "top of page -list5
  gc_form_top_of_page6(30)  TYPE c
                  VALUE 'TOP_OF_PAGE6', "top of page -list6
  gc_form_end_of_list5(30)  TYPE c
                    VALUE 'END_OF_LIST5', "end of page -list5
  gc_form_set_pf_status(30) TYPE c
                   VALUE 'STATUS_MAIN',  "GUI status
  gc_line_size              TYPE sylinsz VALUE '255',      " Line size.
  gc_value_1                TYPE i VALUE 1,
  gc_value_0                TYPE i VALUE 0,
  gc_ok(4)                  TYPE c VALUE '-OK ',    "status
  gc_err(4)                 TYPE c VALUE '-ERR',    "status
  gc_hr(4)                  TYPE c VALUE '-HR ',    "status
  gc_hrpost(6)              TYPE c VALUE 'HRPOST',
  gc_trans(5)               TYPE c VALUE 'TRANS',   "call transaction
  gc_info(8)                TYPE c VALUE 'POSTINFO',
*      gc_ul(2)         TYPE c VALUE 'UL'.  "Under line for Sort
  gc_3m                     TYPE slis_tabname VALUE '3M', "Tabname
  gc_3s                     TYPE slis_tabname VALUE '3S', "Tabname
  gc_2m                     TYPE slis_tabname VALUE '2M', "Tabname
  gc_2s                     TYPE slis_tabname VALUE '2S', "Tabname
  gc_l                      TYPE c VALUE 'L', "Ddictxt
  gc_s                      TYPE c VALUE 'S',
  gc_c                      TYPE c VALUE 'C', "centered
  gc_r                      TYPE c VALUE 'R'. "right justified
CONSTANTS:
  gc_tabname_header TYPE slis_tabname VALUE
                              'GT_OUTTAB_CHKHDR'.

********************End of Coding ALV C5053249*******************


****************** Extrakt-Definition ***************************
FIELD-GROUPS:
         header, datarec.

*insert   status hdcnt  valut  budat  bldat  waers   into header.
INSERT   status
         hdcnt
         valut
         budat
         bldat
         waers
         sortkrit
         xlocalcurr                                         "mp45A
                            INTO header.

INSERT   ckrec-cknum  belnr  amount_chk  amount_doc  pdate
*         T012K HBKID HKTID  AGKON                   INTO DATAREC."30E
         t012k hbkid hktid agkon gjahr buzei INTO datarec.  "note 301177
*        T012K HBKID HKTID                          INTO DATAREC.   "30E

*---------------------------------------------------------------*
*  Parameters                                                   *
*---------------------------------------------------------------*
*------- Dateiangaben -------------------------------------------------
SELECTION-SCREEN  BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-100.
PARAMETERS: ck_file  LIKE rlgrap-filename,
            pcupload LIKE rfpdo1-febpcupld,
            pnumc    LIKE febpdo-pnumc_ebck DEFAULT 'X'.
SELECTION-SCREEN  END OF BLOCK 1.

*------- Buchungsparameter ---------------------------------------------
SELECTION-SCREEN  BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-101.
SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS: post  LIKE febpdo-xcall_ebck RADIOBUTTON GROUP 1.
SELECTION-SCREEN
  COMMENT 03(29) TEXT-103 FOR FIELD post.
PARAMETERS: pa_mode  LIKE rfpdo-allgazmd NO-DISPLAY.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS: pa_xbdc  LIKE febpdo-xbinpt   RADIOBUTTON GROUP 1.
SELECTION-SCREEN
  COMMENT 03(29) TEXT-104 FOR FIELD pa_xbdc.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: testrun LIKE rfpdo1-febtestl RADIOBUTTON GROUP 1.
SELECTION-SCREEN
  COMMENT 03(29) TEXT-106 FOR FIELD testrun.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS: hr_post AS CHECKBOX.
SELECTION-SCREEN
  COMMENT 03(40) TEXT-105 FOR FIELD hr_post.
SELECTION-SCREEN: END OF LINE.
PARAMETERS: group(08)    TYPE c OBLIGATORY.
PARAMETERS: i_budat LIKE bkpf-budat DEFAULT sy-datum,
            i_bldat LIKE bkpf-bldat DEFAULT sy-datum,
            i_blart LIKE bkpf-blart OBLIGATORY.
SELECTION-SCREEN  END OF BLOCK 2.
*------- Ausgabeparameter ----------------------------------------------
SELECTION-SCREEN  BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-102.
PARAMETERS: p_file LIKE rfpdo1-febpausz,   " Print Checkfile
            p_prot LIKE rfpdo1-febpausz,   " Print long Protokoll
            p_post LIKE rfpdo1-febpausz.   " Print Posting Info
SELECTION-SCREEN  END OF BLOCK 3.

INITIALIZATION.
****************Begin of coding ALV C5053249********************
  gv_repid = sy-repid.
******************End of coding ALV C5053249********************
*---------------------------------------------------------------*
*  Parameters                                                   *
*---------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK 1.
  IF pcupload = 'X'.
    IF  ck_file+1(1) NE ':'.
      SET CURSOR FIELD 'CK_FILE'.
      MESSAGE e706.
    ENDIF.
  ENDIF.
  CHECK pcupload EQ space.                                  "n1605587
* begin "n1517472
  CALL FUNCTION 'FILE_VALIDATE_NAME'
    EXPORTING
      logical_filename  = gc_filename
      parameter_1       = sy-repid
*     parameter_2       = i_blart                           "n2071858
*     parameter_3       = group                             "n2071858
    CHANGING
      physical_filename = ck_file
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
* end "n1517472


AT SELECTION-SCREEN ON BLOCK 2.
  IF hr_post = 'X' AND post IS INITIAL.
    MESSAGE e072.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR ck_file.
  DATA: l_files TYPE filetable,                             "mo271101
        h_files TYPE file_table,                            "mo271101
        l_rc    LIKE sy-subrc.                              "mo271101
  CALL METHOD cl_gui_frontend_services=>file_open_dialog    "mo271101
    CHANGING                                                "mo271101
      file_table              = l_files                     "mo271101
      rc                      = l_rc                        "mo271101
    EXCEPTIONS                                              "mo271101
      file_open_dialog_failed = 1                           "mo271101
      cntl_error              = 2                           "mo271101
      error_no_gui            = 3                           "mo271101
      not_supported_by_gui    = 4                           "mo271101
      OTHERS                  = 5.                          "mo271101
  IF sy-subrc <> 0 OR l_rc < 0.                             "mo271101
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno       "mo271101
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.    "mo271101
  ENDIF.                                                    "mo271101

  READ TABLE l_files INDEX 1 INTO h_files.                  "mo271101
  ck_file = h_files-filename.                               "mo271101
*  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'                 "mo271101
*       EXPORTING
*            STATIC    = 'X'
*       CHANGING
*            FILE_NAME = CK_FILE.

AT SELECTION-SCREEN OUTPUT.

* S4 Cloud Edition: Disable checkbox for PC/Appserver upload and
* set PC upload to true

  IF cl_cos_utilities=>is_s4h_cloud( ) = abap_true.
    LOOP AT SCREEN.
      IF screen-name = 'PCUPLOAD'.
        screen-active = '0'.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
    pcupload = 'X'.
  ENDIF.

*eject
*---------------------------------------------------------------*
*  START-OF-SELECTION                                           *
*---------------------------------------------------------------*
START-OF-SELECTION.
  open = false.
*  PERFORM log_create CHANGING gs_log_handle.
  IF post = 'X'.
*     post immediately
    function = calltrans.
    xbdcc    = 'X'.
  ENDIF.
  IF pa_xbdc = 'X'.
*     create batch input
    function = batchinput.
    xbdcc    = space.
  ENDIF.

  PERFORM read_data.

  IF p_file = 'X'.
*-- Avoid initial BUKRS - field: Defaults to page-header for company
*-- 0000. Fill instead with non-existing value - leads to printout of
*-- header-text for client.
    bhdgd-bukrs = '----'.                                   "ak101199
    PERFORM print_check_file.
  ENDIF.


  LOOP AT checks.
    CASE checks-rec+0(1).
      WHEN '1'.
*       Header-Record
        MOVE checks-rec TO head.
        PERFORM process_header.
      WHEN '5'.
*       Data-Record
        MOVE checks-rec TO ckrec.
        PERFORM process_single_check.
      WHEN '9'.
*       Trailer-Record
        MOVE checks-rec TO trailer.
        PERFORM process_trailer.
    ENDCASE.
  ENDLOOP.

*eject
*---------------------------------------------------------------*
*  END-OF-SELECTION                                             *
*---------------------------------------------------------------*
*  sort extract data                                            *
*  create batch input session                                   *
*---------------------------------------------------------------*
END-OF-SELECTION.

  NEW-PAGE.
* initialize
  CLEAR: trans_cnt.
  CLEAR: amount_sum.
  CLEAR: item_cnt.
  CLEAR: collect_tab.
  CLEAR: err_cnt.
  CLEAR: bkpf_komk_ok, hr_cnt.
  REFRESH: collect_tab.

* sort extract
  SORT.

* generate transactions
  BREAK omrani.
  CLEAR gv_count.
  LOOP.
    gv_count = gv_count + 1.
    PERFORM fill_ftpost_i.
*   AT NEW STATUS separates checks in good, error and HR sessions
    AT NEW status.

      IF function = calltrans.
*       post witch call transaction
        groupname =    group.
        groupname+8(4) = '-ERR'.
        CONDENSE groupname NO-GAPS.
      ELSE.
*       create batch input
        CASE status.
          WHEN '1'.
*         session with good transactions
            groupname =    group.
            groupname+8(4) = '-OK '.
            CONDENSE groupname NO-GAPS.
          WHEN '2'.
*         session with error tranactions
            groupname =    group.
            groupname+8(4) = '-ERR'.
            CONDENSE groupname NO-GAPS.
          WHEN 'P'.                                         "mp40A
*         session with HR transactions
            CLEAR: bkpf_komk_ok, hr_cnt.
            IF hr_post = 'X'.
              groupname = group.                            "mp40A
              groupname+8(4) = '-HR'.                       "mp40A
              CONDENSE groupname NO-GAPS.                   "mp40A
            ENDIF.
        ENDCASE.
      ENDIF.
*     open BATCH-INPUT SESSION (only if OPEN = FALSE, no testrun)
      IF NOT ( status = 'P' AND hr_post IS INITIAL ).
        PERFORM posting_interface_start.
      ENDIF.
    ENDAT.

    item_cnt   = item_cnt + 1.
    amount_sum = amount_sum + amount_chk.

    IF status = 'P'.
*   collect HR-check references for later docno update in PAYR
      PERFORM collect_for_payr_update.
    ENDIF.

*   get new document header for compressed doc if new currency
    AT NEW waers.                                           "mp45A
      CLEAR: bkpf_komk_ok.                                  "mp45A
    ENDAT.                                                  "mp45A

*   get new document header for compressed doc if new sortkrit
    AT NEW sortkrit.
      CLEAR: bkpf_komk_ok.
    ENDAT.

    IF status EQ 'P' AND hr_post = 'X'.                     "mp40A
      hr_cnt = hr_cnt + 1.
      IF bkpf_komk_ok IS INITIAL.
*     new document header only at beginning of compressed data
        PERFORM belegkopf_hr.
        bkpf_komk_ok = 'X'.
      ENDIF.
    ELSEIF status = 'P'.
      hr_cnt = hr_cnt + 1.
    ELSEIF status NE 'P'.
      PERFORM fill_ftclear.
    ENDIF.
*     Falls die max. Anzahl BELNR erreicht ist, Batch-Input erzeugen
    IF status = '1'.
      IF item_cnt = 100.
        PERFORM fill_ftpost.
*********Begin of comment ALV C5053249***************************
*        PERFORM POSTING_INTERFACE_CLEARING.
***********END of comment ALV C5053249***************************

*********Begin of coding ALV C5053249***************************
        PERFORM posting_interface_clearing USING status.
************END of coding ALV C5053249***************************
        CLEAR amount_sum.                                   "mp40A
        CLEAR item_cnt.
      ENDIF.
    ENDIF.
    IF status = '2'.
      PERFORM fill_ftpost.
*********Begin of comment ALV C5053249***************************
*      PERFORM POSTING_INTERFACE_CLEARING.
***********END of comment ALV C5053249***************************

*********Begin of coding ALV C5053249***************************
      PERFORM posting_interface_clearing USING status.
************END of coding ALV C5053249***************************
      CLEAR amount_sum.                                     "mp40A
      CLEAR item_cnt.
    ENDIF.

    AT END OF waers.
      IF status = '1' AND item_cnt > 0.
        BREAK omrani.
        PERFORM fill_ftpost_h.
*********Begin of comment ALV C5053249***************************
*        PERFORM POSTING_INTERFACE_CLEARING.
************END of comment ALV C5053249***************************

*********Begin of coding ALV C5053249***************************
        PERFORM posting_interface_clearing USING status.
************END of coding ALV C5053249***************************
        CLEAR amount_sum.
        CLEAR item_cnt.
      ELSEIF status = 'P' AND item_cnt > 0.                 "mp40A
        IF hr_post = 'X'.
          PERFORM hr_fb01.
        ELSE.
          PERFORM info_no_hr_posting.
          PERFORM update_payr_no_hr_posting.
        ENDIF.
        CLEAR collect_tab.
        REFRESH collect_tab.
        CLEAR amount_sum.                                   "mp40A
        CLEAR item_cnt.                                     "mp40A
      ENDIF.
    ENDAT.

    AT END OF sortkrit.
      IF status = 'P' AND item_cnt > 0.
*     posting has not yet occurred (no end of currency yet)
        IF hr_post = 'X'.
          PERFORM hr_fb01.
        ELSE.
          PERFORM info_no_hr_posting.
          PERFORM update_payr_no_hr_posting.
        ENDIF.
        CLEAR collect_tab.
        REFRESH collect_tab.
        CLEAR amount_sum.                                   "mp40A
        CLEAR item_cnt.                                     "mp40A
      ENDIF.
    ENDAT.

    AT END OF status.
      IF status = 'P' AND item_cnt > 0.
*     posting has not yet occurred (end of currency or sortkrit yet)
        IF hr_post = 'X'.
          PERFORM hr_fb01.
        ELSE.
          PERFORM info_no_hr_posting.
          PERFORM update_payr_no_hr_posting.
        ENDIF.
        CLEAR collect_tab.
        REFRESH collect_tab.
        CLEAR amount_sum.                                   "mp40A
        CLEAR item_cnt.                                     "mp40A
      ENDIF.
* session are created for each status
      IF function = batchinput.
*       Close batch input session
*       sessions are created for each status
*       except if forbidden for HR
        IF NOT ( status = 'P' AND hr_post IS INITIAL ).
          PERFORM posting_interface_end.
          IF p_post = 'X'.
            PERFORM display_posting_info.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDAT.

    AT LAST.
*********Begin of comment ALV C5053249***************************
*      ULINE.
*      NEW-PAGE.
***********End of comment ALV C5053249***************************
*     Close posting interface
      IF function = calltrans.
        IF NOT ( status = 'P' AND hr_post IS INITIAL ).
          PERFORM posting_interface_end.
*          IF P_POST = 'X'.
          PERFORM display_posting_info.
*          ENDIF.
        ENDIF.
      ENDIF.
    ENDAT.
  ENDLOOP.

  IF total_checks NE 0.
    PERFORM display_check_file_overview.
  ENDIF.

  IF total_fi_checks NE 0.
    PERFORM display_total_fi_checks.
  ENDIF.

  IF total_prenums NE 0.
    PERFORM display_total_prenums.
  ENDIF.

  IF hr_total NE 0.
    PERFORM display_total_number_hr.
  ENDIF.

  IF normal_prenums_total NE 0.
    PERFORM display_total_normal_checks.
  ENDIF.
*********Begin of coding ALV C5053249***************************
  DESCRIBE TABLE gt_outtab_file         LINES gv_count_list1.
  DESCRIBE TABLE gt_outtab_chkhdr       LINES gv_count_list2_hdr.
  DESCRIBE TABLE gt_outtab_chkdet       LINES gv_count_list2_det.
  DESCRIBE TABLE gt_outtab_post_ok      LINES gv_count_list3_hdr.
  DESCRIBE TABLE gt_outtab_postdet_ok   LINES gv_count_list3_det.
  DESCRIBE TABLE gt_outtab_post_err     LINES gv_count_list4_hdr.
  DESCRIBE TABLE gt_outtab_postdet_err  LINES gv_count_list4_det.
  DESCRIBE TABLE gt_outtab_hrpost      LINES gv_count_list5.
  IF NOT gv_count_list1 IS INITIAL.
    CLEAR gv_flag.
    gv_count_a = gc_value_1.
    PERFORM display_list1.
  ELSE.
    gv_flag = 'X'.
    gv_count_a = gc_value_0.
    gv_count_b = gc_value_1.
    PERFORM display_list_2.
  ENDIF.

***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*eject
*--------------------------------------------------------------*
*  Seitenanfangsverarbeitung                                   *
*--------------------------------------------------------------*
*TOP-OF-PAGE.
*------------------------Batch-Heading-Routine aufrufen--------*
*  PERFORM BATCH-HEADING(RSBTCHH0).
*  ULINE.
*  IF TESTRUN = 'X'.
*    FORMAT COLOR COL_HEADING.
*    WRITE: /01 SY-VLINE,
*               TEXT-008,
*           132 SY-VLINE.
*    WRITE: /01 SY-VLINE,
*               TEXT-007,
*           132 SY-VLINE.
*    WRITE: /01 SY-VLINE,
*               TEXT-008,
*           132 SY-VLINE.
*    FORMAT RESET.
*  ENDIF.
***********End of comment ALV C5053249***************************

*eject
***************************************************************
* Form-Routinen                                               *
***************************************************************
*---------------------------------------------------------------*
*  SUBROUTINE  POSTING_INTERFACE_CLEARING.                      *
*---------------------------------------------------------------*
*********Begin of comment ALV C5053249***************************
*FORM POSTING_INTERFACE_CLEARING.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
*&---------------------------------------------------------------------*
*&      Form  POSTING_INTERFACE_CLEARING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IV_STATUS  text
*----------------------------------------------------------------------*
FORM posting_interface_clearing  USING    iv_status.
  CLEAR: blntab, fttax.
  REFRESH: blntab, fttax.

  trans_cnt = trans_cnt + 1.

  IF testrun NE 'X'.
    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv   = 'AUSGZAHL'
        i_tcode   = 'FB05'
*     IMPORTING  E_SUBRC   = SUBRC
*       E_MSGID   = MSGID
*       E_MSGTY   = MSGTY
*       E_MSGNO   = MSGNO
*       E_MSGV1   = MSGV1
*       E_MSGV2   = MSGV2
*       E_MSGV3   = MSGV3
*       E_MSGV4   = MSGV4
      TABLES
        t_ftpost  = ftpost
        t_ftclear = ftclear
        t_fttax   = fttax
        t_blntab  = blntab.

    DESCRIBE TABLE blntab LINES tfill_blntab.
    IF tfill_blntab = 0.
      err_cnt = err_cnt + 1.
    ENDIF.

  ENDIF.

  IF p_post = 'X'.
*     print posting information
*********Begin of comment ALV C5053249***************************
*    PERFORM PRINT_POSTING_INFORMATION.
***********End of comment ALV C5053249***************************
***********Begin of coding ALV C5053249***************************
    PERFORM print_posting_information USING iv_status.
***********End of coding ALV C5053249***************************
  ENDIF.

  REFRESH: ftpost, ftclear, blntab.
  CLEAR tfill_blntab.
* clear:   amount_sum.

ENDFORM.                    " POSTING_INTERFACE_CLEARING
*---------------------------------------------------------------*
*  Subroutine PRINT_POSTING_INFORMATION.                        *
*****************************************************************
* Comment added C5053249: Here status is passed as a parameter  *
* to check for the status if '1' then pass the data to OK       *
* internal table elseif '2' pass to ERR internal table          *
*****************************************************************
*********Begin of comment ALV C5053249***************************
*FORM PRINT_POSTING_INFORMATION.
*********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
FORM print_posting_information USING iv_status.
  DATA: lv_text(130) TYPE c.
*********End of coding ALV C5053249***************************
  DATA: char10(10) TYPE c.



  CLEAR char130.


  IF function = calltrans.
    IF tfill_blntab = 0.
*       document was not posted. batch input had been created
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
*      WRITE: /01 SY-VLINE,
*                 TEXT-049,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      MOVE gc_trans TO gs_tab_postinfo-status.
      MOVE TEXT-049 TO gs_tab_postinfo-char130.
      APPEND gs_tab_postinfo TO gt_tab_postinfo.
      CLEAR gs_tab_postinfo.
***********End of coding ALV C5053249***************************
    ELSE.
*       document was posted sucessfully
      FORMAT COLOR COL_NORMAL.
      char130 = TEXT-048.
      LOOP AT blntab.
        REPLACE '&' WITH blntab-belnr INTO char130.
        REPLACE '&' WITH blntab-bukrs INTO char130.
      ENDLOOP.
*********Begin of comment ALV C5053249***************************
*      WRITE: /01 SY-VLINE,
*                 CHAR130,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      MOVE gc_trans TO gs_tab_postinfo-status.
      MOVE char130 TO gs_tab_postinfo-char130.
      APPEND gs_tab_postinfo TO gt_tab_postinfo.
      CLEAR gs_tab_postinfo.
    ENDIF.
  ENDIF.
  IF char130 IS INITIAL.
    lv_text = TEXT-007.
    char130 = lv_text+0(4).
  ENDIF.

***********End of coding ALV C5053249***************************



*   first line item
  amount_sum = amount_sum * -1.
  WRITE amount_sum TO char17 CURRENCY waers.
  WRITE valut TO char10 DD/MM/YYYY.
  order = order + 1.                                        "857884
*********Begin of comment ALV C5053249***************************
*  FORMAT RESET.
*  FORMAT COLOR COL_HEADING.
*  WRITE: /01 SY-VLINE,
*          03 TEXT-051,
*          17 T012K-HKONT,
*          42 WAERS,
*          50 CHAR17,
*          70 TEXT-052,
*          83 CHAR10,
*         132 SY-VLINE.
*
*  FORMAT RESET.
*  FORMAT COLOR COL_NORMAL.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  IF iv_status = '1'.
    MOVE: t012k-hkont TO gs_outtab_post_ok-hkont,
          waers       TO gs_outtab_post_ok-waers,
          char17      TO gs_outtab_post_ok-amount,
          char10      TO gs_outtab_post_ok-valut,
          char130     TO gs_outtab_post_ok-postinfo,
          order       TO gs_outtab_post_ok-order.           "857884
    APPEND gs_outtab_post_ok TO gt_outtab_post_ok.
    CLEAR  gs_outtab_post_ok.
***********End of coding ALV C5053249***************************
*   clearing line items
    LOOP AT p_ftclear.
      WRITE p_ftclear-amount TO char17 CURRENCY p_ftclear-waers.
*********Begin of comment ALV C5053249***************************
*    WRITE: /01 SY-VLINE,
*            05 TEXT-042,
*            30 P_FTCLEAR-BELNR,
*            42 P_FTCLEAR-WAERS,
*            50 CHAR17,
*           132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      MOVE: t012k-hkont     TO gs_outtab_postdet_ok-hkont,
            p_ftclear-belnr TO gs_outtab_postdet_ok-belnr,
            p_ftclear-waers TO gs_outtab_postdet_ok-waers,
            char17          TO gs_outtab_postdet_ok-wrbtr,
            char130         TO gs_outtab_postdet_ok-postinfo,
            order           TO gs_outtab_postdet_ok-order.  "857884
      APPEND gs_outtab_postdet_ok TO gt_outtab_postdet_ok.
      CLEAR  gs_outtab_postdet_ok.
    ENDLOOP.

  ELSEIF iv_status = '2'.
    MOVE: t012k-hkont TO gs_outtab_post_err-hkont,
          waers       TO gs_outtab_post_err-waers,
          char17      TO gs_outtab_post_err-amount,
          char10      TO gs_outtab_post_err-valut,
          char130     TO gs_outtab_post_err-postinfo,
          order       TO gs_outtab_post_err-order.          "857884
    .
    APPEND gs_outtab_post_err TO gt_outtab_post_err.
    CLEAR  gs_outtab_post_err.
*   clearing line items
    LOOP AT p_ftclear.
      WRITE p_ftclear-amount TO char17 CURRENCY p_ftclear-waers.
      MOVE: t012k-hkont     TO gs_outtab_postdet_err-hkont,
            p_ftclear-belnr TO gs_outtab_postdet_err-belnr,
            p_ftclear-waers TO gs_outtab_postdet_err-waers,
            char17          TO gs_outtab_postdet_err-wrbtr,
            char130         TO gs_outtab_postdet_err-postinfo,
            order       TO gs_outtab_post_err-order.        "857884

      APPEND gs_outtab_postdet_err TO gt_outtab_postdet_err.
      CLEAR  gs_outtab_postdet_err.
    ENDLOOP.
  ENDIF.



  CLEAR char130.
  REFRESH p_ftclear.


*********Begin of comment ALV C5053249***************************
*  ULINE.
***********End of comment ALV C5053249***************************
ENDFORM.                    "PRINT_POSTING_INFORMATION



*---------------------------------------------------------------*
*  SUBROUTINE  FILL_FTPOST.                                     *
*---------------------------------------------------------------*
FORM fill_ftpost.
* Post Document: Header Data
  CLEAR ftpost.
  ftpost-stype = 'K'.
  ftpost-count = 1.

  WRITE bldat TO fvalue DD/MM/YYYY.
  PERFORM ftpost_field USING 'BKPF-BLDAT' fvalue.         "Document Date

  WRITE i_blart TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BLART' fvalue.         "Doc Type

  WRITE t012k-bukrs TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BUKRS' fvalue.    "Company Code

  WRITE budat    TO fvalue DD/MM/YYYY.
  PERFORM ftpost_field USING 'BKPF-BUDAT' fvalue.         "Buchungsdatum

  WRITE waers TO fvalue.
  PERFORM ftpost_field USING 'BKPF-WAERS' fvalue.

  IF NOT pdate IS INITIAL AND xlocalcurr IS INITIAL.        "mp45A
* provide chrec-pdate as FX-translation date if foreign currency
    WRITE pdate    TO fvalue DD/MM/YYYY.                    "mp45A
    PERFORM ftpost_field USING 'BKPF-WWERT' fvalue.         "mp45A
  ENDIF.                                                    "mp45A

  IF status = '2'.
*   only if each transaction is for one check
    WRITE ckrec-cknum TO fvalue.
    PERFORM ftpost_field USING 'BKPF-XBLNR' fvalue.  "Referenz Doc
  ENDIF.

* Post Document: Fields of first line item
  CLEAR ftpost.
  ftpost-stype = 'P'.                  " Line Item
  ftpost-count = '1'.                  " Nr Line Item

  PERFORM ftpost_field USING 'RF05A-NEWBS' '50'.          " Post Key

  WRITE t012k-hkont TO fvalue.
  PERFORM ftpost_field USING 'RF05A-NEWKO' fvalue.        " Account

*  WRITE AMOUNT_SUM TO FVALUE CURRENCY WAERS.                  "ak281099
  WRITE amount_sum TO fvalue CURRENCY waers NO-GROUPING.    "ak281099
  CONDENSE fvalue NO-GAPS.
  PERFORM ftpost_field USING 'BSEG-WRBTR' fvalue.         " Amount

  IF NOT valut IS INITIAL.
    WRITE valut TO fvalue DD/MM/YYYY.
    PERFORM ftpost_field USING 'BSEG-VALUT' fvalue.       " Value Date
  ENDIF.

*--------------------------------------------------------------------*
* SFIN Cash: determine HBKID and HKTID
* Begins
  IF cl_fclm_switch_utility=>is_switch_active( ) EQ abap_true.
    DATA l_field_prop     TYPE faus1.
    CALL FUNCTION 'FI_FIELD_SELECTION_DETERMINE'
      EXPORTING
        i_bschl     = '50'
        i_bukrs     = t012k-bukrs
        i_saknr     = t012k-hkont
      IMPORTING
        e_faus1     = l_field_prop
*       E_FAUS2     =
      EXCEPTIONS
        customizing = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF l_field_prop+36(1) NE '-'
      AND ( NOT t012k-hbkid IS INITIAL
        OR NOT t012k-hktid IS INITIAL ).
      WRITE t012k-hbkid TO fvalue.
      PERFORM ftpost_field USING 'BSEG-HBKID' fvalue.        " House Bank

      WRITE t012k-hktid TO fvalue.
      PERFORM ftpost_field USING 'BSEG-HKTID' fvalue.        " House Bank Account
    ENDIF.
  ENDIF.
*                                                                 Ends
*--------------------------------------------------------------------*
ENDFORM.                    "FILL_FTPOST

*eject
*--------------------------------------------------------------------*
*  Buchungsschnittstelle: Feld hinzufügen                            *
*--------------------------------------------------------------------*
FORM ftpost_field  USING fnam LIKE ftpost-fnam
                         fval LIKE ftpost-fval.

  CLEAR: ftpost-fnam, ftpost-fval.
  ftpost-fnam     = fnam.
  ftpost-fval     = fval.
  APPEND ftpost.

ENDFORM.                    "FTPOST_FIELD


*---------------------------------------------------------------*
*  SUBROUTINE  FILL_FTCLEAR.                                    *
*---------------------------------------------------------------*
FORM fill_ftclear.
  CLEAR ftclear.
*{   REPLACE        BDAK904826                                        1
*\  ftclear-agkoa  = 'S'.
  break sarvari.
  ftclear-agkoa  = BSEG-KOART." K
  ftclear-AGUMS  = BSEG-UMSKz . "'W'.
  SELECT SINGLE lifnr FROM bsik INTO agkon WHERE belnr = belnr AND gjahr = gjahr and bukrs = t012k-bukrs and umskz = BSEG-UMSKz." 'W'.
*}   REPLACE
  ftclear-agbuk  = t012k-bukrs.
  ftclear-agkon  = agkon.                                   "30E
  ftclear-xnops  = 'X'.
  ftclear-selfd  = 'BELNR'.
  ftclear-selvon = belnr.
  IF NOT gjahr IS INITIAL AND NOT buzei IS INITIAL.  "insert note 301177
    ftclear-selvon+10 = gjahr.                       "insert note 301177
    ftclear-selvon+14 = buzei.                       "insert note 301177
  ENDIF.                                             "insert note 301177
  APPEND ftclear.

* Print Info
  IF p_post = 'X'.
    CLEAR p_ftclear.
    p_ftclear-belnr  = belnr.
    p_ftclear-waers  = waers.
    p_ftclear-amount = amount_doc.
    APPEND p_ftclear.
  ENDIF.
ENDFORM.                    "FILL_FTCLEAR

*---------------------------------------------------------------*
*  SUBROUTINE  PROCESS_HEADER.                                  *
*---------------------------------------------------------------*
*  Read    T012  and T012K to get HBKID and HKTID               *
*---------------------------------------------------------------*
FORM process_header.

  DATA :                                                    "ak020699
    hlp_rfdt_id   TYPE feb_hlp_rfdt_id_struct,
    sap_curr_code LIKE t012k-waers,
    bnkn2         LIKE t012k-bnkn2.                         "ak010999

  CLEAR: hbkid, hktid.
  hdcnt = hdcnt + 1.
  FORMAT COLOR COL_HEADING.

*  initialize header
  CLEAR:  valut,
          budat,
          bldat,
          waers,
          status.

  IF pcupload = 'T' AND ck_file(4) = 'FCHR'.                "ak020699
    hlp_rfdt_id = ck_file.
*-- report called by submit - determine account from rfc-key
    SELECT SINGLE * FROM t012k WHERE bukrs = hlp_rfdt_id-zbukr
                               AND   hbkid = hlp_rfdt_id-hbkid
                               AND   hktid = hlp_rfdt_id-hktid.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM t012 WHERE bukrs = hlp_rfdt_id-zbukr
                                AND   hbkid = hlp_rfdt_id-hbkid.

      IF sy-subrc NE 0.
        MESSAGE a244(fb) WITH hlp_rfdt_id-hbkid hlp_rfdt_id-zbukr.
      ENDIF.
    ELSE.
      MESSAGE a245(fb) WITH hlp_rfdt_id-hbkid hlp_rfdt_id-zbukr.
    ENDIF.
  ELSE.
*-- report called with file-upload - determine account from header-line
    t012-bankl  = head-bankl.
    IF NOT head-iso_curr_code IS INITIAL.
*      PERFORM CURRENCY_CODE_ISO_TO_SAP USING HEAD-ISO_CURR_CODE
*                                             SAP_CURR_CODE.
      bnkn2 = head-accnr.
      CALL FUNCTION 'EB_CURRENCY_CODE_ISO_TO_SAP'           "ak010999
        EXPORTING
          id_iso_code       = head-iso_curr_code
          id_bankl          = t012-bankl
          id_bnkn2          = bnkn2
        IMPORTING
          ed_sap_code       = sap_curr_code
        EXCEPTIONS
          no_unique_account = 0.                            "ak171199

*      IF SY-SUBRC <> 0.                                       "ak010999
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
    ELSE.
      CLEAR sap_curr_code.
    ENDIF.
    t012k-bankn = head-accnr.
    bnkn2 = t012k-bankn.     "local copy to avoid type-conflict hw822973


    CALL FUNCTION 'GET_BANK_ACCOUNT'
      EXPORTING
        i_bankl = t012-bankl
        i_bnkn2 = bnkn2                   "hw822973
        i_waers = sap_curr_code                             "ak020699
      IMPORTING
        e_t012  = t012
        e_t012k = t012k.
*-- explicit handling of exceptions not necessary here since no
*-- ROLLBACK_FEBKO required
*         EXCEPTIONS                                           "ak161199
*              BANK_ACCOUNT_NOT_FOUND = 1
*              BANK_KEY_FALSE         = 2
*              BANK_KEY_NOT_FOUND     = 3
*              MULTIPLE_BANK_ACCOUNT  = 4
*              INPUT_WRONG            = 5
*              CURRENCY_FALSE         = 6
*              CURRENCY_NOT_FOUND     = 7
*              OTHERS                 = 8.

*    CASE SY-SUBRC.
*      WHEN 0.
**     alles ok
*      WHEN 1.
*        MESSAGE A736(FB) WITH HEAD-BANKL  HEAD-ACCNR.
*      WHEN 2.
*        MESSAGE A735(FB) WITH HEAD-BANKL  HEAD-ACCNR.
*      WHEN 3.
*        MESSAGE A755(FB) WITH HEAD-BANKL.
*      WHEN 4.
**      MESSAGE A757(FB) WITH HEAD-ACCNR  HEAD-BANKL.
*        MESSAGE A752(FB) WITH T012K-BANKN.
*      WHEN 6 OR 7.
*        MESSAGE A141(FV) WITH SAP_CURR_CODE HEAD-BANKL T012K-BANKN.
*      WHEN OTHERS.
*        MESSAGE A736(FB) WITH HEAD-BANKL  HEAD-ACCNR.
*    ENDCASE.
  ENDIF.

  IF t012k-hkont IS INITIAL.
    MESSAGE a737 WITH t012k-hbkid  t012k-hktid t012k-bukrs.
  ENDIF.



  hbkid = t012k-hbkid.
  hktid = t012k-hktid.

  PERFORM authority_check.

  SELECT SINGLE * FROM t001 WHERE bukrs = t012k-bukrs.
  IF sy-subrc NE 0.
    MESSAGE a701 WITH t012k-bukrs.
  ENDIF.

*  if not ( bhdgd-bukrs is initial ) and                    "note 353457
*     bhdgd-bukrs ne t012k-bukrs.
*    hdcnt = 1.
*  endif.

* store local currency of cc belonging to house bank account
  localcurr = t001-waers.                                   "mp45A

*   Set Document date and Posting date
  IF NOT i_budat IS INITIAL.
    budat = i_budat.
  ELSE.
    budat = head-crdat.
  ENDIF.
  IF NOT i_bldat IS INITIAL.
    bldat = i_bldat.
  ELSE.
    bldat = head-crdat.
  ENDIF.
*********Begin of comment ALV C5053249***************************
* print new header-record
*  FORMAT COLOR COL_HEADING.
*  IF HDCNT = 1.
*    CLEAR:  BHDGD-LINE1, BHDGD-LINE2.
*    BHDGD-LINE1 = SY-TITLE.
*    CHAR60      = TEXT-003.
*    BHDGD-LINE2 = CHAR60.
*    BHDGD-INIFL = '0'.
**    BHDGD-BUKRS = T012K-BUKRS.                          "note 315308
*    BHDGD-UNAME = SY-UNAME.
*    BHDGD-REPID = SY-REPID.
*    NEW-PAGE.
*  ELSE.
**    WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.
*  ENDIF.
*  WRITE:  /1      SY-VLINE,
*           2      TEXT-030,
*                  HDCNT,
*           132    SY-VLINE.
*  WRITE:  /1      SY-VLINE,
*           2      TEXT-031,
*           17(15) HEAD-BANKL,
*           54(19) TEXT-032,
*           74(05) HBKID,
*           132    SY-VLINE.
*  WRITE:  /1      SY-VLINE,
*           2      TEXT-033,
*           17(35) HEAD-ACCNR,
*           54(19) TEXT-034,
*           74(05) HKTID,
*           112(14) TEXT-114,
*"ak010999
*           127(4)  T012K-BUKRS,
*"ak010999
*           132    SY-VLINE.
*  ULINE.

*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  MOVE: hdcnt       TO gs_outtab_chkhdr-head_rec,
        head-bankl  TO gs_outtab_chkhdr-bankl,
        hbkid       TO gs_outtab_chkhdr-hbkid,
        head-accnr  TO gs_outtab_chkhdr-bankn,
        hktid       TO gs_outtab_chkhdr-hktid,
        t012k-bukrs TO gs_outtab_chkhdr-bukrs.
  APPEND gs_outtab_chkhdr TO gt_outtab_chkhdr.
  CLEAR  gs_outtab_chkhdr.
***********End of coding ALV C5053249***************************
ENDFORM.                    "PROCESS_HEADER

*eject
*---------------------------------------------------------------*
* FORM PROCESS_SINGLE_CHECK.
*---------------------------------------------------------------*
*  Check if check is in PAYR and not cashed                     *
*---------------------------------------------------------------*
FORM process_single_check.
  DATA: payr_hits LIKE sy-dbcnt.

  CLEAR: status.
  CLEAR: payr_ok.
  PERFORM counter_total_checks.
  IF pnumc = 'X'.
*-------- prenumbered checks -----------------------------------------
    SELECT * FROM payr WHERE zbukr = t012k-bukrs
                       AND   hbkid = hbkid
                       AND   hktid = hktid
                       AND   chect = ckrec-cknum
                       AND   ichec = space.
    ENDSELECT.
    payr_hits = sy-dbcnt.
  ELSE.
*-------- blank checks -------------------------------------------------
    payr_hits = 0.
  ENDIF.

  IF payr_hits = 0.
*-------- check not found in PAYR, search in BKPF ----------------------
    SELECT * FROM bkpf WHERE bukrs = t012k-bukrs
                         AND belnr = ckrec-cknum.
    ENDSELECT.
    IF sy-subrc = 0.
      PERFORM counter_fi_checks.
      IF p_prot = 'X'.
        IF pnumc = 'X'.
          char130 = TEXT-018.
        ELSE.
          char130 = TEXT-020.
        ENDIF.
        REPLACE '&' WITH ckrec-cknum INTO char130.
        FORMAT COLOR COL_NORMAL.
*********Begin of comment ALV C5053249***************************
*        WRITE: /01 SY-VLINE,
*                   CHAR130,
*               132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl     = head-bankl.
        gs_outtab_chkdet-bankn     = head-accnr.
        gs_outtab_chkdet-bukrs     = t012k-bukrs.
        gs_outtab_chkdet-messtext  = char130.
        gs_outtab_chkdet-cknum     = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkhdr.
        CLEAR gs_outtab_chkdet.
        IF pnumc = 'X'.
          CLEAR char130.
          char130 = TEXT-110.
*********Begin of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*          WRITE: /01 SY-VLINE,
*                  31 CHAR130,
*                 132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
          gs_outtab_chkdet-bankl        = head-bankl.
          gs_outtab_chkdet-bankn        = head-accnr.
          gs_outtab_chkdet-bukrs        = t012k-bukrs.
          gs_outtab_chkdet-messtext+31  = char130.
          gs_outtab_chkdet-cknum        = ckrec-cknum.
          APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
          CLEAR gs_outtab_chkdet.
        ENDIF.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*        FORMAT RESET.
*********End of comment ALV C5053249***************************
      ENDIF.
      PERFORM currency_check.
      PERFORM extract_check.
    ELSE.
      PERFORM counter_unknown.
      IF pnumc = 'X'.
        MOVE TEXT-011 TO char130.
      ELSE.
        MOVE TEXT-073 TO char130.
      ENDIF.
      REPLACE '&' WITH ckrec-cknum INTO char130.
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
*      WRITE: /01 SY-VLINE,
*                 CHAR130,
*             132 SY-VLINE.
*      FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_chkdet-bankl     = head-bankl.
      gs_outtab_chkdet-bankn     = head-accnr.
      gs_outtab_chkdet-bukrs     = t012k-bukrs.
      gs_outtab_chkdet-messtext  = char130.
      gs_outtab_chkdet-cknum     = ckrec-cknum.
      APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
      CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
    ENDIF.
*    ULINE.
  ELSEIF payr_hits = 1.
*-------- check found in PAYR ----------------------------------------
    PERFORM counter_prenums.
    IF NOT payr-voidr IS INITIAL.
*-------- check has been voided -----------------------------------"30C
      PERFORM counter_voided_prenums.
      PERFORM print_check_voided.                           "30C
    ELSEIF payr-xbanc IS INITIAL.                           "30C
*-------- check neither already paid nor voided -----------------------
      PERFORM counter_valid_prenumbered.
      IF payr-laufi+5(1) = 'P'.
*-------- check comes from Houman Ressouces
        hr_total = hr_total + 1.
        valut = ckrec-valut.
        pdate = ckrec-pdate.
        PERFORM print_hr_check_info USING ckrec-cknum.
        PERFORM check_amount_single_check USING amount_ok.
*       Correct amount on check and in payr.
        IF amount_ok = true.
          IF hr_post EQ 'X'.
*           Determine intermediate account for HR Checks.
            PERFORM determine_hr_interim_account USING acc_ok.
*           account found or manual check
            IF acc_ok = true OR acc_ok = 'M'.
              PERFORM update_payr.
              IF acc_ok = 'M'.
                hr_post_man_check = hr_post_man_check + 1.
                PERFORM display_manual_check.
              ELSE.
                PERFORM extract_hr_check.
                PERFORM counter_hr_post.
              ENDIF.
*           error in customizing (no account found)
            ELSE.
              hr_post_proc = hr_post_proc + 1.
              PERFORM display_missing_account.
            ENDIF.
*         no posting of HR checks (parameter)
          ELSE.
            PERFORM counter_hr_post.
            PERFORM extract_hr_check.
            PERFORM update_payr.
          ENDIF.
        ELSE.
*       Amount is not correct
          hr_differing_amount = hr_differing_amount + 1.
          PERFORM display_wrong_amount_hr.
        ENDIF.
      ELSE.
*      normal check
        PERFORM counter_normal_checks.
        SELECT SINGLE * FROM bkpf WHERE bukrs = t012k-bukrs
                                    AND belnr = payr-vblnr
                                    AND gjahr = payr-gjahr.
        IF sy-subrc = 0.
          payr_ok = true.
          PERFORM currency_check.
          PERFORM extract_check.
        ELSE.
          PERFORM counter_oprenums_invalid_docno.
          PERFORM display_missing_or_wrong_docno.
        ENDIF.
      ENDIF.
    ELSE.
*     check already paid
      PERFORM counter_cashed_checks.
      char130 = TEXT-012.
      REPLACE '&' WITH ckrec-cknum INTO char130.
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
*      WRITE: /01 SY-VLINE,
*                 CHAR130,
*             132 SY-VLINE.
*      FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_chkdet-bankl     = head-bankl.
      gs_outtab_chkdet-bankn     = head-accnr.
      gs_outtab_chkdet-bukrs     = t012k-bukrs.
      gs_outtab_chkdet-messtext  = char130.
      gs_outtab_chkdet-cknum     = ckrec-cknum.
      APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
      CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
    ENDIF.
*********Begin of comment ALV C5053249***************************
*    ULINE.
***********End of comment ALV C5053249***************************
  ELSE.
*   check is in more than one payment method -> error
    PERFORM counter_multiple_prenums.
    PERFORM print_multiple_pay_methods.
*    ULINE.
  ENDIF.
ENDFORM.                    "PROCESS_SINGLE_CHECK

*---------------------------------------------------------------------*
*       FORM EXTRACT_CHECK                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM extract_check.
  DATA: c_zah LIKE bseg-ktosl VALUE 'ZAH'.           "mp - 133304
  DATA: item TYPE c.
  DATA: item_open TYPE c.
  DATA: amount_ok TYPE c.

  DATA: BEGIN OF x1bseg OCCURS 0.                           "ak191099
      INCLUDE STRUCTURE bseg.                               "ak191099
  DATA: END OF   x1bseg.                                    "ak191099
  DATA: BEGIN OF x2bseg OCCURS 0.                           "ak191099
      INCLUDE STRUCTURE bseg.                               "ak191099
  DATA: END OF   x2bseg.                                    "ak191099
  DATA: nx1bseg TYPE n.                                     "ak191099
  DATA: nx2bseg TYPE n.                                     "ak191099

  belnr      = bkpf-belnr.
  gjahr      = bkpf-gjahr.

  IF         ckrec-amount IS INITIAL                        "ak281099
     AND NOT ckrec-bankl  IS INITIAL                        "ak281099
     AND     pcupload     =  'T'                            "ak281099
     AND     ck_file(4)   = 'FCHR'.                         "ak281099

*-- (mis-)use BANKL-field for amount to avoid cut-off          "ak281099
    amount_chk = ckrec-bankl / 100.                         "ak281099
  ELSE.                                                     "ak281099
    amount_chk = ckrec-amount.
    amount_chk = amount_chk / 100.
  ENDIF.                                                    "ak281099

  pdate  = ckrec-pdate.
  item = false.
  item_open = false.
  amount_ok = false.
  CLEAR agkon.                                              "30E
  CLEAR buzei.                                              "note 301177

*-- select candidates for wanted document line
  SELECT * FROM bseg WHERE bukrs = t012k-bukrs
                       AND belnr = belnr
                       AND gjahr = gjahr
                       AND shkzg = 'H'                      "30E
*{   REPLACE        BDAK904826                                        1
*\                       AND koart = 'S'                      "30E
                       AND koart = 'K'                      "30E
*}   REPLACE
                       AND xopvw = 'X'                      "30E
*                      AND KTOSL = SPACE.                 "mp - 133304
                       AND ktosl IN (c_zah , space).      "mp - 133304

    PERFORM check_if_relevant_to_cash_flow USING t012k-bukrs "ak230899
                                                 bseg-hkont "ak230899
                                        CHANGING gd_cfr_flag. "ak230899

*    CHECK NOT GD_CFR_FLAG IS INITIAL.                         "ak230899
    IF gd_cfr_flag IS INITIAL.                              "ak191099
      x2bseg = bseg.                                        "ak191099
      APPEND x2bseg.                                        "ak191099
      nx2bseg = nx2bseg + 1.                                "ak191099
    ELSE.                                                   "ak191099
      x1bseg = bseg.                                        "ak191099
      APPEND x1bseg.                                        "ak191099
      nx1bseg = nx1bseg + 1.                                "ak191099
    ENDIF.                                                  "ak191099
  ENDSELECT.                                                "ak191099

  IF sy-subrc IS INITIAL.                                   "ak191099
*-- at least one doc.-line found

*   CHECK BSEG-SHKZG = 'H'.            "Special G/L indicator   "30E
*   CHECK BSEG-KOART = 'S'.            "Account TYPE            "30E
*   CHECK BSEG-XOPVW = 'X'.            "Open Items              "30E

    agkon = bseg-hkont.                                     "30E
    item = true.
    buzei = bseg-buzei.                                     "note 301177
    IF nx1bseg > 1.
*-- more than one doc.-lines selected: issue an error-message and take
*-- last line ( arbitrarily )
      char130 = TEXT-120.
      REPLACE '&' WITH ckrec-cknum INTO char130.
      REPLACE '&' WITH belnr       INTO char130.
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
*      WRITE: /01 SY-VLINE,
*                 CHAR130,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_chkdet-bankl     = head-bankl.
      gs_outtab_chkdet-bankn     = head-accnr.
      gs_outtab_chkdet-bukrs     = t012k-bukrs.
      gs_outtab_chkdet-messtext  = char130.
      gs_outtab_chkdet-cknum     = ckrec-cknum.
      APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
      CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
      LOOP AT x1bseg.
        char130 = TEXT-121.
        REPLACE '&' WITH belnr       INTO char130.
        REPLACE '&' WITH x1bseg-hkont INTO char130.
*********Begin of comment ALV C5053249***************************
*        WRITE: /01 SY-VLINE,
*                   CHAR130,
*               132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl     = head-bankl.
        gs_outtab_chkdet-bankn     = head-accnr.
        gs_outtab_chkdet-bukrs     = t012k-bukrs.
        gs_outtab_chkdet-messtext  = char130.
        gs_outtab_chkdet-cknum     = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
*********End of coding ALV C5053249***************************
        char130 = TEXT-122.
        REPLACE '&' WITH x1bseg-hkont INTO char130.
*********Begin of comment ALV C5053249***************************
*        WRITE: /01 SY-VLINE,
*                31 CHAR130,
*               132 SY-VLINE.
**********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl        = head-bankl.
        gs_outtab_chkdet-bankn        = head-accnr.
        gs_outtab_chkdet-bukrs        = t012k-bukrs.
        gs_outtab_chkdet-messtext+31  = char130.
        gs_outtab_chkdet-cknum        = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
      ENDLOOP.
*-- must go to faulty-folder
      status = '2'.
      IF payr_ok = true.
        PERFORM update_payr.
      ENDIF.
    ENDIF.
    IF nx1bseg = 0 AND nx2bseg > 1.
*--
      char130 = TEXT-120.
      REPLACE '&' WITH ckrec-cknum INTO char130.
      REPLACE '&' WITH belnr       INTO char130.
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
*      WRITE: /01 SY-VLINE,
*                 CHAR130,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_chkdet-bankl     = head-bankl.
      gs_outtab_chkdet-bankn     = head-accnr.
      gs_outtab_chkdet-bukrs     = t012k-bukrs.
      gs_outtab_chkdet-messtext  = char130.
      gs_outtab_chkdet-cknum     = ckrec-cknum.
      APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
      CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
      LOOP AT x2bseg.
        char130 = TEXT-023.
        REPLACE '&' WITH belnr      INTO char130.
        REPLACE '&' WITH x2bseg-hkont INTO char130.
*********Begin of comment ALV C5053249***************************
*        WRITE: /01 SY-VLINE,
*                31 CHAR130,
*               132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl        = head-bankl.
        gs_outtab_chkdet-bankn        = head-accnr.
        gs_outtab_chkdet-bukrs        = t012k-bukrs.
        gs_outtab_chkdet-messtext+31  = char130.
        gs_outtab_chkdet-cknum        = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
        char130 = TEXT-024.
        REPLACE '&' WITH x2bseg-hkont INTO char130.
*********Begin of comment ALV C5053249***************************
*        WRITE: /01 SY-VLINE,
*                31 CHAR130,
*                132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl        = head-bankl.
        gs_outtab_chkdet-bankn        = head-accnr.
        gs_outtab_chkdet-bukrs        = t012k-bukrs.
        gs_outtab_chkdet-messtext+31  = char130.
        gs_outtab_chkdet-cknum        = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
***********Endoding ALV C5053249***************************
        FORMAT RESET.
      ENDLOOP.
*-- must go to faulty-folder
      status = '2'.
      IF payr_ok = true.
        PERFORM update_payr.
      ENDIF.
    ENDIF.

    IF nx1bseg = 1 OR ( nx1bseg = 0 AND nx2bseg = 1 ).
      IF nx1bseg = 1.
        bseg = x1bseg.
      ELSE.
        bseg = x2bseg.
      ENDIF.
      agkon = bseg-hkont.                                   "n928632
      IF bseg-augbl NE space.
*       item already cleared
        char130 = TEXT-014.
        REPLACE '&' WITH ckrec-cknum INTO char130.
        REPLACE '&' WITH belnr       INTO char130.
*********Begin of comment ALV C5053249***************************
*        FORMAT COLOR COL_NEGATIVE.
*        WRITE: /01 SY-VLINE,
*                   CHAR130,
*               132 SY-VLINE.
*********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl     = head-bankl.
        gs_outtab_chkdet-bankn     = head-accnr.
        gs_outtab_chkdet-bukrs     = t012k-bukrs.
        gs_outtab_chkdet-messtext  = char130.
        gs_outtab_chkdet-cknum     = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*        FORMAT RESET.
***********End of comment ALV C5053249***************************
*       if item is cleared, set PAYR to Payed Check
        IF payr_ok = true.
          PERFORM update_payr.
        ENDIF.

        CLEAR: char130.
        char130 = TEXT-025.
*********Begin of comment ALV C5053249***************************
*        FORMAT COLOR COL_NEGATIVE.
*        WRITE: /01 SY-VLINE,
*                31 CHAR130,
*               132 SY-VLINE.
*        FORMAT RESET.
*        ULINE.
***********End of coding ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        gs_outtab_chkdet-bankl        = head-bankl.
        gs_outtab_chkdet-bankn        = head-accnr.
        gs_outtab_chkdet-bukrs        = t012k-bukrs.
        gs_outtab_chkdet-messtext+31  = char130.
        gs_outtab_chkdet-cknum        = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
      ELSE.
*       item not cleared
        item_open = true.
        valut      = ckrec-valut.
        amount_doc = bseg-wrbtr.
        IF amount_chk  EQ amount_doc.
*---      amounts ok
          status = '1'.
          amount_ok = true.
          buzei = bseg-buzei.                               "note 301177
          IF p_prot = 'X'.
            char130 = TEXT-016.
            REPLACE '&' WITH ckrec-cknum INTO char130.
            REPLACE '&' WITH belnr       INTO char130.
*********Begin of comment ALV C5053249***************************
*            FORMAT COLOR COL_NORMAL.
*            WRITE: /01 SY-VLINE,
*                       CHAR130,
*                   132 SY-VLINE.
*            FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
            gs_outtab_chkdet-bankl     = head-bankl.
            gs_outtab_chkdet-bankn     = head-accnr.
            gs_outtab_chkdet-bukrs     = t012k-bukrs.
            gs_outtab_chkdet-messtext  = char130.
            gs_outtab_chkdet-cknum     = ckrec-cknum.
            APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
            CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
          ENDIF.
*         set PAYR to Payed Check
          IF payr_ok = true.
            PERFORM update_payr.
          ENDIF.
        ELSE.
*---      amounts not ok
*-----    check if acc. Cash receipt account / cash disbursement account
          CLEAR skb1.                                         "30D
          SELECT SINGLE * FROM skb1 WHERE bukrs = t012k-bukrs
                                      AND saknr = bseg-hkont. "30D
*                                     AND XGKON = 'X'.            "30D
*       IF SY-SUBRC = 0.                                        "30D
          status = '2'.
          char130 = TEXT-015.
          REPLACE '&' WITH ckrec-cknum INTO char130.
          REPLACE '&' WITH belnr       INTO char130.
*********Begin of comment ALV C5053249***************************
*          FORMAT COLOR COL_NEGATIVE.
*          WRITE: /01 SY-VLINE,
*                     CHAR130,
*                 132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
          gs_outtab_chkdet-bankl     = head-bankl.
          gs_outtab_chkdet-bankn     = head-accnr.
          gs_outtab_chkdet-bukrs     = t012k-bukrs.
          gs_outtab_chkdet-messtext  = char130.
          gs_outtab_chkdet-cknum     = ckrec-cknum.
          APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
          CLEAR gs_outtab_chkdet.
*********End of coding ALV C5053249***************************
          IF skb1-xgkon IS INITIAL.                           "30D
            char130 = TEXT-023.                               "30D
            REPLACE '&' WITH belnr      INTO char130.         "30D
            REPLACE '&' WITH bseg-hkont INTO char130.         "30D
*********Begin of comment ALV C5053249***************************
*            WRITE: /01 SY-VLINE,                              "30D
*                    31 CHAR130,                               "30D
*                   132 SY-VLINE.                              "30D
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
            gs_outtab_chkdet-bankl        = head-bankl.
            gs_outtab_chkdet-bankn        = head-accnr.
            gs_outtab_chkdet-bukrs        = t012k-bukrs.
            gs_outtab_chkdet-messtext+31  = char130.
            gs_outtab_chkdet-cknum        = ckrec-cknum.
            APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
            CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
            char130 = TEXT-024.                               "30D
            REPLACE '&' WITH bseg-hkont INTO char130.         "30D
*********Begin of comment ALV C5053249***************************
*            WRITE: /01 SY-VLINE,                              "30D
*                    31 CHAR130,                               "30D
*                   132 SY-VLINE.                              "30D
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
            gs_outtab_chkdet-bankl        = head-bankl.
            gs_outtab_chkdet-bankn        = head-accnr.
            gs_outtab_chkdet-bukrs        = t012k-bukrs.
            gs_outtab_chkdet-messtext+31  = char130.
            gs_outtab_chkdet-cknum        = ckrec-cknum.
            APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
            CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
          ENDIF.                                              "30D
          FORMAT RESET.
          IF payr_ok = true.
            PERFORM update_payr.
          ENDIF.
        ENDIF.                                              "ak291099
        IF testrun NE 'X'.
          EXTRACT datarec.             "No clearing data -> extract
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.                                                    "ak191099

*--- if no item with right account number: send message
  IF item = false.
    char130 = TEXT-017.
    REPLACE '&' WITH ckrec-cknum INTO char130.
    REPLACE '&' WITH belnr       INTO char130.
*********Begin of comment ALV C5053249***************************
*    FORMAT COLOR COL_NEGATIVE.
*    WRITE: /01 SY-VLINE,
*            02 CHAR130,
*           132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    gs_outtab_chkdet-bankl       = head-bankl.
    gs_outtab_chkdet-bankn       = head-accnr.
    gs_outtab_chkdet-bukrs       = t012k-bukrs.
    gs_outtab_chkdet-messtext+2  = char130.
    gs_outtab_chkdet-cknum       = ckrec-cknum.
    APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
    CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
    IF payr_ok = true.
      CLEAR: char130.
      MOVE TEXT-064 TO char130.
*********Begin of comment ALV C5053249***************************
*      WRITE: /01 SY-VLINE,
*              31 CHAR130,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_chkdet-bankl        = head-bankl.
      gs_outtab_chkdet-bankn        = head-accnr.
      gs_outtab_chkdet-bukrs        = t012k-bukrs.
      gs_outtab_chkdet-messtext+31  = char130.
      gs_outtab_chkdet-cknum        = ckrec-cknum.
      APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
      CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
      PERFORM counter_no_valid_item.
    ELSE.
      PERFORM counter_fi_no_valid_item.
    ENDIF.
*********Begin of comment ALV C5053249***************************
*    FORMAT RESET.
***********End of comment ALV C5053249***************************
* item is true
  ELSE.
    IF payr_ok = true.
      IF item_open = false.
        PERFORM counter_prenums_no_open_item.
      ELSE.
        PERFORM counter_valid_other_prenums.
        IF amount_ok = true.
          PERFORM counter_oprenums_amount_ok.
        ELSE.
          PERFORM counter_oprenums_amount_ne.
        ENDIF.
      ENDIF.
    ELSE.
      IF item_open = false.
        PERFORM counter_fi_no_open_item.
      ELSE.
        PERFORM counter_valid_fi_op.
        IF amount_ok = true.
          PERFORM counter_fi_amount_ok.
        ELSE.
          PERFORM counter_fi_amount_ne.
        ENDIF.
      ENDIF.

    ENDIF.
*********Begin of comment ALV C5053249***************************
*    FORMAT RESET.
***********End of comment ALV C5053249***************************
  ENDIF.
ENDFORM.                    "EXTRACT_CHECK

*eject
*---------------------------------------------------------------*
* FORM PROCESS_TRAILER.
*---------------------------------------------------------------*
*                                                               *
*---------------------------------------------------------------*
FORM update_payr.
  payr-xbanc = 'X'.
  IF NOT pdate IS INITIAL.
    payr-bancd = pdate.
  ELSE.
    payr-bancd = valut.
  ENDIF.

  char130 = TEXT-130.                               "insert note 0217171
  REPLACE '&' WITH payr-chect INTO char130.         "insert note 0217171

*  FORMAT COLOR COL_NORMAL.
  IF testrun NE 'X'.
    UPDATE payr.
    IF sy-subrc = 0.
*      IF P_PROT = 'X'.
      IF NOT pdate IS INITIAL.                              "hw441845
*********Begin of comment ALV C5053249***************************
*         WRITE: /01 SY-VLINE,
*              03 CHAR130,              "insert note 0217171
*              31 TEXT-046,
*             132 SY-VLINE.
*********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        CONCATENATE char130 TEXT-046 INTO gs_outtab_chkdet-messtext
                                     SEPARATED BY space.
        gs_outtab_chkdet-bankl     = head-bankl.
        gs_outtab_chkdet-bankn     = head-accnr.
        gs_outtab_chkdet-bukrs     = t012k-bukrs.
        gs_outtab_chkdet-cknum     = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
      ELSE.                                                 "hw441845
*********Begin of comment ALV C5053249***************************
*         WRITE: /01 SY-VLINE,          "hw441845
*              03 CHAR130,              "hw441845
*              31 TEXT-040,             "hw441845
*             132 SY-VLINE.             "hw441845
*********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
        CONCATENATE char130 TEXT-040 INTO gs_outtab_chkdet-messtext
                                     SEPARATED BY space.
        gs_outtab_chkdet-bankl     = head-bankl.
        gs_outtab_chkdet-bankn     = head-accnr.
        gs_outtab_chkdet-bukrs     = t012k-bukrs.
        gs_outtab_chkdet-cknum     = ckrec-cknum.
        APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
        CLEAR gs_outtab_chkdet.
*********End of coding ALV C5053249***************************
      ENDIF.                                                "hw441845
*      ENDIF.
    ELSE.
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
*      WRITE: /01 SY-VLINE,
*              03 CHAR130,                       "insert note 0217171
*              31 TEXT-047,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      CONCATENATE char130 TEXT-047 INTO gs_outtab_chkdet-messtext
                                     SEPARATED BY space.
      gs_outtab_chkdet-bankl     = head-bankl.
      gs_outtab_chkdet-bankn     = head-accnr.
      gs_outtab_chkdet-bukrs     = t012k-bukrs.
      gs_outtab_chkdet-cknum     = ckrec-cknum.
      APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
      CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
    ENDIF.
  ELSE.
*********Begin of comment ALV C5053249***************************
*    WRITE: /01 SY-VLINE,
*            03 CHAR130,                         "insert note 0217171
*            31 TEXT-045,
*           132 SY-VLINE.
*********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    CONCATENATE char130 TEXT-045 INTO gs_outtab_chkdet-messtext
                                     SEPARATED BY space.
    gs_outtab_chkdet-bankl     = head-bankl.
    gs_outtab_chkdet-bankn     = head-accnr.
    gs_outtab_chkdet-bukrs     = t012k-bukrs.
    gs_outtab_chkdet-cknum     = ckrec-cknum.
    APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
    CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of comment ALV C5053249***************************
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
ENDFORM.                    "UPDATE_PAYR

*eject
*---------------------------------------------------------------*
* FORM PROCESS_TRAILER.
*---------------------------------------------------------------*
*                                                               *
*---------------------------------------------------------------*
FORM process_trailer.
*********Begin of comment ALV C5053249***************************
*  ULINE.
*  NEW-PAGE.
***********ENd of comment ALV C5053249***************************
ENDFORM.                    "PROCESS_TRAILER

*eject
*---------------------------------------------------------------*
* FORM PRINT_CHECK_FILE.
*---------------------------------------------------------------*
FORM print_check_file.
*********Begin of comment ALV C5053249***************************
*  CLEAR:  BHDGD-LINE1, BHDGD-LINE2.
*  BHDGD-LINE1 = SY-TITLE.
*  CHAR60 = TEXT-002.
*  REPLACE '&' WITH CK_FILE INTO CHAR60.
*  BHDGD-LINE2 = CHAR60.
*  BHDGD-INIFL = '0'.
*  NEW-PAGE.
***********End of comment ALV C5053249***************************

  LOOP AT checks.
*********Begin of comment ALV C5053249***************************
*    WRITE: / CHECKS-REC.
*********ENd of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    MOVE: checks-rec TO gs_outtab_file-record.
    APPEND gs_outtab_file TO gt_outtab_file.
    CLEAR gs_outtab_file.
***********End of coding ALV C5053249***************************
  ENDLOOP.
ENDFORM.                    "PRINT_CHECK_FILE

*eject
*---------------------------------------------------------------*
*  FORM READ_DATA.
*---------------------------------------------------------------*
*  Read Data from PC or UNIX-File-System into table CHECKS      *
*---------------------------------------------------------------*
FORM read_data.
  DATA: l_ck_file TYPE                   string,                                 "UC
        l_checks  TYPE STANDARD TABLE OF string.                "UC
  DATA: l_upload_codepage TYPE abap_encoding.               "n1971966
  REFRESH: checks.
  IF pcupload = 'X'.
*   upload data from pc-drive
    l_ck_file = ck_file.                                       "UC
    GET PARAMETER ID 'UCP' FIELD l_upload_codepage.         "n1971966
* Use function instead of method suppressing S_GUI             "n1971966
*    CALL METHOD cl_gui_frontend_services=>gui_upload           "UC
*      EXPORTING                                                   "
*        filename                = l_ck_file                       "
*        filetype                = 'ASC'                           "
*      CHANGING                                                    "
*        data_tab                = l_checks                        "
*      EXCEPTIONS                                                  "
*        file_open_error         = 1                               "
*        file_read_error         = 2                               "
*        OTHERS                  = 18.                         "n1971966
*    CALL FUNCTION 'WS_UPLOAD'                                  "UC
*         EXPORTING
*              FILENAME        = CK_FILE
*              FILETYPE        = 'ASC'
*         TABLES
*              DATA_TAB        = CHECKS
*         EXCEPTIONS
*              FILE_OPEN_ERROR = 1
*              FILE_READ_ERROR = 2.
    CALL FUNCTION 'GUI_UPLOAD'                                 "n1971966
      EXPORTING                                               "n1971966
        filename        = l_ck_file             "n1971966
        filetype        = 'ASC'                 "n1971966
        codepage        = l_upload_codepage     "n1971966
        no_auth_check   = 'X'                   "n1971966
      TABLES                                                   "n1971966
        data_tab        = l_checks              "n1971966
      EXCEPTIONS                                               "n1971966
        file_open_error = 1                     "n1971966
        file_read_error = 2                     "n1971966
        OTHERS          = 18.                   "n1971966
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e704 WITH ck_file+2 ck_file+0(2).
      WHEN 2.
        MESSAGE e705 WITH ck_file+2 ck_file+0(2).
      WHEN OTHERS.
    ENDCASE.
    checks[] = l_checks[].                                      "UC
  ELSEIF pcupload = 'T'.
*   import data from database
    IMPORT check_file FROM DATABASE rfdt(ck) ID ck_file.

    IF sy-subrc = 0.
      LOOP AT check_file.
        checks = check_file.
        APPEND checks.
      ENDLOOP.
    ELSE.
      MESSAGE e707 WITH ck_file.
    ENDIF.
  ELSE.
*   upload data from unix-file-system
*------- open check-file  --------------------------------------
* begin "n1517472
    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        logical_filename  = gc_filename
        parameter_1       = sy-repid
*       parameter_2       = i_blart                         "n2071858
*       parameter_3       = group                           "n2071858
      CHANGING
        physical_filename = ck_file
      EXCEPTIONS
        OTHERS            = 1.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
* end "n1517472
    OPEN DATASET ck_file IN TEXT MODE FOR INPUT ENCODING DEFAULT. "UC
    IF sy-subrc NE 0.
      MESSAGE e002 WITH ck_file.
    ENDIF.

*------- store checks in internal Table CHECKS ------------------
    DO.
      CLEAR checks.
      READ DATASET ck_file INTO checks-rec.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.
      APPEND checks.
    ENDDO.
    CLOSE DATASET ck_file.
  ENDIF.
ENDFORM.                    "READ_DATA
*---------------------------------------------------------------*
* FORM CALCULATE_VALUTA.                                        *
*---------------------------------------------------------------*
*       Falls Feld-2 des Einzel-Datensatzes gefüllt ist,        *
*       wird dieser Wert (nach einer Prüfung) als Valutadatum   *
*       genommen, ansonsten wird das Tagesdatum genommen.       *
*---------------------------------------------------------------*
FORM calculate_valuta.

  IF ckrec-valut IS INITIAL.
    valut = sy-datum.
  ELSE.
    valut = ckrec-valut.
  ENDIF.
ENDFORM.                    "CALCULATE_VALUTA

*---------------------------------------------------------------*
*    FORM CURRENCY_CHECK.                                       *
*---------------------------------------------------------------*
*    Check if to post in local curreny                          *
*---------------------------------------------------------------*
FORM currency_check.
  waers = bkpf-waers.
  IF waers NE localcurr.                                    "mp45a
    CLEAR: xlocalcurr.                                      "mp45a
  ELSE.                                                     "mp45a
    xlocalcurr = 'X'.                                       "mp45a
  ENDIF.                                                    "mp45a
* IF BKPF-WAERS EQ T001-WAERS.
*   LOCALCURR = TRUE.
* ELSE.
*   LOCALCURR = FALSE.
* ENDIF.
ENDFORM.                    "CURRENCY_CHECK

*eject
*---------------------------------------------------------------------*
*       FORM POSTING_INTERFACE_END.
*---------------------------------------------------------------------*
FORM posting_interface_end.
* Buchungsschnittstelle schließen
  IF testrun NE 'X'.
    CALL FUNCTION 'POSTING_INTERFACE_END'.
  ENDIF.

  open = false.
ENDFORM.                    "POSTING_INTERFACE_END


*---------------------------------------------------------------------*
*       FORM POSTING_INTERFACE_START.
*---------------------------------------------------------------------*
FORM posting_interface_start.
  DATA: i_user LIKE apqi-userid.

  i_user = sy-uname.

* Interne Buchungsschnittstelle initialisieren
  IF testrun NE 'X' AND open = false.
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_function = function
        i_mode     = mode
        i_group    = groupname
        i_user     = i_user
        i_xbdcc    = xbdcc.
  ENDIF.
*********Begin of comment ALV C5053249***************************

*     Print Posting Information: Head-Data of new session
*  IF P_POST = 'X'.
*    CLEAR:  BHDGD-LINE1, BHDGD-LINE2.
*    BHDGD-LINE1 = SY-TITLE.
*    IF FUNCTION = CALLTRANS.
*      CHAR60 = TEXT-005.
*    ELSE.
*      CHAR60 = TEXT-004.
*      REPLACE '&' WITH GROUPNAME INTO CHAR60.
*    ENDIF.
*    BHDGD-LINE2 = CHAR60.
*    BHDGD-INIFL = '0'.
*    CLEAR TRANS_CNT.
*    IF FUNCTION = BATCHINPUT.
*      NEW-PAGE.
*    ENDIF.
*  ENDIF.
***********End of comment ALV C5053249***************************
  open = true.
ENDFORM.                    "POSTING_INTERFACE_START
*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM authority_check.
  AUTHORITY-CHECK OBJECT 'F_PAYR_BUK'
    ID 'BUKRS' FIELD t012k-bukrs
    ID 'ACTVT' FIELD '84'.
  IF sy-subrc NE 0.
    MESSAGE e639(fs) WITH t012k-bukrs.
  ENDIF.
ENDFORM.                               " AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  PRINT_CHECK_VOIDED                               "30C
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_check_voided.
  DATA: c_voidd(10) TYPE c.
  WRITE payr-voidd TO c_voidd DD/MM/YYYY.
  char130 = TEXT-021.
  REPLACE '&' WITH ckrec-cknum INTO char130.
  REPLACE '&' WITH payr-voidu  INTO char130.
  REPLACE '&' WITH c_voidd     INTO char130.
  SELECT SINGLE * FROM tvoit WHERE langu = sy-langu
                               AND voidr = payr-voidr.
  IF sy-subrc = 0.
    REPLACE '&' WITH tvoit-voidt INTO char130.
  ENDIF.
*********Begin of comment ALV C5053249***************************

*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*             CHAR130,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_chkdet-bankl     = head-bankl.
  gs_outtab_chkdet-bankn     = head-accnr.
  gs_outtab_chkdet-bukrs     = t012k-bukrs.
  gs_outtab_chkdet-messtext  = char130.
  gs_outtab_chkdet-cknum     = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
***********Begin of comment ALV C5053249***************************
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
ENDFORM.                               " PRINT_CHECK_VOIDED
*&---------------------------------------------------------------------*
*&      Form  PRINT_MULTIPLE_PAY_METHODS                     "30C
*&---------------------------------------------------------------------*
*       Check is in multiple payment methods. no posting               *
*----------------------------------------------------------------------*
FORM print_multiple_pay_methods.
  char130 = TEXT-022.
  REPLACE '&' WITH ckrec-cknum INTO char130.
*********Begin of comment ALV C5053249***************************

*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*             CHAR130,
*         132 SY-VLINE.
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_chkdet-bankl     = head-bankl.
  gs_outtab_chkdet-bankn     = head-accnr.
  gs_outtab_chkdet-bukrs     = t012k-bukrs.
  gs_outtab_chkdet-messtext  = char130.
  gs_outtab_chkdet-cknum     = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
ENDFORM.                               " PRINT_MULTIPLE_PAY_METHODS
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_HR_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM extract_hr_check.
  DATA: char35(35) TYPE c.

*--- Check if valid HR check ------------------------------------------*

*--- Set header data --------------------------------------------------*
  status = 'P'.
*       hdcnt determined at new head line index
*       valut determined by single record ckrec-valut
*       budat determined by 1. selection screen or 2. head-crdat
*       bldat determined by 1. selection screen or 2. head-crdat
  waers = payr-waers.
*--- set xlocalcurr for x-rate date
  IF waers NE localcurr.                                    "mp45a
    CLEAR: xlocalcurr.                                      "mp45a
  ELSE.                                                     "mp45a
    xlocalcurr = 'X'.                                       "mp45a
  ENDIF.                                                    "mp45a
*--- set datarec data
*       ckrec-cknum determined by single record ckrec
  CLEAR: belnr.
*       AMOUNT_CHK determined by check_amount_single_check
  CLEAR: amount_doc.
*       pdate determined by single record ckrec-pdate
*       T012k determined by new head line
*       HBKID determined by new head line
*       HKTID determined by new head line
*       AGKON  determined by T042I-UKONT
  CLEAR: char35.
  CONCATENATE payr-laufd payr-laufi INTO char35.
  MOVE payr-rzawe TO char35+28(1).
  MOVE payr-strgb TO char35+30(4).
  sortkrit = char35.

  IF testrun NE 'X'.
    EXTRACT datarec.
  ENDIF.
ENDFORM.                               " EXTRACT_HR_CHECK

*&---------------------------------------------------------------------*
*&      Form  PRINT_HR_CHECK_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CKREC_CKNUM  text                                          *
*      -->P_PAYR_PERNR  text                                           *
*----------------------------------------------------------------------*
FORM print_hr_check_info USING  cknum LIKE ckrec-cknum.

  char130 = TEXT-019.
  REPLACE '&' WITH cknum INTO char130.
*********Begin of comment ALV C5053249***************************
*  FORMAT COLOR COL_NORMAL.
*  WRITE: /01 SY-VLINE,
*             CHAR130,
*         132 SY-VLINE.
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_chkdet-bankl     = head-bankl.
  gs_outtab_chkdet-bankn     = head-accnr.
  gs_outtab_chkdet-bukrs     = t012k-bukrs.
  gs_outtab_chkdet-messtext  = char130.
  gs_outtab_chkdet-cknum     = cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
ENDFORM.                               " PRINT_HR_CHECK_INFO
*&---------------------------------------------------------------------*
*&      Form  BELEGKOPF_HR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM belegkopf_hr.

* clear and refresh ftpost
  REFRESH: ftpost.
  CLEAR: ftpost.
* fill FTPOST header entries
  ftpost-stype = 'K'.
  ftpost-count = '1'.
* Belegdatum
* document date
  WRITE bldat TO fvalue DD/MM/YYYY.
  PERFORM ftpost_field USING 'BKPF-BLDAT' fvalue.
* Belegart
* document type
  WRITE i_blart TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BLART' fvalue.
* Buchungskreis
* company code
  WRITE t012k-bukrs TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BUKRS' fvalue.
* Buchungsdatum
* posting date
  WRITE budat TO fvalue DD/MM/YYYY.
  PERFORM ftpost_field USING 'BKPF-BUDAT' fvalue.
* Währung
* currency
  WRITE waers TO fvalue.
  PERFORM ftpost_field USING 'BKPF-WAERS' fvalue.
* Umrechnungsdatum
* translation date
  IF NOT pdate IS INITIAL AND xlocalcurr IS INITIAL.        "mp45A
* provide chrec-pdate as FX-translation date if foreign currency
    WRITE pdate    TO fvalue DD/MM/YYYY.                    "mp45A
    PERFORM ftpost_field USING 'BKPF-WWERT' fvalue.         "mp45A
  ENDIF.                                                    "mp45A
* Belegkopftext
* document header text
  WRITE 'HR-Schecks'(053) TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BKTXT' fvalue.

  ftpost-count = '0'.
ENDFORM.                               " BELEGKOPF_HR

*&---------------------------------------------------------------------*
*&      Form  FB01_POSTING_AUFRUFEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fb01_posting_aufrufen.

  CLEAR: blntab, fttax.
  REFRESH: blntab, fttax.

  trans_cnt = trans_cnt + 1.

  CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
    EXPORTING
      i_tcode  = tcode
    IMPORTING
      e_msgid  = msgid
      e_msgno  = msgno
      e_msgty  = msgty
      e_msgv1  = msgv1
      e_msgv2  = msgv2
      e_msgv3  = msgv3
      e_msgv4  = msgv4
      e_subrc  = subrc
    TABLES
      t_blntab = blntab
      t_ftpost = ftpost
      t_fttax  = fttax
    EXCEPTIONS
*     ACCOUNT_MISSING          = 1
*     COMPANY_CODE_MISSING     = 2
*     POSTING_KEY_INVALID      = 3
*     POSTING_KEY_MISSING      = 4
*     RECORD_TYPE_INVALID      = 5
*     TRANSACTION_CODE_INVALID = 6
*     AMOUNT_FORMAT_ERROR      = 7
*     TOO_MANY_LINE_ITEMS      = 8
      OTHERS   = 9.

  IF sy-subrc IS INITIAL.
*  CASE SY-SUBRC.
*    WHEN 0.
    IF subrc EQ 0.
      PERFORM list_compressed_check_nrs.
      PERFORM counter_success.
      IF function = calltrans.
        PERFORM druck_message.
      ENDIF.
    ELSE.
      PERFORM list_compressed_check_nrs.
      PERFORM counter_failure.
      PERFORM druck_message.
    ENDIF.
  ELSE.
*    WHEN 1.
**     ACCOUNT_MISSING
*      MESSAGE E743 WITH AGKON ' '.
*    WHEN 2.
**     COMPANY_CODE_MISSING
*     already checked
*    WHEN 3.
**     POSTING_KEY_INVALID
*      MESSAGE E741.
*    WHEN 4.
**     POSTING_KEY_MISSING
*      MESSAGE E741.
*    WHEN 5.
**     RECORD_TYPE_INVALID
*      MESSAGE E076.
**  WHEN 6.
**     TRANSACTION_CODE_INVALID
**     always FB01
*    WHEN 7.
**     AMOUNT_FORMAT_ERROR
*      MESSAGE E079.
**  WHEN 8.
**     TOO_MANY_LINE_ITEMS
**     always compressed posting -> two line items
*    WHEN 9.
*     Other error with posting_interface_document
*      MESSAGE E081.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDCASE.
  ENDIF.
ENDFORM.                               " FB01_POSTING_AUFRUFEN
*&---------------------------------------------------------------------*
*&      Form  FB01_BSEG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fb01_bseg.
  ftpost-stype = 'P'.
*  Sollzeile
  PERFORM fb01_bseg_soll.
*  Habenzeile
  PERFORM fb01_bseg_haben.
ENDFORM.                                                    " FB01_BSEG

*&---------------------------------------------------------------------*
*&      Form  FB01_BSEG_SOLL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fb01_bseg_soll.
  DATA: char35(35) TYPE c.
  DATA: faus1(50) TYPE c.

  ftpost-count = ftpost-count + 1.
  PERFORM ftpost_field USING 'RF05A-NEWBS' '40'.
  WRITE agkon TO fvalue .
  PERFORM ftpost_field USING 'RF05A-NEWKO' fvalue.
  PERFORM amounts_to_ftpost USING amount_sum.
  IF NOT sortkrit+30(4) IS INITIAL.
    CALL FUNCTION 'FI_FIELD_SELECTION_DETERMINE'           "note 786178
      EXPORTING
        i_bschl     = '40'
        i_bukrs     = t012k-bukrs
        i_saknr     = t012k-hkont
      IMPORTING
        e_faus1     = faus1
      EXCEPTIONS
        customizing = 1
        OTHERS      = 2.
    IF sy-subrc = 0 AND faus1+32(1) NE '-'.
      MOVE sortkrit+30(4) TO fvalue.
      PERFORM ftpost_field USING 'COBL-GSBER' fvalue.
    ENDIF.
  ENDIF.
  IF NOT valut IS INITIAL.
    WRITE valut TO fvalue DD/MM/YYYY.
    PERFORM ftpost_field USING 'BSEG-VALUT' fvalue.       " Value Date
  ENDIF.
  MOVE sortkrit TO char35.
  WRITE char35+0(18) TO fvalue.
  PERFORM ftpost_field USING 'BSEG-ZUONR' fvalue.
ENDFORM.                               " FB01_BSEG_SOLL

*&---------------------------------------------------------------------*
*&      Form  FB01_BSEG_HABEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fb01_bseg_haben.
  DATA: char35(35) TYPE c.
  DATA: faus1(50) TYPE c.

  ftpost-count = ftpost-count + 1.
  PERFORM ftpost_field USING 'RF05A-NEWBS' '50'.
  WRITE t012k-hkont TO fvalue .
  PERFORM ftpost_field USING 'RF05A-NEWKO' fvalue.
  PERFORM amounts_to_ftpost USING amount_sum.
  IF NOT sortkrit+30(4) IS INITIAL.
    CALL FUNCTION 'FI_FIELD_SELECTION_DETERMINE'      "note 786178
      EXPORTING
        i_bschl     = '50'
        i_bukrs     = t012k-bukrs
        i_saknr     = t012k-hkont
      IMPORTING
        e_faus1     = faus1
      EXCEPTIONS
        customizing = 1
        OTHERS      = 2.
    IF sy-subrc = 0 AND faus1+32(1) NE '-'.
      MOVE sortkrit+30(4) TO fvalue.
      PERFORM ftpost_field USING 'COBL-GSBER' fvalue.
    ENDIF.
  ENDIF.
  IF NOT valut IS INITIAL.
    WRITE valut TO fvalue DD/MM/YYYY.
    PERFORM ftpost_field USING 'BSEG-VALUT' fvalue.       " Value Date
  ENDIF.
  MOVE sortkrit TO char35.
  MOVE char35+0(18) TO fvalue.
  PERFORM ftpost_field USING 'BSEG-ZUONR' fvalue.
ENDFORM.                               " FB01_BSEG_HABEN

*&---------------------------------------------------------------------*
*&      Form  AMOUNTS_TO_FTPOST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AMOUNT_SUM  text                                           *
*----------------------------------------------------------------------*
FORM amounts_to_ftpost USING amount_tot LIKE bseg-wrbtr.
  WRITE amount_tot TO fvalue CURRENCY waers.
  CONDENSE fvalue.
  PERFORM ftpost_field USING 'BSEG-WRBTR' fvalue.
ENDFORM.                               " AMOUNTS_TO_FTPOST
*&---------------------------------------------------------------------*
*&      Form  DRUCK_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM druck_message.
  DATA: char300(300) TYPE c.

  CLEAR char300.
  CLEAR t100.
* CLEAR ERR_CNT.

  SELECT SINGLE * FROM t100
          WHERE sprsl = sy-langu
            AND arbgb = msgid
            AND msgnr = msgno.

* Error in Posting interface -> Subrc ne 0
  IF subrc NE 0.
    err_cnt = err_cnt + 1.
    char300 = '>>>> Fehler & &:'(060).
    REPLACE '&' WITH msgid INTO char300.
    REPLACE '&' WITH msgno INTO char300.
    CONDENSE char300.
  ENDIF.

  char300+30   = t100-text.
  REPLACE '&1'   WITH '&'   INTO char300.
  REPLACE '&2'   WITH '&'   INTO char300.
  REPLACE '&3'   WITH '&'   INTO char300.
  REPLACE '&4'   WITH '&'   INTO char300.
  REPLACE '&V1&' WITH '&'   INTO char300.
  REPLACE '&V2&' WITH '&'   INTO char300.
  REPLACE '&V3&' WITH '&'   INTO char300.
  REPLACE '&V4&' WITH '&'   INTO char300.
  REPLACE '&v1&' WITH '&'   INTO char300.
  REPLACE '&v2&' WITH '&'   INTO char300.
  REPLACE '&v3&' WITH '&'   INTO char300.
  REPLACE '&v4&' WITH '&'   INTO char300.
  REPLACE '&' WITH msgv1 INTO char300.
  REPLACE '&' WITH msgv2 INTO char300.
  REPLACE '&' WITH msgv3 INTO char300.
  REPLACE '&' WITH msgv4 INTO char300.
  CONDENSE char300.
*********Begin of comment ALV C5053249***************************
* FORMAT INTENSIFIED OFF.
*  IF SUBRC EQ 0.
*    FORMAT COLOR COL_TOTAL.
*  ELSE.
*    FORMAT COLOR COL_NEGATIVE.
*  ENDIF.
*  WRITE: /01 SY-VLINE.
*  WRITE:    10(110)  CHAR300 ,
*         132 SY-VLINE.
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  CLEAR gs_outtab_hrpost.
  MOVE gc_hrpost TO gs_outtab_hrpost-cknum.
  MOVE char300 TO gs_outtab_hrpost-messtext+10(110).
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR gs_outtab_hrpost.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DRUCK_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_POSTING_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_posting_info.
* print posting information. footer-line
*  if p_post = 'X'.
*********Begin of comment ALV C5053249***************************
*  FORMAT RESET.
*  FORMAT COLOR COL_TOTAL.
*  ULINE.
***********End of comment ALV C5053249***************************
  IF function = calltrans.
*     Call transaction
    IF err_cnt > 0 AND testrun NE 'X'.
*********Begin of comment ALV C5053249***************************
*      FORMAT COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
      MOVE TEXT-043 TO char130.
      REPLACE '&' WITH groupname INTO char130.
      REPLACE '&' WITH err_cnt   INTO char130.
*********Begin of comment ALV C5053249***************************
*      WRITE: /01 SY-VLINE,
*                 CHAR130,
*             132 SY-VLINE.
*      ULINE.
*      FORMAT COLOR COL_TOTAL.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
      MOVE: groupname TO gs_tab_postinfo-status,
            char130   TO gs_tab_postinfo-char130.
      APPEND gs_tab_postinfo TO gt_tab_postinfo.
      CLEAR gs_tab_postinfo.
***********End of coding ALV C5053249***************************
    ENDIF.
  ELSE.
*     batch input mode
    IF testrun = 'X'.
      MOVE TEXT-044 TO char130.
    ELSE.
      MOVE TEXT-043 TO char130.
    ENDIF.
    REPLACE '&' WITH groupname INTO char130.
    REPLACE '&' WITH trans_cnt INTO char130.
*********Begin of comment ALV C5053249***************************
*    WRITE: /01 SY-VLINE,
*               CHAR130,
*           132 SY-VLINE.
*    ULINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    MOVE: groupname TO gs_tab_postinfo-status,
          char130   TO gs_tab_postinfo-char130.
    APPEND gs_tab_postinfo TO gt_tab_postinfo.
    CLEAR gs_tab_postinfo.
***********End of coding ALV C5053249***************************
  ENDIF.
*  endif.
ENDFORM.                               " DISPLAY_POSTING_INFO
*&---------------------------------------------------------------------*
*&      Form  COLLECT_FOR_PAYR_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM collect_for_payr_update.
  CLEAR: collect_tab.

  MOVE: t012-bukrs TO collect_tab-bukrs,
        t012-hbkid TO collect_tab-hbkid,
        t012k-hktid TO collect_tab-hktid,
        ckrec-cknum TO collect_tab-chect.
  APPEND collect_tab.
ENDFORM.                               " COLLECT_FOR_PAYR_UPDATE
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PAYR_REC_BELNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_payr_rec_belnr.

  IF function = batchinput.
    blntab-belnr = '*'.
  ENDIF.

  LOOP AT collect_tab.
    UPDATE payr SET rec_belnr = blntab-belnr
                    rec_gjahr = blntab-gjahr
                WHERE zbukr = t012-bukrs
                  AND hbkid = hbkid
                  AND hktid = hktid
                  AND ichec = space                         "ak280100
                  AND chect = collect_tab-chect.
  ENDLOOP.
ENDFORM.                               " UPDATE_PAYR_REC_BELNR
*&---------------------------------------------------------------------*
*&      Form  DETERMINE_HR_INTERIM_ACCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VRKTO_HR  text                                             *
*----------------------------------------------------------------------*
FORM determine_hr_interim_account USING p_acc_ok.
  STATICS: bukrs_old LIKE t012-bukrs,
           hbkid_old LIKE t012-hbkid,
           hktid_old LIKE t012k-hktid,
           waers_old LIKE payr-waers,
           rzawe_old LIKE payr-rzawe,
           agkon_old LIKE agkon.

  CLEAR: t042i.

  IF    t012-bukrs NE bukrs_old
     OR hbkid NE hbkid_old
     OR hktid NE hktid_old
     OR payr-rzawe NE rzawe_old
     OR payr-waers NE waers_old.

    IF payr-rzawe IS INITIAL.
*   manually issued check
      p_acc_ok = 'M'.
    ELSE.
*   no manually written check
      p_acc_ok = true.

      SELECT SINGLE * FROM  t042i
             WHERE  zbukr       = t012-bukrs
             AND    hbkid       = hbkid
             AND    zlsch       = payr-rzawe
             AND    waers       = payr-waers
             AND    hktid       = hktid .

      IF sy-subrc NE 0.
        SELECT SINGLE * FROM  t042i
               WHERE  zbukr       = t012-bukrs
               AND    hbkid       = hbkid
               AND    zlsch       = payr-rzawe
               AND    waers       = space
               AND    hktid       = hktid .
        IF sy-subrc NE 0.
          p_acc_ok = false.
        ELSEIF t042i-ukont IS INITIAL.
          p_acc_ok = false.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_acc_ok = true OR p_acc_ok = 'M'.
      IF p_acc_ok = true.
        agkon_old = t042i-ukont.
      ELSEIF p_acc_ok = 'M'.
        CLEAR: agkon_old.
      ENDIF.
    ENDIF.

    MOVE:  t012-bukrs   TO bukrs_old,
           hbkid        TO hbkid_old,
           hktid        TO hktid_old,
           payr-rzawe   TO rzawe_old,
           payr-waers   TO waers_old.
  ENDIF.

  IF p_acc_ok = true OR p_acc_ok = 'M'.
    agkon = agkon_old.
  ELSE.
    CLEAR: agkon.
  ENDIF.
ENDFORM.                               " DETERMINE_HR_INTERIM_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  LIST_COMPRESSED_CHECK_NRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_compressed_check_nrs.
  DATA: col TYPE i.
  DATA: cnt_index TYPE i.
  DATA: laufi LIKE payr-laufi.
  DATA: char8 LIKE payr-laufd.
  DATA: i_sortkrit LIKE sortkrit.
********Begin of coding ALV C5053249***********************************
  DATA: lv_text(130) TYPE c.
**********End of coding ALV C5053249***********************************
  MOVE sortkrit TO i_sortkrit.
  MOVE i_sortkrit+0(8) TO char8.
  SHIFT i_sortkrit LEFT BY 8 PLACES.
  MOVE i_sortkrit TO laufi.
*********Begin of comment ALV C5053249***************************
*  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
*  ULINE.
*  WRITE: /01 SY-VLINE,
*           3 'Schecknummern der komprimierten HR-Buchung:'(054),
*         132 SY-VLINE.
*  WRITE: /01 SY-VLINE,
*           3 'Tag der Ausführung:'(059),
*             CHAR8 DD/MM/YYYY,
*         132 SY-VLINE,
*         /01 SY-VLINE,
*           3'Identifikation:'(061),
*             LAUFI,
*         132 SY-VLINE.
*  FORMAT COLOR COL_NORMAL.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-054 TO gs_outtab_hrpost-messtext+3.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR gs_outtab_hrpost.

  CLEAR lv_text.
  CONCATENATE TEXT-059 char8 INTO lv_text+3 SEPARATED BY space.
  MOVE lv_text TO gs_outtab_hrpost-messtext.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR: gs_outtab_hrpost,
         lv_text.

  CONCATENATE TEXT-061 laufi INTO lv_text+3 SEPARATED BY space.
  MOVE lv_text TO gs_outtab_hrpost-messtext.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR: gs_outtab_hrpost,
         lv_text.
***********End of coding ALV C5053249***************************
  DESCRIBE TABLE collect_tab LINES lin_collect_tab.
  SORT collect_tab BY chect.
  DO.
    READ TABLE collect_tab INDEX sy-index.
    IF sy-subrc NE 0.
*********Begin of comment ALV C5053249***************************
*      WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
      EXIT.
    ELSE.
*********Begin of comment ALV C5053249***************************
*      CNT_INDEX = CNT_INDEX + 1.
*      IF CNT_INDEX = 1.
*        WRITE: /01 SY-VLINE.
*        WRITE AT 6 COLLECT_TAB-CHECT.
*      ELSEIF CNT_INDEX LT 9.
*        COL = ( CNT_INDEX * 15 ) - 9.
*        WRITE AT COL COLLECT_TAB-CHECT.
*      ELSE.
*        WRITE: 132 SY-VLINE.
*        WRITE: /01 SY-VLINE.
*        WRITE AT 6 COLLECT_TAB-CHECT.
*        CNT_INDEX = 1.
*      ENDIF.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
      MOVE collect_tab-chect TO gs_outtab_hrpost-cknum .
      MOVE space TO gs_outtab_hrpost-messtext.
      APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
      CLEAR gs_outtab_hrpost.
***********End of coding ALV C5053249***************************
    ENDIF.
  ENDDO.
***********Begin of coding ALV C5053249***************************
  CLEAR lv_text.
  MOVE TEXT-062 TO lv_text.
  WRITE lin_collect_tab TO lv_text+22 LEFT-JUSTIFIED.
  MOVE lv_text TO gs_outtab_hrpost-messtext.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR: gs_outtab_hrpost,
         lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE: /01 SY-VLINE,
*           6 'Gesamtzahl:'(062),
*             LIN_COLLECT_TAB,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
ENDFORM.                               " LIST_COMPRESSED_CHECK_NRS
*&---------------------------------------------------------------------*
*&      Form  HR_FB01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hr_fb01.
  PERFORM fb01_bseg.                                        "mp40A
  PERFORM fb01_posting_aufrufen.                            "mp40A
  PERFORM update_payr_rec_belnr.
ENDFORM.                                                    " HR_FB01
*&---------------------------------------------------------------------*
*&      Form  INFO_NO_HR_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM info_no_hr_posting.
  PERFORM list_compressed_check_nrs.
  PERFORM counter_no_post.
  PERFORM display_business_area.
  PERFORM display_total_amount.
  PERFORM display_warning_no_hr_posting.
ENDFORM.                               " INFO_NO_HR_POSTING
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_WARNING_NO_HR_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_warning_no_hr_posting.
  DATA: lv_text(130) TYPE c.
*********Begin of comment ALV C5053249***************************
*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*          14 'Es wurden keine HR-Schecks gebucht (Parameter)!'(056),
*         132  SY-VLINE.
*  WRITE: /01 SY-VLINE,
*          14 'Sie müssen manuell buchen!'(057),
*         132  SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  CLEAR lv_text.
  MOVE TEXT-056 TO lv_text.
  MOVE lv_text TO gs_outtab_hrpost-messtext.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR: gs_outtab_hrpost,
         lv_text.

  CLEAR lv_text.
  MOVE TEXT-057 TO lv_text.
  MOVE lv_text TO gs_outtab_hrpost-messtext.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR: gs_outtab_hrpost,
         lv_text.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DISPLAY_WARNING_NO_HR_POSTING

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOTAL_AMOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_amount.
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE.
*  WRITE:    10 'Total amount:'(055),
*                AMOUNT_SUM CURRENCY WAERS,
*                WAERS,
*           132  SY-VLINE.
*************End of Comment ALV C5053249*****************************
***********Begin of Coding ALV C5053249*****************************
  DATA: lv_text(130) TYPE c.

  CLEAR lv_text.
  MOVE TEXT-055 TO lv_text.
  WRITE: amount_sum CURRENCY waers TO lv_text+22 LEFT-JUSTIFIED,
         waers TO lv_text+35 LEFT-JUSTIFIED.
  MOVE lv_text TO gs_outtab_hrpost-messtext.
  APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
  CLEAR: gs_outtab_hrpost,
         lv_text.
*************End of Coding ALV C5053249*****************************
ENDFORM.                               " DISPLAY_TOTAL_AMOUNT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PAYR_NO_HR_POSTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_payr_no_hr_posting.

  blntab-belnr = '-'.

  LOOP AT collect_tab.
    UPDATE payr SET rec_belnr = blntab-belnr
                    rec_gjahr = blntab-gjahr
                WHERE zbukr = collect_tab-bukrs
                  AND hbkid = collect_tab-hbkid
                  AND hktid = collect_tab-hktid
                  AND ichec = space                         "ak280100
                  AND chect = collect_tab-chect.
  ENDLOOP.
ENDFORM.                               " UPDATE_PAYR_NO_HR_POSTING
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MISSING_OR_WRONG_DOCNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_missing_or_wrong_docno.
  MOVE TEXT-010 TO char130.
  REPLACE '&' WITH ckrec-cknum INTO char130.
  REPLACE '&' WITH payr-vblnr INTO char130.
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*             CHAR130,
*         132 SY-VLINE.
*************End of Comment ALV C5053249*****************************
***********Begin of Coding ALV C5053249*****************************
  gs_outtab_chkdet-bankl     = head-bankl.
  gs_outtab_chkdet-bankn     = head-accnr.
  gs_outtab_chkdet-bukrs     = t012k-bukrs.
  gs_outtab_chkdet-messtext  = char130.
  gs_outtab_chkdet-cknum     = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of Coding ALV C5053249*****************************
  CLEAR: char130.
  MOVE TEXT-013 TO char130.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
*************End of Comment ALV C5053249*****************************
***********Begin of Coding ALV C5053249*****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
*************End of Coding ALV C5053249*****************************
  CLEAR: char130.
  MOVE TEXT-064 TO char130.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
*************End of Comment ALV C5053249*****************************
***********Begin of Coding ALV C5053249*****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
*************End of Coding ALV C5053249*****************************
  CLEAR: char130.
  MOVE TEXT-057 TO char130.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
*  FORMAT RESET.
*  ULINE.
*************End of Comment ALV C5053249*****************************
***********Begin of Coding ALV C5053249*****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
*************End of Coding ALV C5053249*****************************
ENDFORM.                               " DISPLAY_MISSING_OR_WRONG_DOCNO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MISSING_ACCOUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_missing_account.
  CLEAR char130.
  MOVE TEXT-026 TO char130.
  REPLACE '&' WITH t012-bukrs INTO char130.
  REPLACE '&' WITH hbkid INTO char130.
  REPLACE '&' WITH hktid INTO char130.
  REPLACE '&' WITH payr-rzawe INTO char130.
  REPLACE '&' WITH payr-waers INTO char130.
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
*************End of Comment ALV C5053249*****************************
***********Begin of Coding ALV C5053249*****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of Coding ALV C5053249*****************************
  CLEAR: char130.
  MOVE TEXT-028 TO char130.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
ENDFORM.                               " DISPLAY_MISSING_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_BUSINESS_AREA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_business_area.
  DATA: char4(4) TYPE c.
  DATA: char1(1) TYPE c.
***********Begin of Coding ALV C5053249********************************
  DATA: lv_text(130) TYPE c.
*************End of Coding ALV C5053249********************************
  MOVE sortkrit+30(4) TO char4.
  MOVE sortkrit+28(1) TO char1.
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************

  IF NOT char4 IS INITIAL.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:    10 'Geschäftsbereich:'(058),
*                  CHAR4,
*              132 SY-VLINE,
*              /01 SY-VLINE,
*               10 'Zahlweg:'(065),
*                 CHAR1.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    CLEAR lv_text.
    MOVE TEXT-058 TO lv_text.
    MOVE lv_text TO gs_outtab_hrpost-messtext.
    APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
    CLEAR: gs_outtab_hrpost,
           lv_text.

    CLEAR lv_text.
*    CONCATENATE text-065 char1 INTO lv_text SEPARATED BY space.
    MOVE TEXT-065 TO lv_text.
    MOVE char1    TO lv_text+22.
    MOVE lv_text TO gs_outtab_hrpost-messtext.
    APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
    CLEAR: gs_outtab_hrpost,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:    10 'Zahlweg:'(065),
*                 CHAR1.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    CLEAR lv_text.
*    CONCATENATE text-065 char1 INTO lv_text SEPARATED BY space.
    MOVE TEXT-065 TO lv_text.
    MOVE char1    TO lv_text+22.
    MOVE lv_text TO gs_outtab_hrpost-messtext.
    APPEND gs_outtab_hrpost TO gt_outtab_hrpost.
    CLEAR: gs_outtab_hrpost,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE:   132  SY-VLINE.
***********End of comment ALV C5053249***************************
ENDFORM.                               " DISPLAY_BUSINESS_AREA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOTAL_NUMBER_HR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_number_hr.
  DATA: lv_text(90).

***********Begin of Comment ALV C5053249*****************************
*  FORMAT RESET.
*  FORMAT COLOR COL_TOTAL.
*  ULINE.
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*           3 'Gesamtzahl Personalabrechnungsschecks:'(063),
*          65 HR_TOTAL,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-063 TO gs_outtab_stats-commentext+3.
  WRITE hr_total TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*  WRITE:    /01 SY-VLINE,
*             10 TEXT-069,
*             25 'manuelle Schecks (keine Buchung)'(066).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-069 TO lv_text+10.
  MOVE TEXT-066 TO lv_text+25.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
  IF hr_post_man_check NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 HR_POST_MAN_CHECK COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE hr_post_man_check TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 HR_POST_MAN_CHECK .
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE hr_post_man_check TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: 132 SY-VLINE.

*  WRITE:    /01 SY-VLINE,
*             25 'kein Gegenkonto (keine Buchung)'(074).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-074 TO lv_text+25.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
  IF hr_post_proc NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 HR_POST_PROC COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE hr_post_proc TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 HR_POST_PROC .
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE hr_post_proc TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: 132 SY-VLINE.

*  WRITE:    /01 SY-VLINE,
*             25 'abweichender Betrag (keine Buchung)'(112).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-112 TO lv_text+25.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
  IF hr_differing_amount NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 HR_DIFFERING_AMOUNT COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE hr_differing_amount TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 HR_DIFFERING_AMOUNT.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE hr_differing_amount TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: 132 SY-VLINE.
*  WRITE: /01 SY-VLINE,
*          63 '==============='(068),
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_stats-commentext+63 = TEXT-068.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
        lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*          10 'Gesamtzahl zur Verbuchung selektierter Schecks:'(067),
*          65 TOTAL_HR_POST COLOR COL_TOTAL,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-067 TO lv_text+10.
  gs_outtab_stats-commentext = lv_text.
  WRITE total_hr_post TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  IF total_hr_post NE 0.
    IF hr_post = 'X'.
***********Begin of Comment ALV C5053249*****************************
*     FORMAT COLOR COL_NORMAL.
*      WRITE: /01 SY-VLINE,
*              20 'davon'(069),
*              132 SY-VLINE,
*             /01 SY-VLINE,
*              25 'erfolgreich gebucht / Transaktionen'(070),
*              65 HR_TOTAL_POST_SUCCESS,
*                '/',
*              79 HR_TOTAL_TRANS_SUCCESS,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
      MOVE TEXT-069 TO lv_text+20.
      gs_outtab_stats-commentext = lv_text.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
      MOVE TEXT-070 TO lv_text+25.
      gs_outtab_stats-commentext = lv_text.
      WRITE hr_total_post_success TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      gs_outtab_stats-commentext+70 = '/'.
      gs_outtab_stats-commentext+79 = hr_total_trans_success.
      WRITE hr_total_trans_success TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*      WRITE: /01 SY-VLINE,
*              25 'keine Buchung / Transaktionen'(071).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
      MOVE TEXT-070 TO lv_text+25.
      gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
      IF hr_total_post_fail NE 0.
***********Begin of Comment ALV C5053249*****************************
*        WRITE:  65 HR_TOTAL_POST_FAIL COLOR COL_NEGATIVE,
*                '/'.
*        WRITE:  79 HR_TOTAL_TRANS_FAIL COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
        WRITE hr_total_post_fail TO gs_outtab_stats-commentext+65
        LEFT-JUSTIFIED.
        gs_outtab_stats-commentext+70 = '/'.
        WRITE hr_total_trans_fail TO gs_outtab_stats-commentext+65
        LEFT-JUSTIFIED.
        APPEND gs_outtab_stats TO gt_outtab_stats.
        CLEAR: gs_outtab_stats,
               lv_text.
***********End of coding ALV C5053249***************************
      ELSE.
***********Begin of Comment ALV C5053249*****************************
*        WRITE:  65 HR_TOTAL_POST_FAIL COLOR COL_TOTAL,
*                '/'.
*        WRITE:  79 HR_TOTAL_POST_FAIL COLOR COL_TOTAL.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
        WRITE hr_total_post_fail TO gs_outtab_stats-commentext+65
        LEFT-JUSTIFIED.
        gs_outtab_stats-commentext+70 = '/'.
        WRITE hr_total_trans_fail TO gs_outtab_stats-commentext+65
        LEFT-JUSTIFIED.
        APPEND gs_outtab_stats TO gt_outtab_stats.
        CLEAR: gs_outtab_stats,
               lv_text.
      ENDIF.
*      WRITE:   132 sy-vline.
    ELSE.
***********Begin of Comment ALV C5053249*****************************
*      WRITE: /01 SY-VLINE,
*              25 'keine Buchung (Parameter!)'(072),
*              65 TOTAL_HR_POST COLOR COL_NEGATIVE,
*             132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
      MOVE TEXT-072 TO lv_text+25.
      gs_outtab_stats-commentext = lv_text.
      WRITE total_hr_post TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ENDIF.
  ENDIF.
  gs_outtab_stats-commentext = space.
  APPEND gs_outtab_stats TO gt_outtab_stats.
***********Begin of Comment ALV C5053249*****************************
*  ULINE.
***********End of comment ALV C5053249***************************
ENDFORM.                               " DISPLAY_TOTAL_NUMBER_HR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MANUAL_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_manual_check.
  DATA: lv_wrbtr(13) TYPE c,
        lv_waers(5)  TYPE c.
  CLEAR: lv_wrbtr,
         lv_waers.
  CLEAR char130.
  MOVE TEXT-035 TO char130.
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
  CLEAR char130.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE.
*  WRITE:    31 TEXT-027,
*               PAYR-RWBTR CURRENCY PAYR-WAERS,
*               PAYR-WAERS,
*           132  SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  lv_wrbtr = payr-rwbtr.
  lv_waers = payr-waers.
  CONCATENATE TEXT-027 lv_wrbtr lv_waers INTO char130
                                       SEPARATED BY space.
  gs_outtab_chkdet-bankl       = head-bankl.
  gs_outtab_chkdet-bankn       = head-accnr.
  gs_outtab_chkdet-bukrs       = t012k-bukrs.
  gs_outtab_chkdet-messtext+31 = char130.
  gs_outtab_chkdet-cknum       = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
  CLEAR: char130.
***********End of coding ALV C5053249***************************
  MOVE TEXT-057 TO char130.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE,
*          31 CHAR130,
*         132 SY-VLINE.
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DISPLAY_MANUAL_CHECK
*&---------------------------------------------------------------------*
*&      Form  COUNTER_SUCCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_success.
  hr_total_post_success = hr_total_post_success + lin_collect_tab.
  CLEAR: lin_collect_tab.
  hr_total_trans_success = hr_total_trans_success + 1.
ENDFORM.                               " COUNTER_SUCCESS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_FAILURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_failure.
  hr_total_post_fail = hr_total_post_fail + lin_collect_tab.
  CLEAR: lin_collect_tab.
  hr_total_trans_fail = hr_total_trans_fail + 1.
ENDFORM.                               " COUNTER_FAILURE
*&---------------------------------------------------------------------*
*&      Form  COUNTER_NO_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_no_post.
  hr_total_no_post = hr_total_no_post + lin_collect_tab.
  CLEAR: lin_collect_tab.
  hr_no_post_trans = hr_no_post_trans + 1.
ENDFORM.                               " COUNTER_NO_POST
*&---------------------------------------------------------------------*
*&      Form  COUNTER_VALID_PRENUMBERED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_valid_prenumbered.
  total_valid_prenums = total_valid_prenums + 1.
ENDFORM.                               " COUNTER_VALID_PRENUMBERED

*&---------------------------------------------------------------------*
*&      Form  COUNTER_VOIDED_PRENUMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_voided_prenums.
  total_voided_prenums = total_voided_prenums + 1.
ENDFORM.                               " COUNTER_VOIDED_PRENUMS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_CASHED_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_cashed_checks.
  total_cashed_prenums = total_cashed_prenums + 1.
ENDFORM.                               " COUNTER_CASHED_CHECKS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_PRENUMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_prenums.
  total_prenums = total_prenums + 1.
ENDFORM.                               " COUNTER_PRENUMS
*&---------------------------------------------------------------------*
*&      Form  COUNTER_HR_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_hr_post.
  total_hr_post = total_hr_post + 1.
ENDFORM.                               " COUNTER_HR_POST
*&---------------------------------------------------------------------*
*&      Form  COUNTER_TOTAL_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_total_checks.
  total_checks = total_checks + 1.
ENDFORM.                               " COUNTER_TOTAL_CHECKS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_FI_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_fi_checks.
  total_fi_checks = total_fi_checks + 1.
ENDFORM.                               " COUNTER_FI_CHECKS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_UNKNOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_unknown.
  total_unknown = total_unknown + 1.
ENDFORM.                               " COUNTER_UNKNOWN

*&---------------------------------------------------------------------*
*&      Form  COUNTER_MULTIPLE_PRENUMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_multiple_prenums.
  total_multiple_prenums = total_multiple_prenums + 1.
ENDFORM.                               " COUNTER_MULTIPLE_PRENUMS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOTAL_NORMAL_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_normal_checks.
  DATA: lv_text(90).

***********Begin of Comment ALV C5053249*****************************
*  FORMAT RESET.
*  FORMAT COLOR COL_TOTAL.
*  ULINE.
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*           3 'Gesamtzahl andere vornumerierte Schecks:'(075),
*          65 NORMAL_PRENUMS_TOTAL,
*         132 SY-VLINE.
*  FORMAT COLOR COL_NORMAL.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-075 TO lv_text.
  gs_outtab_stats-commentext = lv_text.
  WRITE normal_prenums_total TO gs_outtab_stats-commentext+65
  LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*  WRITE:    /01 SY-VLINE,
*             10 TEXT-069,
*             25 'Fehlerhafte Belegnummer (keine Buchung)'(076).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
  MOVE TEXT-069 TO lv_text+10.
  MOVE TEXT-076 TO lv_text+25.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
  IF hr_post_man_check NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 OPRENUMS_INVALID_DOCNO COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE oprenums_invalid_docno TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:  65 OPRENUMS_INVALID_DOCNO .
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    WRITE oprenums_invalid_docno TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************

  IF prenums_no_valid_item NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:    /01 SY-VLINE,
*               25 'keine gültige Belegzeile (keine Buchung)'(077).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249****************************
    MOVE TEXT-077 TO lv_text+25.
    gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
    IF prenums_no_valid_item NE 0.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  65 PRENUMS_NO_VALID_ITEM COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      WRITE prenums_no_valid_item TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ELSE.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  65 PRENUMS_NO_VALID_ITEM .
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      WRITE prenums_no_valid_item TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ENDIF.
***********Begin of Comment ALV C5053249*****************************
*    WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
  ENDIF.

  IF prenums_no_open_item NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:    /01 SY-VLINE,
*               25 'keine offene Belegzeile (keine Buchung)'(078).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    MOVE TEXT-078 TO lv_text+25.
    gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
    IF prenums_no_open_item NE 0.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  65 PRENUMS_NO_OPEN_ITEM COLOR COL_NEGATIVE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      WRITE prenums_no_open_item TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ELSE.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  65 PRENUMS_NO_OPEN_ITEM .
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      WRITE prenums_no_open_item TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ENDIF.
***********Begin of Comment ALV C5053249*****************************
*    WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE,
*          63 '==============='(068),
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  gs_outtab_stats-commentext+63 = TEXT-068.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
        lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*          10 'Gesamtzahl zur Verbuchung selektierter Schecks:'(067),
*          65 VALID_OTHER_PRENUMS COLOR COL_TOTAL,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  MOVE TEXT-067 TO lv_text+10.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*  gs_outtab_stats-commentext+65 = VALID_OTHER_PRENUMS.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  WRITE valid_other_prenums TO gs_outtab_stats-commentext+65
  LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  IF valid_other_prenums NE 0.
***********Begin of Comment ALV C5053249*****************************
*    WRITE: /01 SY-VLINE,
*            20 'davon'(069),
*            132 SY-VLINE,
*           /01 SY-VLINE,
*            25 'Beträge übereinstimmend'(079),
*            65 OPRENUMS_AMOUNT_OK,
*           132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    MOVE TEXT-069 TO lv_text+20.
    gs_outtab_stats-commentext = lv_text.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
    MOVE TEXT-079 TO lv_text+25.
    gs_outtab_stats-commentext = lv_text.
***********Begin of Comment ALV C5053249*****************************
*    gs_outtab_stats-commentext+65 = OPRENUMS_AMOUNT_OK.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE oprenums_amount_ok TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
***********Begin of Comment ALV C5053249*****************************
*    WRITE:    /01 SY-VLINE.
***********End of comment ALV C5053249***************************
    IF function = calltrans.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  25 TEXT-080.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_stats-commentext+25 = TEXT-080.
    ELSE.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  25 'Beträge abweichend'(109).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      gs_outtab_stats-commentext+25 = TEXT-109.
***********End of coding ALV C5053249***************************
    ENDIF.
    IF oprenums_amount_ne NE 0.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  65 OPRENUMS_AMOUNT_NE COLOR COL_NEGATIVE.
*      gs_outtab_stats-commentext+65 = OPRENUMS_AMOUNT_NE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      WRITE oprenums_amount_ne TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR gs_outtab_stats.
***********End of coding ALV C5053249***************************
    ELSE.
***********Begin of Comment ALV C5053249*****************************
*      WRITE:  65 OPRENUMS_AMOUNT_NE COLOR COL_TOTAL.
*      gs_outtab_stats-commentext+65 = OPRENUMS_AMOUNT_NE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      WRITE oprenums_amount_ne TO gs_outtab_stats-commentext+65
      LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR gs_outtab_stats.
***********End of coding ALV C5053249***************************
    ENDIF.
***********Begin of Comment ALV C5053249*****************************
*    WRITE:   132 SY-VLINE.
***********End of comment ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  ULINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  gs_outtab_stats-commentext = space.
  APPEND gs_outtab_stats TO gt_outtab_stats.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DISPLAY_TOTAL_NORMAL_CHECKS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_NORMAL_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_normal_checks.
  normal_prenums_total = normal_prenums_total + 1.
ENDFORM.                               " COUNTER_NORMAL_CHECKS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_OPRENUMS_INVALID_DOCNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_oprenums_invalid_docno.
  oprenums_invalid_docno = oprenums_invalid_docno + 1.
ENDFORM.                               " COUNTER_OPRENUMS_INVALID_DOCNO
*&---------------------------------------------------------------------*
*&      Form  COUNTER_NO_VALID_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_no_valid_item.
  prenums_no_valid_item = prenums_no_valid_item + 1.
ENDFORM.                               " COUNTER_NO_VALID_ITEM
*&---------------------------------------------------------------------*
*&      Form  COUNTER_PRENUMS_NO_OPEN_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_prenums_no_open_item.
  prenums_no_open_item = prenums_no_open_item + 1.
ENDFORM.                               " COUNTER_PRENUMS_NO_OPEN_ITEM
*&---------------------------------------------------------------------*
*&      Form  COUNTER_OPRENUMS_AMOUNT_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_oprenums_amount_ok.
  oprenums_amount_ok = oprenums_amount_ok + 1.
ENDFORM.                               " COUNTER_OPRENUMS_AMOUNT_OK

*&---------------------------------------------------------------------*
*&      Form  COUNTER_VALID_OTHER_PRENUMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_valid_other_prenums.
  valid_other_prenums = valid_other_prenums + 1.
ENDFORM.                               " COUNTER_VALID_OTHER_PRENUMS

*&---------------------------------------------------------------------*
*&      Form  COUNTER_OPRENUMS_AMOUNT_NE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_oprenums_amount_ne.
  oprenums_amount_ne = oprenums_amount_ne + 1.
ENDFORM.                               " COUNTER_OPRENUMS_AMOUNT_NE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CHECK_FILE_OVERVIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_check_file_overview.
  DATA: total_chks TYPE i.
  DATA: lv_text(90).

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL.
***********Begin of Comment ALV C5053249*****************************
*  ULINE.
***********End of comment ALV C5053249***************************
  FORMAT COLOR COL_TOTAL.
***********Begin of Comment ALV C5053249*****************************
*  WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
  IF pcupload = 'T'.
***********Begin of Comment ALV C5053249*****************************
*    WRITE: 3 'Gesamtzahl zu verarbeitender Schecks:'(081).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE 'Gesamtzahl zu verarbeitender Schecks:'(081) TO
                      gs_outtab_stats-commentext.
  ELSE.
***********Begin of Comment ALV C5053249*****************************
*    WRITE: 3 'Gesamtzahl zu verarbeitender Schecks in Datei:'(113).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE 'Gesamtzahl zu verarbeitender Schecks in Datei:'(113) TO
                      gs_outtab_stats-commentext.
***********End of coding ALV C5053249***************************
  ENDIF.
***********Begin of Comment ALV C5053249*****************************
*  WRITE:  65 TOTAL_CHECKS,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  WRITE total_checks TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR gs_outtab_stats.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE:    /01 SY-VLINE,
*             10 TEXT-069.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+10 = TEXT-069.
***********End of coding ALV C5053249***************************
  IF total_unknown NE 0.
    IF pnumc = 'X'.
*********Begin of comment ALV C5053249***************************
*      WRITE:  25 'Schecknummer weder in Belegdatenbank'(082),
*                 132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      lv_text+25 = TEXT-082.
      gs_outtab_stats-commentext = lv_text.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*      WRITE:    /01 SY-VLINE,
*                 25 'noch im Scheckregister (keine Buchung)'(084),
*                 65 TOTAL_UNKNOWN COLOR COL_NEGATIVE,
*                132 SY-VLINE.
*      WRITE:    /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      lv_text+25 = TEXT-084.
      gs_outtab_stats-commentext = lv_text.
      WRITE total_unknown TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ELSE.
*********Begin of comment ALV C5053249***************************
*      WRITE:
*                 25 'Schecknummer nicht in Belegdatenbank'(099),
*                132 SY-VLINE.
*      WRITE:    /01 SY-VLINE,
*                 25 '(keine Buchung)'(108),
*                 65 TOTAL_UNKNOWN COLOR COL_NEGATIVE,
*                132 SY-VLINE.
*      WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
      lv_text+25 = TEXT-099.
      gs_outtab_stats-commentext = lv_text.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.

      lv_text+25 = TEXT-108.
      gs_outtab_stats-commentext = lv_text.
      WRITE total_unknown TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
             lv_text.
***********End of coding ALV C5053249***************************
    ENDIF.
  ENDIF.
*********Begin of comment ALV C5053249***************************
*  WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
  IF total_multiple_prenums NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:
*               25 'vornumerierte Schecks mit'(083),
*               132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-083.
    gs_outtab_stats-commentext = lv_text.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*    WRITE:    /01 SY-VLINE,
*               25 'mehreren Zahlwegen (keine Buchung)'(085),
*               65 TOTAL_MULTIPLE_PRENUMS COLOR COL_NEGATIVE,
*               132 SY-VLINE.
*    WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-085.
    gs_outtab_stats-commentext = lv_text.
    WRITE total_multiple_prenums TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
  ENDIF.

  IF total_fi_checks NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:
*               25 'nichtvornumerierte Schecks'(087),
*               65 TOTAL_FI_CHECKS,
*               132 SY-VLINE.
*    WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-087.
    gs_outtab_stats-commentext = lv_text.
    WRITE total_fi_checks TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.

*********Begin of comment ALV C5053249***************************
  IF total_prenums NE 0.
*    WRITE:
*               25 'vornumerierte Schecks'(086),
*               65 TOTAL_PRENUMS,
*               132 SY-VLINE.
*    WRITE:   /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-086.
    gs_outtab_stats-commentext = lv_text.
    WRITE total_prenums TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
*********Begin of comment ALV C5053249***************************
*  WRITE:  63 '==============='(068).
*  WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  gs_outtab_stats-commentext+63 = TEXT-068.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  total_chks = total_fi_checks + total_prenums.
*********Begin of comment ALV C5053249***************************
*  WRITE: /01 SY-VLINE,
*          10 'Gesamtzahl verarbeitbarer Schecks:'(088),
*          65 TOTAL_CHKS,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+10 = TEXT-088.
  gs_outtab_stats-commentext = lv_text.
  WRITE total_chks TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.

  gs_outtab_stats-commentext = space.
  APPEND gs_outtab_stats TO gt_outtab_stats.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  ULINE.
***********End of comment ALV C5053249***************************
ENDFORM.                               " DISPLAY_CHECK_FILE_OVERVIEW

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOTAL_PRENUMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_prenums.
  DATA: lv_text(90).

*********Begin of comment ALV C5053249***************************
*  FORMAT RESET.
*  FORMAT COLOR COL_TOTAL.
*  ULINE.
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*           3 'Gesamtzahl vornumerierte Schecks:'(089),
*          65 TOTAL_PRENUMS,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+3 = TEXT-089.
  gs_outtab_stats-commentext = lv_text.
  WRITE total_prenums TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE:    /01 SY-VLINE,
*             10 TEXT-069.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+10 = TEXT-069.
***********End of coding ALV C5053249***************************
  IF total_voided_prenums NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:  25 'ungültig laut Scheckregister'(090),
*             132 SY-VLINE,
*            /01 SY-VLINE,
*             25 TEXT-108,
*            65 TOTAL_VOIDED_PRENUMS COLOR COL_NEGATIVE.
*    WRITE: 132 SY-VLINE.
*    WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-090.
    gs_outtab_stats-commentext = lv_text.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.

    MOVE TEXT-108 TO lv_text+25.
    gs_outtab_stats-commentext = lv_text.
    WRITE total_voided_prenums TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
*********Begin of comment ALV C5053249***************************
*    WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
  ENDIF.

  IF total_cashed_prenums NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:
*               25 'bereits eingelöst laut Scheckregister'(094),
*              132 SY-VLINE,
*              /01 SY-VLINE,
*               25 TEXT-108,
*               65 TOTAL_CASHED_PRENUMS COLOR COL_NEGATIVE,
*               132 SY-VLINE.
*    WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-094.
    gs_outtab_stats-commentext = lv_text.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.


    lv_text+25 = TEXT-108.
    gs_outtab_stats-commentext = lv_text.
    WRITE total_cashed_prenums TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.

  IF normal_prenums_total NE 0
     OR hr_total NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:
*               25 'Personalabrechnungsschecks'(096),
*               65 HR_TOTAL,
*               132 SY-VLINE.
*    WRITE:   /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-096.
    gs_outtab_stats-commentext = lv_text.
    WRITE hr_total TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.

  IF normal_prenums_total NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:
*               25 'andere vornumerierte Schecks'(097),
*               65 NORMAL_PRENUMS_TOTAL,
*               132 SY-VLINE.
*    WRITE: /01 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+25 = TEXT-097.
    gs_outtab_stats-commentext = lv_text.
    WRITE normal_prenums_total TO gs_outtab_stats-commentext+65
    LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
*********Begin of comment ALV C5053249***************************
*  WRITE:  63 '==============='(068).
*  WRITE: 132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  gs_outtab_stats-commentext+63 = TEXT-068.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE: /01 SY-VLINE,
*          10 'Gesamtzahl gültiger vornumerierter Schecks:'(098),
*          65 TOTAL_VALID_PRENUMS,
*         132 SY-VLINE.
*
*  ULINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+10 = TEXT-098.
  gs_outtab_stats-commentext = lv_text.
  WRITE total_valid_prenums TO gs_outtab_stats-commentext+65
  LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
       lv_text.
  gs_outtab_stats-commentext = space.
  APPEND gs_outtab_stats TO gt_outtab_stats.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DISPLAY_TOTAL_PRENUMS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOTAL_FI_CHECKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_fi_checks.
  DATA: lv_text(90).

*********Begin of comment ALV C5053249***************************
*  FORMAT RESET.
*  FORMAT COLOR COL_TOTAL.
*  ULINE.
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*           3 'Gesamtzahl nichtvornumerierte Schecks:'(107),
*          65 TOTAL_FI_CHECKS,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+3 = TEXT-107.
  gs_outtab_stats-commentext = lv_text.
  WRITE total_fi_checks TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
       lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE:    /01 SY-VLINE,
*             10 TEXT-069.
*
*  WRITE:
*            25 'keine gültige Belegzeile (keine Buchung)'(077).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+10 = TEXT-069.
  lv_text+25 = TEXT-077.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
  IF fi_no_valid_item NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:  65 FI_NO_VALID_ITEM COLOR COL_NEGATIVE.
*     gs_outtab_stats-commentext+65 = FI_NO_VALID_ITEM.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE fi_no_valid_item TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
*********Begin of comment ALV C5053249***************************
*    WRITE:  65 FI_NO_VALID_ITEM .
*    gs_outtab_stats-commentext+65 = FI_NO_VALID_ITEM.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE fi_no_valid_item TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
           lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
*********Begin of comment ALV C5053249***************************
*  WRITE: 132 SY-VLINE.
*
*  WRITE:    /01 SY-VLINE,
*             25 'keine offene Belegzeile (keine Buchung)'(078).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+25 = TEXT-078.
  gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
  IF fi_no_open_item NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE:  65 FI_NO_OPEN_ITEM COLOR COL_NEGATIVE.
*    gs_outtab_stats-commentext+65    = FI_NO_OPEN_ITEM.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE fi_no_open_item TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  ELSE.
*********Begin of comment ALV C5053249***************************
*    WRITE:  65 FI_NO_OPEN_ITEM .
*    gs_outtab_stats-commentext+65    = FI_NO_OPEN_ITEM.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    WRITE fi_no_open_item TO gs_outtab_stats-commentext+65
LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
  ENDIF.
*********Begin of comment ALV C5053249***************************
*  WRITE: 132 SY-VLINE.
*  WRITE: /01 SY-VLINE,
*          63 '==============='(068),
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  gs_outtab_stats-commentext+63 = TEXT-068.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
        lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  FORMAT COLOR COL_TOTAL.
*  WRITE: /01 SY-VLINE,
*          10 'Gesamtzahl zur Verbuchung selektierter Schecks:'(067),
*          65 VALID_FI_OP COLOR COL_TOTAL,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
  lv_text+10 = TEXT-067.
  gs_outtab_stats-commentext = lv_text.
  WRITE valid_fi_op TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
  APPEND gs_outtab_stats TO gt_outtab_stats.
  CLEAR: gs_outtab_stats,
       lv_text.
***********End of coding ALV C5053249***************************
  IF valid_fi_op NE 0.
*********Begin of comment ALV C5053249***************************
*    WRITE: /01 SY-VLINE,
*            20 'davon'(069),
*            132 SY-VLINE,
*           /01 SY-VLINE,
*            25 'Beträge übereinstimmend'(079),
*            65 FI_AMOUNT_OK,
*           132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+20 = TEXT-069.
    gs_outtab_stats-commentext = lv_text.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.

    lv_text+25 = TEXT-079.
    gs_outtab_stats-commentext = lv_text.
    WRITE fi_amount_ok TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED.
    APPEND gs_outtab_stats TO gt_outtab_stats.
    CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*    WRITE: /01 SY-VLINE,
*            25 'Beträge abweichend (=> Batch-Input)'(080).
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************
    lv_text+10 = TEXT-080.
    gs_outtab_stats-commentext = lv_text.
***********End of coding ALV C5053249***************************
    IF fi_amount_ne NE 0.
*********Begin of comment ALV C5053249***************************
*      WRITE:  65 FI_AMOUNT_NE COLOR COL_NEGATIVE.
*      gs_outtab_stats-commentext+65    = FI_AMOUNT_NE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************

      WRITE fi_amount_ne TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED
 .
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
    ELSE.
*********Begin of comment ALV C5053249***************************
*      WRITE:  65 FI_AMOUNT_NE COLOR COL_TOTAL.
*      gs_outtab_stats-commentext+65    = FI_AMOUNT_NE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************

      WRITE fi_amount_ne TO gs_outtab_stats-commentext+65 LEFT-JUSTIFIED
 .
      APPEND gs_outtab_stats TO gt_outtab_stats.
      CLEAR: gs_outtab_stats,
         lv_text.
***********End of coding ALV C5053249***************************
    ENDIF.
*********Begin of comment ALV C5053249***************************
*    WRITE:   132 SY-VLINE.
***********End of comment ALV C5053249***************************
  ENDIF.
*********Begin of comment ALV C5053249***************************
*  ULINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************

  gs_outtab_stats-commentext = space.
  APPEND gs_outtab_stats TO gt_outtab_stats.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DISPLAY_TOTAL_FI_CHECKS
*&---------------------------------------------------------------------*
*&      Form  COUNTER_FI_NO_VALID_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_fi_no_valid_item.
  fi_no_valid_item = fi_no_valid_item + 1.
ENDFORM.                               " COUNTER_FI_NO_VALID_ITEM

*&---------------------------------------------------------------------*
*&      Form  COUNTER_FI_NO_OPEN_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_fi_no_open_item.
  fi_no_open_item = fi_no_open_item + 1.
ENDFORM.                               " COUNTER_FI_NO_OPEN_ITEM

*&---------------------------------------------------------------------*
*&      Form  COUNTER_VALID_FI_OP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_valid_fi_op.
  valid_fi_op = valid_fi_op + 1.
ENDFORM.                               " COUNTER_VALID_FI_OP

*&---------------------------------------------------------------------*
*&      Form  COUNTER_FI_AMOUNT_OK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_fi_amount_ok.
  fi_amount_ok = fi_amount_ok + 1.
ENDFORM.                               " COUNTER_FI_AMOUNT_OK

*&---------------------------------------------------------------------*
*&      Form  COUNTER_FI_AMOUNT_NE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM counter_fi_amount_ne.
  fi_amount_ne = fi_amount_ne + 1.
ENDFORM.                               " COUNTER_FI_AMOUNT_NE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AMOUNT_SINGLE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_AMOUNT_OK  text                                            *
*----------------------------------------------------------------------*
FORM check_amount_single_check USING    p_amount_ok.
  DATA:
    xrwbtr LIKE payr-rwbtr.                                 "ak080800

  IF         ckrec-amount IS INITIAL                        "ak281099
     AND NOT ckrec-bankl  IS INITIAL                        "ak281099
     AND     pcupload     =  'T'                            "ak281099
     AND     ck_file(4)   = 'FCHR'.                         "ak281099

*-- (mis-)use BANKL-field for amount to avoid cut-off          "ak281099
    amount_chk = ckrec-bankl / 100.                         "ak281099
  ELSE.                                                     "ak281099
    amount_chk = ckrec-amount.
    amount_chk = amount_chk / 100.
  ENDIF.

*  PAYR-RWBTR = ABS( PAYR-RWBTR ).                             "ak080800
  xrwbtr = abs( payr-rwbtr ).                               "ak080800
*  IF AMOUNT_CHK EQ PAYR-RWBTR.
  IF amount_chk EQ xrwbtr.                                  "ak080800
    p_amount_ok = true.
  ELSE.
    p_amount_ok = false.
  ENDIF.
ENDFORM.                               " CHECK_AMOUNT_SINGLE_CHECK
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_WRONG_AMOUNT_HR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_wrong_amount_hr.
*********Begin of comment ALV C5053249***************************
*  FORMAT COLOR COL_NEGATIVE.
*  WRITE: /01 SY-VLINE,
*          31 TEXT-111,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************

  char130 = TEXT-111.
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE: /01 SY-VLINE,
*          31 TEXT-064,
*         132 SY-VLINE.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************

  char130 = TEXT-064.
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
***********End of coding ALV C5053249***************************
*********Begin of comment ALV C5053249***************************
*  WRITE: /01 SY-VLINE,
*          31 TEXT-025,
*         132 SY-VLINE.
*  FORMAT RESET.
***********End of comment ALV C5053249***************************
*********Begin of coding ALV C5053249***************************

  char130 = TEXT-025.
  gs_outtab_chkdet-bankl        = head-bankl.
  gs_outtab_chkdet-bankn        = head-accnr.
  gs_outtab_chkdet-bukrs        = t012k-bukrs.
  gs_outtab_chkdet-messtext+31  = char130.
  gs_outtab_chkdet-cknum        = ckrec-cknum.
  APPEND gs_outtab_chkdet TO gt_outtab_chkdet.
  CLEAR gs_outtab_chkdet.
  CLEAR char130.
***********End of coding ALV C5053249***************************
ENDFORM.                               " DISPLAY_WRONG_AMOUNT_HR

*----------------------------------------------------------------------*
* Form  CURRENCY_CODE_ISO_TO_SAP                                       *
* ak020699                                                             *
*----------------------------------------------------------------------*
*FORM CURRENCY_CODE_ISO_TO_SAP USING    P_ISO_CODE
*                                       P_SAP_CODE.
*
*  STATICS: OLD_ISO_CODE LIKE TCURC-ISOCD,
*           OLD_SAP_CODE LIKE TCURC-WAERS.
*  DATA:    UNIQUE LIKE BDWFAP_PAR-CALLTRANS.
*
*  CLEAR: P_SAP_CODE, UNIQUE.
*
*  CHECK NOT P_ISO_CODE IS INITIAL.
*
*  IF P_ISO_CODE NE OLD_ISO_CODE.
*    CLEAR: OLD_SAP_CODE.
*    CALL FUNCTION 'CURRENCY_CODE_ISO_TO_SAP'
*         EXPORTING
*              ISO_CODE  = P_ISO_CODE
*         IMPORTING
*              SAP_CODE  = P_SAP_CODE
*              UNIQUE    = UNIQUE
*         EXCEPTIONS
*              NOT_FOUND = 1
*              OTHERS    = 2.
*
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.
*      IF UNIQUE NE 'X'.
**     iso curreny code is assigned to more than one sap currency code
*        MESSAGE E073 WITH P_ISO_CODE SY-DATUM.
*      ENDIF.
*      OLD_ISO_CODE = P_ISO_CODE.
*      OLD_SAP_CODE = P_SAP_CODE.
*    ENDIF.
*  ELSE.
*    P_SAP_CODE = OLD_SAP_CODE.
*  ENDIF.
*
*ENDFORM.
*  " CURRENCY_CODE_ISO_TO_SAP

*----------------------------------------------------------------------*
* FORM CHECK_IF_RELEVANT_TO_CASH_FLOW                                  *
* ak230899                                                             *
*----------------------------------------------------------------------*
FORM check_if_relevant_to_cash_flow USING    i_bukrs TYPE bukrs
                                             i_saknr TYPE saknr
                                    CHANGING c_flag  TYPE c.

  STATICS:
    BEGIN OF xskb1 OCCURS 0,
      bukrs LIKE skb1-bukrs,
      saknr LIKE skb1-saknr,
      xgkon LIKE skb1-xgkon,
    END OF xskb1.

  READ TABLE xskb1 WITH KEY bukrs = i_bukrs
                            saknr = i_saknr.

  IF sy-subrc <> 0.
    SELECT SINGLE * FROM skb1 INTO CORRESPONDING FIELDS OF xskb1
                                   WHERE bukrs = i_bukrs
                                   AND   saknr = i_saknr.

    IF sy-subrc <> 0.
      MESSAGE e871 WITH i_saknr i_bukrs.
    ENDIF.

    APPEND xskb1.
  ENDIF.
  c_flag = xskb1-xgkon.
ENDFORM.                                "CHECK_IF_RELEVANT_TO_CASH_FLOW

*&---------------------------------------------------------------------*
*&      Form  display_list1
*&---------------------------------------------------------------------*
*       ALV list display for first list
*----------------------------------------------------------------------*
FORM display_list1 .

  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        ls_layo  TYPE slis_layout_alv,
        lt_evts1 TYPE slis_t_event,
        ls_prnt  TYPE slis_print_alv.

  DATA: lv_counter TYPE i VALUE 1.

*... the fieldcatalog holds the metadata which describes the
*    output table

  PERFORM set_fieldcatalog USING lv_counter CHANGING lt_fcat.

  PERFORM set_layout USING gv_count_a CHANGING ls_layo.

*... In the events table you can register the list to certain
*    events

  PERFORM set_events    USING lv_counter
                     CHANGING lt_evts1.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
*     i_callback_pf_status_set = 'HANDLE_EVENT_PF_STATUS'
*     i_callback_user_command  = 'HANDLE_EVENT_USER_COMMAND'
      is_layout          = ls_layo
      it_fieldcat        = lt_fcat
      it_events          = lt_evts1
      is_print           = ls_prnt
    TABLES
      t_outtab           = gt_outtab_file
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_list1

*&---------------------------------------------------------------------*
*&      Form  set_fieldcatalog
*&---------------------------------------------------------------------*
*       Populate the fieldcat for various list passing counter for
*       List identification
*----------------------------------------------------------------------*
*      -->IV_COUNTER  list identifier
*      <--XT_FCAT     FIeldcat internal table to be changed
*----------------------------------------------------------------------*
FORM set_fieldcatalog    USING iv_counter TYPE i
                      CHANGING xt_fcat    TYPE slis_t_fieldcat_alv.


  DATA: lt_fcat TYPE slis_t_fieldcat_alv,
        ls_fcat TYPE slis_fieldcat_alv.

  CASE iv_counter.
    WHEN 1.
      PERFORM build_fieldcat    USING gc_struc1
                             CHANGING xt_fcat.
*... IMPORTANT NOTE: Event Double Click
*    In order to have this work we will set the tabname to
*    the list number
      ls_fcat-tabname = '1'.
      MODIFY xt_fcat FROM ls_fcat TRANSPORTING tabname
        WHERE tabname NE ls_fcat-tabname.

    WHEN 2.
* second ALV fieldcat populated
      ls_fcat-fieldname = 'HEAD_REC'.
      ls_fcat-tabname   = gc_tabname_header.
      ls_fcat-seltext_l = TEXT-030.
      ls_fcat-ddictxt   = gc_l.
      APPEND ls_fcat TO lt_fcat.
      CLEAR ls_fcat.

      PERFORM build_fieldcat    USING gc_struc2
                             CHANGING lt_fcat.

      ls_fcat-tabname = gc_2m.
      MODIFY lt_fcat FROM ls_fcat TRANSPORTING tabname
        WHERE tabname NE ls_fcat-tabname.
      APPEND LINES OF lt_fcat TO xt_fcat.

      CLEAR lt_fcat.

      PERFORM build_fieldcat    USING gc_struc3
                             CHANGING lt_fcat.

      ls_fcat-tabname = gc_2s.
      MODIFY lt_fcat FROM ls_fcat TRANSPORTING tabname
        WHERE tabname NE ls_fcat-tabname.
      APPEND LINES OF lt_fcat TO xt_fcat.

    WHEN 3 OR 4.
*... the third and fourth list fieldcat populated
*
      PERFORM build_fieldcat    USING gc_struc5
                             CHANGING lt_fcat.

      ls_fcat-tabname = gc_3m.
      MODIFY lt_fcat FROM ls_fcat TRANSPORTING tabname
        WHERE tabname NE ls_fcat-tabname.
      APPEND LINES OF lt_fcat TO xt_fcat.

      CLEAR lt_fcat.

      PERFORM build_fieldcat    USING gc_struc6
                             CHANGING lt_fcat.

      ls_fcat-tabname = gc_3s.
      MODIFY lt_fcat FROM ls_fcat TRANSPORTING tabname
        WHERE tabname NE ls_fcat-tabname.
      APPEND LINES OF lt_fcat TO xt_fcat.
*... the fifth list fieldcat populated
    WHEN 5.
      PERFORM build_fieldcat    USING gc_struc7
                             CHANGING xt_fcat.


    WHEN 6.
*... the Sixth list fieldcat populated
      PERFORM build_fieldcat    USING gc_struc4
                             CHANGING xt_fcat.

*... IMPORTANT NOTE: Event Double Click
*    In order to have this work we will set the tabname to
*    the list number
      ls_fcat-tabname = '5'.
      MODIFY xt_fcat FROM ls_fcat TRANSPORTING tabname
        WHERE tabname NE ls_fcat-tabname.
  ENDCASE.
ENDFORM.                    " set_fieldcatalog
*eject
*&---------------------------------------------------------------------*
*&      Form  STATUS_MAIN
*&---------------------------------------------------------------------*
*   User defined GUI status
*----------------------------------------------------------------------*
FORM status_main USING iv_extab TYPE slis_t_extab.          "#EC CALLED
  SET PF-STATUS 'STANDARD_ALV'.
ENDFORM.                              "STATUS_MAIN
*eject
*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE1
*&---------------------------------------------------------------------
*   Handle event top of page for first list
*----------------------------------------------------------------------

FORM top_of_page1.                                          "#EC CALLED

  CLEAR:  bhdgd-line1, bhdgd-line2.
  bhdgd-bukrs = '-----'.
  bhdgd-line1 = sy-title.
  char60 = TEXT-002.
  REPLACE '&' WITH ck_file INTO char60.
  bhdgd-line2 = char60.
  bhdgd-inifl = '0'.

  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  IF testrun = gc_value_x.

    REFRESH: gt_header1.
    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-007 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_header1.
ENDFORM.                    "TOP_OF_PAGE1
*eject
*&---------------------------------------------------------------------
*&      Form  keyinfo_build
*&---------------------------------------------------------------------
*      <--XS_KEYINFO  Populate the structure with Key columns
*----------------------------------------------------------------------

FORM set_keyinfo_1  CHANGING xs_keyinfo TYPE slis_keyinfo_alv.

  CLEAR xs_keyinfo.
  xs_keyinfo-header01 = 'BANKL'.
  xs_keyinfo-item01   = 'BANKL'.
  xs_keyinfo-header02 = 'BANKN'.
  xs_keyinfo-item02   = 'BANKN'.
  xs_keyinfo-header03 = 'BUKRS'.
  xs_keyinfo-item03   = 'BUKRS'.
ENDFORM.                    " keyinfo_build
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       For list append set the field to 'X' else to 'Y'
*----------------------------------------------------------------------*
*      -->IV_COUNTER  list identifier
*      <--XS_LAYO     Layout structure to be modified
*----------------------------------------------------------------------*
FORM set_layout USING iv_counter CHANGING xs_layo TYPE slis_layout_alv.

* If the counter is other than 1 then set list_append to 'X' else to 'Y'
  CASE iv_counter.
    WHEN 1.
      xs_layo-list_append       = gc_value_y.
      xs_layo-min_linesize      = gc_line_size.

    WHEN OTHERS.
      xs_layo-list_append       = gc_value_x.
  ENDCASE.

  xs_layo-no_hotspot        = gc_value_x.
  xs_layo-colwidth_optimize = gc_value_x.

ENDFORM.                    " set_layout

*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
*       Populate the events for all the list(s) by passing list counter
*----------------------------------------------------------------------*
*      -->IV_COUNTER  list identifier
*      <--XT_FCAT     FIeldcat internal table to be populated
*----------------------------------------------------------------------*
FORM set_events    USING iv_counter        TYPE i
                CHANGING xt_eventtab_list1 TYPE slis_t_event.
  DATA:   ls_events TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = xt_eventtab_list1
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  CASE iv_counter.
    WHEN 1.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_top_of_page.
      IF sy-subrc = 0.
        ls_events-form = gc_form_top_of_page1.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_top_of_page.
      ENDIF.

      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_pf_status_set.
      IF sy-subrc = 0.
        ls_events-form = gc_form_set_pf_status.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_pf_status_set.
      ENDIF.

      READ TABLE xt_eventtab_list1 INTO ls_events WITH
                 KEY name = slis_ev_end_of_list.
      IF sy-subrc = 0.
        ls_events-form = gc_form_end_of_list .
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_end_of_list.
      ENDIF.

    WHEN 2.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_top_of_page.
      IF sy-subrc = 0.
        ls_events-form = gc_form_top_of_page2.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_top_of_page.
      ENDIF.
      IF NOT gv_flag IS INITIAL.
        READ TABLE xt_eventtab_list1 INTO ls_events WITH
        KEY name = slis_ev_end_of_list.
        IF sy-subrc = 0.
          ls_events-form = gc_form_end_of_list .
          MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
          WHERE name = slis_ev_end_of_list.
        ENDIF.

        READ TABLE xt_eventtab_list1 INTO ls_events WITH
        KEY name = slis_ev_pf_status_set.
        IF sy-subrc = 0.
          ls_events-form = gc_form_set_pf_status.
          MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
          WHERE name = slis_ev_pf_status_set.
        ENDIF.
      ENDIF.
    WHEN 3.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_top_of_page.
      IF sy-subrc = 0.
        ls_events-form = gc_form_top_of_page3.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_top_of_page.
      ENDIF.
    WHEN 4.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_top_of_page.
      IF sy-subrc = 0.
        ls_events-form = gc_form_top_of_page4.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_top_of_page.
      ENDIF.
    WHEN 5.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_top_of_page.
      IF sy-subrc = 0.
        ls_events-form = gc_form_top_of_page5.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_top_of_page.
      ENDIF.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_end_of_list.
      IF sy-subrc = 0.
        ls_events-form = gc_form_end_of_list5.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_end_of_list.
      ENDIF.
    WHEN 6.
      READ TABLE xt_eventtab_list1 INTO ls_events WITH
      KEY name = slis_ev_top_of_page.
      IF sy-subrc = 0.
        ls_events-form = gc_form_top_of_page6.
        MODIFY xt_eventtab_list1 FROM ls_events TRANSPORTING form
        WHERE name = slis_ev_top_of_page.
      ENDIF.
    WHEN OTHERS.
*       do nothing
  ENDCASE.
ENDFORM.                    " set_events

*&---------------------------------------------------------------------*
*&      Form  END_OF_ALV_LIST
*&---------------------------------------------------------------------*
*       End of List
*----------------------------------------------------------------------*
FORM end_of_alv_list.                                       "#EC *
* Check if the global variable GV_FLAG is initial else
* this is not first list and hence other lists should not
* be appended to this
  IF gv_flag IS INITIAL.
    PERFORM display_list_2.

    IF NOT ( gv_count_list3_hdr IS INITIAL AND
             gv_count_list3_det IS INITIAL ).
      PERFORM display_list_3.
    ENDIF.

    IF NOT ( gv_count_list4_hdr IS INITIAL AND
             gv_count_list4_det IS INITIAL ).
      PERFORM display_list_4.
    ENDIF.

    IF NOT gv_count_list5 IS INITIAL.
      PERFORM display_list_5.
    ENDIF.

    PERFORM display_list_stats.
  ELSEIF NOT gv_flag IS INITIAL.
    IF NOT ( gv_count_list3_hdr IS INITIAL AND
             gv_count_list3_det IS INITIAL ).
      PERFORM display_list_3.
    ENDIF.

    IF NOT ( gv_count_list4_hdr IS INITIAL AND
             gv_count_list4_det IS INITIAL ).
      PERFORM display_list_4.
    ENDIF.

    IF NOT gv_count_list5 IS INITIAL.
      PERFORM display_list_5.
    ENDIF.

    PERFORM display_list_stats.
  ENDIF.
ENDFORM.                    " END_OF_ALV_LIST

*&---------------------------------------------------------------------*
*&      Form  display_list_2
*&---------------------------------------------------------------------*
*   Second List display( if first list is not diaplayed, this is first)
*----------------------------------------------------------------------*
FORM display_list_2 .

  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        ls_layo  TYPE slis_layout_alv,
        lt_evts2 TYPE slis_t_event,
        ls_prnt  TYPE slis_print_alv,
        ls_key   TYPE slis_keyinfo_alv.
  DATA: lv_counter TYPE i VALUE 2.
  SORT gt_outtab_chkdet BY cknum.


*... the fieldcatalog holds the metadata which describes the
*    output table
  PERFORM set_fieldcatalog USING lv_counter CHANGING lt_fcat.

*... the layout of the display can been modified by setting
*    the layout structure
  PERFORM set_layout USING gv_count_b  CHANGING ls_layo.
*... Change the fieldcatalog either for different headings or colpos
  PERFORM fieldcat_change USING lv_counter CHANGING lt_fcat.

*... In the events table you can register the list to certain
*    events
  PERFORM set_events    USING lv_counter
                     CHANGING lt_evts2.

*... the key information links the master and slave table together.
  PERFORM set_keyinfo_1      CHANGING ls_key.

*... delete double entries in header file
  DELETE ADJACENT DUPLICATES FROM gt_outtab_chkhdr          "n1229117
                  COMPARING bankl bankn bukrs hbkid hktid.  "n1229117

*... Call the API for displaying a simple Heirarchy List
  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = ls_layo
      it_fieldcat        = lt_fcat
      it_events          = lt_evts2
      i_tabname_header   = gc_2m
      i_tabname_item     = gc_2s
      is_keyinfo         = ls_key
      is_print           = ls_prnt
    TABLES
      t_outtab_header    = gt_outtab_chkhdr
      t_outtab_item      = gt_outtab_chkdet
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_list_2

*&---------------------------------------------------------------------

*&      Form  TOP_OF_PAGE2
*&---------------------------------------------------------------------

*   Handle event top of page for second list
*----------------------------------------------------------------------

FORM top_of_page2.                                          "#EC CALLED
  CLEAR:  bhdgd-line1, bhdgd-line2.
  bhdgd-line1 = sy-title.
  char60      = TEXT-003.
  bhdgd-line2 = char60.
  bhdgd-inifl = '0'.
  bhdgd-uname = sy-uname.
  bhdgd-repid = sy-repid.


  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  IF testrun = gc_value_x.

    REFRESH: gt_header1.
    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-007 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = gt_header1.

  ENDIF.
ENDFORM.                    "TOP_OF_PAGE2

*&---------------------------------------------------------------------*
*&      Form  display_list_3
*&---------------------------------------------------------------------*
*       Display ALV list 3
*----------------------------------------------------------------------*

FORM display_list_3 .
  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        ls_layo  TYPE slis_layout_alv,
        lt_evts3 TYPE slis_t_event,
        ls_key   TYPE slis_keyinfo_alv,
        ls_prnt  TYPE slis_print_alv.
  DATA: lv_counter TYPE i VALUE 3.


*... the fieldcatalog holds the metadata which describes the
*    output table

  PERFORM set_fieldcatalog USING lv_counter CHANGING lt_fcat.

  PERFORM set_layout USING lv_counter CHANGING ls_layo.

*... Change the fieldcatalog either for different headings or colpos
  PERFORM fieldcat_change USING lv_counter CHANGING lt_fcat.

*... the key information links the master and slave table together.
  PERFORM set_keyinfo_2      CHANGING ls_key.

*... In the events table you can register the list to certain
*    events
  PERFORM set_events    USING lv_counter
                     CHANGING lt_evts3.

*... Call the API for displaying a simple Heirarchy List
  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = ls_layo
      it_fieldcat        = lt_fcat
      it_events          = lt_evts3
      i_tabname_header   = gc_3m
      i_tabname_item     = gc_3s
      is_keyinfo         = ls_key
      is_print           = ls_prnt
    TABLES
      t_outtab_header    = gt_outtab_post_ok
      t_outtab_item      = gt_outtab_postdet_ok
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_list_3

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE3
*&---------------------------------------------------------------------
*   Handle event top of page for second list
*----------------------------------------------------------------------

FORM top_of_page3.                                          "#EC CALLED
  DATA: lv_groupname LIKE apqi-groupid.

  REFRESH : gt_header1.
  CLEAR: gs_line1.

  lv_groupname      = group.
  lv_groupname+8(4) = gc_ok.
  CONDENSE lv_groupname NO-GAPS.

  IF p_post = gc_value_x.
    CLEAR:  bhdgd-line1, bhdgd-line2.
    bhdgd-line1 = sy-title.
    IF function = calltrans.
      char60 = TEXT-005.
    ELSE.
      char60 = TEXT-004.
      REPLACE '&' WITH lv_groupname INTO char60.
    ENDIF.
    bhdgd-line2 = char60.
    bhdgd-inifl = '0'.
  ENDIF.

  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  IF testrun = gc_value_x.

    REFRESH: gt_header1.
    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-007 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

  ENDIF.

  READ TABLE gt_tab_postinfo INTO gs_tab_postinfo WITH KEY
                             status = lv_groupname.
  IF sy-subrc = 0.
    CLEAR gs_line1.
    MOVE 'H' TO gs_line1-typ.
    MOVE gs_tab_postinfo-char130+0(40) TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1.
    MOVE 'H' TO gs_line1-typ.
    MOVE gs_tab_postinfo-char130+41(89) TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

  ENDIF.
  IF NOT gt_header1 IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = gt_header1.
  ENDIF.
ENDFORM.                    "TOP_OF_PAGE3
*&---------------------------------------------------------------------
*&      Form  keyinfo_build
*&---------------------------------------------------------------------
*       Set the Key link between header and detail for Hierarchy list
*----------------------------------------------------------------------*
*      <--XS_KEYINFO  Structure which is populated with the key columns
*----------------------------------------------------------------------

FORM set_keyinfo_2  CHANGING xs_keyinfo TYPE slis_keyinfo_alv.

  CLEAR xs_keyinfo.
  xs_keyinfo-header01 = 'HKONT'.
  xs_keyinfo-item01   = 'HKONT'.
  xs_keyinfo-header02 = 'POSTINFO'.
  xs_keyinfo-item02   = 'POSTINFO'.
  xs_keyinfo-header03 = 'ORDER'.                            "857884
  xs_keyinfo-item03   = 'ORDER'.                            "857884
ENDFORM.                    " keyinfo_build
*&---------------------------------------------------------------------*
*&      Form  display_list_4
*&---------------------------------------------------------------------*
*       Display ALV list 4
*----------------------------------------------------------------------*
FORM display_list_4 .
  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        ls_layo  TYPE slis_layout_alv,
        lt_evts4 TYPE slis_t_event,
        ls_key   TYPE slis_keyinfo_alv,
        ls_prnt  TYPE slis_print_alv.
  DATA: lv_counter TYPE i VALUE 4.


*... the fieldcatalog holds the metadata which describes the
*    output table

  PERFORM set_fieldcatalog USING lv_counter CHANGING lt_fcat.

  PERFORM set_layout USING lv_counter CHANGING ls_layo.

*... Change the fieldcatalog either for different headings or colpos
  PERFORM fieldcat_change USING lv_counter CHANGING lt_fcat.
*... the key information links the master and slave table together.
  PERFORM set_keyinfo_2      CHANGING ls_key.

*... In the events table you can register the list to certain
*    events

  PERFORM set_events    USING lv_counter
                     CHANGING lt_evts4.

*... Call the API for displaying a simple Heirarchy List
  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = ls_layo
      it_fieldcat        = lt_fcat
      it_events          = lt_evts4
      i_tabname_header   = gc_3m
      i_tabname_item     = gc_3s
      is_keyinfo         = ls_key
      is_print           = ls_prnt
    TABLES
      t_outtab_header    = gt_outtab_post_err
      t_outtab_item      = gt_outtab_postdet_err
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_list_4

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE4
*&---------------------------------------------------------------------
*   Handle event top of page for second list
*----------------------------------------------------------------------

FORM top_of_page4.                                          "#EC CALLED
  DATA:  lv_groupname LIKE apqi-groupid.

  REFRESH : gt_header1.
  CLEAR: gs_line1.
  lv_groupname      = group.
  lv_groupname+8(4) = gc_err.
  CONDENSE lv_groupname NO-GAPS.

  IF p_post = gc_value_x.
    CLEAR:  bhdgd-line1, bhdgd-line2.
    bhdgd-line1 = sy-title.
    IF function = calltrans.
      char60 = TEXT-005.
    ELSE.
      char60 = TEXT-004.
      REPLACE '&' WITH lv_groupname INTO char60.
    ENDIF.
    bhdgd-line2 = char60.
    bhdgd-inifl = '0'.
  ENDIF.
  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  IF testrun = gc_value_x.

    REFRESH: gt_header1.
    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-007 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.
  ENDIF.
  READ TABLE gt_tab_postinfo INTO gs_tab_postinfo WITH KEY
                             status = lv_groupname.
  IF sy-subrc = 0.
    CLEAR gs_line1.
    MOVE 'H' TO gs_line1-typ.
    MOVE gs_tab_postinfo-char130+0(40) TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1.
    MOVE 'H' TO gs_line1-typ.
    MOVE gs_tab_postinfo-char130+41(89) TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

  ENDIF.
  IF NOT gt_header1 IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = gt_header1.
  ENDIF.
ENDFORM.                    "TOP_OF_PAGE4

*&---------------------------------------------------------------------*
*&      Form  display_list_5
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_list_5 .

  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        lt_evts5 TYPE slis_t_event,
        ls_layo  TYPE slis_layout_alv.


  DATA: lv_counter TYPE i VALUE 5.


*... the fieldcatalog holds the metadata which describes the
*    output table

  PERFORM set_fieldcatalog USING lv_counter CHANGING lt_fcat.

*... Change the fieldcatalog either for different headings or colpos
  PERFORM fieldcat_change USING lv_counter CHANGING lt_fcat.


  PERFORM set_layout USING lv_counter CHANGING ls_layo.

*... In the events table you can register the list to certain
*    events

  PERFORM set_events    USING lv_counter
                     CHANGING lt_evts5.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = ls_layo
      it_fieldcat        = lt_fcat
      it_events          = lt_evts5
    TABLES
      t_outtab           = gt_outtab_hrpost
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " display_list_5

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE5
*&---------------------------------------------------------------------
*   Handle event top of page for second list
*----------------------------------------------------------------------
FORM top_of_page5.                                          "#EC CALLED
  DATA: lv_groupname LIKE apqi-groupid.
  DATA: lt_tab_top TYPE STANDARD TABLE OF fagl_s_rfebck00_hr,
        ls_tab_top TYPE                   fagl_s_rfebck00_hr.
  CLEAR: ls_tab_top,
         gs_line1.
  REFRESH: lt_tab_top,
           gt_header1.
  lv_groupname      = group.
  lv_groupname+8(4) = gc_hr.
  CONDENSE lv_groupname NO-GAPS.

  IF p_post = 'X'.
    CLEAR:  bhdgd-line1, bhdgd-line2.
    bhdgd-line1 = sy-title.
    IF function = calltrans.
      char60 = TEXT-005.
    ELSE.
      char60 = TEXT-004.
      REPLACE '&' WITH lv_groupname INTO char60.
    ENDIF.
    bhdgd-line2 = char60.
    bhdgd-inifl = '0'.
  ENDIF.

  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  READ TABLE gt_tab_postinfo INTO gs_tab_postinfo WITH KEY
                             status = lv_groupname.
  IF sy-subrc = 0.
    CLEAR gs_line1.
    MOVE 'H' TO gs_line1-typ.
    MOVE gs_tab_postinfo-char130+0(40) TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1.
    MOVE 'H' TO gs_line1-typ.
    MOVE gs_tab_postinfo-char130+41(89) TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

  ENDIF.
***NOTE: C5053249
* The HR posting Log internal table is populated with check numbers
* along with the messages. The internal table is populted in such a way
* that the header messages are populated first with check number field
* blank then check numbers alone are populated and finally the status
* messages are populated with check number field blank. Here the
* internal table is split into Top , actual data and then End and
* populated into different internal tables
***In case when HR_POST is checked(post immediately) there will be no
* messages in the END-OF-PAGE, instead the message is transferred to
* TOP-OF-PAGE as the message one line message

*Populate the DATA for TOP-OF-PAGE
  LOOP AT gt_outtab_hrpost INTO gs_outtab_hrpost.
    IF NOT gs_outtab_hrpost-cknum IS INITIAL.
      EXIT.
    ELSE.
      MOVE gs_outtab_hrpost-messtext TO ls_tab_top-messtext.
      APPEND ls_tab_top TO lt_tab_top.
      DELETE TABLE gt_outtab_hrpost FROM gs_outtab_hrpost.
      CLEAR: gs_outtab_hrpost,
             ls_tab_top.
    ENDIF.
  ENDLOOP.
*Populate the data for END-OF-PAGE
  LOOP AT gt_outtab_hrpost INTO gs_outtab_hrpost
                           WHERE cknum IS INITIAL.
    MOVE: gs_outtab_hrpost-messtext TO gs_tab_end-messtext.
    APPEND gs_tab_end TO gt_tab_end.
    DELETE TABLE gt_outtab_hrpost FROM gs_outtab_hrpost.
    CLEAR: gs_outtab_hrpost,
           gs_tab_end.
  ENDLOOP.

  LOOP AT lt_tab_top INTO ls_tab_top.
    WRITE:/1 ls_tab_top-messtext LEFT-JUSTIFIED.
  ENDLOOP.

  IF NOT hr_post IS INITIAL.
    READ TABLE gt_outtab_hrpost INTO gs_outtab_hrpost
                            WITH KEY cknum = gc_hrpost.
    IF sy-subrc = 0.
      WRITE:/1 gs_outtab_hrpost-messtext.
    ENDIF.
  ENDIF.
ENDFORM.                    "TOP_OF_PAGE5

*&---------------------------------------------------------------------
*&      Form  END_OF_PAGE5
*&---------------------------------------------------------------------
*   Handle event end of page for HR post list
*----------------------------------------------------------------------
***In case when HR_POST is checked(post immediately) there will be no
* messages in the END-OF-PAGE, instead the message is transferred to
* TOP-OF-PAGE as the message one line message

FORM end_of_list5.                                          "#EC CALLED
  IF NOT gt_tab_end[] IS INITIAL.
    LOOP AT gt_tab_end INTO gs_tab_end.
      WRITE:/ gs_tab_end-messtext.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "END_OF_PAGE5
*&---------------------------------------------------------------------*
*&      Form  display_list_stats
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_list_stats .

  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        ls_layo  TYPE slis_layout_alv,
        lt_evts6 TYPE slis_t_event,
        ls_prnt  TYPE slis_print_alv.
  DATA: lv_counter TYPE i VALUE 6.


*... the fieldcatalog holds the metadata which describes the
*    output table

  PERFORM set_fieldcatalog USING lv_counter CHANGING lt_fcat.

*... Change the fieldcatalog either for different headings or colpos
  PERFORM fieldcat_change USING lv_counter CHANGING lt_fcat.

  PERFORM set_layout USING lv_counter CHANGING ls_layo.


*... In the events table you can register the list to certain
*    events

  PERFORM set_events    USING lv_counter
                     CHANGING lt_evts6.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = ls_layo
      it_fieldcat        = lt_fcat
      it_events          = lt_evts6
      is_print           = ls_prnt
    TABLES
      t_outtab           = gt_outtab_stats
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " display_list_stats

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE6
*&---------------------------------------------------------------------
*   Handle event top of page for second list
*----------------------------------------------------------------------
FORM top_of_page6.                                          "#EC CALLED

  REFRESH : gt_header1.
  CLEAR: gs_line1.

  CLEAR:  bhdgd-line1, bhdgd-line2.
  bhdgd-line1 = sy-title.
  char60 = TEXT-133.
  bhdgd-line2 = char60.
  bhdgd-inifl = '0'.


  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd = bhdgd.

  IF testrun = gc_value_x.

    REFRESH: gt_header1.
    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-007 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CLEAR gs_line1 .
    MOVE 'H' TO gs_line1-typ.
    MOVE TEXT-008 TO gs_line1-info.
    APPEND gs_line1 TO gt_header1.

    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = gt_header1.
  ENDIF.
ENDFORM.                    "TOP_OF_PAGE6
*&---------------------------------------------------------------------*
*&      Form  fieldcat_change
*&---------------------------------------------------------------------*
*       change the fieldcat
*----------------------------------------------------------------------*
*      -->IV_COUNTER  list identifier
*      <--XT_FCAT     FIeldcat internal table to be changed
*----------------------------------------------------------------------*
FORM fieldcat_change  USING    iv_counter TYPE i
                      CHANGING xt_fcat    TYPE slis_t_fieldcat_alv.
  DATA ls_fcat TYPE slis_fieldcat_alv.
  CASE iv_counter.
    WHEN 1.
*      do nothing
    WHEN 2.
      LOOP AT xt_fcat INTO ls_fcat.
        CASE ls_fcat-tabname.
          WHEN '2S'.
            IF ls_fcat-fieldname = 'BANKL'  OR
               ls_fcat-fieldname = 'BANKN'  OR
               ls_fcat-fieldname = 'BUKRS'  OR
               ls_fcat-fieldname = 'CKNUM'.
              ls_fcat-no_out  = gc_value_x.
            ENDIF.
            IF ls_fcat-fieldname = 'MESSTEXT'.
              ls_fcat-seltext_l  = TEXT-134.
              ls_fcat-ddictxt    = gc_l.
            ENDIF.
          WHEN '2M'.
            ls_fcat-just  = gc_c.
        ENDCASE.

        MODIFY xt_fcat FROM ls_fcat TRANSPORTING no_out
                                                 col_pos
                                                 seltext_l
                                                 ddictxt
                                                 just.
        CLEAR  ls_fcat.
      ENDLOOP.
    WHEN 3 OR 4.
      LOOP AT xt_fcat INTO ls_fcat.
        CASE ls_fcat-tabname.
          WHEN '3M'.
            CASE ls_fcat-fieldname.
              WHEN 'AMOUNT'.
                ls_fcat-seltext_l = TEXT-135.
                ls_fcat-just      = gc_r.
              WHEN 'WAERS'.
                ls_fcat-seltext_l = TEXT-132.
              WHEN 'VALUT'.
                ls_fcat-seltext_l = TEXT-052.
              WHEN 'POSTINFO'.
                ls_fcat-seltext_l = TEXT-131.
              WHEN 'ORDER'.                                 "857884
                ls_fcat-no_out = gc_value_x.                "857884
            ENDCASE.
          WHEN '3S'.
            CASE ls_fcat-fieldname.
              WHEN 'WRBTR'.
                ls_fcat-seltext_l = TEXT-135.
                ls_fcat-just      = gc_r.
              WHEN 'HKONT'.
                ls_fcat-no_out = gc_value_x.
              WHEN 'WAERS'.
                ls_fcat-seltext_l = TEXT-132.
              WHEN 'POSTINFO'.
                ls_fcat-no_out = gc_value_x.
              WHEN 'ORDER'.                                 "857884
                ls_fcat-no_out = gc_value_x.                "857884
            ENDCASE.
        ENDCASE.
        ls_fcat-ddictxt = gc_l.
        MODIFY xt_fcat FROM ls_fcat TRANSPORTING no_out
                                                 seltext_l
                                                 ddictxt
                                                 just.
        CLEAR  ls_fcat.
      ENDLOOP.
    WHEN 5.
      LOOP AT xt_fcat INTO ls_fcat.
        CASE ls_fcat-fieldname.
          WHEN 'CKNUM'.
            ls_fcat-seltext_l = TEXT-136.
            ls_fcat-ddictxt   = gc_l.
          WHEN 'MESSTEXT'.
            ls_fcat-no_out    = gc_value_x.
        ENDCASE.
        MODIFY xt_fcat FROM ls_fcat TRANSPORTING seltext_l
                                                 ddictxt
                                                 no_out.
        CLEAR  ls_fcat.
      ENDLOOP.
    WHEN 6.
      LOOP AT xt_fcat INTO ls_fcat.
        IF ls_fcat-fieldname = 'COMMENTEXT'.
          ls_fcat-seltext_l = TEXT-133.
          ls_fcat-ddictxt   = gc_l.
        ENDIF.
        MODIFY xt_fcat FROM ls_fcat TRANSPORTING seltext_l
                                                 ddictxt.
        CLEAR  ls_fcat.
      ENDLOOP.
    WHEN OTHERS.
*      do nothing
  ENDCASE.
ENDFORM.                    " fieldcat_change
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       build fieldcat using FM 'REUSE_ALV_FIELDCATALOG_MERGE'
*----------------------------------------------------------------------*
*      -->ic_struc structure
*      <--XT_FCAT fieldcatalog table
*----------------------------------------------------------------------*
FORM build_fieldcat    USING ic_struc TYPE dd02l-tabname
                    CHANGING xt_fcat  TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = ic_struc
    CHANGING
      ct_fieldcat            = xt_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " build_fieldcat





*---------------------------------------------------------------*
*  SUBROUTINE  FILL_FTPOST.                                     *
*---------------------------------------------------------------*
FORM fill_ftpost_i.


* Post Document: Fields of first line item
  CLEAR ftpost.
  ftpost-stype = 'P'.                  " Line Item
  ftpost-count = gv_count.                  " Nr Line Item

  PERFORM ftpost_field USING 'RF05A-NEWBS' '50'.          " Post Key

  WRITE t012k-hkont TO fvalue.
  PERFORM ftpost_field USING 'RF05A-NEWKO' fvalue.        " Account

*  WRITE AMOUNT_SUM TO FVALUE CURRENCY WAERS.                  "ak281099
  WRITE amount_chk TO fvalue CURRENCY waers NO-GROUPING.    "ak281099
  CONDENSE fvalue NO-GAPS.
  PERFORM ftpost_field USING 'BSEG-WRBTR' fvalue.         " Amount

  IF NOT valut IS INITIAL.
    WRITE valut TO fvalue DD/MM/YYYY.
    PERFORM ftpost_field USING 'BSEG-VALUT' fvalue.       " Value Date
  ENDIF.

*--------------------------------------------------------------------*
* SFIN Cash: determine HBKID and HKTID
* Begins
  IF cl_fclm_switch_utility=>is_switch_active( ) EQ abap_true.
    DATA l_field_prop     TYPE faus1.
    CALL FUNCTION 'FI_FIELD_SELECTION_DETERMINE'
      EXPORTING
        i_bschl     = '50'
        i_bukrs     = t012k-bukrs
        i_saknr     = t012k-hkont
      IMPORTING
        e_faus1     = l_field_prop
*       E_FAUS2     =
      EXCEPTIONS
        customizing = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF l_field_prop+36(1) NE '-'
      AND ( NOT t012k-hbkid IS INITIAL
        OR NOT t012k-hktid IS INITIAL ).
      WRITE t012k-hbkid TO fvalue.
      PERFORM ftpost_field USING 'BSEG-HBKID' fvalue.        " House Bank

      WRITE t012k-hktid TO fvalue.
      PERFORM ftpost_field USING 'BSEG-HKTID' fvalue.        " House Bank Account
    ENDIF.
  ENDIF.
*                                                                 Ends
*--------------------------------------------------------------------*
ENDFORM.



*---------------------------------------------------------------*
*  SUBROUTINE  FILL_FTPOST.                                     *
*---------------------------------------------------------------*
FORM fill_ftpost_h.
* Post Document: Header Data
  CLEAR ftpost.
  ftpost-stype = 'K'.
  ftpost-count = 1.

  WRITE bldat TO fvalue DD/MM/YYYY.
  PERFORM ftpost_field USING 'BKPF-BLDAT' fvalue.         "Document Date

  WRITE i_blart TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BLART' fvalue.         "Doc Type

  WRITE t012k-bukrs TO fvalue.
  PERFORM ftpost_field USING 'BKPF-BUKRS' fvalue.    "Company Code

  WRITE budat    TO fvalue DD/MM/YYYY.
  PERFORM ftpost_field USING 'BKPF-BUDAT' fvalue.         "Buchungsdatum

  WRITE waers TO fvalue.
  PERFORM ftpost_field USING 'BKPF-WAERS' fvalue.

  IF NOT pdate IS INITIAL AND xlocalcurr IS INITIAL.        "mp45A
* provide chrec-pdate as FX-translation date if foreign currency
    WRITE pdate    TO fvalue DD/MM/YYYY.                    "mp45A
    PERFORM ftpost_field USING 'BKPF-WWERT' fvalue.         "mp45A
  ENDIF.                                                    "mp45A

  IF status = '2'.
*   only if each transaction is for one check
    WRITE ckrec-cknum TO fvalue.
    PERFORM ftpost_field USING 'BKPF-XBLNR' fvalue.  "Referenz Doc
  ENDIF.



  DATA: it_tmp TYPE TABLE OF ftpost.

  REFRESH it_tmp.
  it_tmp[] = ftpost[].
  DELETE ftpost[] WHERE stype = 'P'.
  DELETE it_tmp[] WHERE stype = 'K'.
  APPEND LINES OF it_tmp[] TO ftpost[].
  BREAK omrani.

ENDFORM.
