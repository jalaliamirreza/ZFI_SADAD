REPORT zbrz_fi_rfssld00
       MESSAGE-ID fr
       NO STANDARD PAGE HEADING.
*       LINE-SIZE 132.                                      "n1044838
*----------------------------------------------------------------------*
* Title :  G/L Account Balances
*----------------------------------------------------------------------*
* 18/02/2004 : Report Output Converted To ALV
*
*----------------------------------------------------------------------*
* Change history of ALV Development
* Program description: List the G/L Account Detail and Totals for all
* Company Codes
* Short description of the program:
* Append list is Displayed in ALV format using function module
* 'REUSE_ALV_LIST_DISPLAY' which also display totals for all company
* codes as a seperate list at END OF LIST
* Used ALV Designer for creating layouts...
* FI_MESSAGES_ALV is used to display the messages
*    - Function/flow of the program
*    - Possibly a note on the program templates (through generation)
*---------------------------------------------------------------------*
* Retrofit List separation
*          Group version
*          Local currency in Header missing
*          correction of documentation
*---------------------------------------------------------------------*

*       TABELLEN                                                      *
*---------------------------------------------------------------------*

TABLES: bhdgd,
        ska1,
        skat,
        skb1,
        skc1a,                         "HW-Salden
        skc1c,                         "HW- und FW-Salden
        rfpdo,                         "Doku Parameter
        rfsdo,                         "Doku Select-Options
        t001,
        sscrfields.                    "Handle Push Button
TABLES: faglfreesel.                                        "n1906260
********************* Start of ALV Coding retrofit *********************
TYPE-POOLS: slis.
TYPE-POOLS: kkblo.

CONSTANTS: c_handle1 TYPE slis_handl VALUE 'HAN1',
           c_handle2 TYPE slis_handl VALUE 'HAN2'.

********************* End of ALV Coding retrofit *********************

*eject
*---------------------------------------------------------------------*
*       FLAGS                                                         *
*---------------------------------------------------------------------*
DATA:
  endsu_flag(1)     TYPE c,           "1 - Endsummierung (TOP-OF-PAGE)
  fw_ausgegeben(1)  TYPE c,           "1 - mind. eine FW ausgegeben
  fw_vorhanden(1)   TYPE c,           "1 - mind. eine FW in Summierung
  k_flag(1)         TYPE c,           "1 - mind. ein C-Segment im Kto
  new_konto_flag(1) TYPE c,           "1 - Konto hat gewechselt
  phsperr_flag(1)   TYPE c,           "1 - keine Ausgabe der Überschrft
  sel_flag(1)       TYPE c,           "1 - mind. 1 Satz im Extrakt
  sternchen_flag(1) TYPE c,           "1 - '*' ausgeben (bei Summen)
  top_flag1(1)      TYPE c,           "1 - TOP-OF-PAGE: SAKNR ausgeben
  top_flag2(1)      TYPE c,           "1 - TOP-OF-PAGE: Maske ausgeben
  top_flag3(1)      TYPE c,           "1 - TOP-OF-PAGE: Kto.*(10)ausg.
  xfeld(1)          TYPE c,           "X - Feld für temporäre Belegung
  xfeld2(1)         TYPE c.           "X - Feld für temporäre Belegung

*eject
*---------------------------------------------------------------------*
*       ANDERE VARIABLEN                                              *
*---------------------------------------------------------------------*
DATA: aktwaehr     LIKE skb1-waers,
      bmonp(8)     TYPE p,          "Berichtsperioden (gepackt)
      bukrszaehler TYPE p,          "Anz. Bukrs pro HW und Konto
      fwaerzaehler TYPE i,          "Angezeigte FWAER pro Zw_Summe
      gsberzaehler TYPE i,          "Angezeigte GsBer pro Konto
      hmaske       LIKE ska1-saknr, "Auszugeb. MASKE bei TOP-OF-PAGE
      ivo(2)       TYPE n,          "Vortragsperioden-Obergrenze
      ivu(2)       TYPE n,          "Vortragsperioden-Untergrenze
      maske1       LIKE ska1-saknr,
      maske2       LIKE ska1-saknr,
      maske3       LIKE ska1-saknr,
      pfeld        TYPE wertv12,                            "n1861687
      saldo        TYPE wertv12,
      saldofw      TYPE wertv12,  "Saldo Fremdwährung
      saldohw      TYPE wertv12,  "Saldo Hauswährung
      ums          LIKE skc1a-um01s, "Hilfsfeld für DO VARYING
      umh          LIKE skc1a-um01h, "Hilfsfeld für DO VARYING
      wabukrs      LIKE skb1-bukrs,
      wagsber      LIKE skc1a-gsber,
      wamaske      LIKE ska1-saknr,
      wafwaer      LIKE skb1-waers,
      wasaknr      LIKE ska1-saknr,
      wawaers      LIKE skb1-waers,
      wms          LIKE skc1c-wm01s, "Hilfsfeld für DO VARYING
      wmh          LIKE skc1c-wm01h. "Hilfsfeld für DO VARYING

*eject
*---------------------------------------------------------------------*
*       FELDLEISTEN                                                   *
*---------------------------------------------------------------------*

*------ Endsummierung: HW-Beträge pro HW aufsummieren ------------------
DATA: BEGIN OF endsuhw,                                     "n1861687
        umsav TYPE wertv12, "Saldovortrag
        salvm TYPE wertv12, "Saldo-Vortragperioden
        solbm TYPE wertv12, "Sollsumme Berichtsper.
        habbm TYPE wertv12, "Habensumme Berichtsper.
        salsl TYPE wertv12, "Sollsalden
        salhb TYPE wertv12, "Habensalden
      END   OF endsuhw,

*------ Mikrofiche Gruppenbegriff Normalversion ------------------------
      BEGIN OF mif1,
        bukrs LIKE skb1-bukrs,
        saknr LIKE ska1-saknr,
        gsber LIKE skc1a-gsber,
      END   OF mif1,

*------ Mikrofiche Gruppenbegriff Konzernversion -----------------------
      BEGIN OF mif2,
        saknr LIKE ska1-saknr,
        waers LIKE skb1-waers,
        bukrs LIKE skb1-bukrs,
        gsber LIKE skc1a-gsber,
      END OF mif2,

*------ Kontensalden für Extrakt ---------------------------------------
      BEGIN OF t OCCURS 100,
        saknr    LIKE ska1-saknr,
        skbez    LIKE skat-txt20,
        waers    LIKE skb1-waers,      "Hauswährung
        bukrs    LIKE skb1-bukrs,
        gsber    LIKE skc1a-gsber,
        fwstz    TYPE c,               "Fremdwährungssatz?
        fwaer    LIKE skb1-waers.      "Fremdwährung
        INCLUDE STRUCTURE endsuhw.
        DATA: umsav_fw TYPE wertv12, "Saldovortrag  "n1861687
        salvm_fw TYPE wertv12, "Saldo-Vortragperioden
        solbm_fw TYPE wertv12, "Sollsumme Berichtsperioden
        habbm_fw TYPE wertv12, "Habensumme Berichtsperioden
        salsl_fw TYPE wertv12, "Sollsalden
        salhb_fw TYPE wertv12, "Habensalden
        sakan    LIKE ska1-sakan,      "Kontonummer in Anzeigelänge
        skbzl    LIKE skat-txt50,
      END   OF t,

*------ BukrsSummierung: HW-Beträge aufsummieren -----------------------
      wsumbkr   LIKE endsuhw,

*----- Zwischensumme: FW-Beträge pro FW aufsummieren (Konzernversion) --
      wsumzsbfw LIKE endsuhw,

*----- Zwischensumme: HW-Beträge pro FW aufsummieren (Konzernversion) --
      wsumzsbhw LIKE endsuhw,

*----- Zwischensumme: HW-Beträge pro HW aufsummieren -------------------
      wsumzsend LIKE endsuhw.

*eject
*---------------------------------------------------------------------*
*       INTERNE TABELLEN                                              *
*---------------------------------------------------------------------*

*------ Endsummierung: Währungssummen pro Buchungskreis ----------------
DATA: BEGIN OF endsutab OCCURS 50,
        waers LIKE skb1-waers,        "Hauswährung
        bukrs LIKE skb1-bukrs,        "Buchungskreis
        fwstz TYPE c,               "Fremdwährungssatz?
        fwaer LIKE skb1-waers.        "Fremdwährung
        INCLUDE STRUCTURE endsuhw.
      DATA:
            END   OF endsutab,

*------ Endsummierung: FW-Summen pro HW --------------------------------
            BEGIN OF endsuwaers OCCURS 10,
              fwaer LIKE skb1-waers.        "Fremdwährung
              INCLUDE STRUCTURE endsuhw.
            DATA:
            END   OF endsuwaers.

*------ Auszugebende MESSAGES ------------------------------------------
DATA: BEGIN OF msgtab OCCURS 10.
        INCLUDE STRUCTURE fimsg.
      DATA: END   OF msgtab,

*------ BukrsSummierung: Währungssummen (nur Normalversion) ------------
      BEGIN OF wtabbkr OCCURS 10,
        waers LIKE skb1-waers,        "Hauswährung
        fwaer LIKE skb1-waers,        "Fremdwährung
        fwstz TYPE c.               "Fremdwährungssatz?
        INCLUDE STRUCTURE endsuhw.
      DATA:
      END   OF wtabbkr,

*------ Kontosummierung: Währungssummen --------------------------------
      BEGIN OF wtabkto OCCURS 10,
        waers LIKE skb1-waers,        "Hauswährung
        fwaer LIKE skb1-waers,        "Fremdwährung
        fwstz TYPE c.               "Fremdwährungssatz?
        INCLUDE STRUCTURE endsuhw.
      DATA:
      END   OF wtabkto,

*------ Obere Zwischensummenstufe: Währungssummen ----------------------
      BEGIN OF wtabzsb1 OCCURS 10,
        waers LIKE skb1-waers,        "Hauswährung
        fwaer LIKE skb1-waers,        "Fremdwährung
        bukrs LIKE skb1-bukrs,        "Buchungskreis
        fwstz TYPE c.               "Fremdwährungssatz?
        INCLUDE STRUCTURE endsuhw.
      DATA:
      END   OF wtabzsb1,

*------ Mittlere Zwischensummenstufe: Währungssummen -------------------
      BEGIN OF wtabzsb2 OCCURS 10,
        waers LIKE skb1-waers,        "Hauswährung
        fwaer LIKE skb1-waers,        "Fremdwährung
        bukrs LIKE skb1-bukrs,        "Buchungskreis
        fwstz TYPE c.               "Fremdwährungssatz?
        INCLUDE STRUCTURE endsuhw.
      DATA:
      END   OF wtabzsb2,

*------ Untere Zwischensummenstufe: Währungssummen ---------------------
      BEGIN OF wtabzsb3 OCCURS 10,
        waers LIKE skb1-waers,        "Hauswährung
        fwaer LIKE skb1-waers,        "Fremdwährung
        bukrs LIKE skb1-bukrs,        "Buchungskreis
        fwstz TYPE c.               "Fremdwährungssatz?
        INCLUDE STRUCTURE endsuhw.
      DATA:
      END   OF wtabzsb3,

*------ Zwischensummenstufen abspeichern und sortieren -----------------
      BEGIN OF zs OCCURS 3,
        maske LIKE ska1-saknr,
      END OF zs.

******************** Start of ALV Coding retrofit *********************

**Data Declaration.
CONSTANTS:
  gc_structure  LIKE dd02l-tabname
                     VALUE 'FAGL_S_RFSSLD00_LIST',
  gc_topofpage  TYPE slis_alv_event-form
                     VALUE 'TOP_OF_PAGE',
  gc_endoflist  TYPE slis_alv_event-form
                     VALUE 'END_OF_LIST',
  gc_grouplevch TYPE slis_alv_event-form VALUE
                            'GROUPLEVEL_CHANGE',
*  gc_save(1)    TYPE c VALUE 'X', "List of all users variant
  gc_true(1)    TYPE c VALUE 'X',
  gc_selection  TYPE slis_listheader-typ
                      VALUE 'S'."List Header

DATA: gs_output_value  TYPE fagl_s_rfssld00_list, "Structure for Output
      gs_output_value1 TYPE fagl_s_rfssld00_list, "Structure for Output
      " retrofit
      gs_output_totals TYPE fagl_s_rfssld00_list, "Structure for Totals
      gs_variant       TYPE disvariant, " Structure for Variant
      gs_layout_list   TYPE slis_layout_alv, "Layout Structure
      gs_layout        TYPE slis_layout_alv, "Layout Structure
      gs_stored_header TYPE fagl_s_rfssld00_list,
      GC_SAVE(1)       TYPE C.

DATA: gs_print          TYPE slis_print_alv.       " Print Options
DATA: BEGIN OF gt_output_value OCCURS 1.
        INCLUDE STRUCTURE fagl_s_rfssld00_list .

        DATA : fdate TYPE  char10,
        ktoks TYPE  t077z-ktoks,
        txt30 TYPE  t077z-txt30.

DATA : END OF gt_output_value.
DATA: "gt_output_value   TYPE STANDARD TABLE OF fagl_s_rfssld00_list
  "                 WITH HEADER LINE,
  gt_output_totals        TYPE STANDARD TABLE OF fagl_s_rfssld00_list
                          WITH HEADER LINE,
  gt_top_of_page          TYPE slis_t_listheader, "Table for TOP-OF-PAGE
  gt_fieldcat_list        TYPE slis_t_fieldcat_alv, "Field Catalog Table
  gt_sort                 TYPE slis_t_sortinfo_alv, " Sort Output
  gt_eventtab_list        TYPE slis_t_event, " Events Table
  gs_output_value_layout  TYPE fagl_s_rfssld00_list,
  gs_output_totals_layout TYPE fagl_s_rfssld00_list.


DATA: g_repid         TYPE sy-repid,          "Program Name
      gv_list_counter TYPE i,
      gv_save_counter TYPE i,
      gv_mask1        TYPE ska1-saknr,
      gv_mask2        TYPE ska1-saknr,
      gv_mask3(10).

***Structure for 'BACK', 'EXIT' and 'CANCEL'
DATA:   gs_exit_caused_by_user TYPE slis_exit_by_user.

*DATA l_variant      TYPE disvariant.
*DATA l_variant_help TYPE disvariant.
*DATA: l_exit TYPE c.                 "User-Exit while F4-Help
*DATA l_handle TYPE slis_handl.

******************** End of ALV Coding retrofit *********************

DATA  gd_acc_mode.                                          "n1498883

*-- Start Of Changes on 18 June 2004 : retrofit
DATA: gv_currency TYPE c VALUE '',
      gv_fwstz    TYPE c VALUE ''.
DATA: gd_bukrs LIKE bkpf-bukrs.
DATA: gv_text(255) TYPE c.
*-- End Of Changes on 10 June 2004 : retrofit
DATA: ld_clas_bal_fs10n TYPE boole_d.                       "n1280053
* Historical data
DATA gv_inserted TYPE boole_d.                              "n2439332

INCLUDE zrkasmawf.
*INCLUDE rkasmawf.     "n1477308

*eject
*---------------------------------------------------------------------*
*       PARAMETERS UND SELECT-OPTIONS                                 *
*---------------------------------------------------------------------*
begin_of_block 1.
SELECT-OPTIONS:
  b_monate FOR rfsdo-allgbmon NO-EXTENSION, "Berichtsperioden
  waehrung FOR  rfsdo-ssldwaer,      "Währung (in der Konten geführt)
  kksaldo  FOR rfsdo-ssldsald.       "Saldo des Kontos
* begin note 1689842
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS yecp AS CHECKBOX.  "Include Year-End Closing Postings
SELECTION-SCREEN COMMENT 03(70) TEXT-c03 FOR FIELD yecp.
SELECTION-SCREEN END OF LINE.
* end note 1689842
end_of_block 1.

begin_of_block 2.
PARAMETERS:
  konzvers LIKE rfpdo-dopokonz,        "X - Konzernversion
  fwaehr   LIKE rfpdo-ssldfwar,        "X - mit FW-Salden
* begin of change 28062004 by d002552
  vd_stufe LIKE rfpdo-kshsvdst         "Verdichtungsstufe
                       DEFAULT '0' NO-DISPLAY,
  p_gsau   LIKE rfpdo-duzigsau,
* end of change 28062004 by d002552
  zsb_pos1 LIKE rfpdo-ssldzsbp,        "Position von Zwischensumme 1
  zsb_pos2 LIKE rfpdo-ssldzsbp,        "Position von Zwischensumme 2
  zsb_pos3 LIKE rfpdo-ssldzsbp,        "Position von Zwischensumme 3
  saldo0   LIKE rfpdo-ssldsal0,        "X - Unbebuchte Konten drucken
  xaltkt   LIKE rfpdo1-allgaltk,       "X - Alternative Kontonummer
*                no-display,
  listsep  LIKE rfpdo-allglsep,        "X - Listseparation
  mikfiche LIKE rfpdo-allgmikf,        "X - Mikrofiche-Information
  title    LIKE rfpdo1-allgline.       "Zusatzüberschrift
* begin note 1044838
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS par_ppar AS CHECKBOX. "Do not change print params
SELECTION-SCREEN COMMENT 03(70) TEXT-c02 FOR FIELD par_ppar.
SELECTION-SCREEN END OF LINE.
* end note 1044838
PARAMETERS  pa_stida TYPE hist_date DEFAULT sy-datum MODIF ID std. "Stichtag n2439332
end_of_block 2.

*PARAMETERS: p_sum AS CHECKBOX.

******************** Start of ALV Coding retrofit *********************

SELECTION-SCREEN BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-304.
*SELECTION-SCREEN:
* header
*BEGIN OF LINE,
* COMMENT 1(20) text-300,
* COMMENT pos_low(20) text-301 FOR FIELD par_var1,
* END OF LINE.
* first parameter line
SELECTION-SCREEN BEGIN OF LINE.
* checkbox
*PARAMETERS par_lis1 AS CHECKBOX DEFAULT 'X'.
PARAMETERS par_lis1 LIKE rfpdo3-allgxdli DEFAULT 'X'.
* description
SELECTION-SCREEN: COMMENT (30) TEXT-302
                  FOR FIELD par_lis1,
                  POSITION POS_LOW.
* variant field
SELECTION-SCREEN: COMMENT pos_low(10) TEXT-301
                  FOR FIELD par_var1.
PARAMETERS par_var1 LIKE disvariant-variant.
* push button
SELECTION-SCREEN:
    POSITION POS_HIGH.
SELECTION-SCREEN: PUSHBUTTON (15) TEXT-303
                  USER-COMMAND con1.
SELECTION-SCREEN END OF LINE.

* second parameter line
SELECTION-SCREEN BEGIN OF LINE.
* checkbox
*PARAMETERS par_lis2 AS CHECKBOX DEFAULT 'X'.
PARAMETERS par_lis2 LIKE rfpdo3-allgxdli DEFAULT 'X'.
* description
SELECTION-SCREEN: COMMENT (30) TEXT-305
                  FOR FIELD par_lis2,
                  POSITION POS_LOW.
* variant field
SELECTION-SCREEN: COMMENT pos_low(10) TEXT-301
                  FOR FIELD par_var2.
PARAMETERS par_var2 LIKE disvariant-variant.
* push button
SELECTION-SCREEN:
    POSITION POS_HIGH.
SELECTION-SCREEN: PUSHBUTTON (15) TEXT-303
                  USER-COMMAND con2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK 3.

* no output
PARAMETERS p_no_out TYPE abap_bool NO-DISPLAY.

******************** End of ALV Coding retrofit *********************

*eject
*---------------------------------------------------------------------*
*       FELDGRUPPEN                                                   *
*---------------------------------------------------------------------*
FIELD-GROUPS: header,
              daten.

INSERT maske1 maske2 maske3 t-saknr t-bukrs t-gsber t-fwstz t-waers
       t-fwaer INTO header.

INSERT t-skbez t-skbzl
       t-fwaer
       t-umsav t-salvm t-solbm t-habbm t-salsl t-salhb t-sakan
       t-umsav_fw t-salvm_fw t-solbm_fw t-habbm_fw
       t-salsl_fw t-salhb_fw
INTO daten.

*eject
*---------------------------------------------------------------------*
*       FELDSYMBOLE                                                   *
*---------------------------------------------------------------------*
FIELD-SYMBOLS: <f1>.

*eject
*---------------------------------------------------------------------*
*       INITIALIZATION                                                *
*---------------------------------------------------------------------*
INITIALIZATION.

  MOVE : sy-repid TO g_repid.
*------ Titel für Standardfenster auf Selektionsbild holen -------------
  get_frame_title: 1,2.

*------ Geschäftsjahr vorschlagen --------------------------------------
  CALL FUNCTION 'BUILD_DEFAULT_YEAR'
    TABLES
      xgjahr = sd_gjahr.

*------ Berichtsperioden vorschlagen: 1 bis 16 -------------------------
  CALL FUNCTION 'BUILD_DEFAULT_PERIOD'
    TABLES
      xmonat = b_monate.

  GET PARAMETER ID 'CLASSIC_BAL_FIREP'                      "n1280053
               FIELD ld_clas_bal_fs10n.                     "n1280053
  EXPORT ld_clas_bal_fs10n TO                               "n1280053
               MEMORY ID 'CLASSIC_BALANCE_SDF'.             "n1280053

******************** Start of ALV Coding retrofit *********************
AT SELECTION-SCREEN.

  IF  par_lis1 IS INITIAL
   AND par_lis2 IS INITIAL.
    MESSAGE e261(f7).
  ENDIF.

* begin note 1689842
  IF yecp = 'X'.
    sd_plan = 2.  "Include Year-End Closing Postings
  ELSE.
    CLEAR sd_plan.
  ENDIF.
* end note 1689842
  IF sy-batch EQ space AND sy-tcode NS 'AUDIT'.             "n2439332
    CLEAR pa_stida.
  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_var1.

  PERFORM variant_f4_help USING      c_handle1
                          CHANGING   par_var1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR par_var2.


  PERFORM variant_f4_help USING     c_handle2
                          CHANGING  par_var2.


AT SELECTION-SCREEN ON par_var1.

* Check for the variant ..
  PERFORM check_variant_existance USING c_handle1
                                        par_var1.
* Fill output internal tables with dummy data ..
  IF sscrfields-ucomm = 'CON1'.
    REFRESH gt_output_value.
    DO 3 TIMES.
      CALL FUNCTION 'INITIALIZE_STRUCTURE'
        EXPORTING
          i_n_fill   = 0
          i_i_fill   = 0
        CHANGING
          c_workarea = gs_output_value_layout.

*-- Start Of Changes on 10 May 2004 : retrofit
*      gs_output_value_layout-acytd_bal = gs_output_value_layout-salsl
*                                       + gs_output_value_layout-salhb.
*-- End Of Changes on 10 May 2004 : retrofit

      APPEND gs_output_value_layout TO gt_output_value.
    ENDDO.

    gv_list_counter = 1.
*---- Sort Table ----
    PERFORM t_sort_build   CHANGING gt_sort.
******* Build Field Catalog
    PERFORM fieldcat_list_build USING 'X' CHANGING gt_fieldcat_list.
******* Layout Settings
    gs_layout-group_change_edit = gc_true.
    gs_layout-no_totalline  = gc_true.

*-- Start Of Changes on 10 May 2004 : retrofit
* begin "n1498883
    CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
      IMPORTING
        accessibility     = gd_acc_mode
      EXCEPTIONS
        its_not_available = 1.
    IF gd_acc_mode IS INITIAL.
      gs_layout-no_vline = gc_true.
    ENDIF.
* end "n1498883

******* Set print parameters
    PERFORM print_build.
*-- End Of Changes on 10 May 2004 : retrofit

*  PERFORM layout_build         CHANGING gs_layout_list.
    gs_variant-handle =  c_handle1.
    gs_variant-variant = par_var1.
  IF SY-UNAME = 'MO.MOHAMMADI' OR SY-UNAME = '10003411'.
    GC_SAVE = 'X'.
  ELSE.
    GC_SAVE = ''.
  ENDIF.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = g_repid
        i_structure_name   = gc_structure
        it_fieldcat        = gt_fieldcat_list
        i_save             = gc_save
        is_layout          = gs_layout
        is_variant         = gs_variant
        is_print           = gs_print
      TABLES
        t_outtab           = gt_output_value
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON par_var2.

* Check for the variant ..
  PERFORM check_variant_existance USING c_handle2
                                        par_var2.

* Fill output internal tables with dummy data ..
  IF sscrfields-ucomm = 'CON2'.
    REFRESH gt_output_totals.
    DO 3 TIMES.
      CALL FUNCTION 'INITIALIZE_STRUCTURE'
        EXPORTING
          i_n_fill   = 0
          i_i_fill   = 0
        CHANGING
          c_workarea = gs_output_totals_layout.
      APPEND gs_output_totals_layout TO gt_output_totals.
    ENDDO.

    gv_list_counter = 2.
*---- Sort Table ----
    PERFORM t_sort_build   CHANGING gt_sort.
*---- Layout settings ...
    gs_layout-group_change_edit = gc_true.
    gs_layout-no_totalline  = gc_true.

*-- Start Of Changes on 10 May 2004 : retrofit
* begin "n1498883
    CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
      IMPORTING
        accessibility     = gd_acc_mode
      EXCEPTIONS
        its_not_available = 1.
    IF gd_acc_mode IS INITIAL.
      gs_layout-no_vline = gc_true.
    ENDIF.
* end "n1498883

******* Set print parameters
    PERFORM print_build.
*-- End Of Changes on 10 May 2004 : retrofit

******* Build Field Catalog
  IF SY-UNAME = 'MO.MOHAMMADI' OR SY-UNAME = '10003411'.
    GC_SAVE = 'X'.
  ELSE.
    GC_SAVE = ''.
  ENDIF.


    PERFORM fieldcat_list_build USING 'X' CHANGING gt_fieldcat_list.
    gs_variant-handle     = c_handle2.
    gs_variant-variant = par_var2.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program = g_repid
        i_structure_name   = gc_structure
        it_fieldcat        = gt_fieldcat_list
        i_save             = gc_save
        is_layout          = gs_layout
        is_variant         = gs_variant
      TABLES
        t_outtab           = gt_output_totals
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

******************** END of ALV Coding retrofit *********************

*eject
*---------------------------------------------------------------------*
*       AT SELECTION-SCREEN                                           *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON sd_gjahr.

*------ Geschäftsjahr prüfen: nur ein GJ erlaubt -----------------------
  CALL FUNCTION 'BUILD_DEFAULT_YEAR'
    TABLES
      xgjahr = sd_gjahr.

*------ Verdichtungsstufe prüfen ---------------------------------------
AT SELECTION-SCREEN ON vd_stufe.
  IF vd_stufe >= '0'  AND  vd_stufe <= '4'.
    "richtige Eingabe
  ELSE.
    MESSAGE e103.
  ENDIF.

*------ Stelle für 1. Zwischensumme prüfen -----------------------------
AT SELECTION-SCREEN ON zsb_pos1.
  IF  ( zsb_pos1 = ' ' )
  OR  ( zsb_pos1 >= '0'  AND  zsb_pos1 <= '9' ).
    "richtige Eingabe
  ELSE.
    MESSAGE e104.
  ENDIF.

*------ Stelle für 2. Zwischensumme prüfen -----------------------------
AT SELECTION-SCREEN ON zsb_pos2.
  IF  ( zsb_pos2 = ' ' )
  OR  ( zsb_pos2 >= '0'  AND  zsb_pos2 <= '9' ).
    "richtige Eingabe
  ELSE.
    MESSAGE e104.
  ENDIF.

*------ Stelle für 3. Zwischensumme prüfen -----------------------------
AT SELECTION-SCREEN ON zsb_pos3.
  IF  ( zsb_pos3 = ' ' )
  OR  ( zsb_pos3 >= '0'  AND  zsb_pos3 <= '9' ).
    "richtige Eingabe
  ELSE.
    MESSAGE e104.
  ENDIF.

*------ Berichtsperioden prüfen: 1 bis 16 ------------------------------
AT SELECTION-SCREEN ON b_monate.
  CALL FUNCTION 'BUILD_DEFAULT_PERIOD'
    TABLES
      xmonat = b_monate.

AT SELECTION-SCREEN OUTPUT.                                 "n2439332
  LOOP AT SCREEN.
    IF screen-group1 = 'STD' AND sy-tcode NS 'AUDIT'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*eject
*---------------------------------------------------------------------*
*       START-OF-SELECTION                                            *
*---------------------------------------------------------------------*
START-OF-SELECTION.


  k_flag          = '0'.
  sel_flag        = '0'.
  top_flag1       = '0'.
  top_flag2       = '0'.
  top_flag3       = '0'.
  endsu_flag      = '0'.
  phsperr_flag    = '0'.
  sternchen_flag  = '0'.
  new_konto_flag  = '0'.


  IF pa_stida IS INITIAL OR pa_stida > sy-datum
      OR ( sy-tcode NS 'AUDIT' AND sy-batch NE abap_true ). "n2439332
    pa_stida = sy-datum.
  ENDIF.
  IF pa_stida < sy-datum.                                   "n2439332
    CALL METHOD cl_historical_data=>preread_glacc
      EXPORTING
        iv_date      = pa_stida
        ir_ktopl     = sd_ktopl[]
        ir_saknr     = sd_saknr[]
        ir_bukrs     = sd_bukrs[]
        iv_sel_ska1  = 'X'
        iv_sel_skb1  = 'X'
        iv_sel_altkt = xaltkt.
  ENDIF.

*------ check inserted periods -----------------------------------------
  DATA: ld_monat TYPE monat.
  LOOP AT b_monate.
    MOVE b_monate-low TO ld_monat.
    IF ld_monat GT '16'.
      b_monate-low = '01'.
    ELSE.
      b_monate-low = ld_monat.
    ENDIF.
    MOVE b_monate-high TO ld_monat.
    IF ld_monat GT '16'.
      b_monate-high = '16'.
    ELSE.
      b_monate-high = ld_monat.
    ENDIF.
    MODIFY b_monate.
  ENDLOOP.

*------ Vorperioden ermitteln ------------------------------------------
  bmonp = b_monate-low.
  bmonp = bmonp - 1.
  IF bmonp = 0.
    ivu = ivo = '0'.
  ELSE.
    ivo = bmonp.
    ivu = '1'.
  ENDIF.

  IF vd_stufe NE '0'.
    vd_stufe = '0'.
  ENDIF.

* registration for shedule manager
  PERFORM schedman_start_stop USING 'START'.                "n1477308

*------ Nachrichten sammeln: Initialisierung ---------------------------
  CALL FUNCTION 'FI_MESSAGE_INIT'.

*eject
*---------------------------------------------------------------------*
*       GET SKA1                                                      *
*---------------------------------------------------------------------*
GET ska1.
  IF pa_stida < sy-datum.                                   "n2439332
    CLEAR gv_inserted.
    CALL METHOD cl_historical_data=>recover_data
      EXPORTING
        iv_tabname  = 'SKA1'
      IMPORTING
        ev_inserted = gv_inserted
      CHANGING
        cs_data     = ska1.
    IF gv_inserted = 'X'.
      REJECT.
    ENDIF.
    CALL METHOD cl_historical_data=>recover_skat
      CHANGING
        cs_data = skat.
  ENDIF.
  new_konto_flag = '1'.

*eject
*---------------------------------------------------------------------*
*       GET SKB1                                                      *
*---------------------------------------------------------------------*
GET skb1.

  DATA:
    lv_key_date   TYPE datum,
    lv_period     TYPE susamona,
    lv_skat_txt20 TYPE txt20,
    lv_skat_txt50 TYPE txt50.

  IF pa_stida < sy-datum.                                   "n2439332
    CLEAR gv_inserted.
    CALL METHOD cl_historical_data=>recover_data
      EXPORTING
        iv_tabname  = 'SKB1'
      IMPORTING
        ev_inserted = gv_inserted
      CHANGING
        cs_data     = skb1.
    IF gv_inserted = 'X'.
      REJECT.
    ENDIF.
  ENDIF.
*------ Kontonummer und -bezeichnung in T schreiben --------------------
  IF new_konto_flag = '1'
  OR xaltkt         = 'X'.
    new_konto_flag = '0'.
    IF xaltkt IS INITIAL.
      t-saknr = ska1-saknr.
      t-sakan = ska1-sakan.

      IF idtimeacc_cl_switch_check=>filoc_time_acc_sfws_01( ) EQ 'X'.
        " Note 1731392 - 19-JUN-2012
        IF b_monate-low GT b_monate-high.
          lv_period = b_monate-low.
        ELSE.
          lv_period = b_monate-high.
        ENDIF.
        lv_key_date = idtimeacc_cl_utils=>last_day_in_period(
            im_fiscal_year  = sd_gjahr-low
            im_period       = lv_period
            im_year_variant = t001-periv
        ).
        CALL FUNCTION 'IDCN_GLACC_TEXT_GET'
          EXPORTING
            kontenplan     = t001-ktopl
            sachkonto      = ska1-saknr
            dfrom          = lv_key_date
          IMPORTING
            txt20          = lv_skat_txt20
            txt50          = lv_skat_txt50
          EXCEPTIONS
            not_maintained = 1
            OTHERS         = 2.
        IF sy-subrc = 0.
          MOVE:
            lv_skat_txt20   TO skat-txt20,
            lv_skat_txt50   TO skat-txt50.
        ENDIF.
      ENDIF.
      t-skbez = skat-txt20.
      IF skat-txt50 = space.
        t-skbzl = skat-txt20.
      ELSE.
        t-skbzl = skat-txt50.
      ENDIF.
    ELSE.
      IF idtimeacc_cl_switch_check=>filoc_time_acc_sfws_01( ) EQ 'X'.
        IF b_monate-low GT b_monate-high.
          lv_period = b_monate-low.
        ELSE.
          lv_period = b_monate-high.
        ENDIF.
        lv_key_date = idtimeacc_cl_utils=>last_day_in_period(
            im_fiscal_year  = sd_gjahr-low
            im_period       = lv_period
            im_year_variant = t001-periv
        ).
*       Exportieren Stichtag fur die China G/L-Texte
        idtimeacc_cl_utils=>export_date_to_memory( lv_key_date ).
      ENDIF. " Note 1731392 - 19-JUN-2012
      CALL FUNCTION 'READ_SACHKONTO_ALTKT'
        EXPORTING
          bukrs           = skb1-bukrs
          saknr           = ska1-saknr
          xmass           = 'X'
          xskan           = 'X'
          xtext           = 'X'
          hist_stida      = pa_stida                     "n2439332
        IMPORTING
          altkt           = t-saknr
          altkt_not_found = xfeld
          altkt_sakan     = t-sakan
          ktext           = t-skbez
          ltext           = t-skbzl
          text_not_found  = xfeld2.
*------ Alternative Kontonummer nicht gepflegt -------------------------
      IF xfeld = 'X'.
        CLEAR msgtab.
        msgtab-msort = '1'.
        msgtab-msgid = 'FR'.
        msgtab-msgty = 'E'.
        msgtab-msgno = '274'.
        msgtab-msgv1 = ska1-saknr.
        msgtab-msgv2 = skb1-bukrs.
        CALL FUNCTION 'FI_MESSAGE_COLLECT'
          EXPORTING
            i_fimsg = msgtab.
        REJECT.                        "Keine Ausgabe
      ENDIF.

*------ Text zur alternativen Kontonummer nicht gepflegt ---------------
      IF xfeld2 = 'X'.
        CLEAR msgtab.
        msgtab-msort = '2'.
        msgtab-msgid = 'FR'.
        msgtab-msgty = 'E'.
        msgtab-msgno = '274'.
        msgtab-msgv1 = ska1-saknr.
        msgtab-msgv2 = skb1-bukrs.
        CALL FUNCTION 'FI_MESSAGE_COLLECT'
          EXPORTING
            i_fimsg = msgtab.
      ENDIF.
    ENDIF.

*------ Zwischensummenstellen ermitteln ------------------------------
    CLEAR   zs.
    REFRESH zs.
    maske1 = space.
    maske2 = space.
    maske3 = space.
    PERFORM zsbtab-fuellen USING zsb_pos1.
    PERFORM zsbtab-fuellen USING zsb_pos2.
    PERFORM zsbtab-fuellen USING zsb_pos3.

*------ Zwischensummen-Nummern sortieren -----------------------------
    SORT zs.
    LOOP AT zs.
      CASE sy-tabix.
        WHEN 1.
          maske1 = zs-maske.
        WHEN 2.
          maske2 = zs-maske.
        WHEN 3.
          maske3 = zs-maske.
      ENDCASE.
    ENDLOOP.
  ENDIF.

*------ Ist die FW im selektierten Bereich? ----------------------------
  CHECK skb1-waers IN waehrung.
  REFRESH t.

*------ Boolean-Variablen initialisieren -------------------------------
  saldo   =  0.
  k_flag  = '0'.

*------ Buchungskreis in T stellen -------------------------------------
  t-bukrs = skb1-bukrs.

*------ Währungsfelder in T stellen ------------------------------------
  IF t001-bukrs <> skb1-bukrs.
    SELECT SINGLE * FROM t001 WHERE bukrs = skb1-bukrs.
  ENDIF.

  IF fwaehr = 'X'.
    t-fwaer = skb1-waers.              "Fremdwährung
    t-waers = t001-waers.              "Hauswährung
  ELSE.
    t-fwaer = space.
    t-waers = t001-waers.
  ENDIF.

*eject
*---------------------------------------------------------------------*
*       GET SKC1A                                                     *
*---------------------------------------------------------------------*
GET skc1a.

*------ Hauswährungsbeträge bei Fremdwährungssaldenlisten übergehen ----
  IF  skb1-waers <> t001-waers       "Fremdwährungskonto
  AND fwaehr = 'X'.                  "Fremdwährungssaldenliste
    REJECT.                          "nur SKC1C auswerten
  ENDIF.

*------ Berücksichtigung des Währungstyps ------------------------------
  t-waers = skc1a-hwaer.

*------ Geschäftsbereich in T stellen ----------------------------------
* T-GSBER = SKC1A-GSBER.
  IF NOT p_gsau IS INITIAL.
    t-gsber = skc1a-gsber.
  ELSE.
    t-gsber = space.
  ENDIF.

*------ Saldovortrag Hauswährung in T stellen --------------------------
  t-umsav = skc1a-umsav.

*------ Saldo der Vorperioden ermitteln --------------------------------
  t-salvm = 0.
  DO ivo TIMES VARYING ums FROM skc1a-um01s NEXT skc1a-um02s
               VARYING umh FROM skc1a-um01h NEXT skc1a-um02h.
    t-salvm = t-salvm + ums - umh.
  ENDDO.

*------ Sollsumme der Berichtsperioden in HW ermitteln -----------------
  ADD skc1a-um01s THEN skc1a-um02s UNTIL skc1a-um16s GIVING t-solbm
      ACCORDING TO b_monate.

*------ Habensumme der Berichtsperioden in HW ermitteln ----------------
  ADD skc1a-um01h THEN skc1a-um02h UNTIL skc1a-um16h GIVING t-habbm
      ACCORDING TO b_monate.

*------ Endsaldo in HW ermitteln ---------------------------------------
  pfeld = t-umsav + t-salvm + t-solbm - t-habbm.
  IF pfeld < 0.
    t-salsl = 0.
    t-salhb = pfeld.
  ELSE.
    t-salhb = 0.
    t-salsl = pfeld.
  ENDIF.

  t-salsl_fw = 0.
  t-salhb_fw = 0.
  t-umsav_fw = 0.
  t-salvm_fw = 0.
  t-solbm_fw = 0.
  t-habbm_fw = 0.

*------ HW-Endsalden aller C-Segmente zum Konto im Bukrs aufsummieren --
  saldo = saldo + pfeld.

*------ Kennzeichen für Fremdwährungssatz setzen ----------------------
  t-fwstz = ' '.

*------ HW-Satz in T schreiben -----------------------------------------
  IF saldo0 <> 'X'.
    IF pfeld   <> 0
    OR t-umsav <> 0
    OR t-salvm <> 0
    OR t-solbm <> 0
    OR t-habbm <> 0.
      COLLECT t.
      k_flag = '1'.                  "mind. 1 C-Segment selektiert
    ENDIF.
  ELSE.
    COLLECT t.
    k_flag = '1'.                    "mind. 1 C-Segment selektiert
  ENDIF.

*eject
*---------------------------------------------------------------------*
*       GET SKC1C                                                     *
*---------------------------------------------------------------------*
GET skc1c.

*------ Fremdwährungsbeträge bei Hauswährungssaldenlisten übergehen ----
  IF  skb1-waers <> t001-waers         "Fremdwährungskonto
  AND fwaehr = 'X'.                    "Fremdwährungssaldenliste
    "SKC1C wird auswerten
  ELSE.
    REJECT.                            "nur SKC1A auswerten
  ENDIF.

*------ Berücksichtigung des Währungstyps ------------------------------
  t-waers = skc1c-hwaer.

*------ Geschäftsbereich in T stellen ----------------------------------
* T-GSBER = SKC1C-GSBER.
  IF NOT p_gsau IS INITIAL.
    t-gsber = skc1c-gsber.
  ELSE.
    t-gsber = space.
  ENDIF.

*------ Saldo der Vortragsperioden in HW ermitteln ---------------------
  t-umsav = skc1c-umsav.
  t-salvm = 0.
  DO ivo TIMES VARYING ums FROM skc1c-um01s NEXT skc1c-um02s
               VARYING umh FROM skc1c-um01h NEXT skc1c-um02h.
    t-salvm = t-salvm + ums - umh.
  ENDDO.

*------ Sollsumme Berichtsperioden in HW ermitteln ---------------------
  ADD skc1c-um01s THEN skc1c-um02s UNTIL skc1c-um16s GIVING t-solbm
      ACCORDING TO b_monate.

*------ Habensumme Berichtsperioden in HW ermitteln --------------------
  ADD skc1c-um01h THEN skc1c-um02h UNTIL skc1c-um16h GIVING t-habbm
      ACCORDING TO b_monate.

*------ Endsaldo in HW ermitteln ---------------------------------------
  pfeld = t-umsav + t-salvm + t-solbm - t-habbm.
  IF pfeld < 0.
    t-salsl = 0.
    t-salhb = pfeld.
  ELSE.
    t-salhb = 0.
    t-salsl = pfeld.
  ENDIF.

*------ HW-Endsalden aller C-Segmente zum Konto im Bukrs aufsummieren --
  saldo = saldo + pfeld.               "Saldo in Hauswährung

*------ Kennzeichen für Fremdwährungssatz setzen ----------------------
  t-fwstz = ' '.

  IF skb1-waers NE skc1c-fwaer.  "n1493526 begin
* Not a foreign currency
    t-umsav_fw = 0.
    t-salvm_fw = 0.
    t-solbm_fw = 0.
    t-habbm_fw = 0.
    t-salsl_fw = 0.
    t-salhb_fw = 0.
  ELSE.
*------ Saldo der Vortragsperioden in FW ermitteln ---------------------
    t-umsav_fw = skc1c-slvfw.
    t-salvm_fw = 0.
    DO ivo TIMES VARYING wms FROM skc1c-wm01s NEXT skc1c-wm02s
                 VARYING wmh FROM skc1c-wm01h NEXT skc1c-wm02h.
      t-salvm_fw = t-salvm_fw + wms - wmh.
    ENDDO.

*------ Sollsumme Berichtsperioden in FW ermitteln ---------------------
    ADD skc1c-wm01s THEN skc1c-wm02s UNTIL skc1c-wm16s GIVING t-solbm_fw
        ACCORDING TO b_monate.

*------ Habensumme Berichtsperioden in FW ermitteln --------------------
    ADD skc1c-wm01h THEN skc1c-wm02h UNTIL skc1c-wm16h GIVING t-habbm_fw
        ACCORDING TO b_monate.

*------ Endsaldo in FW ermitteln ---------------------------------------
    pfeld = t-umsav_fw + t-salvm_fw + t-solbm_fw - t-habbm_fw.
    IF pfeld < 0.
      t-salsl_fw = 0.
      t-salhb_fw = pfeld.
    ELSE.
      t-salhb_fw = 0.
      t-salsl_fw = pfeld.
    ENDIF.

  ENDIF.                                  "n1493526 end

*------ FW-Satz in T schreiben -----------------------------------------
  IF saldo0 <> 'X'.
*   IF PFELD   <> 0
    IF t-salsl <> 0
    OR t-salhb <> 0
    OR t-umsav <> 0
    OR t-salvm <> 0
    OR t-solbm <> 0
    OR t-habbm <> 0
    OR t-salsl_fw <> 0
    OR t-salhb_fw <> 0
    OR t-umsav_fw <> 0
    OR t-salvm_fw <> 0
    OR t-solbm_fw <> 0
    OR t-habbm_fw <> 0.
      COLLECT t.
      k_flag = '1'.                    "mind. 1 C-Segment selektiert
    ENDIF.
  ELSE.
    COLLECT t.
    k_flag = '1'.                      "mind. 1 C-Segment selektiert
  ENDIF.

*eject
*---------------------------------------------------------------------*
*       GET SKB1 LATE                                                 *
*---------------------------------------------------------------------*
GET skb1 LATE.

*------ Kontensaldo pro Bukrs im selktierten Bereich? ------------------
  CHECK saldo IN kksaldo.

*------ Konten ohne Saldo ausgeben ==> u.U. Dummy-Satz in T schreiben --
  IF saldo0 = 'X'.                     "Konten mit Saldo = 0 gewünscht?
    IF k_flag = '0'.                   "kein C-Segment selektiert?
      t-gsber = space.
      t-fwstz = space.
      t-umsav = 0.
      t-salvm = 0.
      t-solbm = 0.
      t-habbm = 0.
      t-salsl = 0.
      t-salhb = 0.
      t-umsav_fw = 0.
      t-salvm_fw = 0.
      t-solbm_fw = 0.
      t-habbm_fw = 0.
      t-salsl_fw = 0.
      t-salhb_fw = 0.
      COLLECT t.
    ENDIF.
  ENDIF.

*------ Sätze aus T in Extrakt schreiben -------------------------------
  LOOP AT t.
    EXTRACT daten.
    sel_flag = '1'.                    "mind. 1 Satz im Extrakt
  ENDLOOP.

*eject
*---------------------------------------------------------------------*
*       END-OF-SELECTION                                              *
*---------------------------------------------------------------------*
END-OF-SELECTION.

  FREE MEMORY ID 'CLASSIC_BALANCE_SDF'.                     "n1280053

  gv_list_counter = 0.
*------ Kein Satz im Zwischen-Datenbestand ? ---------------------------
  IF sel_flag = '0'.
    CALL FUNCTION 'FI_MESSAGE_CHECK'
      EXCEPTIONS
        no_message = 01.
    IF sy-subrc = 0.
      MESSAGE s130.                    "Nicht raus, MESSAGES ausgeben

*------ If MESSAGES Exists---------------------------
      phsperr_flag = '1'.
      DATA:  xv_log_head TYPE  slis_listheader-info.
      gs_layout_list-list_append = gc_true.
      gs_layout_list-colwidth_optimize = 'X'.

      CASE msgtab-msort.
        WHEN '1'.
          MOVE : TEXT-f01 TO xv_log_head.
        WHEN '2'.
          MOVE : TEXT-f02 TO xv_log_head.
      ENDCASE.

      CALL FUNCTION 'FI_MESSAGES_ALV'
        EXPORTING
          i_headline = xv_log_head
          is_layout  = gs_layout_list.
    ELSE.
      CALL FUNCTION 'POPUP_NO_LIST'.
    ENDIF.
  ENDIF.

  maske1 = space.
  maske2 = space.
  maske3 = space.
  CLEAR   t.
  REFRESH t.
  REFRESH endsutab.
  CLEAR endsutab.

*------ Leiste für Batch-Heading füllen --------------------------------
  SELECT SINGLE * FROM t001 WHERE bukrs = '0000'.
  CLEAR bhdgd-bukrs.
  MOVE sy-linsz    TO bhdgd-lines.
  MOVE '0'         TO bhdgd-inifl.
  MOVE sy-uname    TO bhdgd-uname.
  MOVE sy-repid    TO bhdgd-repid.
  MOVE sy-title    TO bhdgd-line1.
  MOVE title       TO bhdgd-line2.
  MOVE mikfiche    TO bhdgd-miffl.
  MOVE listsep     TO bhdgd-separ.
  MOVE 'BUKRS'     TO bhdgd-domai.

*------ Extrakt sortieren ----------------------------------------------
  IF konzvers = space.
    SORT BY t-bukrs maske1 maske2 maske3 t-saknr t-fwstz DESCENDING
            t-gsber.
  ELSE.
    SORT BY maske1 maske2 maske3 t-saknr t-waers t-fwaer t-bukrs
            t-fwstz DESCENDING t-gsber.
  ENDIF.

** ---------- Start Of ALV Code -----------------------------------*

  REFRESH gt_output_totals.
  REFRESH gt_output_value.

** ----------End Of ALV Code --------------------------------------*
*eject
*---------------------------------------------------------------------*
* EXTRAKT BEARBEITEN: NORMALVERSION                                   *
*---------------------------------------------------------------------*
  LOOP.
    CASE konzvers.
      WHEN space.

******* AT FIRST *******************************************************
        AT FIRST.
          new_konto_flag = '0'.
        ENDAT.

******* AT NEW T-BUKRS *************************************************
        AT NEW t-bukrs.
          REFRESH wtabbkr.
          CLEAR   wtabbkr.
          CLEAR   wsumbkr.
          bhdgd-bukrs = t-bukrs.
          mif1-bukrs  = t-bukrs.

          IF vd_stufe < '4'.
            MOVE bhdgd-bukrs TO bhdgd-werte.
*           PERFORM NEW-SECTION(RSBTCHH0).
          ENDIF.
        ENDAT.

******* AT NEW MASKE1 **************************************************
        AT NEW maske1.
          IF maske1 <> space.
            REFRESH wtabzsb1.
            CLEAR   wtabzsb1.
            IF vd_stufe < '2'.
*              NEW-PAGE.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT NEW MASKE2 **************************************************
        AT NEW maske2.
          IF maske2 <> space.
            REFRESH wtabzsb2.
            CLEAR   wtabzsb2.
          ENDIF.
        ENDAT.

******* AT NEW MASKE3 **************************************************
        AT NEW maske3.
          IF maske3 <> space.
            REFRESH wtabzsb3.
            CLEAR   wtabzsb3.
          ENDIF.
        ENDAT.

******* AT NEW T-SAKNR *************************************************
        AT NEW t-saknr.
          new_konto_flag = '1'.
          mif1-saknr     = t-saknr.
*-- Start Of Changes on 18 June 2004 : retrofit
          gv_currency = ''.
*-- End Of Changes on 10 June 2004 : retrofit
          IF fwaehr = ' '.
            REFRESH wtabkto.           "Summentabelle für Konto initia-
            CLEAR   wtabkto.           "lisieren (Ausgabe von HW-Sätzen)
          ENDIF.
          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        ENDAT.

******* AT NEW T-FWSTZ (Wechsel zwischen HW- und FW-Sätzen) ************
        AT NEW t-fwstz.
          IF fwaehr = 'X'.
            REFRESH wtabkto.           "Summentabelle für Konto initia-
            CLEAR   wtabkto.           "lisieren (Ausgabe von FW-Sätzen)
          ENDIF.
          IF t-fwstz = 'X'.
            wawaers = t-fwaer.
          ELSE.
            wawaers = t-waers.
          ENDIF.
          CLEAR gsberzaehler.
        ENDAT.

******* AT DATEN *******************************************************
        AT daten.
          IF vd_stufe = '0'.
            mif1-gsber   = t-gsber.
          ELSE.
            mif1-gsber   = space.
          ENDIF.

          bhdgd-grpin = mif1.

*------ Kontonummer und Kontobezeichnung ausgeben ----------------------
*          IF NEW_KONTO_FLAG = '1'.     "Kontonummer hat gewechselt;
*            NEW_KONTO_FLAG  = '0'.     "Ausgabe nicht bei AT NEW T-
*SAKNR
          IF vd_stufe < '2'.         "wegen Batch-Heading-Ausgabe des
            "Geschäeftsbereichs
            RESERVE 3 LINES.

**********retrofit Commented Start ***********************
*              WRITE: SY-VLINE NO-GAP.
**********retrofit Commented End ***********************

**********retrofit Start of ALV Coding ****************

*---- These values are from ** TOP OF PAGE ***-------------
            CLEAR gs_output_value.
            MOVE: t-bukrs TO gs_output_value-bukrs,
                  t-waers TO gs_output_value-waers,
                  t-fwaer TO gs_output_value-fwaer.
* bgin of change d002552
            MOVE t-saknr TO  gs_output_value-saknr.
            MOVE t-sakan TO  gs_output_value-sakan.
**************retrofit END of ALV Coding****************

            IF t-sakan IS INITIAL.
**************retrofit Start of ALV Coding****************

*                WRITE: (10) T-SAKNR COLOR COL_KEY INTENSIFIED.
              MOVE t-saknr TO  gs_output_value-sakan.
            ENDIF.
*-- Start Of Changes on 16 June 2004 : retrofit

            PERFORM conversion_exit_accountno USING     t-saknr
                                              CHANGING  gv_mask1.
            IF zsb_pos1 > '0' AND zsb_pos1 <= '9'.
              MOVE: gv_mask1+0(zsb_pos1) TO gs_output_value-sublevel1.
            ENDIF.
            IF zsb_pos2 > '0' AND zsb_pos2 <= '9'.
              MOVE: gv_mask1+0(zsb_pos2) TO gs_output_value-sublevel2.
            ENDIF.
            IF zsb_pos3 > '0' AND zsb_pos3 <= '9'.
              MOVE : gv_mask1+0(zsb_pos3) TO gs_output_value-sublevel3.
            ENDIF.

*-- End Of Changes on 16 June 2004 : retrofit

**************retrofit END of ALV Coding****************
*              ELSE.

**************retrofit Start of ALV Coding****************
*                WRITE: (10) T-SAKAN COLOR COL_KEY INTENSIFIED.

*-- Start Of Changes on 16 June 2004 : retrofit

*            MOVE t-sakan to  gs_output_value-saknr.
*            PERFORM CONVERSION_EXIT_ACCOUNTNO USING T-SAKAN
*                                              changing  GV_MASK2.
*             IF ZSB_POS1 > '0' AND ZSB_POS1 <= '9'.
*               MOVE: gv_mask2+0(zsb_pos1) TO gs_output_value-sublevel1.
*             ENDIF.
*             IF ZSB_POS2 > '0' AND ZSB_POS2 <= '9'.
*               MOVE: gv_mask2+0(zsb_pos2) TO gs_output_value-sublevel2.
*             ENDIF.
*             IF ZSB_POS3 > '0' AND ZSB_POS3 <= '9'.
*               MOVE: gv_mask2+0(zsb_pos3) TO gs_output_value-sublevel3.
*             ENDIF.

*-- End Of Changes on 16 June 2004 : retrofit

**************retrofit Start of ALV Coding****************

*              ENDIF.
* end of change d002552
**************retrofit Start of ALV Coding****************
*              WRITE:     T-SKBEZ,
*                     132 SY-VLINE.
            MOVE t-skbez TO  gs_output_value-skbez.
            MOVE t-skbzl TO  gs_output_value-skbzl.
**************retrofit End of ALV Coding****************

            top_flag1 = '1'.
          ENDIF.
*           ENDIF.

*------ HW-Zeile oder FW-Zeile ausgeben --------------------------------
          IF vd_stufe = '0'.
            PERFORM salden_ausgeben.
            gsberzaehler = gsberzaehler + 1.
          ENDIF.

*------ Summentabelle für Konto aufsummieren ---------------------------
          CLEAR wtabkto.
          MOVE-CORRESPONDING t TO wtabkto.
*          COLLECT wtabkto.

*------ Summentabelle für 1. Zwischensumme aufsummieren ----------------
          CLEAR wtabzsb1.
          MOVE-CORRESPONDING t TO wtabzsb1.
*          COLLECT wtabzsb1.

*------ Summentabelle für 2. Zwischensumme aufsummieren ----------------
          CLEAR wtabzsb2.
          MOVE-CORRESPONDING t TO wtabzsb2.
*          COLLECT wtabzsb2.

*------ Summentabelle für 3. Zwischensumme aufsummieren ----------------
          CLEAR wtabzsb3.
          MOVE-CORRESPONDING t TO wtabzsb3.
*          COLLECT wtabzsb3.

*------ Summentabelle für Buchungskreis aufsummieren -------------------
          CLEAR wtabbkr.
          MOVE-CORRESPONDING t TO wtabbkr.
*          COLLECT wtabbkr.

*------ Summentabelle für Endsummierung aufsummieren -------------------
          CLEAR endsutab.
          MOVE-CORRESPONDING t TO endsutab.
*          COLLECT endsutab.
        ENDAT.

******* AT END OF T-FWSTZ (Wechsel zwischen HW- und FW-Sätzen) *********
        AT END OF t-fwstz.

*------ Summe der FW-Salden pro Konto ausgeben -------------------------
          IF fwaehr = 'X'.
            IF ( vd_stufe = '0' AND gsberzaehler > 1 ).
              sternchen_flag = '1'.
            ENDIF.

*-- Start Of Changes on 18 June 2004 : retrofit
*            IF ( VD_STUFE = '0' AND GSBERZAEHLER > 1 )
*            OR ( VD_STUFE = '1' ).

            IF  vd_stufe = '1' .
*-- End Of Changes on 18 June 2004 : retrofit
              RESERVE 2 LINES.
              PERFORM saknr_summe.
            ENDIF.
            sternchen_flag = '0'.
          ENDIF.
        ENDAT.

******* AT END OF T-SAKNR **********************************************
        AT END OF t-saknr.
          mif1-gsber  = space.
          bhdgd-grpin = mif1.

*------ Summe der HW-Salden pro Konto ausgeben -------------------------
          IF fwaehr = space.
            IF ( vd_stufe = '0' AND gsberzaehler > 1 ).
              sternchen_flag = '1'.
            ENDIF.

*-- Start Of Changes on 18 June 2004 : retrofit
*            IF ( VD_STUFE = '0' AND GSBERZAEHLER > 1 )
*            OR ( VD_STUFE = '1' ).
            IF  vd_stufe = '1' .

*-- End Of Changes on 10 June 2004 : retrofit

              RESERVE 2 LINES.
              PERFORM saknr_summe.
            ENDIF.
            sternchen_flag = '0'.
          ENDIF.

*------ ULINE nach jedem Konto ausgeben --------------------------------
          IF vd_stufe < '2'.
*            ULINE.
          ENDIF.
          top_flag1 = '0'.
        ENDAT.

******* AT END OF MASKE3 ***********************************************
        AT END OF maske3.
          IF maske3 <> space.
            mif1-saknr  = space.
            bhdgd-grpin = mif1.

            IF vd_stufe < '3'.
              hmaske    = maske3.
              wamaske   = maske3.
              PERFORM zw_summe.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF MASKE2 ***********************************************
        AT END OF maske2.
          IF maske2 <> space.
            mif1-saknr  = space.
            bhdgd-grpin = mif1.

            IF vd_stufe < '3'.
              hmaske    = maske2.
              wamaske   = maske2.
              REFRESH wtabzsb3.
              LOOP AT wtabzsb2.
                wtabzsb3 = wtabzsb2.
                APPEND wtabzsb3.
              ENDLOOP.
              PERFORM zw_summe.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF MASKE1 ***********************************************
        AT END OF maske1.
          IF maske1 <> space.
            mif1-saknr  = space.
            bhdgd-grpin = mif1.

            IF vd_stufe < '3'.
              hmaske    = maske1.
              wamaske   = maske1.
              REFRESH wtabzsb3.
              LOOP AT wtabzsb1.
                wtabzsb3 = wtabzsb1.
                APPEND wtabzsb3.
              ENDLOOP.
              PERFORM zw_summe.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF T-BUKRS **********************************************
        AT END OF t-bukrs.
          bhdgd-grpin = mif1.

          IF vd_stufe < '2'.
*            NEW-PAGE.
          ENDIF.

          IF vd_stufe < '4'.
            PERFORM bukrs_summe_n.
          ENDIF.
        ENDAT.

*eject
*---------------------------------------------------------------------*
* EXTRAKT BEARBEITEN: KONERNVERSION                                   *
*---------------------------------------------------------------------*
      WHEN OTHERS.

******* AT FIRST *******************************************************
        AT FIRST.
          MOVE '0000' TO bhdgd-werte.
          PERFORM new-section(rsbtchh0).
          new_konto_flag = '0'.
        ENDAT.

******* AT NEW MASKE1 ***********************************************
        AT NEW maske1.
          IF maske1 <> space.
            CLEAR   wtabzsb1.
            REFRESH wtabzsb1.
            IF vd_stufe < '3'.
*              NEW-PAGE.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT NEW MASKE2 ***********************************************
        AT NEW maske2.
          IF maske2 <> space.
            CLEAR   wtabzsb2.
            REFRESH wtabzsb2.
          ENDIF.
        ENDAT.

******* AT NEW MASKE3 ***********************************************
        AT NEW maske3.
          IF maske3 <> space.
            CLEAR   wtabzsb3.
            REFRESH wtabzsb3.
          ENDIF.
        ENDAT.

******* AT NEW T-SAKNR **********************************************
        AT NEW t-saknr.
          new_konto_flag = '1'.
          mif2-saknr     = t-saknr.
*-- Start Of Changes on 18 June 2004 : retrofit
          gv_currency = ''.
*-- End Of Changes on 10 June 2004 : retrofit

        ENDAT.

******* AT NEW T-WAERS **********************************************
        AT NEW t-waers.
          mif2-waers   = t-waers.
          bukrszaehler = 0.             "Bukrs einer WAERS initialis.
          CLEAR   wtabkto.
          REFRESH wtabkto.
        ENDAT.

******* AT NEW T-BUKRS **********************************************
        AT NEW t-bukrs.
          bukrszaehler = bukrszaehler + 1.

          IF fwaehr = ' '.
            CLEAR   wtabbkr.
            REFRESH wtabbkr.
          ENDIF.

          IF vd_stufe <= '1'.
            mif2-bukrs = t-bukrs.
          ELSE.
            mif2-bukrs = space.
          ENDIF.

          wabukrs = t-bukrs.
        ENDAT.

******* AT NEW T-FWSTZ (Wechsel zwischen HW- und FW-Sätzen) ************
        AT NEW t-fwstz.
          IF fwaehr = 'X'.
            REFRESH wtabbkr.
            CLEAR   wtabbkr.
          ENDIF.
          wawaers = t-waers.
          wafwaer = t-fwaer.
          wabukrs = t-bukrs.
          CLEAR gsberzaehler.
        ENDAT.

******* AT DATEN ****************************************************
        AT daten.
          IF vd_stufe = '0'.
            mif2-gsber = t-gsber.
          ELSE.
            mif2-gsber = space.
          ENDIF.
          bhdgd-grpin = mif2.
*
*          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*          IF NEW_KONTO_FLAG = '1'.     "Kontonummer hat gewechselt;
*            NEW_KONTO_FLAG  = '0'.     "Ausgabe nicht bei AT <>  T-
*SAKNR

********* retrofit Start of ALV Code **************
          IF vd_stufe < '3'.         "wegen Batch-Heading-Ausgabe von
            "Buchungskreis und Geschäftsber.
*              RESERVE 3 LINES.

*-- Start Of Changes on 18 June 2004 : retrofit
            CLEAR gs_output_value.
*-- End Of Changes on 10 June 2004 : retrofit

            MOVE: t-bukrs TO gs_output_value-bukrs,
                  t-waers TO gs_output_value-waers,
                  t-fwaer  TO gs_output_value-fwaer.

**              WRITE: SY-VLINE NO-GAP.
* begin of change d002552
            MOVE t-saknr TO  gs_output_value-saknr.
            MOVE t-sakan TO  gs_output_value-sakan.
            IF t-sakan IS INITIAL.
              MOVE t-saknr TO  gs_output_value-sakan.
            ENDIF.
            IF zsb_pos1 > '0' AND zsb_pos1 <= '9'.
              MOVE : t-saknr+0(zsb_pos1) TO gs_output_value-sublevel1.
            ENDIF.
            IF zsb_pos2 > '0' AND zsb_pos2 <= '9'.
              MOVE : t-saknr+0(zsb_pos2) TO gs_output_value-sublevel2.
            ENDIF.
            IF zsb_pos3 > '0' AND zsb_pos3 <= '9'.
              MOVE :  t-saknr+0(zsb_pos3) TO gs_output_value-sublevel3.
            ENDIF.
*                WRITE: 2(10) T-SAKNR COLOR COL_KEY INTENSIFIED.
*              ELSE.
*                MOVE t-sakan to  gs_output_value-saknr.
*                IF ZSB_POS1 > '0' AND ZSB_POS1 <= '9'.
*             MOVE : t-sakan+0(zsb_pos1) TO gs_output_value-sublevel1.
*                ENDIF.
*                IF ZSB_POS2 > '0' AND ZSB_POS2 <= '9'.
*             MOVE : t-sakan+0(zsb_pos2) TO gs_output_value-sublevel2.
*                ENDIF.
*                IF ZSB_POS3 > '0' AND ZSB_POS3 <= '9'.
*              MOVE :  t-sakan+0(zsb_pos3) TO gs_output_value-sublevel3.
*                ENDIF.

*                WRITE: 2(10) T-SAKAN COLOR COL_KEY INTENSIFIED.
*              ENDIF.
* end of change d002552
            MOVE t-skbez TO  gs_output_value-skbez.
            MOVE t-skbzl TO  gs_output_value-skbzl.
*              WRITE:     T-SKBEZ,
*                     132 SY-VLINE.
*              TOP_FLAG1 = '1'.
          ENDIF.
********** End Of ALV Code ***************

*          ENDIF.

          wagsber = t-gsber.
          IF vd_stufe = '0'.
            PERFORM salden_ausgeben.
            gsberzaehler = gsberzaehler + 1.
          ENDIF.

*------ Summentabelle für Konto pro Buchungskreis aufsummieren ---------
          CLEAR wtabbkr.
          MOVE-CORRESPONDING t TO wtabbkr.
*          COLLECT wtabbkr.

*------ Summentabelle für Sachkonto aufsummieren -----------------------
          CLEAR wtabkto.
          MOVE-CORRESPONDING t TO wtabkto.
          COLLECT wtabkto.

*------ Summentabelle für 1. Zwischensumme aufsummieren ----------------
          CLEAR wtabzsb1.
          MOVE-CORRESPONDING t TO wtabzsb1.
*          COLLECT wtabzsb1.

*------ Summentabelle für 2. Zwischensumme aufsummieren ----------------
          CLEAR wtabzsb2.
          MOVE-CORRESPONDING t TO wtabzsb2.
*          COLLECT wtabzsb2.

*------ Summentabelle für 3. Zwischensumme aufsummieren ----------------
          CLEAR wtabzsb3.
          MOVE-CORRESPONDING t TO wtabzsb3.
*          COLLECT wtabzsb3.

*------ Summentabelle für Endsummierung aufsummieren -------------------
          CLEAR endsutab.
          MOVE-CORRESPONDING t TO endsutab.
*          COLLECT endsutab.
        ENDAT.

******* AT END OF T-FWSTZ (Wechsel zwischen HW- und FW-Sätzen) ---------
        AT END OF t-fwstz.
          IF fwaehr = 'X'.
*-- Start Of Changes on 21 June 2004 : retrofit
*            IF ( VD_STUFE = '0' AND GSBERZAEHLER > 1 )
*            OR ( VD_STUFE = '1' ).
            IF vd_stufe = '1' .
*-- End Of Changes on 21 June 2004 : retrofit
              PERFORM bukrs_summe_k.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF T-BUKRS *******************************************
        AT END OF t-bukrs.
          mif2-gsber  = space.
          bhdgd-grpin = mif2.

          IF fwaehr = ' '.
*-- Start Of Changes on 21 June 2004 : retrofit
*            IF ( VD_STUFE = '0' AND GSBERZAEHLER > 1 )
*            OR ( VD_STUFE = '1' ).

            IF ( vd_stufe = '1' ).
*-- End Of Changes on 21 June 2004 : retrofit

              PERFORM bukrs_summe_k.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF T-WAERS *******************************************
        AT END OF t-waers.
          mif2-bukrs  = space.
          mif2-waers  = space.
          bhdgd-grpin = mif2.
          IF ( vd_stufe < '2' AND bukrszaehler > '1' ).
            sternchen_flag = '1'.
          ENDIF.
          IF ( vd_stufe < '2' AND bukrszaehler > '1' )
          OR ( vd_stufe = '2' ).
            RESERVE 2 LINES.
            PERFORM saknr_summe.
          ENDIF.
          sternchen_flag = '0'.
        ENDAT.

******* AT END OF T-SAKNR *******************************************
        AT END OF t-saknr.
          IF vd_stufe < '3'.
*
*            ULINE.
          ENDIF.
          top_flag1 = '0'.
        ENDAT.

******* AT END OF MASKE3 ********************************************
        AT END OF maske3.
          IF maske3 <> space.
            mif2-saknr  = space.
            mif2-waers  = space.
            bhdgd-grpin = mif2.

            IF vd_stufe < '4'.
              wamaske  = maske3.
              hmaske   = maske3.
              PERFORM zw_summe.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF MASKE2 ********************************************
        AT END OF maske2.
          IF maske2 <> space.
            mif2-saknr  = space.
            mif2-waers  = space.
            bhdgd-grpin = mif2.

            IF vd_stufe < '4'.
              wamaske  = maske2.
              hmaske   = maske2.
              REFRESH wtabzsb3.
              LOOP AT wtabzsb2.
                wtabzsb3 = wtabzsb2.
                APPEND wtabzsb3.
              ENDLOOP.
              PERFORM zw_summe.
            ENDIF.
          ENDIF.
        ENDAT.

******* AT END OF MASKE1 ********************************************
        AT END OF maske1.
          IF maske1 <> space.
            mif2-saknr  = space.
            mif2-waers  = space.
            bhdgd-grpin = mif2.

            IF vd_stufe < '3'.
*
*              NEW-PAGE.
            ENDIF.

            IF vd_stufe < '4'.
              wamaske  = maske1.
              hmaske   = maske1.
              REFRESH wtabzsb3.
              LOOP AT wtabzsb1.
                wtabzsb3 = wtabzsb1.
                APPEND wtabzsb3.
              ENDLOOP.
              PERFORM zw_summe.
            ENDIF.
          ENDIF.
        ENDAT.
    ENDCASE.                           "CASE KONZVERS

*** AT LAST (für Normal- und Konzernversion) ---------------------------
    AT LAST.
      CLEAR: bhdgd-bukrs,
             bhdgd-grpin.
      endsu_flag     = '1'.

      IF konzvers = space.
        MOVE bhdgd-bukrs TO bhdgd-werte.
        PERFORM new-section(rsbtchh0).
      ELSE.
*
*        NEW-PAGE.
      ENDIF.

*------ Endsummenblatt sortiert ausgeben ------------------------------
      SORT endsutab BY waers bukrs fwstz DESCENDING fwaer.
      LOOP AT endsutab.


*-- Start Of Changes on 18 June 2004 : retrofit
        gv_fwstz = endsutab-fwstz.
*-- End Of Changes on 10 June 2004 : retrofit

        AT FIRST.
          CLEAR wabukrs.
          CLEAR wawaers.
        ENDAT.

        AT NEW waers.
          CLEAR   endsuwaers.
          REFRESH endsuwaers.
          CLEAR   endsuhw.
          wawaers = endsutab-waers.
        ENDAT.

        AT NEW bukrs.
          wabukrs = endsutab-bukrs.
        ENDAT.

*------ FW-Zeile ausgeben ----------------------------------------------
        IF endsutab-fwstz = 'X'.
          RESERVE 3 LINES.
*
*          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
          PERFORM endsumme_ausgeben_fw.

*------ Salden für Summe über alle Bukrs aufsummieren ------------------
          MOVE-CORRESPONDING endsutab TO endsuwaers.
          COLLECT endsuwaers.
        ENDIF.

*------ HW-Zeile ausgeben ----------------------------------------------
        AT END OF fwstz.
          IF endsutab-fwstz = ' '.
            SUM.
            RESERVE 2 LINES.
*
*            FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
            PERFORM endsumme_ausgeben_hw.
            ADD-CORRESPONDING endsutab TO endsuhw.
          ENDIF.
        ENDAT.

*------ FW-Beträge für eine HW ausgeben --------------------------------
        AT END OF waers.
*
*          FORMAT COLOR COL_TOTAL INTENSIFIED.
          PERFORM endsumme_summierung_hw.

*------ Summe aller HW-Beträge für eine HW ausgeben --------------------
          RESERVE 2 LINES.
          MOVE-CORRESPONDING endsuhw TO endsutab.
*
*          FORMAT COLOR COL_TOTAL INTENSIFIED.
          sternchen_flag = '1'.
          PERFORM endsumme_ausgeben_hw.
          sternchen_flag = '0'.
*
*          ULINE.
        ENDAT.
      ENDLOOP.                        "AT ENDSUTAB
    ENDAT.                            "AT LAST
  ENDLOOP.                            "über EXTRAKT

  IF p_no_out IS INITIAL.
**************retrofit Start Of ALV Coding ****************
    IF sel_flag NE '0'.
      IF par_lis2 = 'X' AND par_lis1 <> 'X'.
        gv_save_counter = '2'.
        PERFORM output_totals_alv_list.
      ELSE.
        gv_save_counter = '1'.
        PERFORM output_alv_list.
      ENDIF.
    ENDIF.
**************retrofit Start Of ALV Coding ****************
  ELSE.
    EXPORT balance = gt_output_value TO MEMORY ID 'BALANCE'.
  ENDIF.

* notice of departure from schedule manager
  PERFORM schedman_start_stop USING 'STOP'.                 "n1477308


*eject
*---------------------------------------------------------------------*
*       FORM-Routinen (in alphabetischer Reihenfolge)                 *
*---------------------------------------------------------------------*

*eject
*---------------------------------------------------------------------*
* FORM BUKRS_SUMME_K                                                  *
*---------------------------------------------------------------------*
* Konzernversion: Buchungskreissummen ausgeben                        *
*---------------------------------------------------------------------*
FORM bukrs_summe_k.
*  RESERVE 2 LINES.
*  WAGSBER = '****'.
*
**------ Farbe bestimmen
*------------------------------------------------
*  IF VD_STUFE = '0'.
**    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
*  ELSE.
**    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*  ENDIF.
*
**------ Alle Summenzeilen pro Buchungskreis ausgeben
*-------------------
*  LOOP AT WTABBKR.
*
**------ Ausgabewährung bestimmen
*---------------------------------------
*    IF T-FWSTZ = 'X'.
*      AKTWAEHR = WTABBKR-FWAER.
*    ELSE.
*      AKTWAEHR = WTABBKR-WAERS.
*      CLEAR WAFWAER.
*    ENDIF.
*
**------ Gesamtsaldo ermitteln -------------------------------------
**    SALDO = WTABBKR-SALSL + WTABBKR-SALHB.
*     SALDOHW = WTABBKR-SALSL + WTABBKR-SALHB.
****** retrofit Start Commented ************
*
**------ Schlüsseldaten ausgeben ----------------------------------
*
***    WRITE: / SY-VLINE NO-GAP.
**
**    IF  WAWAERS IS INITIAL
**    AND WAFWAER IS INITIAL
**    AND WABUKRS IS INITIAL.
***
***      WRITE: (17) SPACE NO-GAP COLOR COL_NORMAL INTENSIFIED OFF.
**    ELSE.
***      WRITE: (5) WAWAERS,
***             (5) WAFWAER,
***             (4) WABUKRS.
**    ENDIF.
***
***    WRITE: (4) WAGSBER.
**
***------ Salden ausgeben --------------------------------
***
***    WRITE: 24(17) WTABBKR-UMSAV CURRENCY AKTWAEHR,
***             (17) WTABBKR-SALVM CURRENCY AKTWAEHR,
***             (19) WTABBKR-SOLBM CURRENCY AKTWAEHR NO-GAP,
***             (19) WTABBKR-HABBM CURRENCY AKTWAEHR NO-GAP.
***
***------ Gesamtsaldo ausgeben -------------------------------
**    IF SALDO >= 0.
***
***      WRITE:  98(17) SALDO CURRENCY AKTWAEHR NO-SIGN.
**    ELSE.
***
***      WRITE: 114(17) SALDO CURRENCY AKTWAEHR NO-SIGN.
**    ENDIF.
***
***    WRITE: 132 SY-VLINE.
**
***--- Arbeitsfelder für Anzeige löschen --------------------
***
****** retrofit END Commented ************
*
*
**-- Start Of Changes on 21 June 2004 : retrofit
*
*  IF  WAWAERS IS INITIAL
*    AND WAFWAER IS INITIAL
*    AND WABUKRS IS INITIAL.
*  ELSE.
*      move: " WAWAERS to gs_output_value-waers,
*            " WAFWAER to gs_output_value-fwaer,
*            WTABBKR-FWAER to gs_output_value-fwaer,
*            WABUKRS to gs_output_value-bukrs.
*  ENDIF.
**      move: WTABBKR-WAERS to gs_output_value-waers,
**            WTABBKR-FWAER to gs_output_value-fwaer,
**            WABUKRS to gs_output_value-bukrs.
*
*    Move WAGSBER to gs_output_value-gsber.
*
**------ Salden ausgeben --------------------------------
*If gs_output_value-waers EQ AKTWAEHR.
*
*    move: WTABBKR-UMSAV to gs_output_value-hslvt,
*          WTABBKR-SALVM to gs_output_value-SALVM,
*          WTABBKR-SOLBM to gs_output_value-SOLBM,
*          WTABBKR-HABBM to gs_output_value-habbm.
*
**------ Gesamtsaldo ausgeben -------------------------------
*    IF SALDOHW >= 0.
*      Move SALDOHW to gs_output_value-salsl.
*    ELSE.
*      move SALDOHW to gs_output_value-salhb.
*    ENDIF.
*
*    gs_output_value-acytd_bal = gs_output_value-salsl
*                              + gs_output_value-salhb.
*
*
*  if gv_currency = ''.
*
*    move: WTABBKR-UMSAV to gs_output_value-hslvt_fw,
*          WTABBKR-SALVM to gs_output_value-SALVM_fw,
*          WTABBKR-SOLBM to gs_output_value-SOLBM_fw,
*          WTABBKR-HABBM to gs_output_value-habbm_fw.
*
*    IF SALDOHW >= 0.
*      Move SALDOHW to gs_output_value-salsl_fw.
*    ELSE.
*      move SALDOHW to gs_output_value-salhb_fw.
*    ENDIF.
*
*    gs_output_value-acytd_bal_fw = gs_output_value-salsl_fw
*                              + gs_output_value-salhb_fw.
*  endif.
*
*  Read table gt_output_value into gs_output_value1 with key
*             SAKNR = gs_output_value-SAKNR.
**            GSBER = gs_output_value-gsber.
*  if sy-subrc = 0.
*    if gv_currency <> ''.
*      modify gt_output_value from gs_output_value index sy-tabix
*             transporting hslvt salvm solbm habbm salsl salhb
*                          acytd_bal waers fwaer gsber.
*    else.
*      modify gt_output_value from gs_output_value index sy-tabix.
*    endif.
*  else.
*    APPEND gs_output_value TO gt_output_value.
*  endif.
*
*else.
*
**------ Salden ausgeben --------------------------------
*    gv_currency = 'X'.
*
*
*    move: WTABBKR-UMSAV to gs_output_value-hslvt_fw,
*          WTABBKR-SALVM to gs_output_value-SALVM_fw,
*          WTABBKR-SOLBM to gs_output_value-SOLBM_fw,
*          WTABBKR-HABBM to gs_output_value-habbm_fw.
*
**------ Gesamtsaldo ausgeben -------------------------------
*    IF SALDOHW >= 0.
*      Move SALDOHW to gs_output_value-salsl_fw.
*    ELSE.
*      move SALDOHW to gs_output_value-salhb_fw.
*    ENDIF.
*
*    gs_output_value-acytd_bal_fw = gs_output_value-salsl_fw
*                              + gs_output_value-salhb_fw.
*
*  Read table gt_output_value into gs_output_value1 with key
*             SAKNR = gs_output_value-SAKNR.
**             GSBER = gs_output_value-gsber.
*  if sy-subrc = 0.
*    modify gt_output_value from gs_output_value index sy-tabix
*           transporting hslvt salvm_fw solbm_fw habbm_fw salsl_fw
*                        salhb_fw acytd_bal_fw waers fwaer gsber.
*  else.
*    APPEND gs_output_value TO gt_output_value.
*  endif.
*
*endif.
**   append gs_output_value to gt_output_value.
*
**-- End Of Changes on 21 June 2004 : retrofit
*
*
*    WAWAERS = SPACE.
*    WAFWAER = SPACE.
*    WABUKRS = SPACE.
*    WAGSBER = SPACE.
*  ENDLOOP.
ENDFORM.                    "BUKRS_SUMME_K

*eject
*---------------------------------------------------------------------*
* FORM BUKRS_SUMME_N                                                  *
*---------------------------------------------------------------------*
* Normalversion: Buchungskreissummen ausgebe                          *
*---------------------------------------------------------------------*
FORM bukrs_summe_n.
  fw_ausgegeben  = '0'.

*------ Sortieren der Tabelle für die Buchungskreissummen --------------
  SORT wtabbkr BY waers fwaer fwstz DESCENDING.

*-----  Wird mindestens eine FW ausgegeben? ----------------------------
  LOOP AT wtabbkr.
    IF wtabbkr-fwstz = 'X'.
      fw_ausgegeben = '1'.
      EXIT.
    ENDIF.
  ENDLOOP.

*----- Summe aller reinen HW-Beträge als ersten Eintrag ----------------
  LOOP AT wtabbkr.
    IF wtabbkr-waers = wtabbkr-fwaer.
      DELETE wtabbkr.
      INSERT wtabbkr INDEX 1.
      EXIT.
    ENDIF.
  ENDLOOP.

*------ Sämtliche FW aller HW werden ausgegeben ------------------------
  wasaknr = '**********'.
  LOOP AT wtabbkr.

*------ Salden ausgeben pro Währung (in FW oder in HW) -----------------
    IF wtabbkr-fwstz = space.
      ADD-CORRESPONDING wtabbkr TO wsumbkr. "HW-Sld aller FW summieren
    ENDIF.
    IF wtabbkr-fwstz = 'X'.
      RESERVE 3 LINES.
    ELSE.
      RESERVE 2 LINES.
    ENDIF.
*
*    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    PERFORM bukrs_summen_ausgeben_n.
    top_flag3 = '1'.

*------ U.U. noch eine Leerzeile, nachdem HW-Salden ausgegeben wurden --
    IF  fw_ausgegeben = '1'
    AND wtabbkr-fwstz = ' '.
****** retrofit Start Commented ************
*      WRITE: /    SY-VLINE,
*             132 SY-VLINE.
***** retrofit END Commented ************

    ENDIF.

*------ Summe aller HW-Beträge ausgeben --------------------------------
    AT LAST.
      IF fw_ausgegeben = '1'.
        fw_ausgegeben  = '0'.
        CLEAR wtabbkr.
        wtabbkr-waers = t-waers.
        MOVE-CORRESPONDING wsumbkr TO wtabbkr.
        RESERVE 2 LINES.
*
*        FORMAT COLOR COL_TOTAL INTENSIFIED.
        sternchen_flag = '1'.
        PERFORM bukrs_summen_ausgeben_n.
        sternchen_flag = '0'.
      ENDIF.
    ENDAT.
  ENDLOOP.
*
*  ULINE.
  top_flag3 = '0'.
ENDFORM.                    "BUKRS_SUMME_N

*eject
*---------------------------------------------------------------------*
* FORM BUKRS_SUMMEN_AUSGEBEN_N                                        *
*---------------------------------------------------------------------*
* Normalversion: Buchungskreissummenzeile ausgeben                    *
*---------------------------------------------------------------------*
FORM bukrs_summen_ausgeben_n.

*------ Ausgabewährung ermitteln ---------------------------------------
  IF wtabbkr-fwstz = 'X'.
    aktwaehr = wtabbkr-fwaer.
  ELSE.
    aktwaehr = wtabbkr-waers.
  ENDIF.

*------ Endsaldo bestimmen ---------------------------------------------
  saldohw = wtabbkr-salsl + wtabbkr-salhb.

***** retrofit Start Commented ************

*------ Schlüsseldaten ausgeben ----------------------------------------
**
**  WRITE: /01 SY-VLINE.
*  IF WASAKNR <> SPACE.
**
**    WRITE: 02(10) WASAKNR COLOR COL_KEY INTENSIFIED.
*  ENDIF.
**
**  WRITE:  13    AKTWAEHR,
**          19(4) '****'.
*
**------ Salden ausgeben --------------------------------
**
**  WRITE: 24(17) WTABBKR-UMSAV CURRENCY AKTWAEHR,
**           (17) WTABBKR-SALVM CURRENCY AKTWAEHR,
**           (19) WTABBKR-SOLBM CURRENCY AKTWAEHR NO-GAP,
**           (19) WTABBKR-HABBM CURRENCY AKTWAEHR NO-GAP.
**
**------ Endsaldo ausgeben ----------------------------
*  IF SALDOHW >= 0.
**
**    WRITE:  98(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*    IF STERNCHEN_FLAG = '1'.
**
**      WRITE: '*' NO-GAP.
*    ENDIF.
*  ELSE.
**
**    WRITE: 114(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*    IF STERNCHEN_FLAG = '1'.
**
**      WRITE: '*' NO-GAP.
*    ENDIF.
*  ENDIF.
**
**  WRITE: 132 SY-VLINE.

***** retrofit END Commented ************

  CLEAR wasaknr.
ENDFORM.                    "BUKRS_SUMMEN_AUSGEBEN_N

*eject
*---------------------------------------------------------------------*
* FORM ENDSUMME_AUSGEBEN_FW                                           *
*---------------------------------------------------------------------*
* Endsummierung: FW-Summe je Buchungskreis ausgeben                   *
*---------------------------------------------------------------------*
FORM endsumme_ausgeben_fw.

*  CLEAR gs_output_totals.
*
**------ Endsaldo ermitteln
*---------------------------------------------
*  SALDOFW = ENDSUTAB-SALSL + ENDSUTAB-SALHB.
*
**If gv_fwstz = ''.
*
****** retrofit Start Commented ************
*
***------ Schlüsseldaten ausgeben -----------------------
***
***  WRITE: /      SY-VLINE NO-GAP,
***          2(5)  WAWAERS,
***          8(4)  WABUKRS,
***           (5)  ENDSUTAB-FWAER,
***           (4)  '****'.
***
***------ Salden ausgeben -------------------------------
***
***  WRITE: 24(17) ENDSUTAB-UMSAV CURRENCY ENDSUTAB-FWAER,
***           (17) ENDSUTAB-SALVM CURRENCY ENDSUTAB-FWAER,
***           (19) ENDSUTAB-SOLBM CURRENCY ENDSUTAB-FWAER NO-GAP,
***           (19) ENDSUTAB-HABBM CURRENCY ENDSUTAB-FWAER NO-GAP.
*
*  MOVE :  ENDSUTAB-WAERS  TO  gs_output_totals-waers,
*          ENDSUTAB-FWAER  TO  gs_output_totals-fwaer,
*          ENDSUTAB-BUKRS  TO  gs_output_totals-bukrs,
*          ENDSUTAB-UMSAV  TO  gs_output_totals-hslvt_fw,
*          ENDSUTAB-SALVM  TO  gs_output_totals-salvm_fw,
*          ENDSUTAB-solbm TO  gs_output_totals-solbm_fw,
*          ENDSUTAB-habbm TO  gs_output_totals-habbm_fw.
*
***------ Endsaldo ausgeben ------------------------------
**  IF SALDOFW >= 0.
***
***    WRITE:  98(17) SALDOFW CURRENCY ENDSUTAB-FWAER NO-SIGN.
*      MOVE: SALDOFW TO  gs_output_totals-SALSL_fw.   "Total Debit Bal
*
**  ELSE.
***
***    WRITE: 114(17) SALDOFW CURRENCY ENDSUTAB-FWAER NO-SIGN.
*      MOVE: SALDOFW TO  gs_output_totals-SALHB_fw.   "Total Credit Bal
*
**  ENDIF.
***
***  WRITE: 132 SY-VLINE.
**
**  IF ENDSUTAB-BUKRS <> '****'.
**    Read table gt_output_value into gs_output_value1 with key
**             BUKRS = gs_output_value-BUKRS.
**    if sy-subrc = 0.
**      modify gt_output_totals from gs_output_totals index sy-tabix
**             transporting hslvt salvm solbm habbm salsl salhb
**                          acytd_bal fwaer gsber.
**    else.
**      APPEND gs_output_value TO gt_output_value.
**    endif.
**  endif.
*
**else.
**
**  MOVE :  ENDSUTAB-WAERS  TO  gs_output_totals-waers,
**          ENDSUTAB-FWAER  TO  gs_output_totals-fwaer,
**          ENDSUTAB-BUKRS  TO  gs_output_totals-bukrs,
**          ENDSUTAB-UMSAV  TO  gs_output_totals-hslvt_fw,
**          ENDSUTAB-SALVM  TO  gs_output_totals-salvm_fw,
**          ENDSUTAB-solbm TO  gs_output_totals-solbm_fw,
**          ENDSUTAB-habbm TO  gs_output_totals-habbm_fw.
**
****------ Endsaldo ausgeben ------------------------------
**      MOVE: SALDOFW TO  gs_output_totals-SALSL_fw.   "Total Debit Bal
**      MOVE: SALDOFW TO  gs_output_totals-SALHB_fw.   "Total Credit Bal
**
**  IF ENDSUTAB-BUKRS <> '****'.
**    Read table gt_output_value into gs_output_value1 with key
**             BUKRS = gs_output_value-BUKRS.
**    if sy-subrc = 0.
**      modify gt_output_totals from gs_output_totals index sy-tabix
**             transporting hslvt_fw salvm_fw solbm_fw habbm_fw
**                          salsl_fw salhb_fw acytd_bal_fw fwaer
**                          gsber.
**    else.
**      APPEND gs_output_value TO gt_output_value.
**    endif.
**  endif.
**
**endif.
*
*  IF ENDSUTAB-BUKRS <> '****'.
**    APPEND gs_output_totals to gt_output_totals.
*  ENDIF.
******* retrofit END Commented ************
*
  CLEAR: wabukrs,
         wawaers.
ENDFORM.                    "ENDSUMME_AUSGEBEN_FW

*eject
*---------------------------------------------------------------------*
* FORM ENDSUMME_AUSGEBEN_HW                                           *
*---------------------------------------------------------------------*
* Endsumme: HW-Summe je Buchungskreis ausgeben                        *
*---------------------------------------------------------------------*
FORM endsumme_ausgeben_hw.

*  CLEAR gs_output_totals.
**------ Gesamtsaldo berechnen
*------------------------------------------
*  SALDOHW = ENDSUTAB-SALSL + ENDSUTAB-SALHB.
*
******* Start of ALV Code 5053322 *******************************
*
*
**-- Start Of Changes on 18 June 2004 : retrofit
*If gv_fwstz = ''.
*
**-- End Of Changes on 10 June 2004 : retrofit
*
**------ Schlüsseldaten ausgeben
*----------------------------------------
**
**  WRITE: /       SY-VLINE,
**           2(5)  WAWAERS,
**           8(4)  WABUKRS,
**          19(4)  '****'.
**
**------ Salden ausgeben
*------------------------------------------------
**
**  WRITE: (17) ENDSUTAB-UMSAV CURRENCY ENDSUTAB-WAERS,
**         (17) ENDSUTAB-SALVM CURRENCY ENDSUTAB-WAERS,
**         (19) ENDSUTAB-SOLBM CURRENCY ENDSUTAB-WAERS NO-GAP,
**         (19) ENDSUTAB-HABBM CURRENCY ENDSUTAB-WAERS NO-GAP.
**
*  MOVE :  ENDSUTAB-WAERS  TO  gs_output_totals-waers,
**          gv_FWAER        TO  gs_output_totals-fwaer,
*          ENDSUTAB-BUKRS  TO  gs_output_totals-bukrs,
*          ENDSUTAB-UMSAV  TO  gs_output_totals-hslvt,
*          ENDSUTAB-SALVM  TO  gs_output_totals-salvm,
*          ENDSUTAB-solbm  TO  gs_output_totals-solbm,
*          ENDSUTAB-habbm  TO  gs_output_totals-habbm.
*
**------ Gesamtsaldo ausgeben -----------------------------------
*  IF SALDOHW >= 0.
*
**
**    WRITE:  98(17) SALDOHW CURRENCY ENDSUTAB-WAERS NO-SIGN NO-GAP.
*
*    MOVE: SALDOHW TO  gs_output_totals-SALSL.   "Total Debit Bal
*
*    IF STERNCHEN_FLAG = '1'.
**
**      WRITE: '*' NO-GAP.
*    ENDIF.
*
*  ELSE.
**
**    WRITE: 114(17) SALDOHW CURRENCY ENDSUTAB-WAERS NO-SIGN NO-GAP.
*    MOVE: SALDOHW  TO  gs_output_totals-SALHB. " Credit Total Bal
*
*    IF STERNCHEN_FLAG = '1'.
**
**      WRITE: '*' NO-GAP.
*
*    ENDIF.
*  ENDIF.
*
*  IF ENDSUTAB-BUKRS <> '****'.
*    Read table gt_output_totals into gs_output_value1 with key
*             BUKRS = gs_output_totals-BUKRS.
*  if sy-subrc = 0.
*      modify gt_output_totals from gs_output_totals index sy-tabix
*             transporting hslvt salvm solbm habbm salsl salhb
*                          acytd_bal fwaer gsber.
*  else.
*      APPEND gs_output_totals TO gt_output_totals.
*  endif.
*
*endif.
*
*else.           "retrofit
*
*  MOVE :  ENDSUTAB-WAERS  TO  gs_output_totals-waers,
*          ENDSUTAB-BUKRS  TO  gs_output_totals-bukrs,
*          ENDSUTAB-UMSAV  TO  gs_output_totals-hslvt_fw,
*          ENDSUTAB-SALVM  TO  gs_output_totals-salvm_fw,
*          ENDSUTAB-solbm TO  gs_output_totals-solbm_fw,
*          ENDSUTAB-habbm TO  gs_output_totals-habbm_fw.
*
**------ Gesamtsaldo ausgeben -----------------------------------
*  IF SALDOHW >= 0.
*
*    MOVE: SALDOHW TO  gs_output_totals-SALSL_fw.   "Total Debit Bal
*
*    IF STERNCHEN_FLAG = '1'.
*    ENDIF.
*  ELSE.
*    MOVE: SALDOHW  TO  gs_output_totals-SALHB_fw. " Credit Total Bal
*
*    IF STERNCHEN_FLAG = '1'.
*    ENDIF.
*  ENDIF.
*
*IF ENDSUTAB-BUKRS <> '****'.
*
*  Read table gt_output_totals into gs_output_value1 with key
*             BUKRS = gs_output_totals-BUKRS.
*  if sy-subrc = 0.
*      modify gt_output_totals from gs_output_totals index sy-tabix
*             transporting hslvt_fw salvm_fw solbm_fw habbm_fw
*                          salsl_fw salhb_fw acytd_bal_fw fwaer
*                          gsber.
*  else.
*    APPEND gs_output_totals TO gt_output_totals.
*  endif.
*endif.
*
*endif.          "retrofit
*
**-- End Of Changes on 10 June 2004 : retrofit
*
**  IF ENDSUTAB-BUKRS <> '****'.
**    APPEND gs_output_totals to gt_output_totals.
**  ENDIF.
**
**  WRITE: 132 SY-VLINE.
*
******End Of ALV Code 5053322 *************
*  CLEAR: WABUKRS,
*         WAWAERS.
ENDFORM.                    "ENDSUMME_AUSGEBEN_HW
*
**eject
**---------------------------------------------------------------------*
** FORM ENDSUMME_SUMMIERUNG_HW                                         *
**---------------------------------------------------------------------*
** Endsummierung: Summen der FW's innerhalb einer HW ausgeben          *
**---------------------------------------------------------------------*
FORM endsumme_summierung_hw.
*  WABUKRS = '****'.
*
**------ Saldentabelle sortieren
*----------------------------------------
*  SORT ENDSUWAERS BY FWAER.
*
**------ Sämtliche FW der aktuellen HW anzeigen
*-------------------------
*  LOOP AT ENDSUWAERS.
*    RESERVE 3 LINES.
*
**------ Gesamtsaldo pro FW ermitteln
*-----------------------------------
*
**-- Start Of Changes on 18 June 2004 : retrofit
**    SALDOFW = ENDSUWAERS-SALSL + ENDSUWAERS-SALHB.
**-- End Of Changes on 10 June 2004 : retrofit
*
****** retrofit Start Commented ************
*
**------ Schlüsseldaten pro FW ausgeben
*---------------------------------
**
**    WRITE: /       SY-VLINE NO-GAP,
**           2(5)  WAWAERS,
**           8(4)  WABUKRS,
**            (5)  ENDSUWAERS-FWAER,
**            (4)  '****'.
**
**------ Salden pro FW ausgeben
*-----------------------------------------
**
**    WRITE: 24(17) ENDSUWAERS-UMSAV CURRENCY ENDSUWAERS-FWAER,
**             (17) ENDSUWAERS-SALVM CURRENCY ENDSUWAERS-FWAER,
**             (19) ENDSUWAERS-SOLBM CURRENCY ENDSUWAERS-FWAER
**                                   NO-GAP,
**             (19) ENDSUWAERS-HABBM CURRENCY ENDSUWAERS-FWAER
**                                   NO-GAP.
**
**------ Endsaldo pro FW ausgeben
*---------------------------------------
**    IF SALDOFW >= 0.
**
**      WRITE:  98(17) SALDOFW CURRENCY ENDSUWAERS-FWAER NO-SIGN
**                             NO-GAP,
**                     '*' NO-GAP.
**    ELSE.
**
**      WRITE: 114(17) SALDOFW CURRENCY ENDSUWAERS-FWAER NO-SIGN
**                             NO-GAP,
**                     '*' NO-GAP.
**    ENDIF.
**
**    WRITE: 132 SY-VLINE.
*
****** retrofit End Commented ************
*
*    CLEAR: WABUKRS,
*           WAWAERS.
*  ENDLOOP.
ENDFORM.                    "ENDSUMME_SUMMIERUNG_HW

*eject
*---------------------------------------------------------------------*
* FORM SAKNR_SUMME                                                    *
*---------------------------------------------------------------------*
* Normal-/Konzernversion: Summe der HW- bzw. FW-Salden pro Konto      *
*---------------------------------------------------------------------*
FORM saknr_summe.

*------ Farbe bestimmen ------------------------------------------------
*  IF KONZVERS = SPACE.
*    IF VD_STUFE = '0'.
*      FORMAT COLOR COL_TOTAL INTENSIFIED.
*    ELSE.
*      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*    ENDIF.
*  ELSE.
*    IF VD_STUFE <= '1'.
*      FORMAT COLOR COL_TOTAL INTENSIFIED.
*    ELSE.
*      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*    ENDIF.
*  ENDIF.
*
**------ Endsaldo ermitteln
*---------------------------------------------
*  LOOP AT WTABKTO.
*    RESERVE 3 LINES.
*
**------ Endsaldo ermitteln
*---------------------------------------------
*    SALDOHW = WTABKTO-SALSL + WTABKTO-SALHB.
*
**------ Ausgabewährung ermitteln
*---------------------------------------
*    IF WTABKTO-FWSTZ = 'X'.
*      AKTWAEHR = WTABKTO-FWAER.
*    ELSE.
*      AKTWAEHR = WTABKTO-WAERS.
*    ENDIF.
*
**------ Schlüsseldaten ausgeben
*----------------------------------------
*
******* retrofit Start Commented ************
**
**    WRITE: / SY-VLINE NO-GAP.
*    IF KONZVERS = SPACE.
*      IF WAWAERS IS INITIAL.
**
**        WRITE: 02(17) SPACE COLOR COL_NORMAL INTENSIFIED OFF.
*      ELSE.
**
**    WRITE: 02(11) SPACE COLOR COL_NORMAL INTENSIFIED OFF,
**               13(5)  WAWAERS.
*     MOVE  WAWAERS TO gs_output_value-waers.
*      ENDIF.
**
**      WRITE: 19(4) '****'.
*    ELSE.
**
**      WRITE: 02(5) WTABKTO-WAERS.
*      MOVE  WTABKTO-WAERS TO gs_output_value-waers.
*      IF WTABKTO-FWSTZ = 'X'.
**
**        WRITE: 08(5) WTABKTO-FWAER.
*      MOVE  WTABKTO-FWAER TO gs_output_value-fwaer.
*
*      ENDIF.
**
**      WRITE: 14(4)  '****',
**             19(4)  '****'.
*    ENDIF.
*
**----- Salden ausgeben ------------------------------------------------
*
**    WRITE: 24(17) WTABKTO-UMSAV CURRENCY AKTWAEHR,
**             (17) WTABKTO-SALVM CURRENCY AKTWAEHR,
**             (19) WTABKTO-SOLBM CURRENCY AKTWAEHR NO-GAP,
**             (19) WTABKTO-HABBM CURRENCY AKTWAEHR NO-GAP.
*
*
**-- Start Of Changes on 17 June 2004 : retrofit
** For Balances in Foreign Currencies
*
*If gs_output_value-waers EQ WTABKTO-WAERS.      " retrofit
*
*  MOVE :
*          WTABKTO-UMSAV to gs_output_value-hslvt,
*          WTABKTO-SALVM to gs_output_value-salvm,
*          WTABKTO-SOLBM to gs_output_value-solbm,
*          WTABKTO-HABBM to gs_output_value-habbm.
*
*
**------ Endsaldo ausgeben
*----------------------------------------------
*    IF SALDOHW >= 0.
**
**      WRITE:  98(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*      MOVE SALDOHW to gs_output_value-salsl.
*      IF STERNCHEN_FLAG = '1'.
**
**        WRITE: '*' NO-GAP.
*      ENDIF.
*    ELSE.
**
**      WRITE: 114(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*      MOVE SALDOHW to gs_output_value-salhb.
*      IF STERNCHEN_FLAG = '1'.
**
**        WRITE: '*' NO-GAP.
*      ENDIF.
*    ENDIF.
**
**    WRITE: 132 SY-VLINE.
*
**-- Start Of Changes on 10 May 2004 : retrofit
*      gs_output_value-acytd_bal = gs_output_value-salsl
*                                       + gs_output_value-salhb.
**-- End Of Changes on 10 May 2004 : retrofit
*
**-- Start Of Changes on 18 June 2004 : retrofit
*IF gv_currency = ''.
*  MOVE :
*          WTABKTO-UMSAV to gs_output_value-hslvt_fw,
*          WTABKTO-SALVM to gs_output_value-salvm_fw,
*          WTABKTO-SOLBM to gs_output_value-solbm_fw,
*          WTABKTO-HABBM to gs_output_value-habbm_fw.
*
**------ Endsaldo ausgeben
*
*    IF SALDOHW >= 0.
*      MOVE SALDOHW to gs_output_value-salsl_fw.
*      IF STERNCHEN_FLAG = '1'.
*      ENDIF.
*    ELSE.
*      MOVE SALDOHW to gs_output_value-salhb_fw.
*      IF STERNCHEN_FLAG = '1'.
*      ENDIF.
*    ENDIF.
*
*      gs_output_value-acytd_bal_fw = gs_output_value-salsl_fw
*                                       + gs_output_value-salhb_fw.
*
*endif.
*
*  Read table gt_output_value into gs_output_value1 with key
*             SAKNR = gs_output_value-SAKNR.
**             GSBER = gs_output_value-gsber.
*  if sy-subrc = 0.
*    if gv_currency <> ''.
*      modify gt_output_value from gs_output_value index sy-tabix
*             transporting hslvt salvm solbm habbm salsl salhb
*                          acytd_bal waers fwaer gsber.
*    else.
*        modify gt_output_value from gs_output_value index sy-tabix.
*    endif.
*
*  else.
*    APPEND gs_output_value TO gt_output_value.
*  endif.
*
**-- End Of Changes on 10 June 2004 : retrofit
*
*else.
*
*  gv_currency = 'X'.
*  MOVE :
*          WTABKTO-UMSAV to gs_output_value-hslvt_fw,
*          WTABKTO-SALVM to gs_output_value-salvm_fw,
*          WTABKTO-SOLBM to gs_output_value-solbm_fw,
*          WTABKTO-HABBM to gs_output_value-habbm_fw.
*
*
**------ Endsaldo ausgeben
*----------------------------------------------
*    IF SALDOHW >= 0.
**
**      WRITE:  98(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*      MOVE SALDOHW to gs_output_value-salsl_fw.
*      IF STERNCHEN_FLAG = '1'.
**
**        WRITE: '*' NO-GAP.
*      ENDIF.
*    ELSE.
**
**      WRITE: 114(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*      MOVE SALDOHW to gs_output_value-salhb_fw.
*      IF STERNCHEN_FLAG = '1'.
**
**        WRITE: '*' NO-GAP.
*      ENDIF.
*    ENDIF.
**
**    WRITE: 132 SY-VLINE.
*
**-- Start Of Changes on 10 May 2004 : retrofit
*      gs_output_value-acytd_bal_fw = gs_output_value-salsl_fw
*                                       + gs_output_value-salhb_fw.
*
*  Read table gt_output_value into gs_output_value1 with key
*             SAKNR = gs_output_value-SAKNR.
**             GSBER = gs_output_value-gsber.
*  if sy-subrc = 0.
*    modify gt_output_value from gs_output_value index sy-tabix
*           transporting hslvt salvm_fw solbm_fw habbm_fw salsl_fw
*                        salhb_fw acytd_bal_fw waers fwaer gsber.
*  else.
*    APPEND gs_output_value TO gt_output_value.
*  endif.
*
*Endif.            " retrofit
*
*
**
**  Read table gt_output_value into gs_output_value1 with key
**             SAKNR = gs_output_value-SAKNR .
**  if sy-subrc = 0.
**    modify gt_output_value from gs_output_value index sy-tabix.
**  else.
**    APPEND gs_output_value TO gt_output_value.
**  endif.
**                 " retrofit
*
**-- End Of Changes on 17 June 2004 : retrofit
*
******* retrofit End Commented ********
*
**------ Variablen zur Ausgabe initialisieren
*---------------------------
*    WAWAERS = SPACE.
*    WABUKRS = SPACE.
*    WAGSBER = SPACE.
*  ENDLOOP.
ENDFORM.                    "SAKNR_SUMME


*eject
*---------------------------------------------------------------------*
* FORM ZSBTAB-FUELLEN                                                 *
*---------------------------------------------------------------------*
* Speichert die Sachkontennummer, die entsprechend der Zwischensum-   *
* menbildung modifiziert wird, in der internen Tabelle ZS ab          *
*---------------------------------------------------------------------*
FORM zsbtab-fuellen USING pos LIKE zsb_pos1.
  IF pos > '0' AND pos <= '9'.
    MOVE '**********' TO zs-maske.
    ASSIGN t-saknr(pos) TO <f1>.
    WRITE <f1> TO zs-maske(pos).
    COLLECT zs.                        "kein APPEND, identische
  ENDIF.                               "Eingaben abfangen
ENDFORM.                    "ZSBTAB-FUELLEN

*eject
*---------------------------------------------------------------------*
* FORM ZW_SUMME                                                       *
*---------------------------------------------------------------------*
* Zwischensummen ausgeben                                             *
*---------------------------------------------------------------------*
FORM zw_summe.
  fw_ausgegeben  = '0'.
  fw_vorhanden   = '0'.
  CLEAR wsumzsend.

*----- Summe aller reinen HW-Beträge als ersten Eintrag pro Währung ----
  LOOP AT wtabzsb3.
    IF wtabzsb3-waers = wtabzsb3-fwaer.
      CLEAR wtabzsb3-fwaer.
      MODIFY wtabzsb3.
    ENDIF.

*------ Sind Salden in FW vorhanden? -----------------------------------
    IF wtabzsb3-fwstz = 'X'.
      fw_vorhanden = '1'.
    ENDIF.
  ENDLOOP.

*------ Summentabelle sortieren ----------------------------------------
  SORT wtabzsb3 BY waers fwaer bukrs fwstz DESCENDING.

*------ Normalversion --------------------------------------------------
  LOOP AT wtabzsb3.
    IF konzvers = ' '.

***** AT END OF FWSTZ **************************************************
      AT END OF fwstz.
        SUM.
        IF wtabzsb3-fwstz = 'X'.
          fw_ausgegeben = '1'.
        ELSE.
          ADD-CORRESPONDING wtabzsb3 TO wsumzsend.
        ENDIF.

*------ Salden pro Währung (in FW oder in HW) ausgeben -----------------
        IF wtabzsb3-fwstz = 'X'.
          RESERVE 3 LINES.
        ELSE.
          RESERVE 2 LINES.
        ENDIF.
*
*        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        PERFORM zw_summe_ausgeben_1.
        top_flag2 = '1'.

*------ U.U. noch eine Leerzeile, nachdem HW-Salden ausgegeben wurden --
        IF  fw_vorhanden   = '1'
        AND wtabzsb3-fwstz = ' '.

********* retrofit Start Commented **********
*          WRITE: /    SY-VLINE,
*                  132 SY-VLINE.
******** retrofit End Commented **********
        ENDIF.
      ENDAT.

***** AT END OF WAERS **************************************************
      AT END OF waers.
        SUM.
        IF fw_ausgegeben = '1'.
          MOVE-CORRESPONDING wsumzsend TO wtabzsb3.
          wtabzsb3-fwstz = ' '.        "damit Zeilenvorschub stattfindet

*------ Summe pro HW ausgeben ------------------------------------------
          RESERVE 2 LINES.
          FORMAT COLOR COL_TOTAL INTENSIFIED.
          sternchen_flag = '1'.
          PERFORM zw_summe_ausgeben_1.
          sternchen_flag = '0'.
          fw_ausgegeben  = '0'.
        ENDIF.
      ENDAT.

*------ Konzernversion -------------------------------------------------
    ELSE.

***** AT FIRST *********************************************************
      AT FIRST.

*------ Maskierte Kontonummer ausgeben ---------------------------------
        IF fw_vorhanden = '1'.
          RESERVE 4 LINES.
        ELSE.
          RESERVE 3 LINES.
        ENDIF.
********* retrofit Start Commented **********
*        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*        WRITE: /    SY-VLINE NO-GAP,
*                    WAMASKE COLOR COL_KEY INTENSIFIED,
*                132 SY-VLINE.
******** retrofit End Commented **********
        top_flag2 = '1'.
      ENDAT.

***** AT NEW WAERS *****************************************************
      AT NEW waers.
*
*        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        CLEAR wsumzsend.
        CLEAR fwaerzaehler.
        wawaers = wtabzsb3-waers.
      ENDAT.

***** AT NEW FWAER *****************************************************
      AT NEW fwaer.
        CLEAR wsumzsbhw.
        CLEAR wsumzsbfw.
        CLEAR bukrszaehler.

*------ Leerzeile einfügen (wenn FW vorhanden sind) --------------------
        IF  sy-tabix     > 1
        AND fw_vorhanden = '1'.
          RESERVE 3 LINES.
********* retrofit Start Commented **********
*          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*          WRITE: /    SY-VLINE,
*                  132 SY-VLINE.
******** retrofit End Commented **********
        ENDIF.
      ENDAT.

***** AT NEW BUKRS *****************************************************
      AT NEW bukrs.
        bukrszaehler = bukrszaehler + 1.
      ENDAT.

***** AT END OF FWSTZ **************************************************
      AT END OF fwstz.
        SUM.
        IF vd_stufe <= 1.

*------ Salden pro Buchungskreis (in HW oder in FW) ausgeben -----------
*
*          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
          IF fw_vorhanden = '1'.
            RESERVE 3 LINES.
          ELSE.
            RESERVE 2 LINES.
          ENDIF.
          PERFORM zw_summe_ausgeben_2.
        ENDIF.

*------ Summentabellen und Summenfeldleisten aufaddieren ---------------
        IF wtabzsb3-fwstz = 'X'.
          ADD-CORRESPONDING wtabzsb3 TO wsumzsbfw.
        ELSE.
          ADD-CORRESPONDING wtabzsb3 TO wsumzsend.
          ADD-CORRESPONDING wtabzsb3 TO wsumzsbhw.
        ENDIF.
      ENDAT.

***** AT END OF FWAER **************************************************
      AT END OF fwaer.

*------ Salden pro Fremdwährung in FW ausgeben -------------------------
        IF ( vd_stufe <= '1' AND bukrszaehler > 1 )
        OR ( vd_stufe >= '2' ).
          IF fw_vorhanden = '1'.
            IF NOT ( wsumzsbfw IS INITIAL ).
              MOVE-CORRESPONDING wsumzsbfw TO wtabzsb3.
              wtabzsb3-fwstz = 'X'.
              RESERVE 3 LINES.
              IF vd_stufe < '2'.
*                FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
              ELSE.
*                FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
              ENDIF.
              PERFORM zw_summe_ausgeben_1.
            ENDIF.
          ENDIF.

*------ Salden pro Fremdwährung in HW ausgeben -------------------------
          MOVE-CORRESPONDING wsumzsbhw TO wtabzsb3.
          wtabzsb3-fwstz = ' '.
          RESERVE 2 LINES.
          IF vd_stufe < '2'.
*            FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
          ELSE.
*            FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
          ENDIF.
          PERFORM zw_summe_ausgeben_1.
        ENDIF.
        fwaerzaehler = fwaerzaehler + 1.
      ENDAT.

***** AT END OF WAERS **************************************************
      AT END OF waers.
        IF fwaerzaehler > 1.
          SUM.
          MOVE-CORRESPONDING wsumzsend TO wtabzsb3.
          sternchen_flag = '1'.

******** retrofit Start Commented **********

*------ Leerzeile ausgeben ---------------------------------------------
*          RESERVE 3 LINES.
*          FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*
*          WRITE: /    SY-VLINE,
*                  132 SY-VLINE.
*
*------ Summe pro HW ausgeben ------------------------------------------
*          FORMAT COLOR COL_TOTAL INTENSIFIED.
******** retrofit End Commented **********

          PERFORM zw_summe_ausgeben_1.
          sternchen_flag = '0'.
        ENDIF.
      ENDAT.
    ENDIF.
  ENDLOOP.
*  ULINE.
  top_flag2 = '0'.
ENDFORM.                    "ZW_SUMME

*eject
*---------------------------------------------------------------------*
* FORM ZW_SUMME_AUSGEBEN_1                                            *
*---------------------------------------------------------------------*
* Zwischensummenzeile ausgeben                                        *
*---------------------------------------------------------------------*
FORM zw_summe_ausgeben_1.

*------ Endsaldo bestimmen ---------------------------------------------
  saldohw = wtabzsb3-salsl + wtabzsb3-salhb.

*------ Ausgabewährung bestimmen ---------------------------------------
  IF wtabzsb3-fwstz = 'X'.
    aktwaehr = wtabzsb3-fwaer.
  ELSE.
    aktwaehr = wtabzsb3-waers.
  ENDIF.

*------ Schlüsseldaten ausgeben ----------------------------------------
*
*  WRITE: / SY-VLINE.
  IF konzvers = space.
    IF wamaske <> space.
********* retrofit Start Commented **********
*      WRITE: 02(10) WAMASKE COLOR COL_KEY INTENSIFIED.
******** retrofit End Commented **********
    ENDIF.
********* retrofit Start Commented **********
*    WRITE: 13(5) AKTWAEHR,
*             (4) '****'.
******** retrofit End Commented **********
  ELSE.
    IF wawaers <> space.
*
*      WRITE: 02(5) WAWAERS.
      CLEAR wawaers.
    ENDIF.
    IF wtabzsb3-fwstz = 'X'.
*
*      WRITE: 08(5) WTABZSB3-FWAER.
    ENDIF.
********* retrofit Start Commented **********
*    WRITE: 14(4)  '****',
*             (4)  '****'.
******** retrofit End Commented **********
  ENDIF.

******** retrofit Start Commented **********

*------ Salden ausgeben ------------------------------------------------
*
*  WRITE: 24(17) WTABZSB3-UMSAV CURRENCY AKTWAEHR,
*           (17) WTABZSB3-SALVM CURRENCY AKTWAEHR,
*           (19) WTABZSB3-SOLBM CURRENCY AKTWAEHR NO-GAP,
*           (19) WTABZSB3-HABBM CURRENCY AKTWAEHR NO-GAP.
*
*------ Endsaldo ausgeben ----------------------------------------------
*  IF SALDOHW >= 0.
*
*    WRITE:  98(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*    IF STERNCHEN_FLAG = '1'.
*
*      WRITE: '*' NO-GAP.
*    ENDIF.
*  ELSE.
*
*    WRITE: 114(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN NO-GAP.
*    IF STERNCHEN_FLAG = '1'.
*
*      WRITE: '*' NO-GAP.
*    ENDIF.
*  ENDIF.
*
*  WRITE: 132 SY-VLINE.

******** retrofit End Commented **********

  wawaers = space.
  wabukrs = space.
  wagsber = space.
  wamaske = space.
ENDFORM.                    "ZW_SUMME_AUSGEBEN_1

*eject
*---------------------------------------------------------------------*
* FORM ZW_SUMME_AUSGEBEN_2                                            *
*---------------------------------------------------------------------*
* Konzernversion: Zwischensummenzeile pro Bukrs ausgeben              *
*---------------------------------------------------------------------*
FORM zw_summe_ausgeben_2.

*------ Endsaldo ermitteln ---------------------------------------------
  saldohw = wtabzsb3-salsl + wtabzsb3-salhb.

*------ Schlüsseldaten ausgeben ----------------------------------------
*  WRITE: / SY-VLINE NO-GAP.
  IF wawaers <> space.
*
*    WRITE: 2(5) WAWAERS.
    CLEAR wawaers.
  ENDIF.
  IF wtabzsb3-fwstz <> space.
*
*    WRITE: 8(5) WTABZSB3-FWAER.
    aktwaehr = wtabzsb3-fwaer.
  ELSE.
    aktwaehr = wtabzsb3-waers.
  ENDIF.

******** retrofit Start Commented **********
*  WRITE: 14(4)  WTABZSB3-BUKRS,
*           (4)  '****'.
*
*------ Salden ausgeben -------------------------------------
*  WRITE: 24(17) WTABZSB3-UMSAV CURRENCY AKTWAEHR,
*           (17) WTABZSB3-SALVM CURRENCY AKTWAEHR,
*           (19) WTABZSB3-SOLBM CURRENCY AKTWAEHR NO-GAP,
*           (19) WTABZSB3-HABBM CURRENCY AKTWAEHR NO-GAP.
*
**------ Endsaldo ausgeben ----------------------------------
*  IF SALDOHW >= 0.
*    WRITE:  98(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN.
*  ELSE.
*    WRITE: 114(17) SALDOHW CURRENCY AKTWAEHR NO-SIGN.
*  ENDIF.
*  WRITE: 132 SY-VLINE.
******** retrofit End Commented **********

  wawaers    = space.
  wabukrs    = space.
  wagsber    = space.
  wamaske    = space.
ENDFORM.                    "ZW_SUMME_AUSGEBEN_2

* folgende Routine wird von der Berichtschnittstelle aufgerufen
FORM rsti_selection_exit TABLES it_fieldrm STRUCTURE rstifields
                         USING flg_own_logic.
* Felder aus Feldkatalog entfernen
  it_fieldrm-rfield = 'XX_KTOPL'.    it_fieldrm-trflg = 'E'.
  APPEND it_fieldrm.
  it_fieldrm-rfield = 'XX_BUKRS'.    it_fieldrm-trflg = 'E'.
  APPEND it_fieldrm.
  it_fieldrm-rfield = 'BILVJAHR'.    it_fieldrm-trflg = 'E'.
  APPEND it_fieldrm.
  it_fieldrm-rfield = 'SD_GJAHR'.    it_fieldrm-trflg = 'E'.
* append it_fieldrm.
ENDFORM.                    "RSTI_SELECTION_EXIT


************Start Of ALV Coding retrofit**************************

*&---------------------------------------------------------------------*
*&      Form  output_alv_list
*&---------------------------------------------------------------------*
*         Generating Output using ALV List
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM output_alv_list .

*******List Counter
*  ADD 1 TO gv_list_counter.
  gv_list_counter = 1.
  "PERFORM t_sort_build   CHANGING gt_sort.
******* Build Field Catalog
  PERFORM fieldcat_list_build USING space CHANGING gt_fieldcat_list.
******* Layout Settings
  "  PERFORM layout_build         CHANGING gs_layout_list.
******* Build Events Table
  PERFORM eventtab_list_build  CHANGING gt_eventtab_list.

*-- Start Of Changes on 10 May 2004 : retrofit
******* Set print parameters
  PERFORM print_build.
*-- End Of Changes on 10 May 2004 : retrofit

******* Variants settings
*  PERFORM variant_init_alv     CHANGING gs_variant.

*  IF gs_variant-report IS INITIAL.
  gs_variant-variant    = par_var1.
  gs_variant-report     = g_repid.
  gs_variant-handle     = c_handle1.
*  ENDIF.

******* Display data in ALV Grid ------------------.


  DATA: lv_it_t077z TYPE TABLE OF t077z WITH HEADER LINE.

  REFRESH lv_it_t077z.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lv_it_t077z FROM t077z WHERE spras = 'E' AND ktopl IN sd_ktopl.


  LOOP AT gt_output_value.
    gt_output_value-ktoks = gt_output_value-saknr+3(2).
    CLEAR lv_it_t077z.
    READ TABLE lv_it_t077z WITH KEY ktoks = gt_output_value-ktoks.
    gt_output_value-txt30 = lv_it_t077z-txt30.
    MODIFY gt_output_value.
  ENDLOOP.












  DATA begda TYPE begda.
  DATA periv TYPE periv.
  DATA fdate TYPE char10.
  DATA period TYPE t009b-poper.
  DATA ls_fieldcat   TYPE slis_fieldcat_alv.
  DATA lineno TYPE i.

  SELECT SINGLE periv FROM t001 INTO periv WHERE bukrs = sd_bukrs-low.

  period  = b_monate-high.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr = sd_gjahr-low
*     I_MONMIT             = 00
      i_periv = periv
      i_poper = period
    IMPORTING
      e_date  = begda
* EXCEPTIONS
*     INPUT_FALSE          = 1
*     T009_NOTFOUND        = 2
*     T009B_NOTFOUND       = 3
*     OTHERS  = 4
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  CALL METHOD cl_abap_datfm=>conv_date_int_to_ext
    EXPORTING
      im_datint   = begda
      im_datfmdes = 'C'
    IMPORTING
      ex_datext   = fdate
*     ex_datfmused =
    .

  LOOP AT gt_output_value.
    gt_output_value-fdate = fdate.
    MODIFY gt_output_value.
  ENDLOOP.

  LOOP AT gt_fieldcat_list INTO ls_fieldcat WHERE fieldname = 'FDATE'.
    EXIT.
  ENDLOOP.
  IF sy-subrc IS NOT INITIAL.
    DESCRIBE TABLE gt_fieldcat_list LINES lineno.
    lineno = lineno + 1.
    ls_fieldcat-fieldname = 'FDATE'.
    ls_fieldcat-reptext_ddic = 'تاريخ'.
    ls_fieldcat-inttype = 'C'.
    ls_fieldcat-col_pos = lineno.
    APPEND ls_fieldcat TO gt_fieldcat_list.
  ENDIF.

  LOOP AT gt_fieldcat_list INTO ls_fieldcat WHERE fieldname = 'KTOKS'.
    EXIT.
  ENDLOOP.
  IF sy-subrc IS NOT INITIAL.
    DESCRIBE TABLE gt_fieldcat_list LINES lineno.
    lineno = lineno + 1.
    ls_fieldcat-fieldname = 'KTOKS'.
    ls_fieldcat-reptext_ddic = 'Account Group'.
    ls_fieldcat-inttype = 'C'.
    ls_fieldcat-col_pos = lineno.
    APPEND ls_fieldcat TO gt_fieldcat_list.
  ENDIF.

  LOOP AT gt_fieldcat_list INTO ls_fieldcat WHERE fieldname = 'TXT30'.
    EXIT.
  ENDLOOP.
  IF sy-subrc IS NOT INITIAL.
    DESCRIBE TABLE gt_fieldcat_list LINES lineno.
    lineno = lineno + 1.
    ls_fieldcat-fieldname = 'TXT30'.
    ls_fieldcat-reptext_ddic = 'Account Group'.
    ls_fieldcat-inttype = 'C'.
    ls_fieldcat-col_pos = lineno.
    APPEND ls_fieldcat TO gt_fieldcat_list.
  ENDIF.

  IF SY-UNAME = 'MO.MOHAMMADI' OR SY-UNAME = '10003411'.
    GC_SAVE = 'X'.
  ELSE.
    GC_SAVE = ''.
  ENDIF.


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      "i_callback_program = g_repid
      "i_callback_pf_status_set = 'SET_PF_STATUS'
      "i_structure_name   = gc_structure
      it_fieldcat   = gt_fieldcat_list
      i_save        = gc_save
      is_variant    = gs_variant
      it_events     = gt_eventtab_list
"     is_layout     = gs_layout_list
"     it_sort       = gt_sort
"     is_print      = gs_print
    TABLES
      t_outtab      = gt_output_value
    EXCEPTIONS
      program_error = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " output_alv_list
*&---------------------------------------------------------------------*
*&      Form  fieldcat_list_build
*&---------------------------------------------------------------------*
*       Building Field Catalog
*----------------------------------------------------------------------*
*      <--XT_FIELDCAT_LIST
*----------------------------------------------------------------------*
FORM fieldcat_list_build
  USING x_config TYPE c
  CHANGING xt_fieldcat TYPE slis_t_fieldcat_alv.

  CASE  gv_list_counter.

    WHEN  1.
****First List with details
      PERFORM fieldcat_merge USING gc_structure
                             CHANGING xt_fieldcat.
      PERFORM fieldcat_values_modify CHANGING xt_fieldcat.
    WHEN 2.
**** Second List with Totals
      PERFORM fieldcat_merge USING gc_structure
                             CHANGING xt_fieldcat.
      IF par_lis1 <> 'X' OR x_config IS NOT INITIAL.
**** some settings are made at first list
        PERFORM fieldcat_values_modify CHANGING xt_fieldcat.
      ENDIF.
      PERFORM fieldcat_totals_modify CHANGING xt_fieldcat.

  ENDCASE.

ENDFORM.                    " fieldcat_list_build
*&---------------------------------------------------------------------*
*&      Form  eventtab_list_build
*&---------------------------------------------------------------------*
*       Managing Events
*----------------------------------------------------------------------*
*      <--XT_EVENTTAB_list  text
*----------------------------------------------------------------------*
FORM eventtab_list_build  CHANGING xt_eventtab_list TYPE slis_t_event.

  DATA: ls_events TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = xt_eventtab_list
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*Pass the subroutine name to the TOP-OF-PAGE event when the top of page
*is triggered
  READ TABLE xt_eventtab_list INTO ls_events WITH
       KEY name = slis_ev_top_of_page.
  IF sy-subrc = 0.
    ls_events-form = gc_topofpage.
    MODIFY xt_eventtab_list FROM ls_events
                            TRANSPORTING form
                            WHERE name = slis_ev_top_of_page.
  ENDIF.

*Pass the subroutine name to the END-OF-LIST event when the end of list
*is triggered
  READ TABLE xt_eventtab_list INTO ls_events WITH
       KEY name = slis_ev_end_of_list.
  IF sy-subrc = 0.
    ls_events-form = gc_endoflist.
    MODIFY xt_eventtab_list FROM ls_events
                          TRANSPORTING form
                          WHERE name = slis_ev_end_of_list.
  ENDIF.

  IF NOT listsep IS INITIAL.
*Pass the subroutine name to the grouplevel_change event
    READ TABLE xt_eventtab_list INTO ls_events WITH
         KEY name = slis_ev_grouplevel_change.
    IF sy-subrc = 0.
      ls_events-form = gc_grouplevch.
      MODIFY xt_eventtab_list FROM ls_events TRANSPORTING form
             WHERE name = slis_ev_grouplevel_change.
    ENDIF.
  ENDIF.
ENDFORM.                    " eventtab_list_build

*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE
*
*---------------------------------------------------------------------*
*       Building TOP-OF-PAGE Event
*
*---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC *

  DATA:
    lo_grid         TYPE REF TO cl_salv_form_layout_grid,
    lo_row          TYPE REF TO cl_salv_form_layout_flow,
    lo_text         TYPE REF TO cl_salv_form_text,
    ls_output_value TYPE fagl_s_rfssld00_list.

  DATA: lv_text LIKE gv_text.
  DATA : lv_width TYPE  int4.
  DATA: l_cc TYPE i.                                        "n1507391

*-- Start Of Changes on 14 May 2004 : retrofit
  IF gv_list_counter = 1.
    ls_output_value = gt_output_value. "get current row
    IF ls_output_value-bukrs IS INITIAL.
      "not a full header (from subtotal line) - get last row
      IF gs_stored_header IS NOT INITIAL.
        ls_output_value = gs_stored_header.
      ELSE.
        " get first row
        READ TABLE gt_output_value INDEX 1 INTO ls_output_value.
      ENDIF.
    ENDIF.
    gs_stored_header = ls_output_value.

    IF konzvers IS INITIAL.
      bhdgd-bukrs = ls_output_value-bukrs.
      mif1-bukrs  = ls_output_value-bukrs.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_output_value-saknr
        IMPORTING
          output = mif1-saknr.

      mif1-gsber  = ls_output_value-gsber.
      bhdgd-grpin = mif1.
    ELSE.
      bhdgd-bukrs = ls_output_value-bukrs.
      mif2-bukrs  = ls_output_value-bukrs.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_output_value-saknr
        IMPORTING
          output = mif2-saknr.

      mif2-gsber  = ls_output_value-gsber.
      mif2-waers  = ls_output_value-waers.
      bhdgd-grpin = mif2.
    ENDIF.
  ELSE.
    l_cc = lines( gt_output_totals ).                       "n1507391
    IF l_cc = 1.                                            "n1507391
      bhdgd-bukrs = gt_output_totals-bukrs.
    ELSE.
      bhdgd-bukrs = ' '.
    ENDIF.
    CLEAR mif1.
    CLEAR mif2.
    CLEAR bhdgd-grpin.
  ENDIF.

  " Getting the list info for line size ..
*  CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
*    IMPORTING
*      e_width       = lv_width
*    EXCEPTIONS
*      no_infos      = 1
*      program_error = 2
*      OTHERS        = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  MOVE : lv_width TO bhdgd-lines.
  CALL FUNCTION 'FAGL_BATCH_HEADING_PERFORM'
    EXPORTING
      is_bhdgd     = bhdgd
    IMPORTING
      eo_form_grid = lo_grid.

*  PERFORM BATCH_COMMENT_BUILD  CHANGING gt_top_of_page.
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      IT_LIST_COMMENTARY = GT_TOP_OF_PAGE.
*
*  REFRESH gt_top_of_page.

  PERFORM batch_comment_build  CHANGING lv_text.
  lo_row = lo_grid->add_row( ).
  lo_text = lo_row->create_text( text = lv_text ).

  IF gv_list_counter = 2.
    lo_row = lo_grid->add_row( ).
    lo_text = lo_row->create_text( text = TEXT-tsm ).
  ENDIF.

  cl_salv_form_content=>set( lo_grid ).

ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
* FORM END_OF_LIST
*
*---------------------------------------------------------------------*
* Building END-OF-LIST Event for displaying totals for all comapany
* codes
*
*---------------------------------------------------------------------*
FORM end_of_list.                                           "#EC *


  DATA:  lv_log_head TYPE  slis_listheader-info.
  CASE gv_list_counter.
    WHEN 1.
      IF par_lis2 = 'X'.
        PERFORM output_totals_alv_list.
      ELSE.
        CALL FUNCTION 'FI_MESSAGE_CHECK'
          EXCEPTIONS
            no_message = 01.

*------ If MESSAGES Exists---------------------------
        IF sy-subrc = 0.
          phsperr_flag = '1'.

          gs_layout_list-list_append = gc_true.
          gs_layout_list-colwidth_optimize = 'X'.

          CASE msgtab-msort.
            WHEN '1'.
              MOVE : TEXT-f01 TO lv_log_head.
            WHEN '2'.
              MOVE : TEXT-f02 TO lv_log_head.
          ENDCASE.

          CALL FUNCTION 'FI_MESSAGES_ALV'
            EXPORTING
              i_headline = lv_log_head
              is_layout  = gs_layout_list.
        ENDIF.
        gv_list_counter = gv_save_counter.
      ENDIF.
    WHEN 2.
*------ Check MESSAGES ?------------------------
      CALL FUNCTION 'FI_MESSAGE_CHECK'
        EXCEPTIONS
          no_message = 01.

*------ If MESSAGES Exists---------------------------
      IF sy-subrc = 0.
        phsperr_flag = '1'.

        gs_layout_list-list_append = gc_true.
        gs_layout_list-colwidth_optimize = 'X'.

        CASE msgtab-msort.
          WHEN '1'.
            MOVE : TEXT-f01 TO lv_log_head.
          WHEN '2'.
            MOVE : TEXT-f02 TO lv_log_head.
        ENDCASE.

        CALL FUNCTION 'FI_MESSAGES_ALV'
          EXPORTING
            i_headline = lv_log_head
            is_layout  = gs_layout_list.
      ENDIF.
      gv_list_counter = gv_save_counter.
  ENDCASE.
ENDFORM.                    "End of list

*&---------------------------------------------------------------------*
*&      Form  SET_PF_STATUS
*&---------------------------------------------------------------------*
*       To set the gui status for ALV Grid
*----------------------------------------------------------------------*
FORM set_pf_status USING lv_extab TYPE slis_t_extab.        "#EC *
  DATA: lv_linsz TYPE  sy-linsz.
  DATA: lt_sort  TYPE slis_t_sortinfo_alv.

  SET PF-STATUS 'APPEND'.

  IF NOT listsep IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
      IMPORTING
        e_width = lv_linsz
        et_sort = lt_sort.

    MOVE lv_linsz TO sy-linsz.

    IF konzvers IS INITIAL.
      READ TABLE gt_output_value INDEX 1.
      IF gd_bukrs NE gt_output_value-bukrs.
        MOVE gt_output_value-bukrs TO bhdgd-bukrs.
        gd_bukrs = gt_output_value-bukrs.
      ENDIF.
    ELSE.
      bhdgd-bukrs = space.
    ENDIF.
    MOVE bhdgd-bukrs  TO bhdgd-werte.
    MOVE 'BUKRS'  TO bhdgd-domai.
    MOVE sy-repid TO bhdgd-repid.
    MOVE sy-uname TO bhdgd-uname.

    MOVE: listsep  TO bhdgd-separ.
    PERFORM new-section(rsbtchh0).
  ENDIF.

ENDFORM.                    "SET_PF_STATUS

*&--------------------------------------------------------------*
*&      Form  BATCH_COMMENT_BUILD
*&--------------------------------------------------------------*
*       BATCH Header Display on Top Of Page which is common
*---------------------------------------------------------------*
*      -->XT_TOP_OF_PAGE  list header
*---------------------------------------------------------------*
FORM batch_comment_build CHANGING
                         xv_text LIKE gv_text.

*  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: lv_prev_period(20),
        lv_prev_month(20),
        lv_rep_period(20),
        lv_rep_month(20).

  CONCATENATE ivu '-' ivo INTO lv_prev_month.
  CONCATENATE lv_prev_month sd_gjahr-low INTO lv_prev_period
              SEPARATED BY space.
*  CLEAR LS_LINE.
*  LS_LINE-TYP  = gc_selection.
*  LS_LINE-KEY  = TEXT-TVP.
*  LS_LINE-INFO = LV_PREV_PERIOD.

*  APPEND LS_LINE TO XT_TOP_OF_PAGE.

  CONCATENATE b_monate-low '-' b_monate-high INTO lv_rep_month.
  CONCATENATE lv_rep_month sd_gjahr-low
  INTO lv_rep_period SEPARATED BY space.

*  CLEAR LS_LINE.
*  LS_LINE-TYP  = gc_selection.
*  LS_LINE-KEY  = TEXT-TBP .
*  LS_LINE-INFO = LV_REP_PERIOD.
*  APPEND LS_LINE TO XT_TOP_OF_PAGE.

  CONCATENATE TEXT-tvp lv_prev_period TEXT-tbp lv_rep_period INTO
              xv_text SEPARATED BY space.

*  IF gv_list_counter = 2.
*    CLEAR LS_LINE.
*-- Start Of Changes on 10 June 2004 : retrofit
*    LS_LINE-TYP  = 'H'.
*    LS_LINE-TYP  = 'A'.
*-- Start Of Changes on 10 June 2004 : retrofit
*    LS_LINE-INFO = TEXT-TSM.
*    APPEND LS_LINE TO XT_TOP_OF_PAGE.
*  ENDIF.

ENDFORM.                    " BATCH_COMMENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  fieldcat_merge
*&---------------------------------------------------------------------*
*       Using the specific structure, fieldcatolog will be built
*----------------------------------------------------------------------*
*      -->IC_STRUCTURE  Output Structure
*      <--XT_FIELDCAT  Fieldcat
*----------------------------------------------------------------------*
FORM fieldcat_merge  USING
                        ic_structure
                     CHANGING
                        xt_fieldcat TYPE slis_t_fieldcat_alv.

**** Get Field Catalog for DDIC Structure.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = g_repid
      i_structure_name       = ic_structure
    CHANGING
      ct_fieldcat            = xt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " fieldcat_merge
*&---------------------------------------------------------------------*
*&      Form  layout_build
*&---------------------------------------------------------------------*
*       Build Layout - apeend list is set here
*----------------------------------------------------------------------*
*      <--XS_LAYOUT_GRID  Layout Name
*----------------------------------------------------------------------*
FORM layout_build  CHANGING xs_layout_list TYPE slis_layout_alv.

  CASE gv_list_counter.
    WHEN 1.
      IF par_var1 = 'X'.
        xs_layout_list-list_append        =  'Y'.
      ENDIF.
*     xs_layout_list-colwidth_optimize  =  'X'.
      xs_layout_list-no_totalline  = gc_true.
    WHEN OTHERS.
      IF par_lis2 = 'X' AND par_lis1 <> 'X'.             "note 912584
        xs_layout_list-list_append         =  'Y'.
      ELSE.
        xs_layout_list-list_append         =  'X'.
      ENDIF.
*     xs_layout_list-colwidth_optimize   =  'X'.
*      xs_layout_list-min_linesize        =  200.           "n1076039
                                                            "1162914
      xs_layout_list-no_totalline  = gc_true.
  ENDCASE.

  xs_layout_list-min_linesize        =  200.                "1162914
*-- Start Of Changes on 10 June 2004 : retrofit
* begin "n1498883
  CALL FUNCTION 'GET_ACCESSIBILITY_MODE'
    IMPORTING
      accessibility     = gd_acc_mode
    EXCEPTIONS
      its_not_available = 1.
  IF gd_acc_mode IS INITIAL.
    xs_layout_list-no_vline = gc_true.
  ENDIF.
* end "n1498883
  xs_layout_list-no_hotspot = gc_true.
*-- End Of Changes on 10 June 2004 : retrofit

  xs_layout_list-get_selinfos = 'X'.
                                                            "1133830


ENDFORM.                    " layout_build
*&---------------------------------------------------------------------*
*&      Form  output_totals_alv_list
*&---------------------------------------------------------------------*
*       Displaying list with all the totals for each Comapany codes
*----------------------------------------------------------------------*
FORM output_totals_alv_list .

*******List Counter
*  ADD 1 TO gv_list_counter.
  gv_list_counter = 2.
  REFRESH gt_sort.
  PERFORM t_sort_build   CHANGING gt_sort.
******* Build Field Catalog
  PERFORM fieldcat_list_build USING space CHANGING gt_fieldcat_list.
******* Layout Settings
  PERFORM layout_build         CHANGING gs_layout_list.
******* Build Events Table
  PERFORM eventtab_list_build  CHANGING gt_eventtab_list.
******* Variants settings
*  PERFORM variant_init_alv     CHANGING gs_variant.

*-- Start Of Changes on 10 May 2004 : retrofit
******* Set print parameters
  PERFORM print_build.
*-- End Of Changes on 10 May 2004 : retrofit

  MOVE space    TO bhdgd-bukrs.
  MOVE bhdgd-bukrs  TO bhdgd-werte.
  MOVE 'BUKRS'  TO bhdgd-domai.
  MOVE sy-repid TO bhdgd-repid.
  MOVE sy-uname TO bhdgd-uname.

  IF NOT listsep IS INITIAL
  AND konzvers IS INITIAL.
    MOVE: listsep  TO bhdgd-separ.
    PERFORM new-section(rsbtchh0).
  ENDIF.

* begin of note 1362931
* update accumulated balances
  LOOP AT gt_output_totals.
    IF gt_output_totals-acytd_bal < 0.
      gt_output_totals-salsl = 0.
      gt_output_totals-salhb = gt_output_totals-acytd_bal.
    ELSE.
      gt_output_totals-salhb = 0.
      gt_output_totals-salsl = gt_output_totals-acytd_bal.
    ENDIF.
    IF gt_output_totals-acytd_bal_fw < 0.
      gt_output_totals-salsl_fw = 0.
      gt_output_totals-salhb_fw = gt_output_totals-acytd_bal_fw.
    ELSE.
      gt_output_totals-salhb_fw = 0.
      gt_output_totals-salsl_fw = gt_output_totals-acytd_bal_fw.
    ENDIF.
    MODIFY gt_output_totals.
  ENDLOOP.
* end of note 1362931


******* Display data in ALV Grid.
*  IF gs_variant-report IS INITIAL.
  gs_variant-variant    = par_var2.
  gs_variant-report     = g_repid.
  gs_variant-handle     = c_handle2.
*  ENDIF.
  IF SY-UNAME = 'MO.MOHAMMADI' OR SY-UNAME = '10003411'.
    GC_SAVE = 'X'.
  ELSE.
    GC_SAVE = ''.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = g_repid
      i_structure_name   = gc_structure
      it_fieldcat        = gt_fieldcat_list
      i_save             = gc_save
      is_variant         = gs_variant
      it_events          = gt_eventtab_list
      is_layout          = gs_layout_list
      it_sort            = gt_sort
      is_print           = gs_print
    TABLES
      t_outtab           = gt_output_totals
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " output_totals_alv_list
*&---------------------------------------------------------------------*
*&      Form  fieldcat_values_modify
*&---------------------------------------------------------------------*
*       Modifying the fieldcat
*----------------------------------------------------------------------*
*      <--XT_FIELDCAT  fieldcat
*----------------------------------------------------------------------*
FORM fieldcat_values_modify  CHANGING
                             xt_fieldcat TYPE slis_t_fieldcat_alv.


  DATA : ls_fieldcat   TYPE slis_fieldcat_alv,
         lv_tabix      LIKE sy-tabix,
         lc_ddictxt(1) TYPE c VALUE 'L'.




*-- Start Of Changes on 21 June 2004 : retrofit
  IF fwaehr = 'X'.

    LOOP AT xt_fieldcat INTO ls_fieldcat.
      lv_tabix = sy-tabix.
      CLEAR ls_fieldcat-tech.                               "n1353591
      CASE ls_fieldcat-fieldname.

        WHEN 'BUKRS'.
*        LS_FIELDCAT-FIX_COLUMN  = gc_true.   " Comp Co Fixed
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 1.
*        else.
          IF konzvers = 'X'.
            ls_fieldcat-col_pos = 3.
          ENDIF.

        WHEN 'SAKNR'.
*        LS_FIELDCAT-FIX_COLUMN  = gc_true.   " Acc No Fixed

*-- Start Of Changes on 11 June 2004 : retrofit
*        LS_FIELDCAT-no_convext = GC_TRUE.
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 2.
*        else.
          IF konzvers = 'X'.
            ls_fieldcat-col_pos = 1.
          ENDIF.

        WHEN 'GSBER'.
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 6.
*        endif.

        WHEN 'SKBEZ'.
          IF konzvers = 'X'.
            ls_fieldcat-col_pos = 2.
          ENDIF.

        WHEN 'WAERS'.
*        IF konzvers = 'X'.
*          LS_FIELDCAT-col_pos = 3.
*        ELSE.
*          LS_FIELDCAT-col_pos = 4.
*        endif.

        WHEN 'FWAER'.
*        IF konzvers = 'X'.
*          LS_FIELDCAT-col_pos = 4.
*        ELSE.
*          LS_FIELDCAT-col_pos = 5.
*        endif.
          ls_fieldcat-row_pos = '2'.
          ls_fieldcat-offset  = '37'.
          ls_fieldcat-outputlen = 10.                       "note968162

*-- End Of Changes on 11 June 2004 : retrofit
        WHEN 'HSLVT'.          "Balance C/F

          ls_fieldcat-do_sum = gc_true.

        WHEN 'SALVM'.          "Previous Month
          ls_fieldcat-do_sum = gc_true.

        WHEN 'SOLBM'.          "Debit Amount
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-DDICTXT = lc_ddictxt.


        WHEN 'HABBM'.          "Credit Amount
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-DDICTXT = lc_ddictxt.

        WHEN 'SALSL'.          "Total Debit Balance
          ls_fieldcat-do_sum = gc_true.
          ls_fieldcat-no_sign = gc_true.                    "n1353591
          ls_fieldcat-no_out  = gc_true.

        WHEN 'SALHB'.          "Total Credit Balance
          ls_fieldcat-do_sum = gc_true.
          ls_fieldcat-no_sign = gc_true.                    "n1353591
          ls_fieldcat-no_out  = gc_true.

*-- Start Of Changes on 20 June 2004 : retrofit
        WHEN 'ACYTD_BAL'.
          ls_fieldcat-do_sum = gc_true.

        WHEN 'HSLVT_FW'.          "Balance C/F
          ls_fieldcat-row_pos = '2'.
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-offset = '2'.                          "note968162

        WHEN 'SALVM_FW'.          "Previous Month
          ls_fieldcat-row_pos = '2'.
          ls_fieldcat-do_sum = gc_true.

        WHEN 'SOLBM_FW'.          "Debit Amount
          ls_fieldcat-row_pos = '2'.
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-DDICTXT = lc_ddictxt.


        WHEN 'HABBM_FW'.          "Credit Amount
          ls_fieldcat-row_pos = '2'.
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-DDICTXT = lc_ddictxt.

        WHEN 'SALSL_FW'.          "Total Debit Balance
          ls_fieldcat-do_sum = gc_true.
          ls_fieldcat-no_sign = gc_true.                    "n1353591
          ls_fieldcat-no_out  = gc_true.

        WHEN 'SALHB_FW'.          "Total Credit Balance
          ls_fieldcat-do_sum = gc_true.
          ls_fieldcat-no_sign = gc_true.                    "n1353591
          ls_fieldcat-no_out  = gc_true.

        WHEN 'ACYTD_BAL_FW'.
          ls_fieldcat-row_pos = '2'.
          ls_fieldcat-do_sum = gc_true.

        WHEN 'SAKAN'.
          ls_fieldcat-no_out = gc_true.

        WHEN 'SKBZL'.
          ls_fieldcat-no_out  = gc_true.

*-- End Of Changes on 20 June 2004 : retrofit

        WHEN 'SUBLEVEL1'.
          ls_fieldcat-no_out = gc_true.
        WHEN 'SUBLEVEL2'.
          ls_fieldcat-no_out = gc_true.
        WHEN 'SUBLEVEL3'.
          ls_fieldcat-no_out = gc_true.

      ENDCASE.
      MODIFY xt_fieldcat FROM ls_fieldcat INDEX lv_tabix.

    ENDLOOP.

  ELSE.

    LOOP AT xt_fieldcat INTO ls_fieldcat.
      lv_tabix = sy-tabix.
      CASE ls_fieldcat-fieldname.

        WHEN 'BUKRS'.
*        LS_FIELDCAT-FIX_COLUMN  = gc_true.   " Comp Co Fixed
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 1.
*        else.
          IF konzvers = 'X'.
            ls_fieldcat-col_pos = 3.
          ENDIF.

        WHEN 'SAKNR'.
*        LS_FIELDCAT-FIX_COLUMN  = gc_true.   " Acc No Fixed
*-- Start Of Changes on 11 June 2004 : retrofit
*        LS_FIELDCAT-no_convext = GC_TRUE.
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 2.
*        else.
          IF konzvers = 'X'.
            ls_fieldcat-col_pos = 1.
          ENDIF.

        WHEN 'SKBEZ'.
          IF konzvers = 'X'.
            ls_fieldcat-col_pos = 2.
          ENDIF.

        WHEN 'WAERS'.
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 2.
*        endif.

        WHEN 'GSBER'.
*        IF konzvers = ''.
*          LS_FIELDCAT-col_pos = 5.
*        endif.

*-- End Of Changes on 11 June 2004 : retrofit
        WHEN 'HSLVT'.          "Balance C/F

          ls_fieldcat-do_sum = gc_true.

        WHEN 'SALVM'.          "Previous Month
          ls_fieldcat-do_sum = gc_true.

        WHEN 'SOLBM'.          "Debit Amount
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-DDICTXT = lc_ddictxt.


        WHEN 'HABBM'.          "Credit Amount
          ls_fieldcat-do_sum = gc_true.
*        LS_FIELDCAT-DDICTXT = lc_ddictxt.

        WHEN 'SALSL'.          "Total Debit Balance
          ls_fieldcat-do_sum = gc_true.
          ls_fieldcat-no_sign = gc_true.                    "n1353591
          ls_fieldcat-no_out  = gc_true.

        WHEN 'SALHB'.          "Total Credit Balance
          ls_fieldcat-do_sum = gc_true.
          ls_fieldcat-no_sign = gc_true.                    "n1353591
          ls_fieldcat-no_out  = gc_true.

*-- Start Of Changes on 21 June 2004 : retrofit
        WHEN 'ACYTD_BAL'.
          ls_fieldcat-do_sum = gc_true.

        WHEN 'FWAER'.
          ls_fieldcat-tech = gc_true.

        WHEN 'HSLVT_FW'.          "Balance C/F

          ls_fieldcat-tech = gc_true.

        WHEN 'SALVM_FW'.          "Previous Month
          ls_fieldcat-tech = gc_true.

        WHEN 'SOLBM_FW'.          "Debit Amount
          ls_fieldcat-tech = gc_true.

        WHEN 'HABBM_FW'.          "Credit Amount
          ls_fieldcat-tech = gc_true.

        WHEN 'SALSL_FW'.          "Total Debit Balance
          ls_fieldcat-tech = gc_true.

        WHEN 'SALHB_FW'.          "Total Credit Balance
          ls_fieldcat-tech = gc_true.

        WHEN 'ACYTD_BAL_FW'.
          ls_fieldcat-tech = gc_true.

        WHEN 'SAKAN'.
          ls_fieldcat-no_out = gc_true.

        WHEN 'SKBZL'.
          ls_fieldcat-no_out  = gc_true.

*-- End Of Changes on 21 June 2004 : retrofit

        WHEN 'SUBLEVEL1'.
          ls_fieldcat-no_out = gc_true.
        WHEN 'SUBLEVEL2'.
          ls_fieldcat-no_out = gc_true.
        WHEN 'SUBLEVEL3'.
          ls_fieldcat-no_out = gc_true.

      ENDCASE.
      MODIFY xt_fieldcat FROM ls_fieldcat INDEX lv_tabix.

    ENDLOOP.


  ENDIF.
*-- End Of Changes on 21 June 2004 : retrofit

ENDFORM.                    " fieldcat_values_modify
*&---------------------------------------------------------------------*
*&      Form  T_SORT_BUILD
*&---------------------------------------------------------------------*
*       Setting for Subtotal
*----------------------------------------------------------------------*
*      <--XT_SORT  text
*----------------------------------------------------------------------*
FORM t_sort_build  CHANGING xt_sort TYPE slis_t_sortinfo_alv.

  DATA: ls_sort TYPE slis_sortinfo_alv.

  IF gv_list_counter = 1.

*-- Start Of Changes on 22 June 2004 : retrofit
    IF konzvers = ''.
*----> Company Code
      CLEAR ls_sort.
      ls_sort-fieldname = 'BUKRS'.
      ls_sort-spos      = 1.
      ls_sort-up        = gc_true.
      ls_sort-group     = '*'.
      ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.

      CLEAR ls_sort.
      IF zsb_pos1 > '0' AND zsb_pos1 <= '9'.
        ls_sort-fieldname = 'SUBLEVEL1'.
        ls_sort-spos      = 2.
        ls_sort-up        = gc_true.
        ls_sort-subtot    = gc_true.
        APPEND ls_sort TO xt_sort.
      ENDIF.
      CLEAR ls_sort.
      IF zsb_pos2 > '0' AND zsb_pos2 <= '9'.
        ls_sort-fieldname = 'SUBLEVEL2'.
        ls_sort-spos      = 3.
        ls_sort-up        = gc_true.
        ls_sort-subtot    = gc_true.
        APPEND ls_sort TO xt_sort.
      ENDIF.
      CLEAR ls_sort.
      IF zsb_pos3 > '0' AND zsb_pos3 <= '9'.
        ls_sort-fieldname = 'SUBLEVEL3'.
        ls_sort-spos      = 4.
        ls_sort-up        = gc_true.
        ls_sort-subtot    = gc_true.
        APPEND ls_sort TO xt_sort.
      ENDIF.

      CLEAR ls_sort.
      ls_sort-fieldname = 'SAKNR'.
      ls_sort-spos      = 5.
      ls_sort-up        = gc_true.
* ls_sort-subtot    = gc_true.
      ls_sort-group     = 'UL'.
      APPEND ls_sort TO xt_sort.

      CLEAR ls_sort.
      ls_sort-fieldname = 'WAERS'.
      ls_sort-spos      = 6.
      ls_sort-up        = gc_true.
*  ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.

      CLEAR ls_sort.
      ls_sort-fieldname = 'GSBER'.
      ls_sort-spos      = 7.
      ls_sort-up        = gc_true.
*  ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.

    ELSE.
      CLEAR ls_sort.
      IF zsb_pos1 > '0' AND zsb_pos1 <= '9'.
        ls_sort-fieldname = 'SUBLEVEL1'.
        ls_sort-spos      = 1.
        ls_sort-up        = gc_true.
        ls_sort-subtot    = gc_true.
        APPEND ls_sort TO xt_sort.
      ENDIF.
      CLEAR ls_sort.
      IF zsb_pos2 > '0' AND zsb_pos2 <= '9'.
        ls_sort-fieldname = 'SUBLEVEL2'.
        ls_sort-spos      = 2.
        ls_sort-up        = gc_true.
        ls_sort-subtot    = gc_true.
        APPEND ls_sort TO xt_sort.
      ENDIF.
      CLEAR ls_sort.
      IF zsb_pos3 > '0' AND zsb_pos3 <= '9'.
        ls_sort-fieldname = 'SUBLEVEL3'.
        ls_sort-spos      = 3.
        ls_sort-up        = gc_true.
        ls_sort-subtot    = gc_true.
        APPEND ls_sort TO xt_sort.
      ENDIF.

      CLEAR ls_sort.
      ls_sort-fieldname = 'SAKNR'.
      ls_sort-spos      = 4.
      ls_sort-up        = gc_true.
      ls_sort-subtot    = gc_true.
      ls_sort-group     = 'UL'.
      APPEND ls_sort TO xt_sort.

      CLEAR ls_sort.
      ls_sort-fieldname = 'WAERS'.
      ls_sort-spos      = 5.
      ls_sort-up        = gc_true.
*  ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.

      CLEAR ls_sort.
      ls_sort-fieldname = 'BUKRS'.
      ls_sort-spos      = 6.
      ls_sort-up        = gc_true.
*  ls_sort-group     = '*'.
*  ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.

      CLEAR ls_sort.
      ls_sort-fieldname = 'GSBER'.
      ls_sort-spos      = 7.
      ls_sort-up        = gc_true.
*  ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.

    ENDIF.
*-- End Of Changes on 22 June 2004 : retrofit


  ELSEIF gv_list_counter = 2.
*  IF FWAEHR = 'X'.
    CLEAR ls_sort.
    ls_sort-fieldname = 'WAERS'.
    ls_sort-spos      = 1.
    ls_sort-up        = gc_true.
    ls_sort-subtot    = gc_true.
    APPEND ls_sort TO xt_sort.
    IF fwaehr = gc_true.
      CLEAR ls_sort.
      ls_sort-fieldname = 'FWAER'.
      ls_sort-spos      = 2.
      ls_sort-up        = gc_true.
      ls_sort-subtot    = gc_true.
      APPEND ls_sort TO xt_sort.
    ENDIF.
    CLEAR ls_sort.
    ls_sort-fieldname = 'GSBER'.
    IF fwaehr = gc_true.
      ls_sort-spos      = 3.
    ELSE.
      ls_sort-spos      = 2.
    ENDIF.
    ls_sort-up        = gc_true.
*  ls_sort-subtot    = gc_true.
    APPEND ls_sort TO xt_sort.
*  ENDIF.
  ENDIF.
ENDFORM.                    " T_SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  fieldcat_totals_modify
*&---------------------------------------------------------------------*
*       Modify the fieldcat for totals
*----------------------------------------------------------------------*
*      <--XT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fieldcat_totals_modify  CHANGING
                             xt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA : ls_fieldcat TYPE slis_fieldcat_alv,
         lv_tabix    LIKE sy-tabix.
  LOOP AT xt_fieldcat INTO ls_fieldcat.
    lv_tabix = sy-tabix.
    CASE ls_fieldcat-fieldname.
      WHEN 'SAKNR'.          "Account No
        ls_fieldcat-tech = gc_true.
      WHEN 'SKBEZ'.          "Account Name
        ls_fieldcat-tech = gc_true.
      WHEN 'SKBZL'.          "Account Name
        ls_fieldcat-tech = gc_true.
      WHEN 'FWAER'.          "Account Name
        IF fwaehr = 'X'.
          ls_fieldcat-offset = 5.
        ENDIF.
    ENDCASE.
    MODIFY xt_fieldcat FROM ls_fieldcat INDEX lv_tabix.

  ENDLOOP.
ENDFORM.                    " fieldcat_totals_modify
*&---------------------------------------------------------------------*
*&      Form  variant_f4_help
*&---------------------------------------------------------------------*
*       F4 help for variants for both the append lists
*----------------------------------------------------------------------*
*      -->IC_Handle
*----------------------------------------------------------------------*
FORM variant_f4_help  USING ic_handle TYPE slis_handl
                      CHANGING xv_variant TYPE disvariant-variant.

  DATA: ls_variant TYPE disvariant.
  DATA: l_variant_help TYPE disvariant.
  DATA: l_exit TYPE c.                 "User-Exit while F4-Help
*  DATA: l_handle TYPE slis_handl.

  ls_variant-handle = ic_handle.
  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = ls_variant
      i_save     = 'A'
    IMPORTING
      e_exit     = l_exit
      es_variant = l_variant_help.
  IF l_exit = space.                   "No User-Exit
    xv_variant = l_variant_help-variant.
  ENDIF.

ENDFORM.                    " variant_f4_help
*&---------------------------------------------------------------------*
*&      Form  check_variant_existance
*&---------------------------------------------------------------------*
*       This will check for the variant existance ..
*----------------------------------------------------------------------*
*      -->IC_HANDLE  Handle name..
*      -->IV_VARIANT  VARIANT which handles handle,
*                     variant and report name
*----------------------------------------------------------------------*
FORM check_variant_existance  USING ic_handle TYPE slis_handl
                                    iv_variant TYPE disvariant-variant.
  DATA: ls_variant TYPE disvariant.
  IF iv_variant <> space.
    ls_variant-handle  = ic_handle.
    ls_variant-variant = iv_variant.
    ls_variant-report  = g_repid.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = ls_variant.
  ENDIF.

ENDFORM.                    " check_variant_existance
*&---------------------------------------------------------------------*
*&      Form  CONVERSION_EXIT_ACCOUNTNO
*&---------------------------------------------------------------------*
*       Conversion exit for getting leading zero's
*----------------------------------------------------------------------*
*      -->P_T_SAKNR  text
*      -->P_GV_MASK1  text
*----------------------------------------------------------------------*
FORM conversion_exit_accountno  USING    iv_saknr TYPE ska1-saknr
                                CHANGING xv_mask  TYPE ska1-saknr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = iv_saknr
    IMPORTING
      output = xv_mask.

ENDFORM.                    " CONVERSION_EXIT_ACCOUNTNO


*-- Start Of Changes on 10 May 2004 : retrofit

*&---------------------------------------------------------------------*
*&      Form  print_build
*&---------------------------------------------------------------------*
*  Set print parameters
*----------------------------------------------------------------------*
FORM print_build .

  gs_print-no_print_listinfos     = gc_true.
  gs_print-reserve_lines          = 1.                      "n1034774
  gs_print-no_change_print_params = par_ppar.               "n1044838
*  xs_print-no_print_selinfos      = gc_true.

ENDFORM.                    " print_build

*&---------------------------------------------------------------------*
*&      Form  grouplevel_change
*&---------------------------------------------------------------------*
*       Listseparation at group level change
*----------------------------------------------------------------------*
FORM grouplevel_change  USING ls_lineinfo TYPE slis_lineinfo
                              ls_groups   TYPE kkblo_grouplevels."#EC

  IF NOT listsep IS INITIAL.
    IF konzvers IS INITIAL.
      IF ( gd_bukrs NE gt_output_value-bukrs ).
*  or ( GD_bukrs = GT_OUTAB-BUKRS and ...).     "because of error of ALV

        MOVE gt_output_value-bukrs TO bhdgd-bukrs.
        MOVE bhdgd-bukrs  TO bhdgd-werte.
        MOVE 'BUKRS'  TO bhdgd-domai.
        MOVE sy-repid TO bhdgd-repid.
        MOVE sy-uname TO bhdgd-uname.


        MOVE: listsep  TO bhdgd-separ.
        PERFORM new-section(rsbtchh0).
        gd_bukrs = gt_output_value-bukrs.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "grouplevel_change

*-- End Of Changes on 10 May 2004 : retrofit
*---------------------------------------------------------------------*
* FORM SALDEN_AUSGEBEN                                                *
*---------------------------------------------------------------------*
FORM salden_ausgeben.

  MOVE t-waers TO gs_output_value-waers.
  IF t-waers NE t-fwaer
  AND NOT t-fwaer IS INITIAL.
    MOVE t-fwaer TO gs_output_value-fwaer.
  ELSE.
    IF fwaehr IS INITIAL.
      CLEAR gs_output_value-fwaer.
    ELSE.
      MOVE t-waers TO gs_output_value-fwaer.
    ENDIF.
  ENDIF.
  MOVE t-gsber TO gs_output_value-gsber.

  MOVE : t-umsav TO gs_output_value-hslvt,
         t-salvm TO gs_output_value-salvm,
         t-solbm TO gs_output_value-solbm,
         t-habbm TO gs_output_value-habbm.

  MOVE t-salsl TO gs_output_value-salsl.
  MOVE t-salhb TO gs_output_value-salhb.
  gs_output_value-acytd_bal = gs_output_value-salsl
                                   + gs_output_value-salhb.
* begin of note 1362931
  IF p_gsau IS INITIAL.
* If Business area allocation is not active, accumulated balance
* has to be recalculated
    IF gs_output_value-acytd_bal < 0.
      gs_output_value-salsl = 0.
      gs_output_value-salhb = gs_output_value-acytd_bal.
    ELSE.
      gs_output_value-salhb = 0.
      gs_output_value-salsl = gs_output_value-acytd_bal.
    ENDIF.
  ENDIF.
* end of note 1362931

  MOVE : t-umsav_fw TO gs_output_value-hslvt_fw,
         t-salvm_fw TO gs_output_value-salvm_fw,
         t-solbm_fw TO gs_output_value-solbm_fw,
         t-habbm_fw TO gs_output_value-habbm_fw.

  MOVE t-salsl_fw TO gs_output_value-salsl_fw.
  MOVE t-salhb_fw TO gs_output_value-salhb_fw.

  IF gs_output_value-waers = gs_output_value-fwaer
  AND NOT fwaehr IS INITIAL.
    gs_output_value-hslvt_fw = gs_output_value-hslvt.
    gs_output_value-salvm_fw = gs_output_value-salvm.
    gs_output_value-solbm_fw = gs_output_value-solbm.
    gs_output_value-habbm_fw = gs_output_value-habbm.
    gs_output_value-salsl_fw = gs_output_value-salsl.
    gs_output_value-salhb_fw = gs_output_value-salhb.
  ENDIF.

  gs_output_value-acytd_bal_fw = gs_output_value-salsl_fw
                                       + gs_output_value-salhb_fw.
* begin of note 1362931
  IF p_gsau IS INITIAL.
* If Business area allocation is not active, accumulated balance
* has to be recalculated
    IF gs_output_value-acytd_bal_fw < 0.
      gs_output_value-salsl_fw = 0.
      gs_output_value-salhb_fw = gs_output_value-acytd_bal_fw.
    ELSE.
      gs_output_value-salhb_fw = 0.
      gs_output_value-salsl_fw = gs_output_value-acytd_bal_fw.
    ENDIF.
  ENDIF.
* end of note 1362931

  APPEND gs_output_value TO gt_output_value.

  CLEAR gs_output_value-saknr.
  CLEAR gs_output_value-sakan.
  CLEAR gs_output_value-skbez.
  CLEAR gs_output_value-skbzl.
  CLEAR gs_output_value-sublevel1.
  CLEAR gs_output_value-sublevel2.
  CLEAR gs_output_value-sublevel3.
  gt_output_totals = gs_output_value.
  COLLECT gt_output_totals.

  CLEAR wawaers.
ENDFORM.                    "SALDEN_AUSGEBEN_N

*---------------------------------------------------------------------*
*        FORM SCHEDMAN_START_STOP                                     *
*---------------------------------------------------------------------*
FORM schedman_start_stop USING p_command.
* local statics
  STATICS: ls_key_static LIKE schedman_key.
*local data declaration
  DATA: gs_key      LIKE schedman_key.
  DATA: gt_spono    LIKE schedman_spool.

  DATA: ld_worklist_flag(1).
  DATA: ls_detail   LIKE schedman_detail_user.
  DATA: lt_selkrit  LIKE schedman_selkrit OCCURS 0 WITH HEADER LINE.
  DATA: lt_param    LIKE schedman_selkrit OCCURS 0 WITH HEADER LINE.
  DATA: ls_witem    LIKE scma_witem.
  DATA: ls_event    LIKE scma_event.
  DATA: ls_ext      LIKE schedman_ext.
  DATA: ls_message LIKE schedman_message,
        ld_objects LIKE smmain-nr_of_objects,
        ld_aplstat LIKE smmain-aplstat.

* muss in scmatasks
  ls_detail-repid       = sy-repid.
  ls_detail-variante    = sy-slset.      "<<die variante
  ls_detail-application = 'FI-GL'.
* save some select-options
  CLEAR lt_selkrit.
  lt_selkrit-structure  = 'SKB1'.
  lt_selkrit-field      = 'BUKRS'.
  LOOP AT sd_bukrs.
    MOVE-CORRESPONDING sd_bukrs TO lt_selkrit.
    APPEND lt_selkrit.
  ENDLOOP.

  IF p_command = 'START'.
    ls_witem-wf_witem = wf_witem.
    ls_witem-wf_wlist = wf_wlist.
    CALL FUNCTION 'KPEP_MONI_INIT_RECORD'
      EXPORTING
        ls_detail  = ls_detail
        ls_witem   = ls_witem
      IMPORTING
        ls_key     = ls_key_static
      TABLES
        lt_selkrit = lt_selkrit.

  ELSEIF p_command = 'STOP'.
    ld_aplstat  = '0'.
    ls_event-wf_witem = wf_witem.
    ls_event-wf_okey  = wf_okey.
    IF sel_flag = '0'.
      ls_event-wf_event = 'ERROR'.
    ELSE.
      ls_event-wf_event = 'FINISHED'.
    ENDIF.
    CALL FUNCTION 'KPEP_MONI_CLOSE_RECORD'
      EXPORTING
        ls_key        = ls_key_static
        ls_scma_event = ls_event
      CHANGING
        ld_aplstat    = ld_aplstat
      EXCEPTIONS
        OTHERS        = 0.

  ENDIF.

  COMMIT WORK.           " <<<<<<<<<<  C O M M I T  W O R K  >>>>>>>

ENDFORM.                    "SCHEDMAN_START_STOP
