*----------------------------------------------------------------------*
* PROGRAM SAPMFCHK                                                     *
* ak020699 Changed declaration to new type FEB_HLP_RFDT_ID_STRUCT.     *
*----------------------------------------------------------------------*

PROGRAM sapmfchk MESSAGE-ID fs.

TABLES:
  bkpf,
 *bnka,
  bnka,
  bsec,
  bseg,
  bvor,
  check_head,
  check_rec,
  check_trl,
  kna1,
  lfa1,
  opayf,
 *payr,
  payr,
  pcec,
  rf05r,
  rfdt,
  rfpdo2,
  tvoid,
  tvoit,
  skb1,
  t001,
  t005,
  t012,
  t012k,
 *t012t,
  t012t,
  t021v,
  t041c,
  t041ct,
  t042z,
  tfbuf,
  uf05a,
  usr01.

DATA BEGIN OF COMMON PART cp1.         "Tabelle POSTAB
  INCLUDE rfeposc1.                    "(Übergabetabelle FBL1)
DATA END OF COMMON PART.
DATA BEGIN OF COMMON PART cp3.         "Tabelle KONTAB
  INCLUDE rfeposc3.                    "(Übergabetabelle FBL1)
DATA END OF COMMON PART.
DATA BEGIN OF COMMON PART cp7.         "sonstige Felder
  INCLUDE rfeposc7.                    "(Übergabefelder FBL1)
DATA END OF COMMON PART.

INCLUDE rpc4f_0c.                      "Schnittstelle zum HR

DATA:
  pinfo           LIKE pc407,
  pform           LIKE pc408 OCCURS 70 WITH HEADER LINE.

RANGES:
  tab_check FOR payr-checf,
  tab_hbkid FOR payr-hbkid,
  tab_hktid FOR payr-hktid.

DATA:
  BEGIN OF tab_payr OCCURS 100.        "Tabelle für manuellen
    INCLUDE STRUCTURE payr.            "Scheckrücklauf
    DATA:
    rwbtr_input   LIKE payr-rwbtr,
    valut_input   LIKE bseg-valut,
    bancd_input   LIKE payr-bancd,
    input_amount  LIKE payr-rwbtr,
  END OF tab_payr,
  BEGIN OF tab_schecks OCCURS 2,       "mehrere Schecks zu einer
    zbukr         LIKE payr-zbukr,     "Zahlungsbelegnummer
    hbkid         LIKE payr-hbkid,
    hktid         LIKE payr-hktid,
    rzawe         LIKE payr-rzawe,
    chect         LIKE payr-chect,
  END OF tab_schecks,
  BEGIN OF tab_user OCCURS 10,         "Zuordnung von Benutzern zu
    uname         LIKE sy-uname,       "Nummern zur Verkürzung des Keys
    lfdnr(4)      TYPE n,              "(Speichern in RFDT)
  END OF tab_user,
  BEGIN OF hlp_new,                    "neues Intervall (Umnumerieren)
    checf         LIKE pcec-checf,
    stapf         LIKE pcec-stapl,
    chect         LIKE pcec-chect,
    stapt         LIKE pcec-stapl,
  END OF hlp_new,
  BEGIN OF hlp_old,                    "altes Intervall (Umnumerieren)
    checf         LIKE pcec-checf,
    stapf         LIKE pcec-stapl,
    chect         LIKE pcec-chect,
    stapt         LIKE pcec-stapl,
  END OF hlp_old,
  BEGIN OF hlp_payr.                   "Struktur PAYR vor Änderungen
    INCLUDE STRUCTURE payr.
  DATA: END OF hlp_payr,
  BEGIN OF hlp_payr_lock,              "Key des gesperrten Schecks
    mandt         LIKE payr-mandt,
    zbukr         LIKE payr-zbukr,
    hbkid         LIKE payr-hbkid,
    hktid         LIKE payr-hktid,
    rzawe         LIKE payr-rzawe,
    chect         LIKE payr-chect,
  END OF hlp_payr_lock,
  hlp_rfdt_id TYPE feb_hlp_rfdt_id_struct.
  DATA BEGIN OF bdc OCCURS 12.         "Struktur für den Aufruf von
    INCLUDE STRUCTURE bdcdata.         "Transaktionen
  DATA: END OF bdc,
  BEGIN OF excl OCCURS 11,             "nicht aktive Funktionen
    tcode LIKE sy-tcode,
  END OF excl,
  BEGIN OF check_file OCCURS 50,       "Daten des Rücklauffiles in RFDT:
    record(128) TYPE c,                "CHECK_HEAD, CHECK_REC, CHECK_TRL
  END OF check_file.                   "(Schnittstelle zu RFEBCK00)

*------- Einzelfelder --------------------------------------------------
DATA:
  hlp_answer(1)   TYPE c,              "Antwort aus dem Popup
  hlp_bancd       LIKE payr-bancd,     "Vorschlag bei Scheckrücklauf
  hlp_distance(7) TYPE p,              "Abstand zwischen 2 Schecknummern
  hlp_counter     TYPE p,              "Zähler
  hlp_field(21)   TYPE c,              "für GET CURSOR
  hlp_index       LIKE sy-index,       "zum Lesen in TAB_PAYR
  hlp_input(1)    TYPE c,              "zum Blättern
  hlp_line        LIKE sy-stepl,       "für GET CURSOR
  hlp_lncur       LIKE sy-loopc,       "aktuelle Zeile
  hlp_lnfir       LIKE sy-loopc,       "erste Zeile
  hlp_lnmax       LIKE sy-loopc,       "maximale Anzahl Zeilen
  hlp_loopc       LIKE sy-loopc,       "Anzahl Loopzeilen
  hlp_next(1)     TYPE n,              "1 - Sprung ins nächste Feld
  hlp_reason(3)   TYPE c,              "Grund, warum kein Vergleich
  hlp_result(2)   TYPE c,              "Vergleichsergebnis 2er Schecks
  hlp_save(1)     TYPE n,              "1 - gesichert
  hlp_stodt       LIKE bkpf-budat,     "Storno-Buchungsdatum bei FCH8
  hlp_stomo       LIKE bkpf-monat,     "Storno-Periode bei FCH8
  hlp_subrc       LIKE sy-subrc,       "gemerkter Returncode
  hlp_xzahl       TYPE c,              "Belegzeile markiert als
                                       "Zahlungsvorgang gefunden.
  hlp_tcode       LIKE tstc-tcode,     "für HR-Berechtigungsprüfung
  hlp_verbindg(1) TYPE n,              "1 - Stapelverbindung existiert
  hlp_valut       LIKE bseg-valut,     "Vorschlag bei Scheckrücklauf
  hlp_voidr       LIKE payr-voidr,     "Ungültigkeitsgrund
  hlp_xsofo(1)    TYPE c,              "X Storno kann ohne FBRA erfolgen
  hlp_zahl(6)     TYPE c,              "Zahl im Characterformat
  hlp_zeile(30)   TYPE c,              "Text-900: Zeile &1 / &2
  hlp_sperr       TYPE c,              "Ist sperre gesetz
  HLP_NO_ITEM     TYPE I,

  ok-code         LIKE sy-ucomm,       "gesicherter OK-Code

  xinit(1)        TYPE c.              "Kennzeichen: Initialisierung ok

CONTROLS: fchr_control    TYPE TABLEVIEW USING SCREEN 651,
          check_selection TYPE TABLEVIEW USING SCREEN 201.

TYPES: t_payee_info TYPE STANDARD TABLE OF payee_info,
       t_bsec       TYPE STANDARD TABLE OF bsec.
