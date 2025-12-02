
PROCESS BEFORE OUTPUT.

  MODULE funktionstasten_201.
  MODULE set_size_201.
  LOOP AT tab_schecks WITH CONTROL check_selection CURSOR hlp_lnmax.
    FIELD payr-chect MODULE schecks_anzeigen.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  LOOP.
  ENDLOOP.

  MODULE auswahl_bearbeiten.
