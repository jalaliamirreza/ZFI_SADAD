*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFI_TURNOVER....................................*
DATA:  BEGIN OF STATUS_ZFI_TURNOVER                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFI_TURNOVER                  .
CONTROLS: TCTRL_ZFI_TURNOVER
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFI_TURNOVER                  .
TABLES: ZFI_TURNOVER                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
