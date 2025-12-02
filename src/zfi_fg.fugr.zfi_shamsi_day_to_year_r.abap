FUNCTION zfi_shamsi_day_to_year_r.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(INPUT_DATE) TYPE  CHAR8
*"  EXPORTING
*"     REFERENCE(SH_YEAR) TYPE  INT4
*"     REFERENCE(SH_MONTH) TYPE  INT4
*"     REFERENCE(SH_DAY) TYPE  INT4
*"     REFERENCE(SH_THIS_MONTH_MAX_DAY) TYPE  INT4
*"     REFERENCE(SH_NEXT_MONTH_MAX_DAY) TYPE  INT4
*"----------------------------------------------------------------------
  DATA: e_tage TYPE i.
  DATA: to_date TYPE datum .
  to_date = input_date.
  DATA: from_date TYPE datum VALUE '19020321'.
*  E_TAGE = INPUT_DATE - FROM_DATE.


  e_tage = to_date - from_date.
*  CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*  EXPORTING
*    I_DATUM_BIS                   = TO_DATE
*    I_DATUM_VON                   = FROM_DATE
**   I_KZ_EXCL_VON                 = '0'
**   I_KZ_INCL_BIS                 = '0'
**   I_KZ_ULT_BIS                  = ' '
**   I_KZ_ULT_VON                  = ' '
*   I_STGMETH                     = '2'
**   I_SZBMETH                     = '1'
*  IMPORTING
*    E_TAGE                        = E_TAGE
** EXCEPTIONS
**   DAYS_METHOD_NOT_DEFINED       = 1
**   OTHERS                        = 2
*    .
*  IF SY-SUBRC <> 0.
** Implement suitable error handling here
*  ENDIF.



  DATA: BEGIN OF it1 OCCURS 0,
          yyyy        TYPE c LENGTH 4,
          yearlength  TYPE i,
          from_day    TYPE i,
          to_day      TYPE i,
          leap_status TYPE c,
        END OF it1.
  DATA: wa1 LIKE LINE OF it1.
  wa1-yyyy = 1281 .    wa1-yearlength = 365.   wa1-from_day = 1 .    wa1-to_day = 365 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1282 .    wa1-yearlength = 365.   wa1-from_day = 366 .    wa1-to_day = 730 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1283 .    wa1-yearlength = 365.   wa1-from_day = 731 .    wa1-to_day = 1095 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1284 .    wa1-yearlength = 366.   wa1-from_day = 1096 .   wa1-to_day = 1461 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1285 .    wa1-yearlength = 365.   wa1-from_day = 1462 .   wa1-to_day = 1826 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1286 .    wa1-yearlength = 365.   wa1-from_day = 1827 .   wa1-to_day = 2191 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1287 .    wa1-yearlength = 365.   wa1-from_day = 2192 .   wa1-to_day = 2556 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1288 .    wa1-yearlength = 366.   wa1-from_day = 2557 .   wa1-to_day = 2922 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1289 .    wa1-yearlength = 365.   wa1-from_day = 2923 .   wa1-to_day = 3287 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1290 .    wa1-yearlength = 365.   wa1-from_day = 3288 .   wa1-to_day = 3652 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1291 .    wa1-yearlength = 365.   wa1-from_day = 3653 .   wa1-to_day = 4017 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1292 .    wa1-yearlength = 366.   wa1-from_day = 4018 .   wa1-to_day = 4383 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1293 .    wa1-yearlength = 365.   wa1-from_day = 4384 .   wa1-to_day = 4748 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1294 .    wa1-yearlength = 365.   wa1-from_day = 4749 .   wa1-to_day = 5113 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1295 .    wa1-yearlength = 365.   wa1-from_day = 5114 .   wa1-to_day = 5478 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1296 .    wa1-yearlength = 366.   wa1-from_day = 5479 .   wa1-to_day = 5844 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1297 .    wa1-yearlength = 365.   wa1-from_day = 5845 .   wa1-to_day = 6209 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1298 .    wa1-yearlength = 365.   wa1-from_day = 6210 .   wa1-to_day = 6574 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1299 .    wa1-yearlength = 365.   wa1-from_day = 6575 .   wa1-to_day = 6939 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1300 .    wa1-yearlength = 366.   wa1-from_day = 6940 .   wa1-to_day = 7305 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1301 .    wa1-yearlength = 365.   wa1-from_day = 7306 .   wa1-to_day = 7670 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1302 .    wa1-yearlength = 365.   wa1-from_day = 7671 .   wa1-to_day = 8035 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1303 .    wa1-yearlength = 365.   wa1-from_day = 8036 .   wa1-to_day = 8400 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1304 .    wa1-yearlength = 366.   wa1-from_day = 8401 .   wa1-to_day = 8766 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1305 .    wa1-yearlength = 365.   wa1-from_day = 8767 .   wa1-to_day = 9131 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1306 .    wa1-yearlength = 365.   wa1-from_day = 9132 .   wa1-to_day = 9496 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1307 .    wa1-yearlength = 365.   wa1-from_day = 9497 .   wa1-to_day = 9861 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1308 .    wa1-yearlength = 365.   wa1-from_day = 9862 .   wa1-to_day = 10226 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1309 .    wa1-yearlength = 366.   wa1-from_day = 10227 .    wa1-to_day = 10592 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1310 .    wa1-yearlength = 365.   wa1-from_day = 10593 .    wa1-to_day = 10957 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1311 .    wa1-yearlength = 365.   wa1-from_day = 10958 .    wa1-to_day = 11322 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1312 .    wa1-yearlength = 365.   wa1-from_day = 11323 .    wa1-to_day = 11687 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1313 .    wa1-yearlength = 366.   wa1-from_day = 11688 .    wa1-to_day = 12053 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1314 .    wa1-yearlength = 365.   wa1-from_day = 12054 .    wa1-to_day = 12418 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1315 .    wa1-yearlength = 365.   wa1-from_day = 12419 .    wa1-to_day = 12783 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1316 .    wa1-yearlength = 365.   wa1-from_day = 12784 .    wa1-to_day = 13148 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1317 .    wa1-yearlength = 366.   wa1-from_day = 13149 .    wa1-to_day = 13514 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1318 .    wa1-yearlength = 365.   wa1-from_day = 13515 .    wa1-to_day = 13879 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1319 .    wa1-yearlength = 365.   wa1-from_day = 13880 .    wa1-to_day = 14244 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1320 .    wa1-yearlength = 365.   wa1-from_day = 14245 .    wa1-to_day = 14609 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1321 .    wa1-yearlength = 366.   wa1-from_day = 14610 .    wa1-to_day = 14975 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1322 .    wa1-yearlength = 365.   wa1-from_day = 14976 .    wa1-to_day = 15340 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1323 .    wa1-yearlength = 365.   wa1-from_day = 15341 .    wa1-to_day = 15705 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1324 .    wa1-yearlength = 365.   wa1-from_day = 15706 .    wa1-to_day = 16070 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1325 .    wa1-yearlength = 366.   wa1-from_day = 16071 .    wa1-to_day = 16436 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1326 .    wa1-yearlength = 365.   wa1-from_day = 16437 .    wa1-to_day = 16801 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1327 .    wa1-yearlength = 365.   wa1-from_day = 16802 .    wa1-to_day = 17166 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1328 .    wa1-yearlength = 365.   wa1-from_day = 17167 .    wa1-to_day = 17531 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1329 .    wa1-yearlength = 366.   wa1-from_day = 17532 .    wa1-to_day = 17897 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1330 .    wa1-yearlength = 365.   wa1-from_day = 17898 .    wa1-to_day = 18262 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1331 .    wa1-yearlength = 365.   wa1-from_day = 18263 .    wa1-to_day = 18627 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1332 .    wa1-yearlength = 365.   wa1-from_day = 18628 .    wa1-to_day = 18992 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1333 .    wa1-yearlength = 366.   wa1-from_day = 18993 .    wa1-to_day = 19358 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1334 .    wa1-yearlength = 365.   wa1-from_day = 19359 .    wa1-to_day = 19723 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1335 .    wa1-yearlength = 365.   wa1-from_day = 19724 .    wa1-to_day = 20088 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1336 .    wa1-yearlength = 365.   wa1-from_day = 20089 .    wa1-to_day = 20453 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1337 .    wa1-yearlength = 366.   wa1-from_day = 20454 .    wa1-to_day = 20819 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1338 .    wa1-yearlength = 365.   wa1-from_day = 20820 .    wa1-to_day = 21184 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1339 .    wa1-yearlength = 365.   wa1-from_day = 21185 .    wa1-to_day = 21549 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1340 .    wa1-yearlength = 365.   wa1-from_day = 21550 .    wa1-to_day = 21914 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1341 .    wa1-yearlength = 365.   wa1-from_day = 21915 .    wa1-to_day = 22279 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1342 .    wa1-yearlength = 366.   wa1-from_day = 22280 .    wa1-to_day = 22645 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1343 .    wa1-yearlength = 365.   wa1-from_day = 22646 .    wa1-to_day = 23010 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1344 .    wa1-yearlength = 365.   wa1-from_day = 23011 .    wa1-to_day = 23375 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1345 .    wa1-yearlength = 365.   wa1-from_day = 23376 .    wa1-to_day = 23740 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1346 .    wa1-yearlength = 366.   wa1-from_day = 23741 .    wa1-to_day = 24106 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1347 .    wa1-yearlength = 365.   wa1-from_day = 24107 .    wa1-to_day = 24471 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1348 .    wa1-yearlength = 365.   wa1-from_day = 24472 .    wa1-to_day = 24836 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1349 .    wa1-yearlength = 365.   wa1-from_day = 24837 .    wa1-to_day = 25201 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1350 .    wa1-yearlength = 366.   wa1-from_day = 25202 .    wa1-to_day = 25567 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1351 .    wa1-yearlength = 365.   wa1-from_day = 25568 .    wa1-to_day = 25932 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1352 .    wa1-yearlength = 365.   wa1-from_day = 25933 .    wa1-to_day = 26297 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1353 .    wa1-yearlength = 365.   wa1-from_day = 26298 .    wa1-to_day = 26662 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1354 .    wa1-yearlength = 366.   wa1-from_day = 26663 .    wa1-to_day = 27028 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1355 .    wa1-yearlength = 365.   wa1-from_day = 27029 .    wa1-to_day = 27393 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1356 .    wa1-yearlength = 365.   wa1-from_day = 27394 .    wa1-to_day = 27758 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1357 .    wa1-yearlength = 365.   wa1-from_day = 27759 .    wa1-to_day = 28123 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1358 .    wa1-yearlength = 366.   wa1-from_day = 28124 .    wa1-to_day = 28489 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1359 .    wa1-yearlength = 365.   wa1-from_day = 28490 .    wa1-to_day = 28854 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1360 .    wa1-yearlength = 365.   wa1-from_day = 28855 .    wa1-to_day = 29219 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1361 .    wa1-yearlength = 365.   wa1-from_day = 29220 .    wa1-to_day = 29584 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1362 .    wa1-yearlength = 366.   wa1-from_day = 29585 .    wa1-to_day = 29950 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1363 .    wa1-yearlength = 365.   wa1-from_day = 29951 .    wa1-to_day = 30315 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1364 .    wa1-yearlength = 365.   wa1-from_day = 30316 .    wa1-to_day = 30680 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1365 .    wa1-yearlength = 365.   wa1-from_day = 30681 .    wa1-to_day = 31045 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1366 .    wa1-yearlength = 366.   wa1-from_day = 31046 .    wa1-to_day = 31411 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1367 .    wa1-yearlength = 365.   wa1-from_day = 31412 .    wa1-to_day = 31776 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1368 .    wa1-yearlength = 365.   wa1-from_day = 31777 .    wa1-to_day = 32141 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1369 .    wa1-yearlength = 365.   wa1-from_day = 32142 .    wa1-to_day = 32506 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1370 .    wa1-yearlength = 366.   wa1-from_day = 32507 .    wa1-to_day = 32872 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1371 .    wa1-yearlength = 365.   wa1-from_day = 32873 .    wa1-to_day = 33237 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1372 .    wa1-yearlength = 365.   wa1-from_day = 33238 .    wa1-to_day = 33602 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1373 .    wa1-yearlength = 365.   wa1-from_day = 33603 .    wa1-to_day = 33967 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1374 .    wa1-yearlength = 365.   wa1-from_day = 33968 .    wa1-to_day = 34332 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1375 .    wa1-yearlength = 366.   wa1-from_day = 34333 .    wa1-to_day = 34698 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1376 .    wa1-yearlength = 365.   wa1-from_day = 34699 .    wa1-to_day = 35063 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1377 .    wa1-yearlength = 365.   wa1-from_day = 35064 .    wa1-to_day = 35428 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1378 .    wa1-yearlength = 365.   wa1-from_day = 35429 .    wa1-to_day = 35793 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1379 .    wa1-yearlength = 366.   wa1-from_day = 35794 .    wa1-to_day = 36159 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1380 .    wa1-yearlength = 365.   wa1-from_day = 36160 .    wa1-to_day = 36524 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1381 .    wa1-yearlength = 365.   wa1-from_day = 36525 .    wa1-to_day = 36889 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1382 .    wa1-yearlength = 365.   wa1-from_day = 36890 .    wa1-to_day = 37254 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1383 .    wa1-yearlength = 366.   wa1-from_day = 37255 .    wa1-to_day = 37620 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1384 .    wa1-yearlength = 365.   wa1-from_day = 37621 .    wa1-to_day = 37985 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1385 .    wa1-yearlength = 365.   wa1-from_day = 37986 .    wa1-to_day = 38350 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1386 .    wa1-yearlength = 365.   wa1-from_day = 38351 .    wa1-to_day = 38715 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1387 .    wa1-yearlength = 366.   wa1-from_day = 38716 .    wa1-to_day = 39081 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1388 .    wa1-yearlength = 365.   wa1-from_day = 39082 .    wa1-to_day = 39446 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1389 .    wa1-yearlength = 365.   wa1-from_day = 39447 .    wa1-to_day = 39811 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1390 .    wa1-yearlength = 365.   wa1-from_day = 39812 .    wa1-to_day = 40176 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1391 .    wa1-yearlength = 366.   wa1-from_day = 40177 .    wa1-to_day = 40542 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1392 .    wa1-yearlength = 365.   wa1-from_day = 40543 .    wa1-to_day = 40907 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1393 .    wa1-yearlength = 365.   wa1-from_day = 40908 .    wa1-to_day = 41272 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1394 .    wa1-yearlength = 365.   wa1-from_day = 41273 .    wa1-to_day = 41637 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1395 .    wa1-yearlength = 366.   wa1-from_day = 41638 .    wa1-to_day = 42003 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1396 .    wa1-yearlength = 365.   wa1-from_day = 42004 .    wa1-to_day = 42368 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1397 .    wa1-yearlength = 365.   wa1-from_day = 42369 .    wa1-to_day = 42733 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1398 .    wa1-yearlength = 365.   wa1-from_day = 42734 .    wa1-to_day = 43098 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1399 .    wa1-yearlength = 366.   wa1-from_day = 43099 .    wa1-to_day = 43464 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1400 .    wa1-yearlength = 365.   wa1-from_day = 43465 .    wa1-to_day = 43829 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1401 .    wa1-yearlength = 365.   wa1-from_day = 43830 .    wa1-to_day = 44194 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1402 .    wa1-yearlength = 365.   wa1-from_day = 44195 .    wa1-to_day = 44559 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1403 .    wa1-yearlength = 365.   wa1-from_day = 44560 .    wa1-to_day = 44924 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1404 .    wa1-yearlength = 366.   wa1-from_day = 44925 .    wa1-to_day = 45290 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1405 .    wa1-yearlength = 365.   wa1-from_day = 45291 .    wa1-to_day = 45655 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1406 .    wa1-yearlength = 365.   wa1-from_day = 45656 .    wa1-to_day = 46020 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1407 .    wa1-yearlength = 365.   wa1-from_day = 46021 .    wa1-to_day = 46385 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1408 .    wa1-yearlength = 366.   wa1-from_day = 46386 .    wa1-to_day = 46751 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1409 .    wa1-yearlength = 365.   wa1-from_day = 46752 .    wa1-to_day = 47116 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1410 .    wa1-yearlength = 365.   wa1-from_day = 47117 .    wa1-to_day = 47481 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1411 .    wa1-yearlength = 365.   wa1-from_day = 47482 .    wa1-to_day = 47846 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1412 .    wa1-yearlength = 366.   wa1-from_day = 47847 .    wa1-to_day = 48212 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1413 .    wa1-yearlength = 365.   wa1-from_day = 48213 .    wa1-to_day = 48577 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1414 .    wa1-yearlength = 365.   wa1-from_day = 48578 .    wa1-to_day = 48942 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1415 .    wa1-yearlength = 365.   wa1-from_day = 48943 .    wa1-to_day = 49307 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1416 .    wa1-yearlength = 366.   wa1-from_day = 49308 .    wa1-to_day = 49673 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1417 .    wa1-yearlength = 365.   wa1-from_day = 49674 .    wa1-to_day = 50038 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1418 .    wa1-yearlength = 365.   wa1-from_day = 50039 .    wa1-to_day = 50403 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1419 .    wa1-yearlength = 365.   wa1-from_day = 50404 .    wa1-to_day = 50768 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1420 .    wa1-yearlength = 366.   wa1-from_day = 50769 .    wa1-to_day = 51134 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1421 .    wa1-yearlength = 365.   wa1-from_day = 51135 .    wa1-to_day = 51499 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1422 .    wa1-yearlength = 365.   wa1-from_day = 51500 .    wa1-to_day = 51864 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1423 .    wa1-yearlength = 365.   wa1-from_day = 51865 .    wa1-to_day = 52229 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1424 .    wa1-yearlength = 366.   wa1-from_day = 52230 .    wa1-to_day = 52595 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1425 .    wa1-yearlength = 365.   wa1-from_day = 52596 .    wa1-to_day = 52960 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1426 .    wa1-yearlength = 365.   wa1-from_day = 52961 .    wa1-to_day = 53325 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1427 .    wa1-yearlength = 365.   wa1-from_day = 53326 .    wa1-to_day = 53690 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1428 .    wa1-yearlength = 366.   wa1-from_day = 53691 .    wa1-to_day = 54056 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1429 .    wa1-yearlength = 365.   wa1-from_day = 54057 .    wa1-to_day = 54421 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1430 .    wa1-yearlength = 365.   wa1-from_day = 54422 .    wa1-to_day = 54786 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1431 .    wa1-yearlength = 365.   wa1-from_day = 54787 .    wa1-to_day = 55151 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1432 .    wa1-yearlength = 366.   wa1-from_day = 55152 .    wa1-to_day = 55517 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1433 .    wa1-yearlength = 365.   wa1-from_day = 55518 .    wa1-to_day = 55882 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1434 .    wa1-yearlength = 365.   wa1-from_day = 55883 .    wa1-to_day = 56247 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1435 .    wa1-yearlength = 365.   wa1-from_day = 56248 .    wa1-to_day = 56612 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1436 .    wa1-yearlength = 365.   wa1-from_day = 56613 .    wa1-to_day = 56977 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1437 .    wa1-yearlength = 366.   wa1-from_day = 56978 .    wa1-to_day = 57343 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1438 .    wa1-yearlength = 365.   wa1-from_day = 57344 .    wa1-to_day = 57708 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1439 .    wa1-yearlength = 365.   wa1-from_day = 57709 .    wa1-to_day = 58073 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1440 .    wa1-yearlength = 365.   wa1-from_day = 58074 .    wa1-to_day = 58438 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1441 .    wa1-yearlength = 366.   wa1-from_day = 58439 .    wa1-to_day = 58804 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1442 .    wa1-yearlength = 365.   wa1-from_day = 58805 .    wa1-to_day = 59169 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1443 .    wa1-yearlength = 365.   wa1-from_day = 59170 .    wa1-to_day = 59534 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1444 .    wa1-yearlength = 365.   wa1-from_day = 59535 .    wa1-to_day = 59899 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1445 .    wa1-yearlength = 366.   wa1-from_day = 59900 .    wa1-to_day = 60265 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1446 .    wa1-yearlength = 365.   wa1-from_day = 60266 .    wa1-to_day = 60630 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1447 .    wa1-yearlength = 365.   wa1-from_day = 60631 .    wa1-to_day = 60995 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1448 .    wa1-yearlength = 365.   wa1-from_day = 60996 .    wa1-to_day = 61360 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1449 .    wa1-yearlength = 366.   wa1-from_day = 61361 .    wa1-to_day = 61726 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1450 .    wa1-yearlength = 365.   wa1-from_day = 61727 .    wa1-to_day = 62091 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1451 .    wa1-yearlength = 365.   wa1-from_day = 62092 .    wa1-to_day = 62456 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1452 .    wa1-yearlength = 365.   wa1-from_day = 62457 .    wa1-to_day = 62821 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1453 .    wa1-yearlength = 366.   wa1-from_day = 62822 .    wa1-to_day = 63187 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1454 .    wa1-yearlength = 365.   wa1-from_day = 63188 .    wa1-to_day = 63552 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1455 .    wa1-yearlength = 365.   wa1-from_day = 63553 .    wa1-to_day = 63917 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1456 .    wa1-yearlength = 365.   wa1-from_day = 63918 .    wa1-to_day = 64282 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1457 .    wa1-yearlength = 366.   wa1-from_day = 64283 .    wa1-to_day = 64648 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1458 .    wa1-yearlength = 365.   wa1-from_day = 64649 .    wa1-to_day = 65013 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1459 .    wa1-yearlength = 365.   wa1-from_day = 65014 .    wa1-to_day = 65378 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1460 .    wa1-yearlength = 365.   wa1-from_day = 65379 .    wa1-to_day = 65743 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1461 .    wa1-yearlength = 366.   wa1-from_day = 65744 .    wa1-to_day = 66109 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1462 .    wa1-yearlength = 365.   wa1-from_day = 66110 .    wa1-to_day = 66474 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1463 .    wa1-yearlength = 365.   wa1-from_day = 66475 .    wa1-to_day = 66839 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1464 .    wa1-yearlength = 365.   wa1-from_day = 66840 .    wa1-to_day = 67204 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1465 .    wa1-yearlength = 366.   wa1-from_day = 67205 .    wa1-to_day = 67570 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1466 .    wa1-yearlength = 365.   wa1-from_day = 67571 .    wa1-to_day = 67935 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1467 .    wa1-yearlength = 365.   wa1-from_day = 67936 .    wa1-to_day = 68300 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1468 .    wa1-yearlength = 365.   wa1-from_day = 68301 .    wa1-to_day = 68665 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1469 .    wa1-yearlength = 365.   wa1-from_day = 68666 .    wa1-to_day = 69030 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1470 .    wa1-yearlength = 366.   wa1-from_day = 69031 .    wa1-to_day = 69396 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1471 .    wa1-yearlength = 365.   wa1-from_day = 69397 .    wa1-to_day = 69761 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1472 .    wa1-yearlength = 365.   wa1-from_day = 69762 .    wa1-to_day = 70126 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1473 .    wa1-yearlength = 365.   wa1-from_day = 70127 .    wa1-to_day = 70491 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1474 .    wa1-yearlength = 366.   wa1-from_day = 70492 .    wa1-to_day = 70857 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1475 .    wa1-yearlength = 365.   wa1-from_day = 70858 .    wa1-to_day = 71222 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1476 .    wa1-yearlength = 365.   wa1-from_day = 71223 .    wa1-to_day = 71587 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1477 .    wa1-yearlength = 365.   wa1-from_day = 71588 .    wa1-to_day = 71952 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1478 .    wa1-yearlength = 366.   wa1-from_day = 71953 .    wa1-to_day = 72318 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1479 .    wa1-yearlength = 365.   wa1-from_day = 72319 .    wa1-to_day = 72683 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1480 .    wa1-yearlength = 365.   wa1-from_day = 72684 .    wa1-to_day = 73048 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1481 .    wa1-yearlength = 365.   wa1-from_day = 73049 .    wa1-to_day = 73413 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1482 .    wa1-yearlength = 366.   wa1-from_day = 73414 .    wa1-to_day = 73779 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1483 .    wa1-yearlength = 365.   wa1-from_day = 73780 .    wa1-to_day = 74144 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1484 .    wa1-yearlength = 365.   wa1-from_day = 74145 .    wa1-to_day = 74509 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1485 .    wa1-yearlength = 365.   wa1-from_day = 74510 .    wa1-to_day = 74874 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1486 .    wa1-yearlength = 366.   wa1-from_day = 74875 .    wa1-to_day = 75240 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1487 .    wa1-yearlength = 365.   wa1-from_day = 75241 .    wa1-to_day = 75605 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1488 .    wa1-yearlength = 365.   wa1-from_day = 75606 .    wa1-to_day = 75970 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1489 .    wa1-yearlength = 365.   wa1-from_day = 75971 .    wa1-to_day = 76335 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1490 .    wa1-yearlength = 366.   wa1-from_day = 76336 .    wa1-to_day = 76701 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1491 .    wa1-yearlength = 365.   wa1-from_day = 76702 .    wa1-to_day = 77066 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1492 .    wa1-yearlength = 365.   wa1-from_day = 77067 .    wa1-to_day = 77431 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1493 .    wa1-yearlength = 365.   wa1-from_day = 77432 .    wa1-to_day = 77796 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1494 .    wa1-yearlength = 366.   wa1-from_day = 77797 .    wa1-to_day = 78162 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1495 .    wa1-yearlength = 365.   wa1-from_day = 78163 .    wa1-to_day = 78527 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1496 .    wa1-yearlength = 365.   wa1-from_day = 78528 .    wa1-to_day = 78892 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1497 .    wa1-yearlength = 365.   wa1-from_day = 78893 .    wa1-to_day = 79257 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1498 .    wa1-yearlength = 366.   wa1-from_day = 79258 .    wa1-to_day = 79623 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1499 .    wa1-yearlength = 365.   wa1-from_day = 79624 .    wa1-to_day = 79988 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1500 .    wa1-yearlength = 365.   wa1-from_day = 79989 .    wa1-to_day = 80353 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1501 .    wa1-yearlength = 365.   wa1-from_day = 80354 .    wa1-to_day = 80718 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1502 .    wa1-yearlength = 365.   wa1-from_day = 80719 .    wa1-to_day = 81083 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1503 .    wa1-yearlength = 366.   wa1-from_day = 81084 .    wa1-to_day = 81449 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1504 .    wa1-yearlength = 365.   wa1-from_day = 81450 .    wa1-to_day = 81814 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1505 .    wa1-yearlength = 365.   wa1-from_day = 81815 .    wa1-to_day = 82179 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1506 .    wa1-yearlength = 365.   wa1-from_day = 82180 .    wa1-to_day = 82544 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1507 .    wa1-yearlength = 366.   wa1-from_day = 82545 .    wa1-to_day = 82910 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1508 .    wa1-yearlength = 365.   wa1-from_day = 82911 .    wa1-to_day = 83275 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1509 .    wa1-yearlength = 365.   wa1-from_day = 83276 .    wa1-to_day = 83640 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1510 .    wa1-yearlength = 365.   wa1-from_day = 83641 .    wa1-to_day = 84005 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1511 .    wa1-yearlength = 366.   wa1-from_day = 84006 .    wa1-to_day = 84371 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1512 .    wa1-yearlength = 365.   wa1-from_day = 84372 .    wa1-to_day = 84736 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1513 .    wa1-yearlength = 365.   wa1-from_day = 84737 .    wa1-to_day = 85101 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1514 .    wa1-yearlength = 365.   wa1-from_day = 85102 .    wa1-to_day = 85466 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1515 .    wa1-yearlength = 366.   wa1-from_day = 85467 .    wa1-to_day = 85832 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1516 .    wa1-yearlength = 365.   wa1-from_day = 85833 .    wa1-to_day = 86197 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1517 .    wa1-yearlength = 365.   wa1-from_day = 86198 .    wa1-to_day = 86562 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1518 .    wa1-yearlength = 365.   wa1-from_day = 86563 .    wa1-to_day = 86927 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1519 .    wa1-yearlength = 366.   wa1-from_day = 86928 .    wa1-to_day = 87293 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1520 .    wa1-yearlength = 365.   wa1-from_day = 87294 .    wa1-to_day = 87658 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1521 .    wa1-yearlength = 365.   wa1-from_day = 87659 .    wa1-to_day = 88023 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1522 .    wa1-yearlength = 365.   wa1-from_day = 88024 .    wa1-to_day = 88388 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1523 .    wa1-yearlength = 366.   wa1-from_day = 88389 .    wa1-to_day = 88754 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1524 .    wa1-yearlength = 365.   wa1-from_day = 88755 .    wa1-to_day = 89119 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1525 .    wa1-yearlength = 365.   wa1-from_day = 89120 .    wa1-to_day = 89484 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1526 .    wa1-yearlength = 365.   wa1-from_day = 89485 .    wa1-to_day = 89849 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1527 .    wa1-yearlength = 366.   wa1-from_day = 89850 .    wa1-to_day = 90215 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1528 .    wa1-yearlength = 365.   wa1-from_day = 90216 .    wa1-to_day = 90580 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1529 .    wa1-yearlength = 365.   wa1-from_day = 90581 .    wa1-to_day = 90945 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1530 .    wa1-yearlength = 365.   wa1-from_day = 90946 .    wa1-to_day = 91310 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1531 .    wa1-yearlength = 365.   wa1-from_day = 91311 .    wa1-to_day = 91675 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1532 .    wa1-yearlength = 366.   wa1-from_day = 91676 .    wa1-to_day = 92041 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1533 .    wa1-yearlength = 365.   wa1-from_day = 92042 .    wa1-to_day = 92406 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1534 .    wa1-yearlength = 365.   wa1-from_day = 92407 .    wa1-to_day = 92771 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1535 .    wa1-yearlength = 365.   wa1-from_day = 92772 .    wa1-to_day = 93136 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1536 .    wa1-yearlength = 366.   wa1-from_day = 93137 .    wa1-to_day = 93502 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1537 .    wa1-yearlength = 365.   wa1-from_day = 93503 .    wa1-to_day = 93867 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1538 .    wa1-yearlength = 365.   wa1-from_day = 93868 .    wa1-to_day = 94232 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1539 .    wa1-yearlength = 365.   wa1-from_day = 94233 .    wa1-to_day = 94597 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1540 .    wa1-yearlength = 366.   wa1-from_day = 94598 .    wa1-to_day = 94963 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1541 .    wa1-yearlength = 365.   wa1-from_day = 94964 .    wa1-to_day = 95328 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1542 .    wa1-yearlength = 365.   wa1-from_day = 95329 .    wa1-to_day = 95693 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1543 .    wa1-yearlength = 365.   wa1-from_day = 95694 .    wa1-to_day = 96058 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1544 .    wa1-yearlength = 366.   wa1-from_day = 96059 .    wa1-to_day = 96424 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1545 .    wa1-yearlength = 365.   wa1-from_day = 96425 .    wa1-to_day = 96789 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1546 .    wa1-yearlength = 365.   wa1-from_day = 96790 .    wa1-to_day = 97154 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1547 .    wa1-yearlength = 365.   wa1-from_day = 97155 .    wa1-to_day = 97519 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1548 .    wa1-yearlength = 366.   wa1-from_day = 97520 .    wa1-to_day = 97885 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1549 .    wa1-yearlength = 365.   wa1-from_day = 97886 .    wa1-to_day = 98250 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1550 .    wa1-yearlength = 365.   wa1-from_day = 98251 .    wa1-to_day = 98615 .    wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1551 .    wa1-yearlength = 365.   wa1-from_day = 98616 .    wa1-to_day = 98980 .    wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1552 .    wa1-yearlength = 366.   wa1-from_day = 98981 .    wa1-to_day = 99346 .    wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1553 .    wa1-yearlength = 365.   wa1-from_day = 99347 .    wa1-to_day = 99711 .    wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1554 .    wa1-yearlength = 365.   wa1-from_day = 99712 .    wa1-to_day = 100076 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1555 .    wa1-yearlength = 365.   wa1-from_day = 100077 .   wa1-to_day = 100441 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1556 .    wa1-yearlength = 366.   wa1-from_day = 100442 .   wa1-to_day = 100807 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1557 .    wa1-yearlength = 365.   wa1-from_day = 100808 .   wa1-to_day = 101172 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1558 .    wa1-yearlength = 365.   wa1-from_day = 101173 .   wa1-to_day = 101537 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1559 .    wa1-yearlength = 365.   wa1-from_day = 101538 .   wa1-to_day = 101902 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1560 .    wa1-yearlength = 366.   wa1-from_day = 101903 .   wa1-to_day = 102268 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1561 .    wa1-yearlength = 365.   wa1-from_day = 102269 .   wa1-to_day = 102633 .   wa1-leap_status = 'P' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1562 .    wa1-yearlength = 365.   wa1-from_day = 102634 .   wa1-to_day = 102998 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1563 .    wa1-yearlength = 365.   wa1-from_day = 102999 .   wa1-to_day = 103363 .   wa1-leap_status = 'A' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1564 .    wa1-yearlength = 365.   wa1-from_day = 103364 .   wa1-to_day = 103728 .   wa1-leap_status = 'N' .   APPEND wa1 TO it1 .
  wa1-yyyy = 1565 .    wa1-yearlength = 366.   wa1-from_day = 103729 .   wa1-to_day = 104094 .   wa1-leap_status = 'X' .   APPEND wa1 TO it1 .






  SORT it1 ASCENDING BY from_day.

  LOOP AT it1 INTO wa1 WHERE from_day LE e_tage AND to_day GE e_tage.
    CONTINUE.
  ENDLOOP.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  DATA: isleap TYPE i VALUE 0.
  DATA: day_no TYPE i.
  day_no = e_tage - wa1-from_day + 1.
  IF wa1-leap_status = 'X'. isleap = 1 . ENDIF.

  sh_year = wa1-yyyy.
  IF day_no LE 186.
    sh_month = ( day_no - 1 ) DIV 31 + 1.
    sh_day = day_no - ( ( sh_month - 1 ) * 31 ).
  ELSE.
    day_no = day_no - 186.
    sh_month = ( day_no - 1 ) DIV 30 + 1.
    sh_day = day_no - ( ( sh_month - 1 ) * 30 ) .
    sh_month = sh_month + 6.
  ENDIF.
  IF sh_month LE 6. sh_this_month_max_day = 31. ENDIF.
  IF sh_month GE 7 AND sh_month LE 11. sh_this_month_max_day = 30. ENDIF.
  IF sh_month = 12. sh_this_month_max_day = 29 + isleap. ENDIF.

  IF sh_month GE 1 AND sh_month LE 5. sh_next_month_max_day = 31. ENDIF.
  IF sh_month = 12. sh_next_month_max_day = 31. ENDIF.
  IF sh_month GE 6 AND sh_month LE 10. sh_next_month_max_day = 30. ENDIF.
  IF sh_month = 11. sh_next_month_max_day = 29 + isleap. ENDIF.









ENDFUNCTION.
