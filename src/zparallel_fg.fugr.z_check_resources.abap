FUNCTION z_check_resources.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_GROUP) TYPE  RZLLI_APCL OPTIONAL
*"     VALUE(I_SAFE_PERCENT) TYPE  I DEFAULT 75
*"  EXPORTING
*"     VALUE(E_FREE_WP) TYPE  I
*"----------------------------------------------------------------------

  DATA lt_applserver TYPE TABLE OF rzlli_asvr WITH HEADER LINE .
  DATA lv_applserver TYPE msname2 .
  DATA lv_safe TYPE i .
  DATA lv_dia_wps TYPE i.
  DATA lv_free_dia_wps TYPE i .
  DATA lv_total TYPE i .
  DATA lv_free TYPE i .

  SELECT applserver FROM rzllitab
    INTO TABLE lt_applserver
    WHERE classname EQ i_group.

  LOOP AT lt_applserver .

    lv_applserver = lt_applserver.

    CLEAR lv_dia_wps.
    CLEAR lv_free_dia_wps .
    CALL FUNCTION 'TH_COUNT_WPS'
      EXPORTING
        server       = lv_applserver
      IMPORTING
        dia_wps      = lv_dia_wps
        free_dia_wps = lv_free_dia_wps
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc EQ 0.

      lv_total = lv_dia_wps + lv_total .
      lv_free = lv_free_dia_wps + lv_free.
    ENDIF.

  ENDLOOP.

  lv_safe = ( lv_total * i_safe_percent ) / 100.

  IF lv_free GT lv_safe .
    e_free_wp = lv_free.
  ELSE.
    e_free_wp = 0 .
  ENDIF.





ENDFUNCTION.
