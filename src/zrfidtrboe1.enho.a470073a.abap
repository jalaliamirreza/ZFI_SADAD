"Name: \PR:RFIDTRBOE1\FO:BATCHFIELD\SE:BEGIN\EI
ENHANCEMENT 0 ZRFIDTRBOE1.
*


    DATA: lv_date TYPE datum,
          lv_res  LIKE sy-datum.
    IF gf_datfm EQ 'C'.
      IF strlen( fval ) EQ 8 AND fnam NE 'RF05A-PORTF' AND ( fnam NE 'RF05A-PORTF' AND fnam NE 'BSED-VENDR' )  .
        IF fval+4(2) GE '20'.
          CONCATENATE fval+4(4) fval+2(2) fval+0(2) INTO lv_res.
          lv_date = lv_res.
          CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
            EXPORTING
              date                      = lv_date
            EXCEPTIONS
              plausibility_check_failed = 1
              OTHERS                    = 2.
          IF sy-subrc EQ 0.
            WRITE lv_date TO fval.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.


ENDENHANCEMENT.
