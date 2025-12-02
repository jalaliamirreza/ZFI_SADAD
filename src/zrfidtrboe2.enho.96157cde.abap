"Name: \PR:RFIDTRBOE2\FO:BATCHFIELD\SE:BEGIN\EI
ENHANCEMENT 0 ZRFIDTRBOE2.
*


   DATA: lv_date TYPE datum,
         lv_res  LIKE sy-datum,
         lv_year TYPE char4.

   IF strlen( fval ) EQ 8 OR strlen( fval ) EQ 4 .
     IF strlen( fval ) EQ 8.
       CONCATENATE fval+4(4) fval+2(2) fval+0(2) INTO lv_res.
     ELSE.
       lv_res = fval.
     ENDIF.

     lv_date = lv_res.
     CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
       EXPORTING
         date                      = lv_date
       EXCEPTIONS
         plausibility_check_failed = 1
         OTHERS                    = 2.

     IF sy-subrc EQ 0 AND fval+4(2) GE '20'.
       IF gf_datfm EQ 'C'.
         WRITE lv_date TO fval.
       ENDIF.
     ELSEIF fnam EQ 'RF05A-GJAHS' OR fnam EQ 'RF05L-GJAHR' OR fnam EQ 'RF05R-GJAHR'.

       CALL FUNCTION 'CONVERSION_EXIT_GJAHR_OUTPUT'
         EXPORTING
           input  = fval
         IMPORTING
           output = lv_year.

       IF sy-subrc EQ 0.
         fval = lv_year.
       ENDIF.
     ENDIF.
   ENDIF.


ENDENHANCEMENT.
