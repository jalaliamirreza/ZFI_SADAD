"Name: \PR:SAPLCOIH\EX:SAPLCOIH_OLC_018\EI
ENHANCEMENT 0 ZSAPLCOIH.
 IF caufvd-equnr IS NOT INITIAL and ( caufvd-auart BETWEEN 'ZPM1' and 'ZPM6' or caufvd-auart = 'ZPMT' ).
    data fkbwr TYPE fkber.
    select SINGLE FUNC_AREA FROM itob
                            INNER JOIN csks on csks~kostl = itob~kostl
                            INTO CAUFVD-FUNC_AREA WHERE equnr = caufvd-equnr.
    endif.
 IF TMP_FCODE = 'BU' AND ( SY-TCODE <> 'IW61' AND SY-TCODE <> 'IW62').
   if caufvd-func_area is INITIAL.
     MESSAGE e007(zfi).
   endif.
   IF   caufvd-func_area = 'Z013' or caufvd-func_area = 'Z014'.
       MESSAGE e016(zfi).
   ENDIF.
 endif.
ENDENHANCEMENT.
