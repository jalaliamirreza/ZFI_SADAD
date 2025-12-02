FUNCTION zgenerate_xblnr_for_sd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(DOCTYPE) TYPE  BLART
*"     REFERENCE(BUDAT) TYPE  BUDAT
*"     REFERENCE(BUKRS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(XBLNR) TYPE  XBLNR
*"     REFERENCE(LASTBUDAT) TYPE  BUDAT
*"  EXCEPTIONS
*"      BUDAT_AFTER_TODAY
*"      BUDAT_BEFORE_LAST_DOCUMENT
*"      NO_DOCCLAS_FOR_DOCTYPE
*"----------------------------------------------------------------------

  DATA: doccls      TYPE doccls,
        lastdate    TYPE d,
        groupnumber TYPE nrnr,
        nrlevel     TYPE nrlevel,
        lastxblnr   TYPE xblnr,
        fromnumber  TYPE char20,
        lastnr      TYPE nrlevel,
        fromlevel   TYPE nrlevel,
        mandt       TYPE mandt,
        lv_periv    TYPE t001-periv,
        lv_year(4),
        toyear      TYPE inri-toyear.




  SELECT SINGLE doccls FROM t003_i INTO doccls WHERE blart = doctype AND land1 = 'IR'.
  IF sy-subrc IS INITIAL.

    IF budat > sy-datum.
      MESSAGE e004(zfi) WITH budat.

      "RAISE budat_after_today.
    ENDIF.


    CLEAR: lv_year,lv_periv.
    SELECT SINGLE periv INTO lv_periv FROM t001 WHERE bukrs = bukrs.

    CALL FUNCTION 'GM_GET_FISCAL_YEAR'
      EXPORTING
        i_date                     = budat
        i_fyv                      = lv_periv
      IMPORTING
        e_fy                       = lv_year
      EXCEPTIONS
        fiscal_year_does_not_exist = 1
        not_defined_for_date       = 2
        OTHERS                     = 3.




    SELECT SINGLE lastdate groupnumber FROM ofnum_it_1 INTO (lastdate , groupnumber)
                              WHERE docclass = doccls AND bukrs = bukrs.

    SELECT SINGLE nrlevel fromnumber  FROM nriv INTO (nrlevel ,  fromnumber)
                              WHERE object = 'FIN2_IT'
                                AND subobject = bukrs
                                AND nrrangenr = groupnumber
                                AND toyear = lv_year.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = fromnumber
      IMPORTING
        output = fromlevel.


      SELECT MAX( xblnr ) FROM bkpf INTO lastxblnr WHERE blart = doctype AND bukrs = bukrs AND xblnr >= lastxblnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lastxblnr
      IMPORTING
        output = lastnr.


    IF lastnr < nrlevel AND lastnr > fromlevel.

      nrlevel = lastnr + 1.

    ELSE.


      CLEAR toyear.

      toyear = lv_year.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = groupnumber
          object      = 'FIN2_IT'
          quantity    = '1'
          subobject   = bukrs
          toyear      = toyear
*         IGNORE_BUFFER                 = ' '
        IMPORTING
          number      = nrlevel
*         QUANTITY    =
*         RETURNCODE  =
*         EXCEPTIONS
*         INTERVAL_NOT_FOUND            = 1
*         NUMBER_RANGE_NOT_INTERN       = 2
*         OBJECT_NOT_FOUND              = 3
*         QUANTITY_IS_0                 = 4
*         QUANTITY_IS_NOT_1             = 5
*         INTERVAL_OVERFLOW             = 6
*         BUFFER_OVERFLOW               = 7
*         OTHERS      = 8
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = nrlevel
      IMPORTING
        output = xblnr.

    lastnr = nrlevel - 1.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lastnr
      IMPORTING
        output = lastxblnr.


    SHIFT lastxblnr LEFT DELETING LEADING '0'.



    SELECT SINGLE budat FROM bkpf INTO lastbudat WHERE xblnr = lastxblnr AND bukrs = bukrs AND  gjahr = lv_year .
    IF sy-subrc IS INITIAL.
      IF budat < lastbudat AND bukrs <> '1000'.
        MESSAGE e003(zfi) WITH budat lastbudat.
        RAISE budat_before_last_document.
      ENDIF.
    ELSE.
      WHILE sy-subrc IS NOT INITIAL.
        lastxblnr = lastxblnr - 1.
        IF lastxblnr < fromlevel.
          EXIT.
        ENDIF.
        SELECT SINGLE budat FROM bkpf INTO lastbudat WHERE xblnr = lastxblnr AND bukrs = bukrs AND  gjahr = lv_year .
        IF budat < lastbudat AND bukrs <> '1000'.
          MESSAGE e003(zfi) WITH budat lastbudat.
          "RAISE budat_before_last_document.
        ENDIF.
      ENDWHILE.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = nrlevel
      IMPORTING
        output = xblnr.
    "UPDATE ofnum_it_1 SET lastdate = budat WHERE docclass = doccls and bukrs = bukrs.
  ELSE.
    RAISE no_docclas_for_doctype.

  ENDIF.


ENDFUNCTION.
