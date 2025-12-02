*&--------------------------------------------------------------------*
*& REPORT zzmast01
*&--------------------------------------------------------------------*
REPORT zzmast01 .

tables: mast, stko, stzu.

parameters c_update like csdata-xfeld.

data: begin of t_mast occurs 0.
        include structure mast.
data: end of t_mast.

select * from mast into table t_mast.

if c_update is initial.
    write: / 'TEST modus'.
else.
    write: / 'UPDATE modus'.
endif.

* write: / text-mat, text-wrk, text-anw, text-stl, text-alt.

loop at t_mast.
*    BOM header without STZU entry
  select single * from stzu
     where stlty eq 'M'
     and   stlnr eq t_mast-stlnr.
  if sy-subrc ne 0.
    write: / t_mast-matnr, t_mast-werks, t_mast-stlan, t_mast-stlnr,
             t_mast-stlal.
    if not c_update is initial.
*       Delete all alternatives in MAST and STKO
      delete from mast
         where matnr eq t_mast-matnr
         and   werks eq t_mast-werks
         and   stlan eq t_mast-stlan
         and   stlnr eq t_mast-stlnr.
      delete from stko
         where stlty eq 'M'
         and   stlnr eq t_mast-stlnr.
    endif.

  else.
*    BOM header without STKO entry
    select * from stko up to 1 rows
       where stlty eq 'M'
       and   stlnr eq t_mast-stlnr
       and   stlal eq t_mast-stlal.
    endselect.
    if sy-subrc ne 0.
      write: / t_mast-matnr, t_mast-werks, t_mast-stlan, t_mast-stlnr,
               t_mast-stlal.
*       Delete STZU and 1 alternative in MAST
      if not c_update is initial.
        delete from mast
           where matnr eq t_mast-matnr
           and   werks eq t_mast-werks
           and   stlan eq t_mast-stlan
           and   stlnr eq t_mast-stlnr
           and   stlal eq t_mast-stlal.
        select * from stko up to 1 rows
          where stlty eq 'M'
          and   stlnr eq t_mast-stlnr.
        endselect.
        if sy-subrc ne 0.
          delete from stzu
            where stlty eq 'M'
            and   stlnr eq t_mast-stlnr.
        endif.
      endif.

    endif.
  endif.

endloop.



























