*&---------------------------------------------------------------------*
*& Report ZFI_SADAD_GET_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZFI_SADAD_GET_REPORT.

data: gt_sadad_log type table of zsadad_log,
      ls_sadad_log type zsadad_log,
      gt_fcat type slis_t_fieldcat_alv.




SELECT-OPTIONS: TAXID for ls_sadad_log-taxid,
                xblnr for ls_sadad_log-xblnr,
                serial for ls_sadad_log-serial,
                fkdat for ls_sadad_log-fkdat,
                snd_dt for ls_sadad_log-send_date,
                ZKTOKD for ls_sadad_log-ZKTOKD,
                status for ls_sadad_log-status,
                error1 for ls_sadad_log-error1.


START-OF-SELECTION.
PERFORM run_program.


form run_program.

  select * from zsadad_log into table @gt_sadad_log
    where taxid in @taxid and
          xblnr in @xblnr and
          serial in @serial and
          fkdat in @fkdat and
          send_date in @snd_dt and
          zktokd in @zktokd and
          status in @status and
          error1 in @error1.

   if sy-uname = 'MAMARDANI'.
     ls_sadad_log-taxid = '123'.
     ls_sadad_log-xblnr = '123'.
     ls_sadad_log-serial = '02'.
     ls_sadad_log-fkdat = '20250202'.
     ls_sadad_log-send_date = '20250202'.
     ls_sadad_log-zktokd = '2'.
     ls_sadad_log-status = '3'.
     ls_sadad_log-error1 = 'error'.
     append ls_sadad_log to gt_sadad_log.
   endif.


   PERFORM show_alv.

endform.


form show_alv.

  PERFORM create_fcat.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program                = sy-repid
*      i_callback_pf_status_set          = 'PF_STATUS'
      i_callback_user_command           = 'USER_COMMAND'
*      is_layout                         = wa_layout
      it_fieldcat                       = gt_fcat
     TABLES
       t_outtab                          = gt_sadad_log
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2
             .

endform.

FORM user_command USING p_ucomm TYPE sy-ucomm
                        p_selfield TYPE slis_selfield.

data: lv_answer type c.

  case p_ucomm.
    when '&IC1'.
      if p_selfield-fieldname = 'STATUS'.
        read table gt_sadad_log ASSIGNING FIELD-SYMBOL(<fs_sadad_log>) INDEX p_selfield-TABINDEX.
        if sy-subrc = 0.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question               =  'آيا از تغيير وضعيت مطمئن هستيد؟'
           IMPORTING
             ANSWER                      = lv_answer
                    .
           if lv_answer = '1'.
             <fs_sadad_log>-status = '4'.
             modify zsadad_log from <fs_sadad_log>.
             commit work.
             p_selfield-REFRESH = 'X'.
           endif.

        endif.
      endif.

  ENDCASE.

endform.


form create_fcat.
  data: ls_fcat type slis_fieldcat_alv.
  refresh: gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'TAXID'.
  ls_fcat-seltext_l = 'کد يکتا'.
  ls_fcat-seltext_m = 'کد يکتاي مالياتي'.
  ls_fcat-seltext_s = 'کد يکتاي مالياتي'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'GUID'.
  ls_fcat-seltext_l = 'GUID'.
  ls_fcat-seltext_m = 'GUID'.
  ls_fcat-seltext_s = 'GUID'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'XBLNR'.
  ls_fcat-seltext_l = 'صورتحساب'.
  ls_fcat-seltext_m = 'صورتحساب'.
  ls_fcat-seltext_s = 'صورتحساب'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'SERIAL'.
  ls_fcat-seltext_l = 'سريال'.
  ls_fcat-seltext_m = 'سريال'.
  ls_fcat-seltext_s = 'سريال'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'FKDAT'.
  ls_fcat-seltext_l = 'تاريخ ص'.
  ls_fcat-seltext_m = 'تاريخ صورتحساب'.
  ls_fcat-seltext_s = 'تاريخ صورتحساب'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ZKTOKD'.
  ls_fcat-seltext_l = 'نوع ص'.
  ls_fcat-seltext_m = 'نوع صورتحساب'.
  ls_fcat-seltext_s = 'نوع صورتحساب'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'SEND_DATE'.
  ls_fcat-seltext_l = 'ت ارسال'.
  ls_fcat-seltext_m = 'تاريخ ارسال'.
  ls_fcat-seltext_s = 'تاريخ ارسال'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'STATUS'.
  ls_fcat-seltext_l = 'وضعيت'.
  ls_fcat-seltext_m = 'وضعيت'.
  ls_fcat-seltext_s = 'وضعيت'.
  ls_fcat-hotspot = 'X'.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR1'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR2'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR3'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR4'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR5'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR6'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR7'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR8'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.

  clear: ls_fcat.
  ls_fcat-fieldname = 'ERROR9'.
  ls_fcat-seltext_l = ls_fcat-fieldname.
  ls_fcat-seltext_m = ls_fcat-seltext_l.
  ls_fcat-seltext_s = ls_fcat-seltext_l.
  append ls_fcat to gt_fcat.


endform.
