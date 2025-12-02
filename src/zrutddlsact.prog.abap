*&---------------------------------------------------------------------*
*& Report  REPORT ZRUTDDLSACT_750_2023690.
*&
*&---------------------------------------------------------------------*
*& Activated Ddl Sources
*&
*&---------------------------------------------------------------------*

REPORT ZRUTDDLSACT.

  tables: ddldependency.
  data: ddlname_tab type range of ddldependency-ddlname,
        ddlname_wa  like line of ddlname_tab,
        ddlnames    type table of ddlname.
  data: is_shadow type ABAP_BOOL,
        mode      type dcmode value 'O',
        inactive  type ddinactive,
        begin of objrange,
          sign(1),
          option(2),
          low type ddlname,
          high type ddlname,
        end of objrange,
        logname like ddmass-logname value 'DDLSACT&DATE&&TIME&',
        splevel type n length 4.

  selection-screen begin of block log with frame title text-001.
    parameters: logshow  like ddmass-logshow default 'X'.
  selection-screen end of block log.
  select-options: ddlsname for ddldependency-ddlname.
  parameters: all type ABAP_BOOL as checkbox default ABAP_false.

  start-of-selection.

  if syst-saprl <> '751'.
    if syst-saprl = '740'.
      call function 'OCS_GET_COMPONENT_STATE'
        exporting
          iv_component                   = 'SAP_BASIS'
        IMPORTING
          EV_COMP_LEVEL                  = splevel
        EXCEPTIONS
          UNKNOWN_COMPONENT              = 1
          OTHERS                         = 2.
      if syst-subrc <> 0 or splevel < 11.
        write:/ 'System release should be 750 or 740 SP >= 11 to execute the program: '.
        write:/ 'Use ZRUTDDLSACT_740_2023690 of note 2023690 instead'.
        exit.
      endif.
    else.
      write:/ 'Wrong release for usage of note 2023690'.
      exit.
    endif.
  endif.

  call function 'UPG_IS_SHADOW_SYSTEM'
    IMPORTING
      EV_SHADOW = is_shadow.
  if is_shadow = ABAP_true.
    write:/ 'Program cannot be executed in shadow system'.
    return.
  endif.

  "Select Ddl sources to activate
  if all = ABAP_true and lines( ddlsname ) = 0.
    ddlsname-sign   = 'I'.
    ddlsname-option = 'CP'.
    ddlsname-low    = '*'.
    append ddlsname.
  endif.

  if lines( ddlsname ) > 0.
    if all = ABAP_true.
      select ddlname from ddldependency into table ddlnames
                                        where objectname in ddlsname.
      select ddlname from ddddlsrc appending table ddlnames
                                   where ddlname in ddlsname.
    else.
      select ddlname from ddldependency into table ddlnames
                                        where objectname in ddlsname
                                        and   state = 'N'.
      select ddlname from ddddlsrc appending table ddlnames
                                   where ddlname in ddlsname
                                   and   as4local = 'N'.
    endif.
  endif.
  sort ddlnames.
  delete adjacent duplicates from ddlnames.
  ddlname_wa-sign = 'I'.
  ddlname_wa-option = 'EQ'.
  loop at ddlnames into data(ddlname).
    ddlname_wa-low = ddlname.
    append ddlname_wa to ddlname_tab.
  endloop.
  if lines( ddlnames ) = 0.
    write:/ 'No Ddl Sources found'.
    exit.
  endif.

  submit radmasg0_c3 and return with ddlsname in ddlname_tab
                                with inactive = inactive
                                with ddmode   = mode
                                with authchk  = ''
                                with impchk   = ''
                                with logname  = logname
                                with database = 'X '
                                with file     = ' '      "File-log
                                with logshow  = logshow.
