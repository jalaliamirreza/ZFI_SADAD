*&---------------------------------------------------------------------*
*& Report ZBRZ_FI_UPDATE_BP_ROEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZBRZ_FI_UPDATE_BP_ROEL.

TABLES: ukmbp_cms.

SELECT-OPTIONS p_id for ukmbp_cms-partner.
data lt_data TYPE STANDARD TABLE OF ukmbp_cms.

START-OF-SELECTION.

select * from ukmbp_cms INTO TABLE lt_data WHERE partner in p_id.

  PERFORM set_missing_bupa_roles TABLES lt_data.


FORM set_missing_bupa_roles TABLES itab STRUCTURE ukmbp_cms.
  DATA: ls_cms LIKE LINE OF itab.
  DATA: lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  CONSTANTS: lrole LIKE bapibus1006_bproles-partnerrole VALUE 'UKM000',
             lrolecat LIKE bapibus1006_bproles-partnerrolecategory
                                                       VALUE 'UKM000'.
  LOOP AT itab INTO ls_cms.
    CALL FUNCTION 'BAPI_BUPA_ROLE_EXIST_CHECK_2'
      EXPORTING
        businesspartner             = ls_cms-partner
        businesspartnerrolecategory = lrolecat
*       ALL_BUSINESSPARTNERROLES    = ' '
        businesspartnerrole         = lrole
*       DIFFERENTIATIONTYPEVALUE    =
        validdate                   = sy-datlo
      TABLES
        return                      = lt_return.
    .
    IF NOT lt_return[] IS INITIAL.

      CALL FUNCTION 'BAPI_BUPA_ROLE_ADD_2'
        EXPORTING
          businesspartner             = ls_cms-partner
          businesspartnerrolecategory = lrole
*         ALL_BUSINESSPARTNERROLES    = ' '
          businesspartnerrole         = lrolecat
*         DIFFERENTIATIONTYPEVALUE    =
*         VALIDFROMDATE               = SY-DATLO
*         VALIDUNTILDATE              = '99991231'
        TABLES
          return                      = lt_return.
    ENDIF.          .
    commit WORK.
  ENDLOOP.

ENDFORM.
