class ZCL_IM_BRZ_ADDRESS_SEARCH definition
  public
  final
  create public .

public section.

  interfaces IF_EX_ADDRESS_SEARCH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BRZ_ADDRESS_SEARCH IMPLEMENTATION.


  METHOD if_ex_address_search~address_search.
    BREAK rafighdoust.
    DATA ls_address TYPE adsrchline.
    DATA ls_adkey   TYPE adkey_indx.

    LOOP AT im_t_object_types INTO DATA(ls_object_type) WHERE main_obj = 'X' AND appl_table = 'BUT000'.
    ENDLOOP.


    IF sy-subrc = 0.

      " check if only name and mail are given --> if yes --> do nothing
      LOOP AT im_t_search_fields INTO ls_address WHERE fieldname = 'NAME1'.
        IF strlen( ls_address-content ) > 0.
          SELECT addrnumber FROM adrc INTO CORRESPONDING FIELDS OF TABLE ex_t_search_result WHERE name1 LIKE ls_address-content.
        ENDIF.
      ENDLOOP.
      LOOP AT im_t_search_fields INTO ls_address WHERE fieldname = 'TEL_NUMBER'.
        IF strlen( ls_address-content ) > 0.
          SELECT addrnumber FROM adrc INTO CORRESPONDING FIELDS OF TABLE ex_t_search_result WHERE tel_number LIKE ls_address-content.
        ENDIF.
      ENDLOOP.

      LOOP AT ex_t_search_result INTO ls_adkey.
        ls_adkey-addr_type = '1'.
        MODIFY ex_t_search_result FROM ls_adkey.
      ENDLOOP.

* delete the duplicate entries
      SORT ex_t_search_result BY addrnumber persnumber addr_type.
      DELETE ADJACENT DUPLICATES FROM  ex_t_search_result COMPARING addrnumber persnumber addr_type.




* delete record that is being edited/changed. This record need not to be displayed as duplicate
      IF im_search_mode NE 'S'.
        DELETE ex_t_search_result WHERE addrnumber = im_current_address_key-addrnumber AND
                                        addr_type  = im_current_address_key-addr_type.
      ENDIF.

      DESCRIBE TABLE ex_t_search_result LINES ex_number_of_hits.

      IF ex_number_of_hits > 1.
        ex_search_status = '01'.
      ENDIF.

    ENDIF.




  ENDMETHOD.


  method IF_EX_ADDRESS_SEARCH~INITIALIZE.
  endmethod.


  method IF_EX_ADDRESS_SEARCH~IS_COMPLETE.
  endmethod.


  method IF_EX_ADDRESS_SEARCH~READ_INDEX_FIELD_LIST.
    DATA LS_FIELD TYPE ADFLDLINE.

    ls_field-fieldname = 'NAME1'.
    ls_field-tablename = 'ADRC'.

    APPEND ls_field to EX_FIELD_LIST.

    ls_field-fieldname = 'TEL_NUMBER'.
    ls_field-tablename = 'ADRC'.

    APPEND ls_field to EX_FIELD_LIST.
  endmethod.
ENDCLASS.
