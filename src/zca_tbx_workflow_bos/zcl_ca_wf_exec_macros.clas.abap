"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow: Execution of workflow/BO macros</p>
CLASS zcl_ca_wf_exec_macros DEFINITION PUBLIC
                                       CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Business object short descriptions</p>
      ms_descr    TYPE tojtt     READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! No buffering is implemented due to the fact that the FMs executed by the macros
      "! already buffer instances. Beside that any method of this class refresh
      "! respectively recreate the instance automatically if necessary.
      "!
      "! @parameter is_lpor               | <p class="shorttext synchronized" lang="en">Business object type and key</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      constructor
        IMPORTING
          is_lpor TYPE sibflporb
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Execute BO method</p>
      "!
      "! <p>Prepare the container with the methods REFRESH_CONTAINER and GET_/_SET_CONTAINER_ELEMENT/TABLE.</p>
      "!
      "! @parameter iv_method             | <p class="shorttext synchronized" lang="en">Method name</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      call_method
        IMPORTING
          iv_method TYPE swo_verb
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Delete element from container</p>
      "!
      "! @parameter iv_element_name       | <p class="shorttext synchronized" lang="en">Name of element (parameter)</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      delete_element_from_container
        IMPORTING
          iv_element_name TYPE swc_elem
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get element from the container</p>
      "!
      "! @parameter iv_element_name       | <p class="shorttext synchronized" lang="en">Name of element (parameter)</p>
      "! @parameter ev_element_value      | <p class="shorttext synchronized" lang="en">Value of the element</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_element_from_container
        IMPORTING
          iv_element_name  TYPE swc_elem
        EXPORTING
          ev_element_value TYPE any
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get table from the container</p>
      "!
      "! @parameter iv_element_name       | <p class="shorttext synchronized" lang="en">Name of element (parameter)</p>
      "! @parameter et_element_value      | <p class="shorttext synchronized" lang="en">Table value of the element</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_table_from_container
        IMPORTING
          iv_element_name  TYPE swc_elem
        EXPORTING
          et_element_value TYPE STANDARD TABLE
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get BO instance</p>
      "!
      "! @parameter rs_instance           | <p class="shorttext synchronized" lang="en">Object instance key for macro execution</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_instance
        RETURNING
          VALUE(rs_instance) TYPE obj_record
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get BO key unstructured</p>
      "!
      "! @parameter rv_key                | <p class="shorttext synchronized" lang="en">Business object key</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_key
        RETURNING
          VALUE(rv_key) TYPE sibfboriid
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get property of BO</p>
      "!
      "! @parameter iv_attribute_name     | <p class="shorttext synchronized" lang="en">Attribute name</p>
      "! @parameter ev_attribute_value    | <p class="shorttext synchronized" lang="en">Data value to property</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_property
        IMPORTING
          iv_attribute_name  TYPE swo_verb
        EXPORTING
          ev_attribute_value TYPE any
          eo_attribute_bo    TYPE REF TO zcl_ca_wf_exec_macros
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get BO property of BO</p>
      "!
      "! @parameter iv_attribute_name     | <p class="shorttext synchronized" lang="en">Attribute name</p>
      "! @parameter eo_attribute_value    | <p class="shorttext synchronized" lang="en">Instance of this class for the requested object</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_bo_property
        IMPORTING
          iv_attribute_name  TYPE swo_verb
        EXPORTING
          eo_attribute_value TYPE REF TO zcl_ca_wf_exec_macros
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get table property of BO</p>
      "!
      "! @parameter iv_attribute_name     | <p class="shorttext synchronized" lang="en">Attribute name</p>
      "! @parameter et_attribute_value    | <p class="shorttext synchronized" lang="en">Table value to property</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      get_table_property
        IMPORTING
          iv_attribute_name  TYPE swo_verb
        EXPORTING
          et_attribute_value TYPE STANDARD TABLE
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Refresh instance</p>
      "!
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      refresh
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Refresh container</p>
      refresh_container,

      "! <p class="shorttext synchronized" lang="en">Set element into the container</p>
      "!
      "! @parameter iv_element_name       | <p class="shorttext synchronized" lang="en">Name of element (parameter)</p>
      "! @parameter iv_element_value      | <p class="shorttext synchronized" lang="en">Value for element</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      set_element_into_container
        IMPORTING
          iv_element_name  TYPE swc_elem
          iv_element_value TYPE any
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Set table value into the container</p>
      "!
      "! @parameter iv_element_name       | <p class="shorttext synchronized" lang="en">Name of element (parameter)</p>
      "! @parameter it_element_value      | <p class="shorttext synchronized" lang="en">Table value for element</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      set_table_into_container
        IMPORTING
          iv_element_name  TYPE swc_elem
          it_element_value TYPE STANDARD TABLE
        RAISING
          zcx_ca_wf_exec_macros.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="de">Container for method and event call - refresh before using</p>
      mt_container TYPE swconttab,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Business object type and key</p>
      ms_lpor      TYPE sibflporb,
      "! <p class="shorttext synchronized" lang="en">Object instance key for macro execution</p>
      ms_instance  TYPE obj_record.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create BO object reference</p>
      "!
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Macro execution errors</p>
      create_bo
        RAISING
          zcx_ca_wf_exec_macros.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_wf_exec_macros IMPLEMENTATION.

  METHOD call_method.
    "-----------------------------------------------------------------*
    "   Execute BO method - Prepare MT_CONTAINER with methods
    "-----------------------------------------------------------------*
    refresh( ).

    swc_call_method  ms_instance  iv_method   mt_container.
    IF sy-subrc NE 0.
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                iv_class    = CONV #( ms_lpor-typeid )
                                iv_method   = CONV #( iv_method )
                                is_msg      = CORRESPONDING #( syst )
                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "call_method


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
*    "Key value is set?
*    IF is_lpor-instid IS INITIAL.
*      "Parameter '&1' has invalid value '&2'
*      RAISE EXCEPTION TYPE zcx_ca_wf_exec_macros
*        EXPORTING
*          textid   = zcx_ca_wf_exec_macros=>param_invalid
*          mv_msgty = c_msgty_e
*          mv_msgv1 = 'IS_LPOR-INSTID'
*          mv_msgv2 = 'SPACE' ##no_text.
*    ENDIF.

    "Check business object
    SELECT SINGLE * INTO  @DATA(ls_tojtb)
                    FROM  tojtb
                    WHERE name   EQ @is_lpor-typeid
                      AND active EQ @abap_true.
    IF sy-subrc NE 0.
      "SAP business object &1 does not exist or is not activated
      RAISE EXCEPTION TYPE zcx_ca_wf_exec_macros
        EXPORTING
          textid   = zcx_ca_wf_exec_macros=>bo_not_exist
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( is_lpor-typeid ).
    ENDIF.

    "Get short descriptions
    SELECT SINGLE * INTO  @ms_descr
                    FROM  tojtt
                    WHERE name     EQ @is_lpor-typeid
                      AND language EQ @sy-langu.
    IF sy-subrc NE 0.
      "Nothing found in english -> use german, use english in any other case
      DATA(lv_langu) = SWITCH spras( sy-langu
                                       WHEN 'E' THEN 'D'
                                       ELSE 'E' ) ##no_text.
      SELECT SINGLE * INTO  @ms_descr
                      FROM  tojtt
                      WHERE name     EQ @is_lpor-typeid
                        AND language EQ @lv_langu.
      IF sy-subrc NE 0.
        ms_descr-name  = is_lpor-typeid.
        ms_descr-stext = ms_descr-ntext = 'No description found'(ndf).
      ENDIF.
    ENDIF.

    ms_lpor = is_lpor.

    create_bo( ).
  ENDMETHOD.                    "constructor


  METHOD create_bo.
    "-----------------------------------------------------------------*
    "   Create BO object reference
    "-----------------------------------------------------------------*
    IF ms_lpor-instid IS INITIAL.
      RETURN.
    ENDIF.

    swc_create_object  ms_instance  ms_lpor-typeid   ms_lpor-instid.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                          iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                          iv_function = |SWC_CREATE_OBJECT RBO_{ ms_lpor-typeid }|
                                                          is_msg      = CORRESPONDING #( syst )
                                                          iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_bo


  METHOD delete_element_from_container.
    "-----------------------------------------------------------------*
    "   Delete element from container
    "-----------------------------------------------------------------*
    swc_delete_element  mt_container  iv_element_name.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_class    = CONV #( ms_lpor-typeid )
                                                        iv_method   = CONV #( iv_element_name )
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "delete_element_from_container


  METHOD get_element_from_container.
    "-----------------------------------------------------------------*
    "   Get value to element in container
    "-----------------------------------------------------------------*
    CLEAR ev_element_value.
    swc_get_element  mt_container  iv_element_name   ev_element_value.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_class    = CONV #( ms_lpor-typeid )
                                                        iv_method   = CONV #( iv_element_name )
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_element_from_container


  METHOD get_table_from_container.
    "-----------------------------------------------------------------*
    "   Get table to element in container
    "-----------------------------------------------------------------*
    CLEAR et_element_value.
    swc_get_table  mt_container  iv_element_name   et_element_value.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_class    = CONV #( ms_lpor-typeid )
                                                        iv_method   = CONV #( iv_element_name )
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_table_from_container


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get BO instance
    "-----------------------------------------------------------------*
    refresh( ).

    rs_instance = ms_instance.
  ENDMETHOD.                    "get_instance


  METHOD get_key.
    "-----------------------------------------------------------------*
    "   Get BO key unstructured
    "-----------------------------------------------------------------*
    refresh( ).

    swc_get_object_key  ms_instance  rv_key.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_function = 'SWC_GET_OBJECT_KEY'
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_key


  METHOD get_property.
    "-----------------------------------------------------------------*
    "   Get property of BO
    "-----------------------------------------------------------------*
    refresh( ).

    CLEAR ev_attribute_value.
    swc_get_property  ms_instance  iv_attribute_name   ev_attribute_value.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_class    = CONV #( ms_lpor-typeid )
                                                        iv_method   = CONV #( iv_attribute_name )
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_property


  METHOD get_bo_property.
    "-----------------------------------------------------------------*
    "   Get BO property of BO
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor        TYPE sibflporb,
      ls_bo_instance TYPE obj_record.

    refresh( ).

    CLEAR eo_attribute_value.
    swc_get_property  ms_instance  iv_attribute_name  ls_bo_instance.
    IF sy-subrc EQ 0.
      swc_get_object_key   ls_bo_instance   ls_lpor-instid.
      swc_get_object_type  ls_bo_instance   ls_lpor-typeid.
      ls_lpor-catid = swfco_objtype_bor.

      eo_attribute_value = NEW zcl_ca_wf_exec_macros( ls_lpor ).

    ELSE.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_class    = CONV #( ms_lpor-typeid )
                                                        iv_method   = CONV #( iv_attribute_name )
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_bo_property


  METHOD get_table_property.
    "-----------------------------------------------------------------*
    "   Get BO table property
    "-----------------------------------------------------------------*
    refresh( ).

    CLEAR et_attribute_value.
    swc_get_table_property  ms_instance  iv_attribute_name   et_attribute_value.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_class    = CONV #( ms_lpor-typeid )
                                                        iv_method   = CONV #( iv_attribute_name )
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_table_property


  METHOD refresh_container.
    "-----------------------------------------------------------------*
    "   Refresh container
    "-----------------------------------------------------------------*
    CLEAR mt_container.
  ENDMETHOD.                    "refresh_container


  METHOD refresh.
    "-----------------------------------------------------------------*
    "   Refresh instance
    "-----------------------------------------------------------------*
    IF ms_lpor-instid IS INITIAL.
      RETURN.
    ENDIF.

    swc_refresh_object  ms_instance.
    IF sy-subrc NE 0.
      TRY.
          create_bo( ).

        CATCH zcx_ca_wf_exec_macros INTO DATA(lx_catched).
          DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                            iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                            iv_function = |SWC_REFRESH_OBJECT RBO_{ ms_lpor-typeid }|
                                                            ix_error    = lx_catched ) )  ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "refresh


  METHOD set_element_into_container.
    "-----------------------------------------------------------------*
    "   Set element into the container of this instance
    "-----------------------------------------------------------------*
    swc_set_element mt_container  iv_element_name  iv_element_value.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_function = 'SWC_ELEMENT_SET'
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_element_into_container


  METHOD set_table_into_container.
    "-----------------------------------------------------------------*
    "   Set table into the container of this instance
    "-----------------------------------------------------------------*
    swc_set_element mt_container  iv_element_name  it_element_value.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_exec_macros( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                                        iv_function = 'SWC_ELEMENT_SET'
                                                        is_msg      = CORRESPONDING #( syst )
                                                        iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "set_table_into_container

ENDCLASS.
