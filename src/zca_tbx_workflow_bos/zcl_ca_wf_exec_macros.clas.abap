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
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      constructor
        IMPORTING
          is_lpor TYPE sibflporb
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Execute BO method</p>
      "!
      "! @parameter iv_method             | <p class="shorttext synchronized" lang="en">Method name</p>
      "! @parameter ct_container          | <p class="shorttext synchronized" lang="en">Container with export AND import parameter</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      call_method
        IMPORTING
          iv_method    TYPE swo_verb
        CHANGING
          ct_container TYPE swconttab OPTIONAL
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get value to element in container</p>
      "!
      "! @parameter iv_elem_name          | <p class="shorttext synchronized" lang="en">Element name</p>
      "! @parameter ev_value              | <p class="shorttext synchronized" lang="en">Data value to element</p>
      "! @parameter ct_container          | <p class="shorttext synchronized" lang="en">Container with export AND import parameter</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      get_element
        IMPORTING
          iv_elem_name TYPE swo_verb
        EXPORTING
          ev_value     TYPE any
        CHANGING
          ct_container TYPE swconttab
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get BO instance</p>
      "!
      "! @parameter rs_instance           | <p class="shorttext synchronized" lang="en">Object instance key for macro execution</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      get_instance
        RETURNING
          VALUE(rs_instance) TYPE obj_record
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get BO key unstructured</p>
      "!
      "! @parameter rv_key                | <p class="shorttext synchronized" lang="en">Business object key</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      get_key
        RETURNING
          VALUE(rv_key) TYPE sibfboriid
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Get BO property</p>
      "!
      "! @parameter iv_attr_name          | <p class="shorttext synchronized" lang="en">Attribute name</p>
      "! @parameter iv_is_bo              | <p class="shorttext synchronized" lang="en">1 = Property is a business object</p>
      "! @parameter iv_is_table           | <p class="shorttext synchronized" lang="en">1 = Property is a table type</p>
      "! @parameter ev_value              | <p class="shorttext synchronized" lang="en">Data value to property</p>
      "! @parameter et_table              | <p class="shorttext synchronized" lang="en">Table value to property</p>
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      get_property
        IMPORTING
          iv_attr_name TYPE swo_verb
          iv_is_bo     TYPE dml_boolean  DEFAULT abap_false
          iv_is_table  TYPE dml_boolean  DEFAULT abap_false
        EXPORTING
          ev_value     TYPE any
          et_table     TYPE STANDARD TABLE
        RAISING
          zcx_ca_wf_exec_macros,

      "! <p class="shorttext synchronized" lang="en">Refresh instance</p>
      "!
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      refresh
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
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Business object type and key</p>
      ms_lpor     TYPE sibflporb,
      "! <p class="shorttext synchronized" lang="en">Object instance key for macro execution</p>
      ms_instance TYPE obj_record.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Create BO object reference</p>
      "!
      "! @raising   zcx_ca_wf_exec_macros | <p class="shorttext synchronized" lang="en">Common WF exception: Macro execution errors</p>
      create_bo
        RAISING
          zcx_ca_wf_exec_macros.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.                     "zcl_ca_wf_exec_macros  DEFINITION


CLASS zcl_ca_wf_exec_macros IMPLEMENTATION.

  METHOD call_method.
    "-----------------------------------------------------------------*
    "   Execute BO method
    "-----------------------------------------------------------------*
    refresh( ).

    swc_call_method  ms_instance  iv_method   ct_container.
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
    "Key value is set?
    IF is_lpor-instid IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_exec_macros
        EXPORTING
          textid   = zcx_ca_wf_exec_macros=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR-INSTID'
          mv_msgv2 = 'SPACE' ##no_text.
    ENDIF.

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
    swc_create_object  ms_instance  ms_lpor-typeid   ms_lpor-instid.
    IF sy-subrc NE 0.
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                iv_function = |SWC_CREATE_OBJECT RBO_{ ms_lpor-typeid }|
                                is_msg      = CORRESPONDING #( syst )
                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_bo


  METHOD get_element.
    "-----------------------------------------------------------------*
    "   Get value to element in container
    "-----------------------------------------------------------------*
    CLEAR ev_value.
    swc_get_element  ct_container  iv_elem_name   ev_value.
    IF sy-subrc NE 0.
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                iv_class    = CONV #( ms_lpor-typeid )
                                iv_method   = CONV #( iv_elem_name )
                                is_msg      = CORRESPONDING #( syst )
                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_element


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
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
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
    "   Get BO property
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor        TYPE sibflporb,
      ls_bo_instance TYPE obj_record.

    refresh( ).

    CLEAR ev_value.

    CASE abap_true.
      WHEN iv_is_table.
        IF et_table IS NOT SUPPLIED.
          RAISE EXCEPTION TYPE zcx_ca_wf_exec_macros
            EXPORTING
              textid   = zcx_ca_wf_exec_macros=>param_not_supplied
              mv_msgty = c_msgty_e.
        ENDIF.

        swc_get_table_property  ms_instance  iv_attr_name   et_table.

      WHEN iv_is_bo.
        "BO attribute is requested -> get key values and create an instance of this class
        swc_get_property     ms_instance  iv_attr_name    ls_bo_instance.
        IF sy-subrc EQ 0.
          swc_get_object_key   ls_bo_instance   ls_lpor-instid.
          swc_get_object_type  ls_bo_instance   ls_lpor-typeid.
          ls_lpor-catid = swfco_objtype_bor.

          ev_value = NEW zcl_ca_wf_exec_macros( ls_lpor ).
        ENDIF.

      WHEN OTHERS.
        swc_get_property  ms_instance  iv_attr_name   ev_value.
    ENDCASE.

    IF sy-subrc NE 0.
      DATA(lx_error) =
            CAST zcx_ca_wf_exec_macros(
                     zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                iv_class    = CONV #( ms_lpor-typeid )
                                iv_method   = CONV #( iv_attr_name )
                                is_msg      = CORRESPONDING #( syst )
                                iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_property


  METHOD refresh.
    "-----------------------------------------------------------------*
    "   Refresh instance
    "-----------------------------------------------------------------*
    swc_refresh_object  ms_instance.
    IF sy-subrc NE 0.
      TRY.
          create_bo( ).

        CATCH zcx_ca_wf_exec_macros INTO DATA(lx_catched).
          DATA(lx_error) =
                CAST zcx_ca_wf_exec_macros(
                         zcx_ca_error=>create_exception(
                                    iv_excp_cls = zcx_ca_wf_exec_macros=>c_zcx_ca_wf_exec_macros
                                    iv_function = |SWC_REFRESH_OBJECT RBO_{ ms_lpor-typeid }|
                                    ix_error    = lx_catched ) )  ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDTRY.
    ENDIF.
  ENDMETHOD.                    "refresh

ENDCLASS.                     "zcl_ca_wf_exec_macros  IMPLEMENTATION


