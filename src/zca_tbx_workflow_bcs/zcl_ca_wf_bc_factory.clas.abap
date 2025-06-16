"! <p class="shorttext synchronized" lang="en">CA-TBX: Factory + buffering of Business Classes (= Workflow)</p>
"!
"! <p>This class prerequisite that the workflow class implements the interface ZIF_CA_WORKFLOW. This interface
"! contains the interface IF_WORKFLOW, necessary for workflow objects (classes). Furthermore, it is expected that
"! only one key field is declared. If the key is composed of multiple fields (like the FI document header) then
"! use a flat structure that contains key fields in the correct order.</p>
CLASS zcl_ca_wf_bc_factory DEFINITION PUBLIC
                                      CREATE PRIVATE
                                      FINAL.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_wf_bc_factory.

*   a l i a s e s
    ALIASES:
*     Methods
      create_by_key          FOR zif_ca_wf_bc_factory~create_by_key,
      create_by_lpor         FOR zif_ca_wf_bc_factory~create_by_lpor,
      extract_key_from_input FOR zif_ca_wf_bc_factory~extract_key_from_input,
      release_from_buffer    FOR zif_ca_wf_bc_factory~release_from_buffer.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create a singleton instance of the factory</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Singleton instance of this factory</p>
      create_singleton
        RETURNING
          VALUE(result) TYPE REF TO zif_ca_wf_bc_factory.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer.
        INCLUDE TYPE sibflpor AS s_lpor.
    TYPES:
        o_persistent TYPE REF TO zif_ca_workflow,
      END   OF ty_s_buffer,
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      ty_t_buffer TYPE HASHED TABLE OF ty_s_buffer
                                       WITH UNIQUE KEY primary_key COMPONENTS s_lpor,

      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer_techn_descr,
        "! <p class="shorttext synchronized" lang="en">Object (= class) name</p>
        typeid      TYPE sibftypeid,
        "! <p class="shorttext synchronized" lang="en">Techn. descriptor of the object (class)</p>
        o_class     TYPE REF TO cl_abap_classdescr,
        "! <p class="shorttext synchronized" lang="en">Techn. descriptor of the object key (field or structure)</p>
        o_key_field TYPE REF TO cl_abap_datadescr,
      END   OF ty_s_buffer_techn_descr,
      "! <p class="shorttext synchronized" lang="en">Buffer with technical descriptions</p>
      ty_t_buffer_techn_descr TYPE HASHED TABLE OF ty_s_buffer_techn_descr
                                              WITH UNIQUE KEY primary_key COMPONENTS typeid.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Methodname of Constructor</p>
      c_methname_constructor TYPE abap_methname VALUE 'CONSTRUCTOR' ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Singleton instance of the this factory</p>
      mo_singleton          TYPE REF TO zif_ca_wf_bc_factory,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer             TYPE ty_t_buffer,
      "! <p class="shorttext synchronized" lang="en">Buffer with technical descriptions</p>
      mt_buffer_techn_descr TYPE ty_t_buffer_techn_descr.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Compose workflow instance key</p>
      "!
      "! @parameter iv_key  | <p class="shorttext synchronized" lang="en">Key as character string</p>
      "! @parameter iv_type | <p class="shorttext synchronized" lang="en">Name of the class to be created</p>
      "! @parameter result  | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      compose_lpor
        IMPORTING
          iv_key        TYPE sibfinstid
          iv_type       TYPE sibftypeid
        RETURNING
          VALUE(result) TYPE sibflpor,

      "! <p class="shorttext synchronized" lang="en">Create an instance of requested type</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      create_instance
        IMPORTING
          is_lpor       TYPE sibflpor
        RETURNING
          VALUE(result) TYPE REF TO zif_ca_workflow
        RAISING
          zcx_ca_dbacc
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Find an instance to the workflow instance key in buffer</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      find_in_buffer
        IMPORTING
          is_lpor       TYPE sibflpor
        RETURNING
          VALUE(result) TYPE REF TO zif_ca_workflow
        RAISING
          zcx_ca_dbacc
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get container with key fields of requested object</p>
      "!
      "! @parameter iv_object_type       | <p class="shorttext synchronized" lang="en">Workflow instance type</p>
      "! @parameter result               | <p class="shorttext synchronized" lang="en">Container with key fields</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      get_container_with_key_fields
        IMPORTING
          iv_object_type TYPE sibftypeid
        RETURNING
          VALUE(result)  TYPE REF TO if_swf_cnt_container
        RAISING
          zcx_ca_wf_bc_factory,

      "! <p class="shorttext synchronized" lang="en">Get technical descriptors to workflow class</p>
      "!
      "! @parameter iv_object_type       | <p class="shorttext synchronized" lang="en">Workflow instance type</p>
      "! @parameter result               | <p class="shorttext synchronized" lang="en">Technical descriptors to workflow class</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      get_techn_descriptors
        IMPORTING
          iv_object_type TYPE sibftypeid
        RETURNING
          VALUE(result)  TYPE REF TO ty_s_buffer_techn_descr
        RAISING
          zcx_ca_wf_bc_factory,

      "! <p class="shorttext synchronized" lang="en">Prepare parameters for constr. call of the workflow class</p>
      "!
      "! @parameter ir_techn_descriptors | <p class="shorttext synchronized" lang="en">Technical descriptors to workflow class</p>
      "! @parameter ir_key_field         | <p class="shorttext synchronized" lang="en">Reference of key value</p>
      "! @parameter result               | <p class="shorttext synchronized" lang="en">Parameters of CONSTRUCTOR</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      prepare_params_4_constructor
        IMPORTING
          ir_techn_descriptors TYPE REF TO ty_s_buffer_techn_descr
          ir_key_field         TYPE REF TO data
        RETURNING
          VALUE(result)        TYPE abap_parmbind_tab
        RAISING
          zcx_ca_wf_bc_factory,

      "! <p class="shorttext synchronized" lang="en">Get technical description of the workflow class key</p>
      "!
      "! @parameter iv_object_type       | <p class="shorttext synchronized" lang="en">Workflow instance type</p>
      "! @parameter result               | <p class="shorttext synchronized" lang="en">Technical description of the workflow class key</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      get_techn_descr_of_key_field
        IMPORTING
          iv_object_type TYPE sibftypeid
        RETURNING
          VALUE(result)  TYPE REF TO cl_abap_datadescr
        RAISING
          zcx_ca_wf_bc_factory,

      "! <p class="shorttext synchronized" lang="en">Get details of method CONSTRUCTOR of the workflow class</p>
      "!
      "! @parameter io_class_descr       | <p class="shorttext synchronized" lang="en">Techn. class descriptor of workflow object type</p>
      "! @parameter result               | <p class="shorttext synchronized" lang="en">Technical method description of the constructor method</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      get_details_of_constructor
        IMPORTING
          io_class_descr TYPE REF TO cl_abap_classdescr
        RETURNING
          VALUE(result)  TYPE REF TO abap_methdescr
        RAISING
          zcx_ca_wf_bc_factory,

      "! <p class="shorttext synchronized" lang="en">Check whether the key is defined in a single data object</p>
      "!
      "! @parameter it_key_fields        | <p class="shorttext synchronized" lang="en">Key fields of object</p>
      "! @parameter iv_object_type       | <p class="shorttext synchronized" lang="en">Workflow instance type</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      has_only_one_key_field_defined
        IMPORTING
          it_key_fields  TYPE swfdnamtab
          iv_object_type TYPE sibftypeid
        RAISING
          zcx_ca_wf_bc_factory,

      "! <p class="shorttext synchronized" lang="en">Check whether the passed BC type is a workflow class</p>
      "!
      "! @parameter io_class_descr       | <p class="shorttext synchronized" lang="en">Techn. description of the workflow class type (= class name)</p>
      "! @raising   zcx_ca_wf_bc_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      is_obj_type_a_business_class
        IMPORTING
          io_class_descr TYPE REF TO cl_abap_classdescr
        RAISING
          zcx_ca_wf_bc_factory.

ENDCLASS.                     "zcl_ca_wf_bcs_factory  DEFINITION



CLASS zcl_ca_wf_bc_factory IMPLEMENTATION.

  METHOD compose_lpor.
    "-----------------------------------------------------------------*
    "   Compose workflow instance key
    "-----------------------------------------------------------------*
    result = VALUE #( instid = iv_key
                      typeid = to_upper( iv_type )
                      catid  = swfco_objtype_cl ).
  ENDMETHOD.                    "compose_lpor


  METHOD create_instance.
    "-----------------------------------------------------------------*
    "   Create an instance of requested type
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_key_field  TYPE REF TO data.

    DATA(lr_techn_descriptors) = get_techn_descriptors( is_lpor-typeid ).
    is_obj_type_a_business_class( lr_techn_descriptors->o_class ).

    "Create a correct typed key value for the constructor execution
    CREATE DATA lr_key_field TYPE HANDLE lr_techn_descriptors->o_key_field.
    ASSIGN lr_key_field->* TO FIELD-SYMBOL(<lv_key_field>).
    ASSERT sy-subrc EQ 0.
    <lv_key_field> = is_lpor-instid.

    DATA(lt_parameters) = prepare_params_4_constructor( ir_techn_descriptors = lr_techn_descriptors
                                                        ir_key_field         = lr_key_field ).

    "Create instance of passed object type
    CREATE OBJECT result TYPE (is_lpor-typeid)
      PARAMETER-TABLE lt_parameters.

    "Checks existence of object and create default attribute = readable key with text
    result->check_existence( ).
    result->default_attribute_value( ).
  ENDMETHOD.                    "create_instance


  METHOD create_singleton.
    "-----------------------------------------------------------------*
    "   Create a singleton instance of the factory
    "-----------------------------------------------------------------*
    IF zcl_ca_wf_bc_factory=>mo_singleton IS NOT BOUND.
      zcl_ca_wf_bc_factory=>mo_singleton ?= NEW zcl_ca_wf_bc_factory( ).
    ENDIF.

    result ?= zcl_ca_wf_bc_factory=>mo_singleton.
  ENDMETHOD.                    "create_singleton


  METHOD find_in_buffer.
    "-----------------------------------------------------------------*
    "   Find an instance to the workflow instance key in buffer
    "-----------------------------------------------------------------*
    TRY.
        "Is an instance already created?
        DATA(ls_buffer) = zcl_ca_wf_bc_factory=>mt_buffer[ s_lpor = is_lpor ].
        ls_buffer-o_persistent->refresh( ).

      CATCH cx_sy_itab_line_not_found.
        ls_buffer-o_persistent ?= create_instance( is_lpor ).

        ls_buffer-s_lpor = ls_buffer-o_persistent->lpor( ).
        INSERT ls_buffer INTO TABLE zcl_ca_wf_bc_factory=>mt_buffer.
    ENDTRY.

    result ?= ls_buffer-o_persistent.
  ENDMETHOD.                    "find_in_buffer


  METHOD get_container_with_key_fields.
    "-----------------------------------------------------------------*
    "   Get container with key fields of requested object
    "-----------------------------------------------------------------*
    cl_swf_utl_def_services=>get_object_keyfields(
                                              EXPORTING
                                                im_clstype   = swfco_objtype_cl
                                                im_clsname   = iv_object_type
                                              IMPORTING
                                                ex_return    = DATA(ls_message)
                                                ex_container = result ).

    DATA(lx_error) = CAST zcx_ca_wf_bc_factory( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_bc_factory=>c_zcx_ca_wf_bc_factory
                                                       iv_class    = 'CL_SWF_UTL_DEF_SERVICES'
                                                       iv_method   = 'GET_OBJECT_KEYFIELDS'
                                                       is_msg      = VALUE #( msgty = ls_message-msgtyp
                                                                              msgid = ls_message-workarea
                                                                              msgno = ls_message-message
                                                                              msgv1 = ls_message-variable1
                                                                              msgv2 = ls_message-variable2
                                                                              msgv3 = ls_message-variable3
                                                                              msgv4 = ls_message-variable4 )
                                                       iv_subrc    = ls_message-subrc ) ) ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.
  ENDMETHOD.                    "get_container_with_key_fields


  METHOD get_details_of_constructor.
    "-----------------------------------------------------------------*
    "   Get details of method CONSTRUCTOR of the workflow class
    "-----------------------------------------------------------------*
    result  = REF #( io_class_descr->methods[ name = c_methname_constructor ] OPTIONAL ).
    IF result IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_ca_wf_bc_factory
        EXPORTING
          textid   = zcx_ca_wf_bc_factory=>param_invalid
          mv_msgty = zcx_ca_wf_bc_factory=>c_msgty_e
          mv_msgv1 = CONV #( c_methname_constructor ).
    ENDIF.
  ENDMETHOD.                    "get_details_of_constructor


  METHOD get_techn_descriptors.
    "-----------------------------------------------------------------*
    "   Get technical class descriptor
    "-----------------------------------------------------------------*
    result = REF #( zcl_ca_wf_bc_factory=>mt_buffer_techn_descr[ typeid = iv_object_type ] OPTIONAL ).
    IF result IS BOUND.
      RETURN.
    ENDIF.

    TRY.
        INSERT VALUE #(
            typeid  = iv_object_type
            o_class = CAST cl_abap_classdescr( NEW zcl_ca_ddic( iv_name = iv_object_type )->mo_type_desc )
            o_key_field = get_techn_descr_of_key_field( iv_object_type ) ) INTO TABLE mt_buffer_techn_descr
                                                                           REFERENCE INTO result.

      CATCH zcx_ca_error
            zcx_ca_intern INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_bc_factory( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_wf_bc_factory=>c_zcx_ca_wf_bc_factory
                                                           iv_class    = 'ZCX_CA_WF_BC_FACTORY'
                                                           iv_method   = 'GET_TECHN_DESCRIPTORS'
                                                           ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_techn_descriptors


  METHOD get_techn_descr_of_key_field.
    "-----------------------------------------------------------------*
    "   Compose workflow instance key
    "-----------------------------------------------------------------*
    TRY.
        DATA(lo_key_fld_container) = get_container_with_key_fields( iv_object_type ).
        DATA(lt_key_fields)        = lo_key_fld_container->if_swf_cnt_element_access_1~all_elements_list( ).

        "Multiple key field definition is possible in principle but not supported by this class. This is due to
        "the experience that a flat structured single key field is much more convenient to handle than keys
        "that are split into multiple fields.
        has_only_one_key_field_defined( it_key_fields  = lt_key_fields
                                        iv_object_type = iv_object_type ).

        result ?= lo_key_fld_container->if_swf_cnt_element_access_1~element_get_definition(
                                                                    name = lt_key_fields[ 1 ] )->get_typedescr( ).

      CATCH cx_swf_cnt_container INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_bc_factory( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_wf_bc_factory=>c_zcx_ca_wf_bc_factory
                                                           iv_class    = 'IF_SWF_CNT_CONTAINER'
                                                           iv_method   = 'ELEMENT_GET_VALUE_REF'
                                                           ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_techn_descr_of_key_field


  METHOD has_only_one_key_field_defined.
    "-----------------------------------------------------------------*
    "   Check whether only one key field is defined (flat structure or single field)
    "-----------------------------------------------------------------*
    IF lines( it_key_fields ) NE 1.
      "No or more than one key fields are defined in class &1 - is not supported
      RAISE EXCEPTION TYPE zcx_ca_wf_bc_factory
        EXPORTING
          textid   = zcx_ca_wf_bc_factory=>no_or_multiple_key_fields
          mv_msgty = zcx_ca_wf_bc_factory=>c_msgty_e
          mv_msgv1 = CONV #( iv_object_type ).
    ENDIF.
  ENDMETHOD.                    "has_only_one_key_field_defined


  METHOD is_obj_type_a_business_class.
    "-----------------------------------------------------------------*
    "   Check whether the passed BC type is a workflow class
    "-----------------------------------------------------------------*
    IF NOT line_exists( io_class_descr->interfaces[ name = swfco_if_workflow ] ).
      "Class '&' does not support any workflow program exit interface
      RAISE EXCEPTION TYPE zcx_ca_wf_bc_factory
        EXPORTING
          textid   = zcx_ca_wf_bc_factory=>is_no_workflow_class
          mv_msgty = zcx_ca_wf_bc_factory=>c_msgty_e
          mv_msgv1 = CONV #( io_class_descr->get_relative_name( ) ).
    ENDIF.
  ENDMETHOD.                    "is_obj_type_a_business_class


  METHOD prepare_params_4_constructor.
    "-----------------------------------------------------------------*
    "   Prepare parameters for the constructor call of the workflow class
    "-----------------------------------------------------------------*
    DATA(lr_method_def) = get_details_of_constructor( ir_techn_descriptors->o_class ).
    LOOP AT lr_method_def->parameters REFERENCE INTO DATA(lr_parameter)
                                      WHERE is_optional EQ abap_false
                                        AND type_kind   EQ ir_techn_descriptors->o_key_field->type_kind
                                        AND length      EQ ir_techn_descriptors->o_key_field->length.
      INSERT VALUE #( name  = lr_parameter->name
                      kind  = ir_techn_descriptors->o_class->exporting
                      value = ir_key_field ) INTO TABLE result.
    ENDLOOP.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_wf_bc_factory
        EXPORTING
          textid   = zcx_ca_wf_bc_factory=>param_invalid
          mv_msgty = zcx_ca_wf_bc_factory=>c_msgty_e
          mv_msgv1 = 'NO FITTING PARAMETER FOR KEY FOUND' ##no_text.
    ENDIF.
  ENDMETHOD.                    "prepare_params_4_constructor


  METHOD zif_ca_wf_bc_factory~create_by_key.
    "-----------------------------------------------------------------*
    "   Create a workflow instance by its type and key
    "-----------------------------------------------------------------*
    "Since the method IF_WORKFLOW=>FIND_BY_LPOR is also executed during the design phase of a workflow
    "(= transaction SWDD or SWDD_SCENARIO) NO EXCEPTION can be raised if no key value is passed. Otherwise
    "nobody could design a workflow if this factory class is in use in the corresponding business class.
    IF iv_key IS INITIAL.
      RETURN.
    ENDIF.

    result = find_in_buffer( compose_lpor( iv_key  = iv_key
                                           iv_type = iv_type ) ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~create_by_key


  METHOD zif_ca_wf_bc_factory~create_by_lpor.
    "-----------------------------------------------------------------*
    "   Create an instance by the workflow instance key
    "-----------------------------------------------------------------*
    "Since the method IF_WORKFLOW=>FIND_BY_LPOR is also executed during the design phase of a workflow
    "(= transaction SWDD or SWDD_SCENARIO) NO EXCEPTION can be raised is no key value is passed. Otherwise
    "nobody could design a workflow if this factory class is in use in the corresponding business class.
    IF is_lpor-instid IS INITIAL.
      RETURN.
    ENDIF.

    result = find_in_buffer( is_lpor ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~create_by_lpor


  METHOD zif_ca_wf_bc_factory~extract_key_from_input.
    "-----------------------------------------------------------------*
    "   Extract key from input of consumer
    "-----------------------------------------------------------------*
    result = COND #( WHEN iv_instid IS NOT INITIAL  THEN iv_instid
                     WHEN iv_key    IS NOT INITIAL  THEN iv_key ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~extract_key_from_input


  METHOD zif_ca_wf_bc_factory~release_from_buffer.
    "-----------------------------------------------------------------*
    "   Delete instance from buffer
    "-----------------------------------------------------------------*
    DELETE mt_buffer WHERE s_lpor EQ io_wf_object->bi_persistent~lpor( ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~release_from_buffer

ENDCLASS.                     "zcl_ca_wf_bcs_factory  IMPLEMENTATION




