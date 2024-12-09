"! <p class="shorttext synchronized" lang="en">CA-TBX: Factory + buffering of Business Classes (= Workflow)</p>
CLASS zcl_ca_wf_bcs_factory DEFINITION PUBLIC
                                       CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_wf_bcs_factory.

*   a l i a s e s
    ALIASES:
*     Methods
      create_by_key          FOR zif_ca_wf_bcs_factory~create_by_key,
      create_by_lpor         FOR zif_ca_wf_bcs_factory~create_by_lpor,
      extract_key_from_input FOR zif_ca_wf_bcs_factory~extract_key_from_input,
      release_from_buffer    FOR zif_ca_wf_bcs_factory~release_from_buffer.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create a singleton instance of the factory</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Singleton instance of this factory</p>
      create_singleton
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_bcs_factory.


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
        typeid      TYPE sibftypeid,
        o_key_field TYPE REF TO cl_abap_datadescr,
        o_class     TYPE REF TO cl_abap_classdescr,
      END   OF ty_s_buffer_techn_descr,
      "! <p class="shorttext synchronized" lang="en">Buffer with technical descriptions</p>
      ty_t_buffer_techn_descr TYPE HASHED TABLE OF ty_s_buffer_techn_descr
                                              WITH UNIQUE KEY primary_key COMPONENTS typeid.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Singleton instance of the this factory</p>
      mo_singleton          TYPE REF TO zif_ca_wf_bcs_factory,

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
      "! @parameter iv_typeid             | <p class="shorttext synchronized" lang="en">Workflow instance type (= class name)</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Container with key fields</p>
      "! @raising   zcx_ca_wf_bcs_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      get_container_with_key_fields
        IMPORTING
          iv_typeid     TYPE sibftypeid
        RETURNING
          VALUE(result) TYPE REF TO if_swf_cnt_container
        RAISING
          zcx_ca_wf_bcs_factory,

      "! <p class="shorttext synchronized" lang="en">Get technical description of the workflow class key</p>
      "!
      "! @parameter is_lpor               | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Technical description of the workflow class key</p>
      "! @raising   zcx_ca_wf_bcs_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      get_techn_descr_of_key_field
        IMPORTING
          is_lpor       TYPE sibflpor
        RETURNING
          VALUE(result) TYPE REF TO cl_abap_datadescr
        RAISING
          zcx_ca_wf_bcs_factory,

      "! <p class="shorttext synchronized" lang="en">Check whether the key is defined in a single data object</p>
      "!
      "! @parameter iv_typeid             | <p class="shorttext synchronized" lang="en">Workflow instance type (= class name)</p>
      "! @parameter io_key_fld_container  | <p class="shorttext synchronized" lang="en">Container with key fields</p>
      "! @raising   zcx_ca_wf_bcs_factory | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
      is_a_single_key_field_def
        IMPORTING
          iv_typeid            TYPE sibftypeid
          io_key_fld_container TYPE REF TO if_swf_cnt_container
        RAISING
          zcx_ca_wf_bcs_factory.

ENDCLASS.                     "zcl_ca_wf_bcs_factory  DEFINITION



CLASS zcl_ca_wf_bcs_factory IMPLEMENTATION.

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
      lt_parameters TYPE abap_parmbind_tab,
      lr_key_field  TYPE REF TO data.

    "Create a correct typed key value for the constructor execution
    DATA(lo_key_field_descr) = get_techn_descr_of_key_field( is_lpor ).
    CREATE DATA lr_key_field TYPE HANDLE lo_key_field_descr.
    ASSIGN lr_key_field->* TO FIELD-SYMBOL(<lv_key_field>).
    ASSERT sy-subrc EQ 0.
    <lv_key_field> = is_lpor-instid.

    "Find the parameter name for the key value to pass it into the constructor
    DATA(lo_class_descr) = CAST cl_abap_classdescr( NEW zcl_ca_ddic( iv_name = is_lpor-typeid )->mo_type_desc ).
    DATA(lr_method_def)  = REF #( lo_class_descr->methods[ name = 'CONSTRUCTOR' ] OPTIONAL ) ##no_text.
    IF lr_method_def IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_ca_wf_bcs_factory( textid   = zcx_ca_wf_bcs_factory=>param_invalid
                                                 mv_msgty = zcx_ca_wf_bcs_factory=>c_msgty_e
                                                 mv_msgv1 = 'CONSTRUCTOR' ) ##no_text.
    ENDIF.

    LOOP AT lr_method_def->parameters REFERENCE INTO DATA(lr_parameter)
                                      WHERE is_optional EQ abap_false
                                        AND type_kind   EQ lo_key_field_descr->type_kind
                                        AND length      EQ lo_key_field_descr->length.
      INSERT VALUE #( name  = lr_parameter->name
                      kind  = lo_class_descr->exporting
                      value = lr_key_field ) INTO TABLE lt_parameters.
    ENDLOOP.
    IF sy-subrc NE 0.
      RAISE EXCEPTION NEW zcx_ca_wf_bcs_factory( textid   = zcx_ca_wf_bcs_factory=>param_invalid
                                                 mv_msgty = zcx_ca_wf_bcs_factory=>c_msgty_e
                                                 mv_msgv1 = 'NO FITTING PARAMETER FOR KEY FOUND' ) ##no_text.
    ENDIF.
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
    IF zcl_ca_wf_bcs_factory=>mo_singleton IS NOT BOUND.
      zcl_ca_wf_bcs_factory=>mo_singleton ?= NEW zcl_ca_wf_bcs_factory( ).
    ENDIF.

    result ?= zcl_ca_wf_bcs_factory=>mo_singleton.
  ENDMETHOD.                    "create_singleton


  METHOD find_in_buffer.
    "-----------------------------------------------------------------*
    "   Find an instance to the workflow instance key in buffer
    "-----------------------------------------------------------------*
    TRY.
        "Is an instance already created?
        DATA(ls_buffer) = zcl_ca_wf_bcs_factory=>mt_buffer[ s_lpor = is_lpor ].
        ls_buffer-o_persistent->refresh( ).

      CATCH cx_sy_itab_line_not_found.
        ls_buffer-o_persistent ?= create_instance( is_lpor ).

        ls_buffer-s_lpor = ls_buffer-o_persistent->lpor( ).
        INSERT ls_buffer INTO TABLE zcl_ca_wf_bcs_factory=>mt_buffer.
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
                                                im_clsname   = iv_typeid
                                              IMPORTING
                                                ex_return    = DATA(ls_message)
                                                ex_container = result ).

    DATA(lx_error) = CAST zcx_ca_wf_bcs_factory( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_bcs_factory=>c_zcx_ca_wf_bcs_factory
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


  METHOD get_techn_descr_of_key_field.
    "-----------------------------------------------------------------*
    "   Compose workflow instance key
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error       TYPE REF TO zcx_ca_wf_bcs_factory,
      lr_techn_descr TYPE REF TO ty_s_buffer_techn_descr.

    lr_techn_descr = REF #( zcl_ca_wf_bcs_factory=>mt_buffer_techn_descr[ typeid = is_lpor-typeid ] OPTIONAL ).
    IF lr_techn_descr              IS BOUND AND
       lr_techn_descr->o_key_field IS BOUND.
      result = lr_techn_descr->o_key_field.
      RETURN.
    ENDIF.

    TRY.
        DATA(lo_key_fld_container) = get_container_with_key_fields( is_lpor-typeid ).

        "Multiple key field definition is possible in principle but not supported by this class. This is due to
        "the experience that a flat structured single key field is much more convenient to handle than key fields
        "that are split in multiple parts.
        is_a_single_key_field_def( iv_typeid            = is_lpor-typeid
                                   io_key_fld_container = lo_key_fld_container ).

        DATA(lt_key_fields) = lo_key_fld_container->if_swf_cnt_element_access_1~all_elements_list( ).
        DATA(lv_key_field)  = lt_key_fields[ 1 ].

        result ?= lo_key_fld_container->if_swf_cnt_element_access_1~element_get_definition(
                                                                      name = lv_key_field )->get_typedescr( ).

      CATCH cx_swf_cnt_container INTO DATA(lx_catched).
        lx_error = CAST zcx_ca_wf_bcs_factory( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_wf_bcs_factory=>c_zcx_ca_wf_bcs_factory
                                                           iv_class    = 'IF_SWF_CNT_CONTAINER'
                                                           iv_method   = 'ELEMENT_GET_VALUE_REF'
                                                           ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_techn_descr_of_key_field


  METHOD is_a_single_key_field_def.
    "-----------------------------------------------------------------*
    "   Check whether the key is defined in a single data object (flat structure or single field)
    "-----------------------------------------------------------------*
    IF io_key_fld_container->if_swf_cnt_element_access_1~all_elements_get_count( ) NE 1.
      "No or more than one key fields are defined in class &1 - is not supported
      RAISE EXCEPTION NEW zcx_ca_wf_bcs_factory( textid   = zcx_ca_wf_bcs_factory=>no_or_multiple_key_fields
                                                 mv_msgty = zcx_ca_wf_bcs_factory=>c_msgty_e
                                                 mv_msgv1 = CONV #( iv_typeid ) ).
    ENDIF.
  ENDMETHOD.                    "is_a_single_key_field_def


  METHOD zif_ca_wf_bcs_factory~create_by_key.
    "-----------------------------------------------------------------*
    "   Create a workflow instance by its type and key
    "-----------------------------------------------------------------*
    result = find_in_buffer( compose_lpor( iv_key  = iv_key
                                           iv_type = iv_type ) ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~create_by_key


  METHOD zif_ca_wf_bcs_factory~create_by_lpor.
    "-----------------------------------------------------------------*
    "   Create an instance by the workflow instance key
    "-----------------------------------------------------------------*
    result = find_in_buffer( is_lpor ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~create_by_lpor


  METHOD zif_ca_wf_bcs_factory~extract_key_from_input.
    "-----------------------------------------------------------------*
    "   Extract key from input of consumer
    "-----------------------------------------------------------------*
    result = COND #( WHEN iv_instid IS NOT INITIAL  THEN iv_instid
                     WHEN iv_key    IS NOT INITIAL  THEN iv_key
                     "At least one of the following parameters must be passed: &1 &2 &3 &4
                     ELSE THROW zcx_ca_wf_bcs_factory( textid   = zcx_ca_wf_ga_settings=>param_invalid
                                                       mv_msgty = zcx_ca_wf_ga_settings=>c_msgty_e
                                                       mv_msgv1 = 'IV_KEY'
                                                       mv_msgv2 = 'IS_LPOR' ) ) ##no_text.
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~extract_key_from_input


  METHOD zif_ca_wf_bcs_factory~release_from_buffer.
    "-----------------------------------------------------------------*
    "   Delete instance from buffer
    "-----------------------------------------------------------------*
    DELETE mt_buffer WHERE s_lpor EQ io_wf_object->bi_persistent~lpor( ).
  ENDMETHOD.                    "zif_ca_wf_bcs_factory~release_from_buffer

ENDCLASS.                     "zcl_ca_wf_bcs_factory  IMPLEMENTATION




