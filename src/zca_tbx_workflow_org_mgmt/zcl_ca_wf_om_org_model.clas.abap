"! <p class="shorttext synchronized" lang="en">WF-OM: BC Org. unit determinations</p>
CLASS zcl_ca_wf_om_org_model DEFINITION PUBLIC
                                        CREATE PROTECTED
                                        GLOBAL FRIENDS zif_ca_wf_om_org_model.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_workflow,            " !!! Includes IF_WORKFLOW = BI_OBJECT + BI_PERSISTENT
      zif_ca_wf_om_org_model,
      if_xo_const_message.

*   a l i a s e s
    ALIASES:
*     BI_OBJECT methods
      default_attr_value FOR bi_object~default_attribute_value,
      execute_def_method FOR bi_object~execute_default_method,
      release            FOR bi_object~release,
*     BI_PERSISTENT methods
      find_by_lpor       FOR bi_persistent~find_by_lpor,
      lpor               FOR bi_persistent~lpor,
      refresh            FOR bi_persistent~refresh,
*     ZIF_CA_WORKFLOW methods
      check_existence    FOR zif_ca_workflow~check_existence,
      get_task_descr     FOR zif_ca_workflow~get_task_descr,
      raise_event        FOR zif_ca_workflow~raise_event,
      mo_log             FOR zif_ca_workflow~mo_log,
      mv_default_attr    FOR zif_ca_workflow~mv_default_attr,
*     ZIF_CA_WF_OM_ORG_MODEL methods
      find_employees_2_job       FOR zif_ca_wf_om_org_model~find_employees_2_job,
      find_employees_2_task      FOR zif_ca_wf_om_org_model~find_employees_2_task,
      get_all_managers           FOR zif_ca_wf_om_org_model~get_all_managers,
      get_employees_2_org_object FOR zif_ca_wf_om_org_model~get_employees_2_org_object,
      get_manager                FOR zif_ca_wf_om_org_model~get_manager,
      get_org_model_data         FOR zif_ca_wf_om_org_model~get_org_model_data,
      get_text_of_org_object     FOR zif_ca_wf_om_org_model~get_text_of_org_object,
*     ZIF_CA_WF_OM_ORG_MODEL attributes
      mo_cvc_om                  FOR zif_ca_wf_om_org_model~mo_cvc_om,
      ms_data                    FOR zif_ca_wf_om_org_model~ms_data,
      mv_search_active           FOR zif_ca_wf_om_org_model~mv_search_active,
      mv_valid_on                FOR zif_ca_wf_om_org_model~mv_valid_on.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Plan version</p>
      mv_plvar             TYPE plvar  READ-ONLY.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Instance key - Type and Id of organizational object</p>
      ms_key          TYPE swhactor READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class constructor</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Create instance by LPOR or org. unit Id</p>
      "!
      "! @parameter is_lpor          | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter is_key           | <p class="shorttext synchronized" lang="en">Org. unit key (type + Id)</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      get_instance
        IMPORTING
          is_lpor          TYPE sibflpor OPTIONAL
          is_key           TYPE swhactor  OPTIONAL
          iv_valid_on      TYPE hr_date  DEFAULT sy-datlo
          iv_search_active TYPE abap_boolean DEFAULT abap_true
        RETURNING
          VALUE(result)    TYPE REF TO zif_ca_wf_om_org_model
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Set different plan version</p>
      "!
      "! <p>Use this method to change the plan version of the org. management. This value is base for many
      "! functions of this class and <strong>CLEARS the buffer</strong> therefore.</p>
      "!
      "! @parameter iv_plvar | <p class="shorttext synchronized" lang="en">Plan version</p>
      set_different_plan_version
        IMPORTING
          iv_plvar TYPE plvar.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Short object details</p>
      BEGIN OF ty_s_object_details.
        INCLUDE TYPE swhactor AS s_om_obj_key.
    TYPES:
        name         TYPE pcn_orgtx,
        short_name   TYPE short_d,
        o_org_object TYPE REF TO zcl_ca_wf_om_org_model,
      END   OF ty_s_object_details,

      "! <p class="shorttext synchronized" lang="en">Position owner</p>
      ty_t_pos_owner TYPE STANDARD TABLE OF hrpe_prozt.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">0 = Search NOT higher; > 0 = search up to x levels above</p>
      mv_search_upwards      TYPE hi_ebene.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Managers of the current org. unit in MS_MY_ORG_UNIT</p>
      mt_managers            TYPE zca_wf_t_org_model_data,
      "! <p class="shorttext synchronized" lang="en">Superior org. unit to the current org unit in MS_MY_ORG_UNIT</p>
      mt_superior_org_units  TYPE zca_wf_t_org_model_data,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Workflow object instance key</p>
      ms_lpor                TYPE sibflpor,
      "! <p class="shorttext synchronized" lang="en">Type and Id of organizational object to be searched</p>
      ms_search_4_org_object TYPE swhactor,
      "! <p class="shorttext synchronized" lang="en">Details to the position the object in MS_KEY is assigned to</p>
      ms_my_position         TYPE ty_s_object_details,
      "! <p class="shorttext synchronized" lang="en">Details to the org. unit the object in MS_KEY is assigned to</p>
      ms_my_org_unit         TYPE ty_s_object_details.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Am I a manager of this org. unit (of obj in MS_MY_ORG_UNIT)?</p>
      "!
      "! @parameter iv_auth_check           | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">X = Result of search contains at least one person</p>
      "! @raising   zcx_ca_wf_om_no_manager | <p class="shorttext synchronized" lang="en">WF-OM: No manager found</p>
      "! @raising   zcx_ca_wf_om_org_model  | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      am_i_a_manager_of_the_org_unit
        IMPORTING
          iv_auth_check TYPE hr_authy
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_no_manager
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter is_key                 | <p class="shorttext synchronized" lang="en">Type and Id of org. object (use SWFCO_ORG_* for type)</p>
      "! @parameter iv_valid_on            | <p class="shorttext synchronized" lang="en">Object is valid on</p>
      "! @parameter iv_search_active       | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      constructor
        IMPORTING
          is_key           TYPE swhactor
          iv_valid_on      TYPE hr_date      DEFAULT sy-datlo
          iv_search_active TYPE abap_boolean DEFAULT abap_true
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Create an instance to an org. object</p>
      "!
      "! @parameter is_org_object_key      | <p class="shorttext synchronized" lang="en">Key of the required org. object</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">WF-OM: BC Org. unit determinations</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      create_instance_2_org_object
        IMPORTING
          is_org_object_key TYPE swhactor
        RETURNING
          VALUE(result)     TYPE REF TO zcl_ca_wf_om_org_model
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Determine depending org. objects + a instance of them</p>
      "!
      "! <p>Determines e. g. the position and org. unit to a person. Fills the structures {@link .data:MS_MY_POSITION}
      "! and {@link .data:MS_MY_ORG_UNIT}.</p>
      "!
      "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      determine_dependent_org_obj
        IMPORTING
          iv_auth_check TYPE hr_authy
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Find manager to org. unit, search upwards if requested</p>
      "!
      "! @parameter iv_search_upwards       | <p class="shorttext synchronized" lang="en">0 = Search NOT higher; > 0 = search up to x levels above</p>
      "! @parameter iv_auth_check           | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter iv_search_active        | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state/p>
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">Determined manager</p>
      "! @raising   zcx_ca_wf_om_no_manager | <p class="shorttext synchronized" lang="en">WF-OM: No manager found</p>
      "! @raising   zcx_ca_wf_om_org_model  | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      find_manager_2_org_unit
        IMPORTING
          iv_search_upwards TYPE hi_ebene
          iv_search_active  TYPE abap_boolean DEFAULT abap_true
          iv_auth_check     TYPE hr_authy
        RETURNING
          VALUE(result)     TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_no_manager
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get managers to the current org. unit</p>
      "!
      "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_managers_2_my_org_unit
        IMPORTING
          iv_auth_check TYPE hr_authy
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get members to the current org. object respecting the scope</p>
      "!
      "! @parameter iv_scope               | <p class="shorttext synchronized" lang="en">Scope (only types S + O) -&gt; use ZCL_CA_WF_OM_CVC=>SCOPE-*</p>
      "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">Merged result of FM RH_STRUC_GET</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_members_2_org_object
        IMPORTING
          iv_scope      TYPE zca_wf_e_scope DEFAULT zcl_ca_wf_om_cvc=>scope-manager
          iv_auth_check TYPE hr_authy
        RETURNING
          VALUE(result) TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get all superior org. unit above </p>
      "!
      "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_my_superior_org_units
        IMPORTING
          iv_auth_check TYPE hr_authy
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get next upper org. unit from result of org. unit search</p>
      "!
      "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">Superior org. unit</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: BC Org. unit determinations</p>
      get_next_higher_org_unit_2_me
        IMPORTING
          iv_auth_check TYPE hr_authy
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_om_org_model
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Check whether a person is found to the requested org. unit</p>
      "!
      "! @parameter it_manager             | <p class="shorttext synchronized" lang="en">Manager of org. unit</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">X = Result of search contains at least one person</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_any_person_assigned_2_ounit
        IMPORTING
          it_manager    TYPE zca_wf_t_org_model_data
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Check whether the passed member is manager of this org. unit</p>
      "!
      "! @parameter is_member_key           | <p class="shorttext synchronized" lang="en">Key of a member of the org. unit</p>
      "! @parameter iv_auth_check           | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">X = Member is a manager of the org. unit</p>
      "! @raising   zcx_ca_wf_om_no_manager | <p class="shorttext synchronized" lang="en">WF-OM: No manager found</p>
      "! @raising   zcx_ca_wf_om_org_model  | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_member_a_manager_of_the_ou
        IMPORTING
          is_member_key TYPE swhactor
          iv_auth_check TYPE hr_authy
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_no_manager
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Check whether it is an org. object for multiple people</p>
      "!
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">X = Org. object is a position or an org. unit</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_org_obj_4_multiple_people
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer.
        INCLUDE TYPE sibflpor AS s_lpor.
    TYPES:
        valid_on           TYPE hr_date,
        active_id_searched TYPE abap_boolean,
        o_persistent       TYPE REF TO zif_ca_workflow,
      END   OF ty_s_buffer,
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      ty_t_buffer TYPE HASHED TABLE OF ty_s_buffer
                              WITH UNIQUE KEY primary_key COMPONENTS s_lpor  valid_on  active_id_searched.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">My type Id</p>
      c_my_typeid TYPE sibftypeid VALUE 'ZCL_CA_WF_OM_ORG_MODEL'  ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer     TYPE ty_t_buffer.

ENDCLASS.



CLASS zcl_ca_wf_om_org_model IMPLEMENTATION.

  METHOD am_i_a_manager_of_the_org_unit.
    "-----------------------------------------------------------------*
    "   Am I a manager of this org. unit (of obj in MS_MY_ORG_UNIT)?
    "-----------------------------------------------------------------*
    get_managers_2_my_org_unit( iv_auth_check ).

    result = xsdbool( line_exists( mt_managers[ otype = ms_key-otype
                                                objid = ms_key-objid ] ) ).
    IF ( ms_key-otype EQ swfco_org_position  OR
         ms_key-otype EQ swfco_org_person ) AND
         result       EQ abap_false         AND
         result       IS NOT SUPPLIED.
      "Org. unit &1 has no manager
      RAISE EXCEPTION TYPE zcx_ca_wf_om_no_manager
        EXPORTING
          textid   = zcx_ca_wf_om_no_manager=>org_object_has_no_manager
          mv_msgty = zcx_ca_wf_om_no_manager=>c_msgty_e
          mv_msgv1 = CONV #( zcl_ca_utils=>compose_name_n_techn_id(
                             iv_descr    = ms_my_org_unit-name
                             iv_techn_id = |{ ms_my_org_unit-otype } { ms_my_org_unit-objid ALPHA = OUT }| ) ).
    ENDIF.
  ENDMETHOD.                    "am_i_a_manager_of_the_org_unit


  METHOD bi_object~default_attribute_value.
    "-----------------------------------------------------------------*
    "   Returns a description and/or prepared key of the object.
    "-----------------------------------------------------------------*
    "Example ==> replace the next 3 lines (comment and key preparation) as needed for your purpose
    "TEXT-DAV = Org. object xxxxxxxxxx (t nnnnnn)
    DATA(lv_techn_id) = condense( |{ ms_key-otype } { ms_key-objid ALPHA = OUT }| ).
    mv_default_attr = |{ TEXT-dav } { zcl_ca_utils=>compose_name_n_techn_id( iv_techn_id = lv_techn_id
                                                                             iv_descr    = ms_data-name ) }|.

    result = REF #( mv_default_attr ).
  ENDMETHOD.                    "bi_object~default_attribute_value


  METHOD bi_object~execute_default_method ##needed.
    "-----------------------------------------------------------------*
    "   Execute default method
    "-----------------------------------------------------------------*
*    SET PARAMETER ID 'ppp' FIELD mv_user_id.
*    CALL TRANSACTION 'ttttt' WITH AUTHORITY-CHECK
*                              AND SKIP FIRST SCREEN.
  ENDMETHOD.                    "bi_object~execute_default_method


  METHOD bi_object~release.
    "-----------------------------------------------------------------*
    "   Release instance
    "-----------------------------------------------------------------*
    "Deletes all instances (of different dates and states) of this key / LPOR
    DELETE mt_buffer USING KEY primary_key WHERE s_lpor EQ ms_lpor.
  ENDMETHOD.                    "bi_object~release


  METHOD bi_persistent~find_by_lpor.
    "-----------------------------------------------------------------*
    "   Create business class instance
    "-----------------------------------------------------------------*
    TRY.
        result ?= zcl_ca_wf_om_org_model=>get_instance( is_lpor = lpor ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        "As long as no exceptions are declared for this method, this is
        "currently the best solution.
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "bi_persistent~find_by_lpor


  METHOD bi_persistent~lpor.
    "-----------------------------------------------------------------*
    "   Return instance key
    "-----------------------------------------------------------------*
    result = ms_lpor.
  ENDMETHOD.                    "bi_persistent~lpor


  METHOD bi_persistent~refresh.
    "-----------------------------------------------------------------*
    "   Refresh instance
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "bi_persistent~refresh


  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    "Get standard plan version
    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      EXPORTING
        ask_plvar_dialog = abap_false
      IMPORTING
        act_plvar        = zcl_ca_wf_om_org_model=>mv_plvar
      EXCEPTIONS
        no_active_plvar  = 1
        OTHERS           = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                        iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                        iv_function = 'RH_GET_ACTIVE_WF_PLVAR'
                                                        iv_subrc     = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "class_constructor


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    ms_lpor-typeid = to_upper( c_my_typeid ).
    ms_lpor-catid  = swfco_objtype_cl.

    IF is_key IS INITIAL.
      RETURN.
    ENDIF.

    "Set initial validity date and prepare constants and its validations
    mv_valid_on      = iv_valid_on.
    mv_search_active = iv_search_active.
    mo_cvc_om        = zcl_ca_wf_om_cvc=>get_instance( ).
*    mo_cvc_employee  = zcl_ca_wf_om_cvc_employee=>get_instance( ).

    "Complete and keep several attributes
    "Add leading zeros for numeric object Ids (= all object types, except SAP user Ids)
    ms_key = VALUE swhactor( otype = is_key-otype
                             objid = COND #( WHEN is_key-otype NE swfco_org_user
                                               THEN |{ is_key-objid WIDTH = 8  ALPHA = IN }|
                                               ELSE is_key-objid ) ).
    ms_lpor-instid = ms_search_4_org_object = ms_key.
*    mbo_BO_PERNR-instid = CONV #( ms_key ).
  ENDMETHOD.                    "constructor


  METHOD create_instance_2_org_object.
    "-----------------------------------------------------------------*
    "   Create an instance to an org. object
    "-----------------------------------------------------------------*
    TRY.
        result ?= zcl_ca_wf_om_org_model=>get_instance( is_key           = is_org_object_key
                                                        iv_valid_on      = mv_valid_on
                                                        iv_search_active = mv_search_active ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                         iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                         iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                         iv_method   = 'CREATE_INSTANCE_2_ORG_OBJECT'
                                                         ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_instance_2_org_object


  METHOD determine_dependent_org_obj.
    "-----------------------------------------------------------------*
    "   Determine depending org. objects + a instance of them
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_employee          TYPE REF TO zif_ca_wf_om_employee.

    TRY.
        IF ms_my_org_unit IS NOT INITIAL.
          RETURN.
        ENDIF.

        CASE ms_key-otype.
          WHEN swfco_org_user.
            "Create an employee instance ...
            lo_employee = zcl_ca_wf_om_employee=>get_instance_by_sap_user_id( iv_sap_user_id   = ms_key-objid
                                                                              iv_valid_on      = mv_valid_on
                                                                              iv_search_active = mv_search_active ).
            "... to overwrite the search term (originally provided in the CONSTRUCTOR) by the personnel Id and ...
            "(this is done for the case that another personnel number is active than the instantiate one -> central person).
            ms_search_4_org_object = VALUE #( otype = swfco_org_person
                                              objid = lo_employee->ms_data-pernr ).
            "... to provide the org. assignments
            ms_my_position = CORRESPONDING #( lo_employee->ms_data-s_position ).
            ms_my_org_unit = CORRESPONDING #( lo_employee->ms_data-s_org_unit ).

          WHEN swfco_org_person.
            "Create an employee instance ...
            lo_employee = zcl_ca_wf_om_employee=>get_instance( iv_key           = CONV #( ms_key-objid )
                                                               iv_valid_on      = mv_valid_on
                                                               iv_search_active = mv_search_active ).
            "... to provide the org. assignments of the employee
            ms_my_position = CORRESPONDING #( lo_employee->ms_data-s_position ).
            ms_my_org_unit = CORRESPONDING #( lo_employee->ms_data-s_org_unit ).

          WHEN swfco_org_position.
            ms_my_position = CORRESPONDING #( get_text_of_org_object( ) ).       "Get my own description
            DATA(lt_higher_org_objects) = zif_ca_wf_om_org_model~get_org_model_data(
                                                     iv_eval_path  = mo_cvc_om->evaluation_path-person_2_org_unit
                                                     iv_auth_check = iv_auth_check ).
            ms_my_org_unit = CORRESPONDING #( lt_higher_org_objects[ otype = swfco_org_orgunit ]
                                                                                MAPPING short_name = short
                                                                                        name       = stext ).

          WHEN swfco_org_orgunit.
            ms_my_org_unit = CORRESPONDING #( get_text_of_org_object( ) ).       "Get my own description
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                         iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                         iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                         iv_method   = 'DETERMINE_DEPENDENT_ORG_OBJ'
                                                         ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.

    "Create instances to avoid redundant instantiation
    ms_my_org_unit-o_org_object = create_instance_2_org_object( ms_my_org_unit-s_om_obj_key ).

    IF ms_my_position IS NOT INITIAL.
      ms_my_position-o_org_object = create_instance_2_org_object( ms_my_position-s_om_obj_key ).
    ENDIF.
  ENDMETHOD.                    "determine_dependent_org_obj


  METHOD find_manager_2_org_unit.
    "-----------------------------------------------------------------*
    "   Find manager to current org. unit, search upwards if requested
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_catched  TYPE REF TO zcx_ca_wf_om_no_om_objects,
      lo_org_unit TYPE REF TO zcl_ca_wf_om_org_model.

    mv_search_upwards = iv_search_upwards.

    TRY.
        lo_org_unit = SWITCH #( am_i_a_manager_of_the_org_unit( iv_auth_check )
                        WHEN abap_false THEN ms_my_org_unit-o_org_object
                        WHEN abap_true  THEN get_next_higher_org_unit_2_me( iv_auth_check ) ).

      CATCH zcx_ca_wf_om_no_om_objects INTO lx_catched.
        "If no boss was found, search a level higher in any case
        IF mv_search_upwards GT 0.
          lo_org_unit = get_next_higher_org_unit_2_me( iv_auth_check ).
          mv_search_upwards -= 1.

        ELSE.
          "Org. unit &1 has no manager
          RAISE EXCEPTION TYPE zcx_ca_wf_om_no_manager
            EXPORTING
              textid   = zcx_ca_wf_om_no_manager=>org_object_has_no_manager
              mv_msgty = zcx_ca_wf_om_no_manager=>c_msgty_e
              mv_msgv1 = CONV #( zcl_ca_utils=>compose_name_n_techn_id(
                                           iv_descr    = ms_my_org_unit-name
                                           iv_techn_id = |{ ms_my_org_unit-otype } { ms_my_org_unit-objid ALPHA = OUT }| ) ).
        ENDIF.
    ENDTRY.

    "The following TRY ... CATCH works like DO-loop because of the RETRY near the ENDTRY. The starting
    "instance is ME and will be replaced by the next superior org. unit if no manager was found. This
    "definition must be outside of the TRY ... CATCH to be able to work.
    TRY.
        result = lo_org_unit->get_org_model_data( iv_eval_path  = mo_cvc_om->evaluation_path-manager_2_org_unit
                                                  iv_auth_check = iv_auth_check ) ##no_text.

        is_any_person_assigned_2_ounit( it_manager = result ).

      CATCH zcx_ca_wf_om_no_om_objects INTO lx_catched.
        "If exception is different to 'Nothing found' forward the exception to caller
        IF mv_search_upwards GT 0.
          "No manager found -> search for next superior org. unit
          lo_org_unit = lo_org_unit->get_next_higher_org_unit_2_me( iv_auth_check ).
          mv_search_upwards -= 1.
          RETRY.                      "ATTENTION: This makes this TRY ... CATCH to a loop

        ELSE.
          "Org. unit &1 has no manager
          RAISE EXCEPTION TYPE zcx_ca_wf_om_no_manager
            EXPORTING
              textid   = zcx_ca_wf_om_no_manager=>org_object_has_no_manager
              mv_msgty = zcx_ca_wf_om_no_manager=>c_msgty_e
              mv_msgv1 = CONV #( zcl_ca_utils=>compose_name_n_techn_id(
                                                iv_descr    = lo_org_unit->ms_data-name
                                                iv_techn_id = |{ lo_org_unit->ms_data-otype } | &
                                                              |{ lo_org_unit->ms_data-objid ALPHA = OUT }| ) ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "find_manager_2_org_unit


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor TYPE sibflpor,
      ls_key  TYPE swhactor.

    IF is_lpor IS NOT INITIAL.
      ls_lpor       = is_lpor.
      ls_lpor-catid = swfco_objtype_cl.

      "Set key into structured definition
      IF ls_lpor-instid IS NOT INITIAL.  "Avoid destruction of type conform initial values
        ls_key = CONV #( ls_lpor-instid ).
      ENDIF.

      "Set these values in any case, e. g. to create/get an instance only with the key string
      IF ls_lpor-typeid IS INITIAL.
        ls_lpor-typeid = to_upper( zcl_ca_wf_om_org_model=>c_my_typeid ).
      ENDIF.

    ELSEIF is_key IS NOT INITIAL.
      ls_key = is_key.
      ls_lpor = VALUE #( instid = CONV #( is_key )
                         typeid = to_upper( COND #( WHEN is_lpor-typeid IS NOT INITIAL
                                                      THEN is_lpor-typeid
                                                      ELSE zcl_ca_wf_om_org_model=>c_my_typeid ) )
                         catid  = swfco_objtype_cl ).

    ELSE.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'IS_KEY'
          mv_msgv3 = space
          mv_msgv4 = space ##no_text.
    ENDIF.

    "If the key is still not available create no instance
    IF ls_key IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        "Is an instance already created?
        DATA(ls_buffer) = zcl_ca_wf_om_org_model=>mt_buffer[ KEY primary_key
                                                                 s_lpor             = ls_lpor
                                                                 valid_on           = iv_valid_on
                                                                 active_id_searched = iv_search_active ].
        ls_buffer-o_persistent->refresh( ).

      CATCH cx_sy_itab_line_not_found.
        "Create instance of payment approval object
        CREATE OBJECT ls_buffer-o_persistent TYPE (ls_lpor-typeid)
          EXPORTING
            is_key           = ls_key
            iv_valid_on      = iv_valid_on
            iv_search_active = iv_search_active.

        "Checks existence of object and creates default attribute = readable key with text
        ls_buffer-o_persistent->check_existence( ).
        ls_buffer-o_persistent->default_attribute_value( ).

        ls_buffer-s_lpor             = ls_buffer-o_persistent->lpor( ).
        ls_buffer-valid_on           = iv_valid_on.
        ls_buffer-active_id_searched = iv_search_active.
        INSERT ls_buffer INTO TABLE zcl_ca_wf_om_org_model=>mt_buffer.
    ENDTRY.

    result ?= ls_buffer-o_persistent.
  ENDMETHOD.                    "get_instance


  METHOD get_managers_2_my_org_unit.
    "-----------------------------------------------------------------*
    "   Get managers to the current org. unit
    "-----------------------------------------------------------------*
    IF mt_managers IS NOT INITIAL.
      RETURN.
    ENDIF.

    determine_dependent_org_obj( iv_auth_check ).

    mt_managers = ms_my_org_unit-o_org_object->get_org_model_data(
                                                iv_eval_path  = mo_cvc_om->evaluation_path-manager_2_org_unit
                                                iv_auth_check = iv_auth_check ).

    "Keep personnel Ids AND positions as both is relevant for manager
    DELETE mt_managers WHERE otype NE swfco_org_person
                         AND otype NE swfco_org_position.

    "Sort after the percentage of engagement also by the OBJID (= Personnel no.) for a consistent result
    SORT mt_managers BY otype  vpriox  vprozt DESCENDING  objid.
  ENDMETHOD.                    "get_managers_2_my_org_unit


  METHOD get_members_2_org_object.
    "-----------------------------------------------------------------*
    "   Get members to the current org. object respecting the scope
    "-----------------------------------------------------------------*
    determine_dependent_org_obj( iv_auth_check ).

    "The evaluation path should always return an assignment to a personnel number. Using the
    "attribute MS_ORG_OBJECT_FOR_SEARCH there can only be the object types P, S and O -> see
    "methods CONSTRUCTOR + FIND_PERSONNEL_NUMBER_TO_USER.
    result = get_org_model_data(
                 iv_auth_check = iv_auth_check
                 iv_eval_path  = SWITCH wegid( ms_search_4_org_object-otype
                                   WHEN swfco_org_person   THEN mo_cvc_om->evaluation_path-person_2_org_unit
                                   WHEN swfco_org_position THEN mo_cvc_om->evaluation_path-position_2_person
                                   WHEN swfco_org_orgunit  THEN mo_cvc_om->evaluation_path-staff_2_org_unit ) ) ##no_text.
  ENDMETHOD.                    "get_members_2_org_object


  METHOD get_my_superior_org_units.
    "-----------------------------------------------------------------*
    "   Get next upper org. unit from result of superior org. unit search
    "-----------------------------------------------------------------*
    IF mt_superior_org_units IS NOT INITIAL.
      RETURN.
    ENDIF.

    determine_dependent_org_obj( iv_auth_check ).
    mt_superior_org_units = ms_my_org_unit-o_org_object->get_org_model_data(
                                               iv_eval_path  = mo_cvc_om->evaluation_path-orgunit_2_org_unit
                                               iv_auth_check = iv_auth_check ).
  ENDMETHOD.                    "get_my_superior_org_units


  METHOD get_next_higher_org_unit_2_me.
    "-----------------------------------------------------------------*
    "   Get next upper org. unit from result of superior org. unit search
    "-----------------------------------------------------------------*
    TRY.
        get_my_superior_org_units( iv_auth_check ).

*       Example of a result in table MT_SUPERIOR_ORG_UNITS of the method above => important here = PUP
*       OT OBJID     STEXT            SEQNR LEVEL PDOWN VCOUNT PNEXT PUP
*       O  00000037  Academy            1     1     2     1      0    0    <== starting point
*       O  00000038  HCM department     2     2     3     1      0    1    <== next level = target
*       O  00000047  Management board   3     3     4     1      0    2
*       O  00000003  Company            4     4     5     1      0    3
*       O  00000002  Entire group       5     5     0     0      0    4

        DATA(ls_superior_org_unit) = CORRESPONDING swhactor( mt_superior_org_units[ otype = swfco_org_orgunit
                                                                                    pup   = 1 ] ).

        result = create_instance_2_org_object( ls_superior_org_unit ).

      CATCH cx_sy_itab_line_not_found.
        "Structure PLVAR OTYPE OBJID WEGID: No agent found.
        RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
          MESSAGE ID '5W' TYPE 'E' NUMBER '170'
          WITH zcl_ca_wf_om_org_model=>mv_plvar     ms_key-otype
              |{ ms_key-objid ALPHA = OUT }| mo_cvc_om->evaluation_path-orgunit_2_org_unit.
    ENDTRY.
  ENDMETHOD.                    "get_next_higher_org_unit_2_me


  METHOD is_any_person_assigned_2_ounit.
    "-----------------------------------------------------------------*
    "   Check whether a person is found to the requested org. unit
    "-----------------------------------------------------------------*
    result = xsdbool( line_exists( it_manager[ otype = swfco_org_person ] ) ).

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "Structure PLVAR OTYPE OBJID WEGID: No agent found.
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        MESSAGE ID '5W' TYPE 'E' NUMBER '170'
        WITH zcl_ca_wf_om_org_model=>mv_plvar     ms_key-otype
            |{ ms_key-objid ALPHA = OUT }| mo_cvc_om->evaluation_path-manager_2_org_unit.
    ENDIF.
  ENDMETHOD.                    "is_any_person_assigned_2_ounit


  METHOD is_org_obj_4_multiple_people.
    "-----------------------------------------------------------------*
    "   Check whether it is an org. object for multiple people
    "-----------------------------------------------------------------*
    result = xsdbool( ms_key-otype EQ swfco_org_orgunit  OR
                      ms_key-otype EQ swfco_org_position ).

    IF result EQ abap_false   AND
       result IS NOT SUPPLIED.
      "Search members to a person/SAP user (&1) is not supported
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>a_person_can_t_have_members
          mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
          mv_msgv1 = CONV #( condense( |{ ms_key-otype } { ms_key-objid ALPHA = OUT }| ) ).
    ENDIF.
  ENDMETHOD.                    "is_org_obj_4_multiple_people


  METHOD set_different_plan_version.
    "-----------------------------------------------------------------*
    "   Set different plan version
    "-----------------------------------------------------------------*
    mv_plvar = iv_plvar.
    CLEAR mt_buffer.
  ENDMETHOD.                    "set_different_plan_version


  METHOD is_member_a_manager_of_the_ou.
    "-----------------------------------------------------------------*
    "   Check whether the passed member is a manager of this org. unit
    "-----------------------------------------------------------------*
    get_managers_2_my_org_unit( iv_auth_check ).

    result = xsdbool( line_exists( mt_managers[ otype = is_member_key-otype
                                                objid = is_member_key-objid ] ) ).

    IF result EQ abap_false AND
       result IS NOT SUPPLIED.
      "&1 &2 is not a manager of the org. unit &3
      RAISE EXCEPTION TYPE zcx_ca_wf_om_no_manager
        EXPORTING
          mv_msgty = zcx_ca_wf_om_no_manager=>c_msgty_e
          mv_msgv1 = CONV #( is_member_key-otype )
          mv_msgv2 = CONV #( |{ is_member_key-objid ALPHA = OUT }| )
          mv_msgv3 = CONV #( ms_my_org_unit-o_org_object->mv_default_attr ).
    ENDIF.
  ENDMETHOD.                    "is_member_a_manager_of_the_ou


  METHOD zif_ca_wf_om_org_model~find_employees_2_job.
    "-----------------------------------------------------------------*
    "   Find employees with job x assigned to their position
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_employees        TYPE zca_wf_t_employees_lookup,
      lt_jobs_2_om_object TYPE STANDARD TABLE OF zca_wf_s_org_model_data
                                            WITH NON-UNIQUE SORTED KEY ky_otype COMPONENTS otype  seqnr
                                            WITH NON-UNIQUE SORTED KEY ky_owner COMPONENTS pup    otype.

    mo_cvc_om->is_job_id_valid( iv_job_as ).

    TRY.
        IF ms_my_org_unit IS NOT INITIAL.
          RETURN.
        ENDIF.

        DATA(lv_evaluation_path) =
                       SWITCH wegid( ms_key-otype
                         WHEN swfco_org_person    THEN mo_cvc_om->evaluation_path-job_assignms_2_person
                         WHEN swfco_org_position  THEN mo_cvc_om->evaluation_path-job_assignms_2_position
                         WHEN swfco_org_orgunit   THEN mo_cvc_om->evaluation_path-job_assignms_2_org_unit
                                                            "Parameter '&1' has invalid value '&2'
                         ELSE THROW zcx_ca_wf_om_org_model( textid   = zcx_ca_wf_om_org_model=>param_invalid
                                                            mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
                                                            mv_msgv1 = 'MS_KEY-OTYPE'
                                                            mv_msgv2 = CONV #( ms_key-otype ) ) ) ##no_text.

        lt_jobs_2_om_object = get_org_model_data( iv_eval_path = lv_evaluation_path ).

        IF NOT line_exists( lt_jobs_2_om_object[ KEY ky_otype
                                                 otype = swfco_org_job
                                                 objid = iv_job_as ] ).
          "No org. management objects found to &1 &2 &3 using evalutation path Id &4
          RAISE EXCEPTION NEW zcx_ca_wf_om_no_om_objects( textid   = zcx_ca_wf_om_no_om_objects=>no_om_objects_2_eval_path
                                                          mv_msgty = zcx_ca_wf_om_no_om_objects=>c_msgty_e
                                                          mv_msgv1 = CONV #( swfco_org_job )
                                                          mv_msgv2 = CONV #( iv_job_as )
                                                          mv_msgv3 = space
                                                          mv_msgv4 = CONV #( lv_evaluation_path ) ).
        ENDIF.

        LOOP AT lt_jobs_2_om_object USING KEY ky_otype
                                    REFERENCE INTO DATA(lr_job)  WHERE otype EQ swfco_org_job
                                                                   AND objid EQ iv_job_as
                                    GROUP BY ( job_type    = lr_job->otype
                                               upper_level = lr_job->pup ) REFERENCE INTO DATA(lr_by_job_n_level).

          LOOP AT lt_jobs_2_om_object USING KEY ky_owner
                                      REFERENCE INTO DATA(lr_person_2_job)
                                          WHERE pup   EQ lr_by_job_n_level->upper_level
                                            AND otype EQ swfco_org_person.
            APPEND VALUE #( pernr = lr_person_2_job->objid ) TO lt_employees.
          ENDLOOP.
        ENDLOOP.
        IF lt_employees IS NOT INITIAL.
          result = zcl_ca_wf_om_employee_utils=>find_employees( it_employees     = lt_employees
                                                                iv_valid_on      = iv_valid_on
                                                                iv_search_active = iv_search_active
                                                                iv_raise_excep   = abap_false ).
        ENDIF.

      CATCH zcx_ca_wf_om_no_om_objects INTO DATA(lx_no_job_found).
        RAISE EXCEPTION lx_no_job_found.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                     iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                     iv_method   = 'FIND_EMPLOYEES_2_JOB'
                                                     ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_org_model~find_employees_2_job


  METHOD zif_ca_wf_om_org_model~find_employees_2_task.
    "-----------------------------------------------------------------*
    "   Find employees with task x assigned to their position
    "-----------------------------------------------------------------*
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        MESSAGE ID '38' TYPE zcx_ca_wf_om_org_model=>c_msgty_e NUMBER '038'
        WITH 'Suche nach Task ist derzeit nicht unterstützt!'.
*    "Local data definitions
*    DATA:
*      lt_employees         TYPE zca_wf_t_employees_lookup,
*      lt_tasks_2_om_object TYPE STANDARD TABLE OF zca_wf_s_org_model_data
*                                            WITH NON-UNIQUE SORTED KEY ky_otype COMPONENTS otype  seqnr
*                                            WITH NON-UNIQUE SORTED KEY ky_owner COMPONENTS pup    otype.
*
*    mo_cvc_om->is_task_id_valid( iv_task ).
*
*    TRY.
*        IF ms_my_org_unit IS NOT INITIAL.
*          RETURN.
*        ENDIF.
*
*        DATA(lv_evaluation_path) =
*                       SWITCH wegid( ms_key-otype
*                         WHEN swfco_org_person    THEN mo_cvc_om->evaluation_path-job_assignms_2_person
*                         WHEN swfco_org_position  THEN mo_cvc_om->evaluation_path-job_assignms_2_position
*                         WHEN swfco_org_orgunit   THEN mo_cvc_om->evaluation_path-job_assignms_2_org_unit
*                                                            "Parameter '&1' has invalid value '&2'
*                         ELSE THROW zcx_ca_wf_om_org_model( textid   = zcx_ca_wf_om_org_model=>param_invalid
*                                                            mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
*                                                            mv_msgv1 = 'MS_KEY-OTYPE'
*                                                            mv_msgv2 = CONV #( ms_key-otype ) ) ) ##no_text.
*
*        lt_tasks_2_om_object = get_org_model_data( iv_eval_path = lv_evaluation_path ).
*
*        IF NOT line_exists( lt_tasks_2_om_object[ KEY ky_otype
*                                                  otype = swfco_org_task
*                                                  objid = iv_task ] ).
*          "No org. management objects found to &1 &2 &3 using evalutation path Id &4
*          RAISE EXCEPTION NEW zcx_ca_wf_om_no_om_objects( textid   = zcx_ca_wf_om_no_om_objects=>no_om_objects_2_eval_path
*                                                          mv_msgty = zcx_ca_wf_om_no_om_objects=>c_msgty_e
*                                                          mv_msgv1 = CONV #( swfco_org_task )
*                                                          mv_msgv2 = CONV #( iv_task )
*                                                          mv_msgv3 = space
*                                                          mv_msgv4 = CONV #( lv_evaluation_path ) ).
*        ENDIF.
*
*        LOOP AT lt_tasks_2_om_object USING KEY ky_otype
*                                     REFERENCE INTO DATA(lr_job)  WHERE otype EQ swfco_org_job
*                                                                    AND objid EQ iv_task
*                                     GROUP BY ( job_type    = lr_job->otype
*                                               upper_level = lr_job->pup ) REFERENCE INTO DATA(lr_by_job_n_level).
*
*          LOOP AT lt_tasks_2_om_object USING KEY ky_owner
*                                       REFERENCE INTO DATA(lr_person_2_job)
*                                           WHERE pup   EQ lr_by_job_n_level->upper_level
*                                             AND otype EQ swfco_org_person.
*            APPEND VALUE #( pernr = lr_person_2_job->objid ) TO lt_employees.
*          ENDLOOP.
*        ENDLOOP.
*        IF lt_employees IS NOT INITIAL.
*          result = zcl_ca_wf_om_employee_utils=>find_employees( it_employees     = lt_employees
*                                                                iv_valid_on      = iv_valid_on
*                                                                iv_search_active = iv_search_active
*                                                                iv_raise_excep   = abap_false ).
*        ENDIF.
*
*      CATCH zcx_ca_wf_om_no_om_objects INTO DATA(lx_no_job_found).
*        RAISE EXCEPTION lx_no_job_found.
*
*      CATCH zcx_ca_error INTO DATA(lx_catched).
*        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
*                                                     iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
*                                                     iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
*                                                     iv_method   = 'FIND_EMPLOYEES_2_TASK'
*                                                     ix_error    = lx_catched ) ) ##no_text.
*        IF lx_error IS BOUND.
*          RAISE EXCEPTION lx_error.
*        ENDIF.
*    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_org_model~find_employees_2_task


  METHOD zif_ca_wf_om_org_model~get_all_managers.
    "-----------------------------------------------------------------*
    "   Determine all managers to a organizational object
    "-----------------------------------------------------------------*
    DATA(lt_manager) = find_manager_2_org_unit( iv_search_upwards = iv_search_upwards
                                                iv_search_active  = mv_search_active
                                                iv_auth_check     = iv_auth_check ).

    "Sort after the percentage of engagement also by the OBJID (= Personnel no.) for a consistent result
    SORT lt_manager BY otype  vpriox  vprozt DESCENDING  objid.

    LOOP AT lt_manager INTO DATA(ls_manager)
                       WHERE otype EQ swfco_org_person.
      TRY.
          APPEND INITIAL LINE TO result REFERENCE INTO DATA(lr_manager).
          lr_manager->otype      = ls_manager-otype.
          lr_manager->objid      = ls_manager-objid.
          lr_manager->name       = ls_manager-stext.
          lr_manager->short_name = ls_manager-short.
          lr_manager->o_owner    = zcl_ca_wf_om_employee=>get_instance( iv_key           = ls_manager-objid
                                                                        iv_search_active = mv_search_active
                                                                        iv_valid_on      = mv_valid_on ).

        CATCH zcx_ca_error INTO DATA(lx_catched).
          DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                 iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                 iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                 iv_method   = 'GET_ALL_MANAGERS'
                                                 ix_error    = lx_catched ) ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_wf_om_org_model~get_all_managers


  METHOD zif_ca_wf_om_org_model~get_employees_2_org_object.
    "-----------------------------------------------------------------*
    "   Get SAP user to the current org. object (org. unit leader)
    "-----------------------------------------------------------------*
    mo_cvc_om->is_scope_valid( iv_scope ).

    is_org_obj_4_multiple_people( ).

    DATA(lt_members) = get_members_2_org_object( iv_auth_check ).

    LOOP AT lt_members REFERENCE INTO DATA(lr_member)
                       WHERE otype EQ swfco_org_person.
      TRY.
          IF iv_scope NE mo_cvc_om->scope-all.
            DATA(lv_is_manager) = is_member_a_manager_of_the_ou( iv_auth_check = iv_auth_check
                                                                 is_member_key = CORRESPONDING #( lr_member->* ) ).
            IF ( iv_scope      EQ mo_cvc_om->scope-members_only AND
                 lv_is_manager EQ abap_true ) OR
               ( iv_scope      EQ mo_cvc_om->scope-manager AND
                 lv_is_manager EQ abap_false ).
              CONTINUE.
            ENDIF.
          ENDIF.

          APPEND INITIAL LINE TO result REFERENCE INTO DATA(lr_result).
          lr_result->otype      = lr_member->otype.
          lr_result->objid      = lr_member->objid.
          lr_result->name       = lr_member->stext.
          lr_result->short_name = lr_member->short.
          lr_result->o_owner    = zcl_ca_wf_om_employee=>get_instance( iv_key           = lr_member->objid
                                                                       iv_search_active = mv_search_active
                                                                       iv_valid_on      = mv_valid_on ).

          IF iv_scope        EQ mo_cvc_om->scope-manager AND
             iv_all_managers EQ abap_false.
            RETURN.   "If the leader is requested only the first entry is relevant
          ENDIF.

        CATCH zcx_ca_error INTO DATA(lx_catched).
          lr_result->error   = abap_true.
          lr_result->err_msg = lx_catched->get_text( ).
          lr_result->o_error = lx_catched.
      ENDTRY.
    ENDLOOP.

    IF result IS INITIAL.
      "No people found to &1 &2 to scope &3
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>no_people_found_to_scope
          mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
          mv_msgv1 = CONV #( ms_key-otype )
          mv_msgv2 = CONV #( |{ ms_key-objid ALPHA = OUT }| )
          mv_msgv3 = CONV #( zcl_ca_utils=>compose_name_n_techn_id(
                                             iv_techn_id = iv_scope
                                             iv_descr    = SWITCH string( iv_scope
                                                             WHEN mo_cvc_om->scope-manager      THEN TEXT-man
                                                             WHEN mo_cvc_om->scope-members_only THEN TEXT-mem
                                                             WHEN mo_cvc_om->scope-all          THEN TEXT-all ) ) ).
    ENDIF.
  ENDMETHOD.                    "zif_ca_wf_om_org_model~get_employees_2_org_object


  METHOD zif_ca_wf_om_org_model~get_manager.
    "-----------------------------------------------------------------*
    "   Determine manager to a organizational object
    "-----------------------------------------------------------------*
    DATA(lt_manager) = find_manager_2_org_unit( iv_search_upwards = iv_search_upwards
                                                iv_search_active  = mv_search_active
                                                iv_auth_check     = iv_auth_check ).

    "Sort after the percentage of engagement also by the OBJID (= Personnel no.) for a consistent result
    SORT lt_manager BY otype  vpriox  vprozt DESCENDING  objid.

    LOOP AT lt_manager REFERENCE INTO DATA(lr_manager)
                       WHERE otype EQ swfco_org_person.
      TRY.
          result-otype      = lr_manager->otype.
          result-objid      = lr_manager->objid.
          result-name       = lr_manager->stext.
          result-short_name = lr_manager->short.
          result-o_owner    = zcl_ca_wf_om_employee=>get_instance( iv_key           = lr_manager->objid
                                                                   iv_search_active = mv_search_active
                                                                   iv_valid_on      = mv_valid_on ).

          RETURN.   "As leader is only the first entry relevant

        CATCH zcx_ca_error INTO DATA(lx_catched).
          DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                 iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                 iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                 iv_method   = 'GET_MANAGER'
                                                 ix_error    = lx_catched ) ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "zif_ca_wf_om_org_model~get_manager


  METHOD zif_ca_wf_om_org_model~get_org_model_data.
    "-----------------------------------------------------------------*
    "   Determine (depending) org. unit data (using RH_STRUC_GET)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_objec TYPE STANDARD TABLE OF objec WITH DEFAULT KEY
                                            WITH NON-UNIQUE SORTED KEY ky_object
                                                            COMPONENTS otype objid,
      lt_struc TYPE struc_t.

    "This authority check avoids an endless loop -> see OSS note 779974 / 2014
    CALL FUNCTION 'RH_STRU_AUTHORITY_CHECK'
      EXPORTING
        fcode                    = 'DISP'
        plvar                    = '01'
        otype                    = 'O'
        objid                    = '00000001'
      EXCEPTIONS
        no_stru_authority        = 1
        no_stru_authority_hyper  = 2
        no_stru_authority_at_all = 3
        no_base_authority        = 4
        OTHERS                   = 5 ##fm_subrc_ok ##no_text.

    "Determine depending objects
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype        = ms_search_4_org_object-otype
        act_objid        = ms_search_4_org_object-objid
        act_wegid        = iv_eval_path
        act_begda        = mv_valid_on
        act_endda        = mv_valid_on
        authority_check  = iv_auth_check
        act_plvar        = mv_plvar
        act_tdepth       = iv_tdepth
        act_vflag        = iv_vflag
        act_tflag        = iv_tflag
        text_buffer_fill = iv_tflag
        act_int_flag     = iv_int_flag
        buffer_mode      = abap_false
      TABLES
        result_objec     = lt_objec
        result_struc     = lt_struc
      EXCEPTIONS
        no_entry_found   = 1
        no_plvar_found   = 2
        OTHERS           = 3.
    CASE sy-subrc.
      WHEN 0.
        "At least one connection to another org. object was found

      WHEN 1.
        "No org. management objects found to &1 &2 &3 using evalutation path Id &4
        RAISE EXCEPTION TYPE zcx_ca_wf_om_no_om_objects
          EXPORTING
            textid   = zcx_ca_wf_om_no_om_objects=>no_om_objects_2_eval_path
            mv_msgty = zcx_ca_wf_om_no_om_objects=>c_msgty_e
            mv_msgv1 = CONV #( mv_plvar )
            mv_msgv2 = CONV #( ms_search_4_org_object-otype )
            mv_msgv3 = CONV #( ms_search_4_org_object-objid )
            mv_msgv4 = CONV #( iv_eval_path ).

      WHEN OTHERS.
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                               iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                               iv_function = 'RH_STRUC_GET'
                                               iv_subrc    = sy-subrc ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDCASE.

    "Merge data into returning parameter - it's 1-to-1 relationship
    result = CORRESPONDING #( lt_struc ).
    result = CORRESPONDING #( result FROM  lt_objec
                                     USING KEY ky_object otype = otype
                                                         objid = objid ).
  ENDMETHOD.                    "zif_ca_wf_om_org_model~get_org_model_data


  METHOD zif_ca_wf_om_org_model~get_text_of_org_object.
    "-----------------------------------------------------------------*
    "   Get short description of organizational unit
    "-----------------------------------------------------------------*
    "Get long and short description of department
    SELECT otype,  short AS short_name,
           objid,  stext AS name
                    INTO  CORRESPONDING FIELDS OF @result
                    FROM  hrp1000
                          UP TO 1 ROWS
                    WHERE plvar  EQ @zcl_ca_wf_om_org_model=>mv_plvar
                      AND otype  EQ @ms_key-otype
                      AND objid  EQ @ms_key-objid
                      AND istat  EQ '1'
                      AND begda  LE @mv_valid_on
                      AND endda  GE @mv_valid_on
                      AND langu  EQ @sy-langu.          "#EC CI_NOORDER
    ENDSELECT.
    IF sy-subrc NE 0 AND
       sy-langu EQ 'D' ##no_text.
      result-name = 'No descriptive text found!'(tx1).

    ELSE.
      "Try to get description in English if nothing was found
      SELECT otype,  short AS short_name,
             objid,  stext AS name
                      INTO  CORRESPONDING FIELDS OF @result
                      FROM  hrp1000
                            UP TO 1 ROWS
                      WHERE plvar  EQ @zcl_ca_wf_om_org_model=>mv_plvar
                        AND otype  EQ @ms_key-otype
                        AND objid  EQ @ms_key-objid
                        AND istat  EQ '1'
                        AND begda  LE @mv_valid_on
                        AND endda  GE @mv_valid_on
                        AND langu  EQ 'D'.              "#EC CI_NOORDER
      ENDSELECT.
      IF sy-subrc NE 0.
        result-name = 'No descriptive text found!'(tx1).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "zif_ca_wf_om_org_model~get_text_of_org_object


  METHOD zif_ca_workflow~check_existence.
    "-----------------------------------------------------------------*
    "   Check existence of object
    "-----------------------------------------------------------------*
    "Check if org. unit exist
    CALL FUNCTION 'RH_CHECK_ORG_OBJECT_EXISTS'
      EXPORTING
        act_object_ext       = CONV hrobjec_14( ms_key )
        act_plvar            = mv_plvar
        authority_check      = abap_false
        act_begda            = mv_valid_on
        act_endda            = mv_valid_on
      EXCEPTIONS
        no_org_object        = 1
        org_object_not_found = 2
        no_active_plvar      = 3
        OTHERS               = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                            iv_excp_cls  = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                            iv_function   = 'RH_CHECK_ORG_OBJECT_EXISTS'
                                            iv_subrc      = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    ms_data = get_text_of_org_object( ).
  ENDMETHOD.                    "zif_ca_workflow~check_existence


  METHOD zif_ca_workflow~get_task_descr.
    "-----------------------------------------------------------------*
    "   Assemble task short text
    "-----------------------------------------------------------------*
    "Example ==> see also method BI_OBJECT~DEFAULT_ATTRIBUTE_VALUE
    "TEXT-DAV = Org. object xxxxxxxxxx (t nnnnnn) - Description
    result = |{ mv_default_attr } - { iv_task_desc }|.

    "Use this statement in your task short description, here in this sample for a background step
*    &_WI_OBJECT_ID.GET_TASK_DESCR(IV_TASK_DESC='Post document (BG)')&
  ENDMETHOD.                    "zif_ca_workflow~get_task_descr


  METHOD zif_ca_workflow~raise_event.
    "-----------------------------------------------------------------*
    "   Raise event
    "-----------------------------------------------------------------*
    TRY.
        zcl_ca_wf_wapi_utils=>create_event_extended( is_lpor      = CORRESPONDING #( ms_lpor )
                                                     iv_event     = iv_event
                                                     io_evt_cnt   = io_evt_cnt
                                                     iv_do_commit = abap_true ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = mo_log->add_n_save_exception( ix_catched = lx_catched
                                                       iv_class   = CONV #( ms_lpor-typeid )
                                                       iv_method  = 'RAISE_EVENT' ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_workflow~raise_event

ENDCLASS.
