"! <p class="shorttext synchronized" lang="en">CA-TBX: Organisation model determinations</p>
CLASS zcl_ca_wf_om_org_model DEFINITION PUBLIC
                                        CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Scope of determination</p>
      BEGIN OF cs_scope,
        manager      TYPE char1 VALUE '1' ##no_text,
        members_only TYPE char1 VALUE '2' ##no_text,
        all          TYPE char1 VALUE '3' ##no_text,
      END OF cs_scope.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Plan version</p>
      mv_plvar             TYPE plvar  READ-ONLY.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Type and Id of organizational object</p>
      ms_org_object TYPE swhactor READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Object is valid on</p>
      mv_valid_on   TYPE hr_date READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class constructor</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Find employees by different, normally unique, keys</p>
      "!
      "! @parameter it_employees   | <p class="shorttext synchronized" lang="en">List of keys to determine employees</p>
      "! @parameter iv_raise_excep | <p class="shorttext synchronized" lang="en">X = Raise an exception; ' ' = Return message</p>
      "! @parameter rt_employees   | <p class="shorttext synchronized" lang="en">List of completed employees</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      find_employees
        IMPORTING
          it_employees        TYPE zca_wf_t_employees_lookup
          iv_raise_excep      TYPE abap_boolean DEFAULT abap_true
        RETURNING
          VALUE(rt_employees) TYPE zca_wf_t_employees_lookup
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Transfer determined employees into agent rule result</p>
      "!
      "! <p>The user of this method has to ensure by himself that the receiver in parameter IT_EMPLOYEES are valid.</p>
      "!
      "! @parameter it_employees | <p class="shorttext synchronized" lang="en">List of keys to determine employees</p>
      "! @parameter rt_actors    | <p class="shorttext synchronized" lang="en">Valid employees in agent rule format</p>
      transf_employees_2_rule_result
        IMPORTING
          it_employees     TYPE zca_wf_t_employees_lookup
        RETURNING
          VALUE(rt_actors) TYPE swfuagents,

      "! <p class="shorttext synchronized" lang="en">Transfer agent rule result into format of employee data</p>
      "!
      "! @parameter it_actors    | <p class="shorttext synchronized" lang="en">Valid employees in agent rule format</p>
      "! @parameter iv_valid_on  | <p class="shorttext synchronized" lang="en">Object is valid on</p>
      "! @parameter rt_employees | <p class="shorttext synchronized" lang="en">List of keys to determine employees</p>
      transf_rule_result_2_employees
        IMPORTING
          it_actors           TYPE swfuagents
          iv_valid_on         TYPE hr_date DEFAULT sy-datlo
        RETURNING
          VALUE(rt_employees) TYPE zca_wf_t_employees_lookup.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter is_org_object | <p class="shorttext synchronized" lang="en">Type and Id of org. object (use SWFCO_ORG_* for type)</p>
      "! @parameter iv_valid_on   | <p class="shorttext synchronized" lang="en">Object is valid on</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      constructor
        IMPORTING
          is_org_object TYPE swhactor
          iv_valid_on   TYPE hr_date  DEFAULT sy-datlo
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get employees to the current org. object</p>
      "!
      "! @parameter iv_scope          | <p class="shorttext synchronized" lang="en">Scope (only relevant for pos. + org. unit) -&gt; use CS_SCOPE-*</p>
      "! @parameter iv_search_upwards | <p class="shorttext synchronized" lang="en">X = Search in higher levels if no manager is found</p>
      "! @parameter iv_auth_check     | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Determined employees</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      get_employees_2_org_object
        IMPORTING
          iv_scope          TYPE char1    DEFAULT zcl_ca_wf_om_org_model=>cs_scope-manager
          iv_search_upwards TYPE abap_boolean DEFAULT abap_false
          iv_auth_check     TYPE hr_authy DEFAULT abap_true
        RETURNING
          VALUE(result)     TYPE zca_wf_t_om_objects_lookup
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Determine manager to organizational object</p>
      "!
      "! @parameter iv_search_upwards | <p class="shorttext synchronized" lang="en">X = Search in higher levels if no manager is found</p>
      "! @parameter iv_auth_check     | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Manager to current org. object</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      get_manager
        IMPORTING
          iv_search_upwards TYPE abap_boolean DEFAULT abap_false
          iv_auth_check     TYPE hr_authy DEFAULT abap_true
        RETURNING
          VALUE(result)     TYPE zca_wf_s_om_object_lookup
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Determine (depending) org. unit data (using RH_STRUC_GET)</p>
      "!
      "! @parameter iv_eval_path  | <p class="shorttext synchronized" lang="en">Evaluation path</p>
      "! @parameter iv_auth_check | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter iv_plvar      | <p class="shorttext synchronized" lang="en">Plan version</p>
      "! @parameter iv_tdepth     | <p class="shorttext synchronized" lang="en">Technical depth of structure</p>
      "! @parameter iv_tflag      | <p class="shorttext synchronized" lang="en">X = Supply texts</p>
      "! @parameter iv_vflag      | <p class="shorttext synchronized" lang="en">X = Supply relationship information</p>
      "! @parameter iv_int_flag   | <p class="shorttext synchronized" lang="en">X = Read evaluation path in internal table</p>
      "! @parameter result        | <p class="shorttext synchronized" lang="en">Merged result of FM RH_STRUC_GET</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      get_org_model_data
        IMPORTING
          iv_eval_path  TYPE wegid
          iv_auth_check TYPE hr_authy   DEFAULT abap_true
          iv_plvar      TYPE plvar      DEFAULT zcl_ca_wf_om_org_model=>mv_plvar
          iv_tdepth     TYPE tdepth     DEFAULT 0
          iv_vflag      TYPE hr_vflag   DEFAULT abap_true
          iv_tflag      TYPE hr_tflag   DEFAULT abap_true
          iv_int_flag   TYPE hr_77awint DEFAULT abap_false
        RETURNING
          VALUE(result) TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get short description of organizational unit</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Short description of org. object</p>
      get_text_of_org_object
        RETURNING
          VALUE(result) TYPE zca_wf_s_om_object.


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
      "! <p class="shorttext synchronized" lang="en">Position owner</p>
      ty_t_pos_owner   TYPE STANDARD TABLE OF hrpe_prozt.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Evaluation path</p>
      BEGIN OF cs_eval_path,
        manager_2_orgunit       TYPE wegid VALUE 'BOSSONLY' ##no_text,
        orgunit_2_orgunit       TYPE wegid VALUE 'O-O' ##no_text,
        person_position_orgunit TYPE wegid VALUE 'P-S-O' ##no_text,
        staff_2_orgunit         TYPE wegid VALUE 'SBES' ##no_text,
      END OF cs_eval_path.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Leaders to be excluded for a scope of 'members only'</p>
      mt_excl_leaders          TYPE zca_wf_t_org_model_data,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Type and Id of organizational object for search</p>
      ms_org_object_for_search TYPE swhactor,
      "! <p class="shorttext synchronized" lang="en">Details to the position of the requested object</p>
      ms_my_position           TYPE zca_wf_s_om_object,
      "! <p class="shorttext synchronized" lang="en">Details to the org. unit of the requested object</p>
      ms_my_org_unit           TYPE zca_wf_s_om_object,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Requested scope</p>
      mv_scope                 TYPE char1.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Find personnel number to SAP user Id</p>
      "!
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      find_dependent_org_objects
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Find manager to org. unit, search upwards if requested</p>
      "!
      "! @parameter iv_search_upwards | <p class="shorttext synchronized" lang="en">X = Search in higher levels if no manager is found</p>
      "! @parameter iv_auth_check     | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Determined manager</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      find_manager_2_org_unit
        IMPORTING
          iv_search_upwards TYPE abap_boolean
          iv_auth_check     TYPE hr_authy
        RETURNING
          VALUE(result)     TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get leaders to the org. object in case of 'members only'</p>
      "!
      "! @parameter iv_search_upwards | <p class="shorttext synchronized" lang="en">X = Search in higher levels if no manager is found</p>
      "! @parameter iv_auth_check     | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Merged result of FM RH_STRUC_GET</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_managers_2b_excluded
        IMPORTING
          iv_search_upwards TYPE abap_boolean
          iv_auth_check     TYPE hr_authy
        RETURNING
          VALUE(result)     TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get members to the current org. object respecting the scope</p>
      "!
      "! @parameter iv_search_upwards | <p class="shorttext synchronized" lang="en">X = Search in higher levels if no manager is found</p>
      "! @parameter iv_auth_check     | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Merged result of FM RH_STRUC_GET</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_members_2_org_object
        IMPORTING
          iv_search_upwards TYPE abap_boolean
          iv_auth_check     TYPE hr_authy
        RETURNING
          VALUE(result)     TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get next upper org. unit from result of org. unit search</p>
      "!
      "! @parameter it_superior_org_units  | <p class="shorttext synchronized" lang="en">Superior org. units to this org. unit</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">Id of superior org. unit</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_superior_org_unit
        IMPORTING
          it_superior_org_units TYPE zca_wf_t_org_model_data
        RETURNING
          VALUE(result)         TYPE swhactor
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Check whether a person is found to the requested org. unit</p>
      "!
      "! @parameter it_org_unit_result     | <p class="shorttext synchronized" lang="en">Result of last org. model search</p>
      "! @parameter io_org_object          | <p class="shorttext synchronized" lang="en">Instanz to object of last org. model search</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">X = Result of search contains at least one person</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_any_person_assigned_2_ounit
        IMPORTING
          it_org_unit_result TYPE zca_wf_t_org_model_data
          io_org_object      TYPE REF TO zcl_ca_wf_om_org_model
        RETURNING
          VALUE(result)      TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Check if the personnel Id is a leader of the org. object</p>
      "!
      "! @parameter is_personnel_id | <p class="shorttext synchronized" lang="en">Personnel Id</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">X = Value for scope is valid</p>
      is_member_a_leader
        IMPORTING
          is_personnel_id TYPE swhactor
        RETURNING
          VALUE(result)   TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Check whether it is an org. object for multiple people</p>
      "!
      "! @parameter iv_scope | <p class="shorttext synchronized" lang="en">Scope (only for position + org. unit) -&gt; use CS_SCOPE-*</p>
      "! @parameter result   | <p class="shorttext synchronized" lang="en">X = Org. object is a position or an org. unit</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_org_obj_4_multiple_people
        IMPORTING
          iv_scope      TYPE char1
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Check if the passed scope is valid</p>
      "!
      "! @parameter iv_scope | <p class="shorttext synchronized" lang="en">Scope (only for position + org. unit) -&gt; use CS_SCOPE-*</p>
      "! @parameter result   | <p class="shorttext synchronized" lang="en">X = Value for scope is valid</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_scope_valid
        IMPORTING
          iv_scope      TYPE char1
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Search upwards in org. structure for superior org. unit</p>
      "!
      "! @parameter io_org_object | <p class="shorttext synchronized" lang="en">Current org. unit during drill up org. structure</p>
      "! @parameter result        | <p class="shorttext synchronized" lang="en">Next upper / superior org. unit to input org. unit</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      search_upwards_4_org_unit
        IMPORTING
          io_org_object TYPE REF TO zcl_ca_wf_om_org_model
          iv_auth_check TYPE hr_authy
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_om_org_model
        RAISING
          zcx_ca_wf_om_org_model.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_wf_om_org_model IMPLEMENTATION.

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
      DATA(lx_error) = CAST zcx_ca_wf_om_org_model(
                                    zcx_ca_error=>create_exception(
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
    "Add leading zeros for numeric object Ids = all, except user Ids
    DATA(ls_org_object) = VALUE swhactor( otype = is_org_object-otype
                                          objid = COND #( WHEN is_org_object-otype NE swfco_org_user
                                                            THEN |{ is_org_object-objid WIDTH = 8
                                                                                        ALPHA = IN }|
                                                            ELSE is_org_object-objid ) ).
    "Check if org. unit exist
    CALL FUNCTION 'RH_CHECK_ORG_OBJECT_EXISTS'
      EXPORTING
        act_object_ext       = CONV hrobjec_14( ls_org_object )
        act_plvar            = mv_plvar
        authority_check      = abap_false
        act_begda            = iv_valid_on
        act_endda            = iv_valid_on
      EXCEPTIONS
        no_org_object        = 1
        org_object_not_found = 2
        no_active_plvar      = 3
        OTHERS               = 4.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_om_org_model(
                                zcx_ca_error=>create_exception(
                                                iv_excp_cls  = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                iv_function   = 'RH_CHECK_ORG_OBJECT_EXISTS'
                                                iv_subrc      = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    ms_org_object = ms_org_object_for_search = ls_org_object.
    mv_valid_on   = iv_valid_on.
  ENDMETHOD.                    "constructor


  METHOD find_dependent_org_objects.
    "-----------------------------------------------------------------*
    "   Find dependent org. object to requested org. object
    "   For a SAP user Id it is necessary to find the corresponding
    "   person (personnel Id) to be able to find higher org. objects
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_employee          TYPE REF TO zcl_ca_wf_om_employee.

    TRY.
        CASE ms_org_object-otype.
          WHEN swfco_org_user.
            lo_employee = zcl_ca_wf_om_employee=>get_instance_by_sap_user_id( iv_sap_user_id = ms_org_object-objid
                                                                              iv_valid_on    = mv_valid_on ).
            ms_org_object_for_search = VALUE #( otype = swfco_org_person
                                                objid = lo_employee->ms_data-pernr ).
            ms_my_position = lo_employee->ms_data-s_position.
            ms_my_org_unit = lo_employee->ms_data-s_org_unit.

          WHEN swfco_org_person.
            lo_employee = zcl_ca_wf_om_employee=>get_instance( iv_key      = CONV #( ms_org_object-objid )
                                                               iv_valid_on = mv_valid_on ).
            ms_my_position = lo_employee->ms_data-s_position.
            ms_my_org_unit = lo_employee->ms_data-s_org_unit.

          WHEN swfco_org_position.
            ms_my_position = get_text_of_org_object( ).       "Get my own description
            DATA(lt_higher_org_objects) = get_org_model_data( iv_eval_path  = cs_eval_path-person_position_orgunit
                                                              iv_auth_check = abap_false ).
            ms_my_org_unit = CORRESPONDING #( lt_higher_org_objects[ otype = swfco_org_orgunit ]
                                                                          MAPPING short_name = short
                                                                                  name       = stext ).

          WHEN swfco_org_orgunit.
            ms_my_org_unit = get_text_of_org_object( ).       "Get my own description
        ENDCASE.

      CATCH zcx_ca_param
            zcx_ca_dbacc INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                                         iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                         iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                         iv_method   = 'FIND_DEPENDENT_ORG_OBJECTS'
                                                         ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "find_dependent_org_objects


  METHOD find_employees.
    "-----------------------------------------------------------------*
    "   Find employees by different, normally unique, keys
    "-----------------------------------------------------------------*
    LOOP AT it_employees INTO DATA(ls_employee).
      TRY.
          IF ls_employee-pernr IS NOT INITIAL.
            ls_employee-o_employee = zcl_ca_wf_om_employee=>get_instance(
                                               is_lpor     = VALUE #( instid = CONV #( ls_employee-pernr )
                                                                      typeid = CONV #( ls_employee-class_name ) )
                                               iv_valid_on = ls_employee-valid_on ).

          ELSEIF ls_employee-bname IS NOT INITIAL.
            ls_employee-o_employee = zcl_ca_wf_om_employee=>get_instance_by_sap_user_id(
                                                                   iv_sap_user_id   = ls_employee-bname
                                                                   iv_user_cls_name = ls_employee-class_name
                                                                   iv_valid_on      = ls_employee-valid_on ).

          ELSEIF ls_employee-net_id IS NOT INITIAL.
            ls_employee-o_employee = zcl_ca_wf_om_employee=>get_instance_by_windows_net_id(
                                                                   iv_windows_net_id = ls_employee-net_id
                                                                   iv_user_cls_name  = ls_employee-class_name
                                                                   iv_valid_on       = ls_employee-valid_on ).

          ELSE.
            "Too less data for determination of organizational informations
            RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
              EXPORTING
                textid = zcx_ca_wf_om_org_model=>too_less_data.
          ENDIF.

          ls_employee-bname     = ls_employee-o_employee->ms_data-bname.
          ls_employee-pernr     = ls_employee-o_employee->ms_data-pernr.
          ls_employee-net_id    = ls_employee-o_employee->ms_data-net_id.
          ls_employee-mail_addr = ls_employee-o_employee->ms_data-mail_addr.
          ls_employee-error     = abap_false.
          CLEAR: ls_employee-err_msg,
                 ls_employee-o_error.
          APPEND ls_employee TO rt_employees.

        CATCH zcx_ca_error INTO DATA(lx_catched).
          ls_employee-error   = abap_true.
          ls_employee-err_msg = lx_catched->get_text( ).
          ls_employee-o_error = lx_catched.
          APPEND ls_employee TO rt_employees.
      ENDTRY.
    ENDLOOP.

    IF iv_raise_excep EQ abap_true AND
       lx_catched     IS BOUND.
      "Error occurred during data determination. For details see field "ERR_MSG".
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid = zcx_ca_wf_om_org_model=>error_occurred.
    ENDIF.
  ENDMETHOD.                    "find_employees


  METHOD get_employees_2_org_object.
    "-----------------------------------------------------------------*
    "   Get SAP user to the current org. object (org. unit leader)
    "-----------------------------------------------------------------*
    is_scope_valid( iv_scope ).

    is_org_obj_4_multiple_people( iv_scope ).

    find_dependent_org_objects( ).

    DATA(lt_members) = get_members_2_org_object( iv_search_upwards = iv_search_upwards
                                                 iv_auth_check     = iv_auth_check ).
    mt_excl_leaders  = get_managers_2b_excluded( iv_search_upwards = iv_search_upwards
                                                 iv_auth_check     = iv_auth_check ).

    LOOP AT lt_members REFERENCE INTO DATA(lr_member)
                       WHERE otype EQ swfco_org_person.
      TRY.
          IF is_member_a_leader( VALUE #( otype = lr_member->otype
                                          objid = lr_member->objid ) ).
            CONTINUE.
          ENDIF.

          APPEND INITIAL LINE TO result REFERENCE INTO DATA(lr_result).
          lr_result->otype      = lr_member->otype.
          lr_result->objid      = lr_member->objid.
          lr_result->name       = lr_member->stext.
          lr_result->short_name = lr_member->short.
          lr_result->o_owner    = zcl_ca_wf_om_employee=>get_instance( iv_key = lr_member->objid ).

          IF iv_scope EQ cs_scope-manager.
            RETURN.   "If the leader is requested only the first entry is relevant
          ENDIF.

        CATCH zcx_ca_error INTO DATA(lx_catched).
          lr_result->error    = abap_true.
          lr_result->o_error ?= lx_catched.
          lr_result->err_msg  = lx_catched->get_text( ).
      ENDTRY.
    ENDLOOP.

    IF result IS INITIAL.
      "No people found to &1 &2 to scope &3
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>no_people_found_to_scope
          mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
          mv_msgv1 = CONV #( ms_org_object-otype )
          mv_msgv2 = CONV #( |{ ms_org_object-objid ALPHA = OUT }| )
          mv_msgv3 = CONV #( condense( |{ iv_scope } ({ SWITCH string( iv_scope
                                                          WHEN cs_scope-manager      THEN TEXT-man
                                                          WHEN cs_scope-members_only THEN TEXT-mem
                                                          WHEN cs_scope-all          THEN TEXT-all ) })| ) ).
    ENDIF.
  ENDMETHOD.                    "get_employees_2_org_object


  METHOD find_manager_2_org_unit.
    "-----------------------------------------------------------------*
    "   Find manager to current org. unit, search upwards if requested
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_org_object        TYPE REF TO zcl_ca_wf_om_org_model.

    "Prepare the local instance for iterative search upwards
    IF ms_org_object_for_search-otype EQ swfco_org_orgunit.
      lo_org_object = me.
    ELSE.
      lo_org_object = NEW #( is_org_object = ms_my_org_unit-s_om_obj_key
                             iv_valid_on   = mv_valid_on ).
    ENDIF.

    "The following TRY ... CATCH works like DO-loop because of the RETRY near the ENDTRY. The starting
    "instance is ME and will be replaced by the next superior org. unit if no manager was found. This
    "definition must be outside of the TRY ... CATCH to be able to work.
    TRY.
        result = lo_org_object->get_org_model_data( iv_eval_path  = cs_eval_path-manager_2_orgunit
                                                    iv_auth_check = iv_auth_check ) ##no_text.

        is_any_person_assigned_2_ounit( it_org_unit_result = result
                                        io_org_object      = lo_org_object ).

      CATCH zcx_ca_wf_om_org_model INTO DATA(lx_catched).
        "If exception is different to 'Nothing found' forward the exception to caller
        IF   iv_search_upwards                         EQ abap_false  OR
             "Message = 'Structure PLVAR OTYPE OBJID WEGID: No agent found.'
           ( lx_catched->if_t100_message~t100key-msgid NE '5W'       AND
             lx_catched->if_t100_message~t100key-msgno NE '170' ) ##no_text.
          RAISE EXCEPTION lx_catched.

        ELSE.
          "No manager found -> search for next superior org. unit
          lo_org_object = search_upwards_4_org_unit( io_org_object = lo_org_object
                                                     iv_auth_check = iv_auth_check ).
          RETRY.       "ATTENTION: This makes this TRY ... CATCH to a DO loop
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "find_manager_2_org_unit


  METHOD get_manager.
    "-----------------------------------------------------------------*
    "   Determine manager to organizational object
    "-----------------------------------------------------------------*
    find_dependent_org_objects( ).

    DATA(lt_manager) = find_manager_2_org_unit( iv_search_upwards = iv_search_upwards
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
          result-o_owner    = zcl_ca_wf_om_employee=>get_instance( iv_key = lr_manager->objid ).

          RETURN.   "As leader is only the first entry relevant

        CATCH zcx_ca_error INTO DATA(lx_catched).
          "Raise an exception since it can only be one leader (common and general rule in EVN workflows)
          DATA(lx_error) = CAST zcx_ca_wf_om_org_model(
                                    zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                                   iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                   iv_method   = 'GET_MANAGER'
                                                   ix_error    = lx_catched ) ) ##no_text.
          IF lx_error IS BOUND.
            RAISE EXCEPTION lx_error.
          ENDIF.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "get_manager


  METHOD get_managers_2b_excluded.
    "-----------------------------------------------------------------*
    "   Get leaders to the current org. object in case of 'members only'
    "-----------------------------------------------------------------*
    IF mv_scope            EQ cs_scope-members_only AND
       ms_org_object-otype EQ swfco_org_orgunit.
      "Get leaders to exclude them for a 'members only'-result
      result = find_manager_2_org_unit( iv_search_upwards = iv_search_upwards
                                        iv_auth_check     = iv_auth_check ).
    ENDIF.
  ENDMETHOD.                    "get_managers_2b_excluded


  METHOD get_members_2_org_object.
    "-----------------------------------------------------------------*
    "   Get members to the current org. object respecting the scope
    "-----------------------------------------------------------------*
    IF mv_scope EQ cs_scope-manager.
      result = find_manager_2_org_unit( iv_search_upwards = iv_search_upwards
                                        iv_auth_check     = iv_auth_check ).

    ELSE.
      "The evaluation path should always return an assignment to a personnel number. Using the
      "attribute MS_ORG_OBJECT_FOR_SEARCH there can only be the object types P, S and O -> see
      "methods CONSTRUCTOR + FIND_PERSONNEL_NUMBER_TO_USER.
      result = get_org_model_data( iv_auth_check = iv_auth_check
                                   iv_eval_path  = SWITCH wegid( ms_org_object_for_search-otype
                                                     WHEN swfco_org_person   THEN cs_eval_path-person_position_orgunit
                                                     WHEN swfco_org_position OR   "STAFF_2_O... = for scope all + members only
                                                          swfco_org_orgunit  THEN cs_eval_path-staff_2_orgunit ) ) ##no_text.
    ENDIF.

    IF mv_scope EQ cs_scope-manager.
      "Sort after the percentage of engagement also by the OBJID (= Personnel no.) for a consistent result
      SORT result BY otype  vpriox  vprozt DESCENDING  objid.
    ENDIF.
  ENDMETHOD.                    "get_members_2_org_object


  METHOD get_org_model_data.
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
        act_otype        = ms_org_object_for_search-otype
        act_objid        = ms_org_object_for_search-objid
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
    IF sy-subrc NE 0.
      DATA(lx_error) =
           CAST zcx_ca_wf_om_org_model(
                  zcx_ca_error=>create_exception(
                           iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                           iv_function = 'RH_STRUC_GET'
                           iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Merge data into returning parameter - it's 1-to-1 relationship
    result = CORRESPONDING #( lt_struc ).
    result = CORRESPONDING #( result FROM  lt_objec
                                     USING KEY ky_object otype = otype
                                                         objid = objid ).
  ENDMETHOD.                    "get_org_data


  METHOD get_superior_org_unit.
    "-----------------------------------------------------------------*
    "   Get next upper org. unit from result of superior org. unit search
    "-----------------------------------------------------------------*
    TRY.
        result = CORRESPONDING #( it_superior_org_units[ otype = swfco_org_orgunit
                                                         pup   = 1 ] ).

      CATCH cx_sy_itab_line_not_found.
        "Structure PLVAR OTYPE OBJID WEGID: No agent found.
        RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
          MESSAGE ID '5W' TYPE 'E' NUMBER '170'
          WITH zcl_ca_wf_om_org_model=>mv_plvar ms_org_object-otype
              |{ ms_org_object-objid ALPHA = OUT }| cs_eval_path-orgunit_2_orgunit.
    ENDTRY.
  ENDMETHOD.                    "get_superior_org_unit


  METHOD get_text_of_org_object.
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
                      AND otype  EQ @ms_org_object-otype
                      AND objid  EQ @ms_org_object-objid
                      AND istat  EQ '1'
                      AND begda  LE @mv_valid_on
                      AND endda  GE @mv_valid_on
                      AND langu  EQ @sy-langu.          "#EC CI_NOORDER
    ENDSELECT.
    IF sy-subrc NE 0 AND
       sy-langu EQ 'E' ##no_text.
      result-name = 'No descriptive text found!'(tx1).

    ELSE.
      "Try to get description in English if nothing was found
      SELECT otype,  short AS short_name,
             objid,  stext AS name
                      INTO  CORRESPONDING FIELDS OF @result
                      FROM  hrp1000
                            UP TO 1 ROWS
                      WHERE plvar  EQ @zcl_ca_wf_om_org_model=>mv_plvar
                        AND otype  EQ @ms_org_object-otype
                        AND objid  EQ @ms_org_object-objid
                        AND istat  EQ '1'
                        AND begda  LE @mv_valid_on
                        AND endda  GE @mv_valid_on
                        AND langu  EQ 'E'.              "#EC CI_NOORDER
      ENDSELECT.
      IF sy-subrc NE 0.
        result-name = 'No descriptive text found!'(tx1).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_text_of_org_object


  METHOD is_any_person_assigned_2_ounit.
    "-----------------------------------------------------------------*
    "   Check whether a person is found to the requested org. unit
    "-----------------------------------------------------------------*
    result = xsdbool( line_exists( it_org_unit_result[ otype = swfco_org_person ] ) ).

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "Structure PLVAR OTYPE OBJID WEGID: No agent found.
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        MESSAGE ID '5W' TYPE 'E' NUMBER '170'
        WITH zcl_ca_wf_om_org_model=>mv_plvar ms_org_object-otype
            |{ ms_org_object-objid ALPHA = OUT }| cs_eval_path-manager_2_orgunit.
    ENDIF.
  ENDMETHOD.                    "is_any_person_assigned_2_ounit


  METHOD is_member_a_leader.
    "-----------------------------------------------------------------*
    "   Check if the personnel Id is a leader of the org. object
    "-----------------------------------------------------------------*
    result = abap_false.
    IF mv_scope EQ cs_scope-members_only AND
       line_exists( mt_excl_leaders[ otype = is_personnel_id-otype
                                     objid = is_personnel_id-objid ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_member_a_leader


  METHOD is_org_obj_4_multiple_people.
    "-----------------------------------------------------------------*
    "   Check whether it is an org. object for multiple people
    "-----------------------------------------------------------------*
    result = abap_true.
    IF   iv_scope            NE cs_scope-manager AND
       ( ms_org_object-otype EQ swfco_org_user    OR
         ms_org_object-otype EQ swfco_org_person ).
      result = abap_false.
    ENDIF.

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "Search members to a person/SAP user (&1) is not supported
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>a_person_can_t_have_members
          mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
          mv_msgv1 = CONV #( condense( |{ ms_org_object-otype } { ms_org_object-objid ALPHA = OUT }| ) ).
    ENDIF.
  ENDMETHOD.                    "is_org_obj_4_multiple_people


  METHOD is_scope_valid.
    "-----------------------------------------------------------------*
    "   Check if the passed scope is valid
    "-----------------------------------------------------------------*
    result = abap_false.
    IF iv_scope CO '123' ##no_text.
      mv_scope = iv_scope.
      result   = abap_true.
    ENDIF.

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>param_invalid
          mv_msgty = zcx_ca_wf_om_org_model=>c_msgty_e
          mv_msgv1 = 'IV_SCOPE'
          mv_msgv2 = CONV #( iv_scope ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_scope_valid


  METHOD search_upwards_4_org_unit.
    "-----------------------------------------------------------------*
    "   Search upwards in org. structure for superior org. unit
    "-----------------------------------------------------------------*
    "Get superior org. units to the one found in the CONSTRUCTOR of this org. object
    DATA(lt_superior_org_units) =
        NEW zcl_ca_wf_om_org_model( is_org_object = ms_my_org_unit-s_om_obj_key
                                    iv_valid_on   = mv_valid_on
                                           )->get_org_model_data( iv_eval_path  = cs_eval_path-orgunit_2_orgunit
                                                                  iv_auth_check = iv_auth_check ) ##no_text.

    result = NEW #( is_org_object = get_superior_org_unit( lt_superior_org_units )
                    iv_valid_on   = mv_valid_on ).
  ENDMETHOD.                    "find_superior_org_unit


  METHOD transf_employees_2_rule_result.
    "-----------------------------------------------------------------*
    "   Transfer determined employees into agent rule result
    "-----------------------------------------------------------------*
    rt_actors = VALUE #( FOR ls_employee IN it_employees
                                            WHERE ( bname IS NOT INITIAL )
                                     ( otype = swfco_org_user
                                       objid = ls_employee-o_employee->ms_data-bname ) ).
  ENDMETHOD.                    "transf_employees_2_rule_result


  METHOD transf_rule_result_2_employees.
    "-----------------------------------------------------------------*
    "   Transfer agent rule result into format of employee data
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_employee_lookup   TYPE zca_wf_s_employee_lookup.

    LOOP AT it_actors REFERENCE INTO DATA(lr_actor).
      CASE lr_actor->otype.
        WHEN swfco_org_user.
          ls_employee_lookup-bname = lr_actor->objid.

        WHEN swfco_org_person.
          ls_employee_lookup-pernr = lr_actor->objid.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

      ls_employee_lookup-valid_on = iv_valid_on.
      APPEND ls_employee_lookup TO rt_employees.
    ENDLOOP.
  ENDMETHOD.                    "transf_rule_result_2_employees

ENDCLASS.
