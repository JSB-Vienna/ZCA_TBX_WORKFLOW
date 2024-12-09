"! <p class="shorttext synchronized" lang="en">WF-OM: Conversion + helper utilities regarding employees</p>
CLASS zcl_ca_wf_om_employee_utils DEFINITION PUBLIC
                                             CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Mass instantiation of employees by different keys</p>
      "!
      "! <p>Provide in parameter {@link .meth:find_employees.data:it_employees} a list of personnel Ids or
      "! SAP user Ids or the Windows Ids, e. g. from a customizing or a workflow receiver list, and get the
      "! completed values and instances. Then you have e. g. access to the mail address to distribute a mail
      "! all these people.</p>
      "!
      "! <p>Use the silent mode in case it is not important to reach anyone of your input by providing ABAP_FALSE
      "! to parameter {@link .meth:find_employees.data:iv_raise_excep} and check therefore column ERROR in the
      "! result table.</p>
      "!
      "! @parameter it_employees     | <p class="shorttext synchronized" lang="en">List of keys to determine employees</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Object is valid on</p>
      "! @parameter iv_search_active | <p class="shorttext synchronized" lang="en">X = Search for an active person</p>
      "! @parameter iv_raise_excep   | <p class="shorttext synchronized" lang="en">X = Raise an exception; ' ' = Return message</p>
      "! @parameter rt_employees     | <p class="shorttext synchronized" lang="en">List of completed employees</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">Org. model determination exception</p>
      find_employees
        IMPORTING
          it_employees        TYPE zca_wf_t_employees_lookup
          iv_valid_on         TYPE hr_date      DEFAULT sy-datlo
          iv_search_active    TYPE abap_boolean DEFAULT abap_true
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


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.                     "zcl_ca_wf_om_employee_utils  DEFINITION


CLASS zcl_ca_wf_om_employee_utils IMPLEMENTATION.

  METHOD find_employees.
    "-----------------------------------------------------------------*
    "   Mass instantiation of employees by different keys
    "-----------------------------------------------------------------*
    rt_employees = it_employees.
    LOOP AT rt_employees REFERENCE INTO DATA(lr_employee).
      TRY.
          IF lr_employee->pernr IS NOT INITIAL.
            lr_employee->o_employee = zcl_ca_wf_om_employee=>get_instance(
                                           is_lpor          = VALUE #( instid = CONV #( lr_employee->pernr )
                                                                       typeid = CONV #( lr_employee->employee_cls_type ) )
                                           iv_valid_on      = iv_valid_on
                                           iv_search_active = iv_search_active
                                           iv_create_sap_user_cls_of_type = lr_employee->employee_cls_type ).

          ELSEIF lr_employee->bname IS NOT INITIAL.
            lr_employee->o_employee = zcl_ca_wf_om_employee=>get_instance_by_sap_user_id(
                                                 iv_create_employee_cls_of_type = lr_employee->employee_cls_type
                                                 iv_create_sap_user_cls_of_type = lr_employee->sap_user_cls_type
                                                 iv_sap_user_id   = lr_employee->bname
                                                 iv_valid_on      = iv_valid_on
                                                 iv_search_active = iv_search_active ).

          ELSEIF lr_employee->net_id IS NOT INITIAL.
            lr_employee->o_employee = zcl_ca_wf_om_employee=>get_instance_by_windows_net_id(
                                                 iv_create_employee_cls_of_type = lr_employee->employee_cls_type
                                                 iv_create_sap_user_cls_of_type = lr_employee->sap_user_cls_type
                                                 iv_windows_net_id = lr_employee->net_id
                                                 iv_valid_on       = iv_valid_on
                                                 iv_search_active  = iv_search_active ).

          ELSE.
            "Too less data for determination of organizational informations
            RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
              EXPORTING
                textid = zcx_ca_wf_om_org_model=>too_less_data.
          ENDIF.

          lr_employee->bname     = lr_employee->o_employee->ms_data-bname.
          lr_employee->pernr     = lr_employee->o_employee->ms_data-pernr.
          lr_employee->net_id    = lr_employee->o_employee->ms_data-net_id.
          lr_employee->mail_addr = lr_employee->o_employee->ms_data-mail_addr.
          lr_employee->valid_on  = iv_valid_on.
          lr_employee->error     = abap_false.
          CLEAR: lr_employee->err_msg,
                 lr_employee->o_error.

        CATCH zcx_ca_error INTO DATA(lx_catched).
          lr_employee->error   = abap_true.
          lr_employee->err_msg = lx_catched->get_text( ).
          lr_employee->o_error = lx_catched.
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


  METHOD transf_employees_2_rule_result.
    "-----------------------------------------------------------------*
    "   Transfer determined employees into agent rule result
    "-----------------------------------------------------------------*
    rt_actors = VALUE #( FOR ls_employee IN it_employees
                                            WHERE ( error EQ abap_false AND
                                                    bname IS NOT INITIAL )
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

ENDCLASS.                     "zcl_ca_wf_om_employee_utils  IMPLEMENTATION


