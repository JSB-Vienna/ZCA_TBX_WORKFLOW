"! <p class="shorttext synchronized" lang="en">WF-OM: BC Employee in Org. management</p>
INTERFACE zif_ca_wf_om_employee PUBLIC.
* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">WF-OM: Constants and value checks for org. object Employee</p>
    mo_cvc_employee  TYPE REF TO zcl_ca_wf_om_cvc_employee READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">WF-OM: Constants and value checks for org. management</p>
    mo_cvc_om        TYPE REF TO zcl_ca_wf_om_cvc READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">BC Extended user</p>
    mo_user          TYPE REF TO zcl_ca_wf_user READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">WF-OM: BC Manager in Org. management (workflow-capable)</p>
    mo_manager       TYPE REF TO zif_ca_wf_om_employee READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Instance of last occurred exception</p>
    mx_last_excep    TYPE REF TO zcx_ca_wf_om_employee READ-ONLY,

*   s t r u c t u r e s
    "! <p class="shorttext synchronized" lang="en">Object data</p>
    ms_data          TYPE zca_wf_s_employee READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">X = Errors occurred while determine additional data</p>
    mv_is_erroneous  TYPE abap_boolean READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Compatible HCM person for usage in workflow expressions</p>
    mv_agent         TYPE swp_agent READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
    mv_search_active TYPE abap_boolean READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Object is valid on</p>
    mv_valid_on      TYPE hr_date READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Read name details and compose with salutation</p>
    "!
    "! @parameter iv_incl_mr_mrs | <p class="shorttext synchronized" lang="en">X = Include Mr or Mrs (no impact on the title)</p>
    "! @parameter rv_name_n_id   | <p class="shorttext synchronized" lang="en">Composed name with Id</p>
    compose_name_n_salutation
      IMPORTING
        iv_incl_mr_mrs      TYPE abap_boolean DEFAULT abap_false
      RETURNING
        VALUE(rv_name_n_id) TYPE name_text,

    "! <p class="shorttext synchronized" lang="en">Find manager/approver via OM search rule</p>
    "!
    "! @parameter iv_search_rule_id       | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule Id</p>
    "! @parameter iv_search_level         | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule level</p>
    "! @parameter iv_return_last_before_selfappr | <p class="shorttext synchronized" lang="en">X = Return last approver before self-approving employee</p>
    "! @parameter ev_is_self_approving    | <p class="shorttext synchronized" lang="en">X = Returned manager is a self-approving employee</p>
    "! @parameter ro_manager              | <p class="shorttext synchronized" lang="en">Manager</p>
    "! @raising   zcx_ca_wf_om_no_manager | <p class="shorttext synchronized" lang="en">WF-OM: No manager found</p>
    "! @raising   zcx_ca_wf_om_employee   | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    find_manager_via_search_rule
      IMPORTING
        iv_search_rule_id              TYPE zca_wf_e_search_rule_id
        iv_search_level                TYPE zca_wf_e_search_rule_level
        iv_return_last_before_selfappr TYPE abap_boolean DEFAULT abap_false
      EXPORTING
        ev_is_self_approving           TYPE abap_boolean
      RETURNING
        VALUE(ro_manager)              TYPE REF TO zif_ca_wf_om_employee
      RAISING
        zcx_ca_wf_om_no_manager
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Get my manager</p>
    "!
    "! @parameter iv_search_upwards     | <p class="shorttext synchronized" lang="en">0 = Search NOT higher; > 0 = search up to x levels above</p>
    "! @parameter iv_raise_exception    | <p class="shorttext synchronized" lang="en">X = Raise exception instead of flag MV_IS_ERRONEOUS</p>
    "! @parameter ro_manager            | <p class="shorttext synchronized" lang="en">Manager</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    get_my_manager
      IMPORTING
        iv_search_upwards  TYPE hi_ebene     DEFAULT 0
        iv_raise_exception TYPE abap_boolean DEFAULT abap_false
      RETURNING
        VALUE(ro_manager)  TYPE REF TO zif_ca_wf_om_employee
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Has the employee left the company / group?</p>
    "!
    "! @parameter rv_has_left           | <p class="shorttext synchronized" lang="en">X = Employee has_left the company group</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    has_left_the_company
      RETURNING
        VALUE(rv_has_left) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the requested JOB assigned to her/his position?</p>
    "!
    "! @parameter iv_job_as             | <p class="shorttext synchronized" lang="en">Job Id => use const MO_CVC_OM-> or ZCL_CA_WF_OM_CVC=>JOB_AS*</p>
    "! @parameter rv_is_assigned        | <p class="shorttext synchronized" lang="en">X = Job is assigned</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    is_assigned_to
      IMPORTING
        iv_job_as             TYPE zca_wf_e_job_id
      RETURNING
        VALUE(rv_is_assigned) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the manager a department lead or higher?</p>
    "!
    "! @parameter rv_is_a_lead          | <p class="shorttext synchronized" lang="en">X = Is a department lead or higher</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    is_a_departm_lead_or_higher
      RETURNING
        VALUE(rv_is_a_lead) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the employee active?</p>
    "!
    "! @parameter rv_is_active          | <p class="shorttext synchronized" lang="en">X = Employee is active</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    is_active
      RETURNING
        VALUE(rv_is_active) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the employee transferred within the company / group?</p>
    "!
    "! @parameter rv_is_transferred | <p class="shorttext synchronized" lang="en">X = Employee is transferred</p>
    is_transferred
      RETURNING
        VALUE(rv_is_transferred) TYPE abap_boolean,

    "! <p class="shorttext synchronized" lang="en">Is the requested TASK assigned to her/his position?</p>
    "!
    "! @parameter iv_task               | <p class="shorttext synchronized" lang="en">Task Id => use const MO_CVC_OM-> or ZCL_CA_WF_OM_CVC=>TASK*</p>
    "! @parameter rv_is_assigned        | <p class="shorttext synchronized" lang="en">X = Task is assigned</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    is_responsible_for
      IMPORTING
        iv_task               TYPE zca_wf_e_task_id
      RETURNING
        VALUE(rv_is_assigned) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the SAP user available?</p>
    "!
    "! @parameter rv_is_available       | <p class="shorttext synchronized" lang="en">X = SAP user is available</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    is_sap_user_available
      RETURNING
        VALUE(rv_is_available) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Load org. assignmts for requested org. object (only O or S!)</p>
    "!
    "! @parameter is_org_object         | <p class="shorttext synchronized" lang="en">Org. object key</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    load_assigned_om_objects
      IMPORTING
        is_org_object TYPE swhactor
      RAISING
        zcx_ca_wf_om_employee.

ENDINTERFACE.
