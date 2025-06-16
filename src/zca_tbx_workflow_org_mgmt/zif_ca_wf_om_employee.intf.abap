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
    "! <p class="shorttext synchronized" lang="en">Change validity date -&gt; this REFRESHES (!) the data</p>
    "!
    "! @parameter iv_valid_on           | <p class="shorttext synchronized" lang="en">New validity date</p>
    "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
    change_validity_date_n_refresh
      IMPORTING
        iv_valid_on TYPE hr_date DEFAULT sy-datlo
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Read name details and compose salutation</p>
    "!
    "! @parameter iv_incl_mr_mrs | <p class="shorttext synchronized" lang="en">X = Include Mr or Mrs (no impact on the title)</p>
    compose_name_n_salutation
      IMPORTING
        iv_incl_mr_mrs TYPE abap_boolean DEFAULT abap_false,

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

    "! <p class="shorttext synchronized" lang="en">Is the personnel master record in status active?</p>
    is_personnel_master_active
      RETURNING
        VALUE(rv_is_active) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the personnel master record locked?</p>
    is_personnel_master_locked
      RETURNING
        VALUE(rv_is_locked) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee,

    "! <p class="shorttext synchronized" lang="en">Is the SAP user available?</p>
    is_sap_user_available
      RETURNING
        VALUE(rv_is_available) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_employee.


ENDINTERFACE.
