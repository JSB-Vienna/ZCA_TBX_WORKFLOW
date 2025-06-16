"! <p class="shorttext synchronized" lang="en">WF-OM: BC Org. unit determinations</p>
INTERFACE zif_ca_wf_om_org_model PUBLIC.
* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">WF-OM: Constants and value checks for org. management</p>
    mo_cvc_om        TYPE REF TO zcl_ca_wf_om_cvc READ-ONLY,

*   s t r u c t u r e s
    "! <p class="shorttext synchronized" lang="en">Details to organizational object</p>
    ms_data          TYPE zca_wf_s_om_object READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
    mv_search_active TYPE abap_boolean READ-ONLY,
    "! <p class="shorttext synchronized" lang="en">Object is valid on</p>
    mv_valid_on      TYPE hr_date READ-ONLY.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Determine all managers to a organizational object</p>
    "!
    "! @parameter iv_search_upwards      | <p class="shorttext synchronized" lang="en">0 = Search NOT higher; > 0 = search up to x levels above</p>
    "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
    "! @parameter result                 | <p class="shorttext synchronized" lang="en">Managers to the current org. object</p>
    "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
    get_all_managers
      IMPORTING
        iv_search_upwards TYPE hi_ebene     DEFAULT 0
        iv_auth_check     TYPE hr_authy     DEFAULT abap_false
      RETURNING
        VALUE(result)     TYPE zca_wf_t_om_objects_lookup
      RAISING
        zcx_ca_wf_om_org_model,

    "! <p class="shorttext synchronized" lang="en">Get employees to the current org. object</p>
    "!
    "! <p>You can determine all or a group of members of instantiate org. unit. There two groups, the staff
    "! (without managers) or only the managers. <strong><em>BUT BE aware</em></strong> that it is not possible
    "! to search upwards through the org. hierarchies for the next higher available manager! For purpose please
    "! use the method {@link .meth:get_manager}.</p>
    "!
    "! @parameter iv_scope               | <p class="shorttext synchronized" lang="en">Scope (only types S + O) -&gt; use ZCL_CA_WF_OM_CVC=>SCOPE-*</p>
    "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
    "! @parameter iv_all_managers        | <p class="shorttext synchronized" lang="en">X = Return all managers</p>
    "! @parameter result                 | <p class="shorttext synchronized" lang="en">Determined employees</p>
    "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
    get_employees_2_org_object
      IMPORTING
        iv_scope      TYPE char1    DEFAULT zcl_ca_wf_om_cvc=>scope-manager
        iv_auth_check TYPE hr_authy DEFAULT abap_false
        iv_all_managers type abap_boolean default abap_false
      RETURNING
        VALUE(result) TYPE zca_wf_t_om_objects_lookup
      RAISING
        zcx_ca_wf_om_org_model,

    "! <p class="shorttext synchronized" lang="en">Determine one manager to a organizational object</p>
    "!
    "! @parameter iv_search_upwards      | <p class="shorttext synchronized" lang="en">0 = Search NOT higher; > 0 = search up to x levels above</p>
    "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
    "! @parameter result                 | <p class="shorttext synchronized" lang="en">Manager to the current org. object</p>
    "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
    get_manager
      IMPORTING
        iv_search_upwards TYPE hi_ebene     DEFAULT 0
        iv_auth_check     TYPE hr_authy     DEFAULT abap_false
      RETURNING
        VALUE(result)     TYPE zca_wf_s_om_object_lookup
      RAISING
        zcx_ca_wf_om_org_model,

    "! <p class="shorttext synchronized" lang="en">Determine (depending) org. unit data (using RH_STRUC_GET)</p>
    "!
    "! @parameter iv_eval_path           | <p class="shorttext synchronized" lang="en">Evaluation path</p>
    "! @parameter iv_auth_check          | <p class="shorttext synchronized" lang="en">X = Authority check is active</p>
    "! @parameter iv_tdepth              | <p class="shorttext synchronized" lang="en">Technical depth of structure</p>
    "! @parameter iv_tflag               | <p class="shorttext synchronized" lang="en">X = Supply texts</p>
    "! @parameter iv_vflag               | <p class="shorttext synchronized" lang="en">X = Supply relationship information</p>
    "! @parameter iv_int_flag            | <p class="shorttext synchronized" lang="en">X = Read evaluation path in internal table</p>
    "! @parameter result                 | <p class="shorttext synchronized" lang="en">Merged result of FM RH_STRUC_GET</p>
    "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
    get_org_model_data
      IMPORTING
        iv_eval_path  TYPE wegid
        iv_auth_check TYPE hr_authy   DEFAULT abap_false
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

ENDINTERFACE.
