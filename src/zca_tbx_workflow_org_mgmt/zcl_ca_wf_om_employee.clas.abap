"! <p class="shorttext synchronized" lang="en">WF-OM: BC Employee in Org. management</p>
CLASS zcl_ca_wf_om_employee DEFINITION PUBLIC
                                       CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_workflow,            " !!! Includes IF_WORKFLOW = BI_OBJECT + BI_PERSISTENT
      zif_ca_wf_om_employee,
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
*     ZIF_CA_WF_OM_EMPLOYEE methods
      find_manager_via_search_rule FOR zif_ca_wf_om_employee~find_manager_via_search_rule,
      compose_name_n_salutation    FOR zif_ca_wf_om_employee~compose_name_n_salutation,
      has_left_the_company         FOR zif_ca_wf_om_employee~has_left_the_company,
      is_a_departm_lead_or_higher  FOR zif_ca_wf_om_employee~is_a_departm_lead_or_higher,
      is_sap_user_available        FOR zif_ca_wf_om_employee~is_sap_user_available,
      load_assigned_om_objects     FOR zif_ca_wf_om_employee~load_assigned_om_objects,
      is_transferred     FOR zif_ca_wf_om_employee~is_transferred,
      get_my_manager     FOR zif_ca_wf_om_employee~get_my_manager,
      is_active          FOR zif_ca_wf_om_employee~is_active,
      is_assigned_to     FOR zif_ca_wf_om_employee~is_assigned_to,
      is_responsible_for FOR zif_ca_wf_om_employee~is_responsible_for,
*     ZIF_CA_WF_OM_EMPLOYEE attributes
      mo_cvc_employee    FOR zif_ca_wf_om_employee~mo_cvc_employee,
      mo_cvc_om          FOR zif_ca_wf_om_employee~mo_cvc_om,
      mo_user            FOR zif_ca_wf_om_employee~mo_user,
      mo_manager         FOR zif_ca_wf_om_employee~mo_manager,
      mx_last_excep      FOR zif_ca_wf_om_employee~mx_last_excep,
      ms_data            FOR zif_ca_wf_om_employee~ms_data,
      mv_is_erroneous    FOR zif_ca_wf_om_employee~mv_is_erroneous,
      mv_agent           FOR zif_ca_wf_om_employee~mv_agent,
      mv_search_active   FOR zif_ca_wf_om_employee~mv_search_active,
      mv_valid_on        FOR zif_ca_wf_om_employee~mv_valid_on.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Instance key - Personnel number</p>
      mv_key          TYPE pernr_d READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create instance by LPOR or by personnel number</p>
      "!
      "! <p>To influence the employee class type provide parameter {@link .meth:get_instance.data:is_lpor}
      "! instead of parameter {@link .meth:get_instance.data:iv_key}. Therefore <strong><em>copy</em></strong>
      "! the following statement for your purpose:</p>
      "! <p><strong><em>DATA(employee) = zcl_ca_wf_om_employee=>get_instance(<br>
      "!                                               is_lpor = VALUE #( instid = CONV #( field_with_Pers.Id )<br>
      "!                                                                  typeid = 'Your_Subclass_name' ) ) ##no_text.</em></strong></p>
      "!
      "! <p>If in the communication data of the personnel master data an SAP user is available the attribute
      "! {@link .data:MO_USER} will automatically be created. You can influence the user class type providing
      "! an existing class name in parameter {@link .meth:get_instance.data:iv_create_sap_user_cls_of_type}.</p>
      "!
      "! <p>If an exception occurs during the instantiation of the user class <strong><em>NO EXCEPTION</em></strong>
      "! is raised. This is as the user instantiation is an additional service of this class but not its main
      "! focus. Check the attributes {@link .data:mv_is_erroneous} and {@link .data:mx_last_excep} right after
      "! the call of this method if necessary.</p>
      "!
      "! @parameter is_lpor          | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter iv_key           | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! <p>This flag influences if the person is an active employee or not at the given date. It checks if the
      "! value of PA0000-STAT2 is either '3' = active (default) OR any other value.</p>
      "! <br>
      "! @parameter iv_create_sap_user_cls_of_type | <p class="shorttext synchronized" lang="en">Name of (inherited) SAP user class</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      get_instance
        IMPORTING
          is_lpor                        TYPE sibflpor OPTIONAL
          iv_key                         TYPE pernr_d  OPTIONAL
          iv_valid_on                    TYPE hr_date  DEFAULT sy-datlo
          iv_search_active               TYPE abap_boolean DEFAULT abap_true
          iv_create_sap_user_cls_of_type TYPE seoclsname DEFAULT 'ZCL_CA_WF_USER' ##no_text
        RETURNING
          VALUE(result)                  TYPE REF TO zif_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance by windows net Id</p>
      "!
      "! <p>You can influence the employee class type and/or the user class type by providing an existing class name
      "! in parameter  {@link .meth:get_instance.data:iv_create_employee_cls_of_type}, or respectively in parameter
      "! {@link .meth:get_instance.data:iv_create_sap_user_cls_of_type}.</p>
      "!
      "! <p>For further information see the details of {@link .meth:get_instance}, but ignore the first paragraph.</p>
      "!
      "! @parameter iv_windows_net_id | <p class="shorttext synchronized" lang="en">Windows net Id</p>
      "! @parameter iv_valid_on       | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active  | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! <p>This flag influences if the person is an active employee or not at the given date. It checks if the
      "! value of PA0000-STAT2 is either '3' = active (default) OR any value.</p>
      "! <br>
      "! @parameter iv_create_employee_cls_of_type | <p class="shorttext synchronized" lang="en">Name of (inherited) employee class</p>
      "! @parameter iv_create_sap_user_cls_of_type | <p class="shorttext synchronized" lang="en">Name of (inherited) SAP user class</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param      | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc      | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      get_instance_by_windows_net_id
        IMPORTING
          iv_windows_net_id              TYPE zca_wf_e_windows_net_id
          iv_search_active               TYPE abap_boolean DEFAULT abap_true
          iv_valid_on                    TYPE hr_date      DEFAULT sy-datlo
          iv_create_employee_cls_of_type TYPE seoclsname   DEFAULT 'ZCL_CA_WF_OM_EMPLOYEE' ##no_text
          iv_create_sap_user_cls_of_type TYPE seoclsname   DEFAULT 'ZCL_CA_WF_USER' ##no_text
        RETURNING
          VALUE(result)                  TYPE REF TO zif_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance by SAP user Id</p>
      "!
      "! <p>You can influence the employee class type and/or the user class type by providing an existing class name
      "! in parameter  {@link .meth:get_instance.data:iv_create_employee_cls_of_type}, or respectively in parameter
      "! {@link .meth:get_instance.data:iv_create_sap_user_cls_of_type}.</p>
      "!
      "! <p>For further information see the details of {@link .meth:get_instance}, but ignore the first paragraph.</p>
      "!
      "! @parameter iv_sap_user_id   | <p class="shorttext synchronized" lang="en">SAP user id</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! <p>This flag influences if the person is an active employee or not at the given date. It checks if the
      "! value of PA0000-STAT2 is either '3' = active (default) OR any value.</p>
      "! <br>
      "! @parameter iv_create_employee_cls_of_type | <p class="shorttext synchronized" lang="en">Name of (inherited) user / employee class</p>
      "! @parameter iv_create_sap_user_cls_of_type | <p class="shorttext synchronized" lang="en">Name of (inherited) SAP user class</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      get_instance_by_sap_user_id
        IMPORTING
          iv_sap_user_id                 TYPE xubname
          iv_valid_on                    TYPE hr_date      DEFAULT sy-datlo
          iv_search_active               TYPE abap_boolean DEFAULT abap_true
          iv_create_employee_cls_of_type TYPE seoclsname   DEFAULT 'ZCL_CA_WF_OM_EMPLOYEE' ##no_text
          iv_create_sap_user_cls_of_type TYPE seoclsname   DEFAULT 'ZCL_CA_WF_USER' ##no_text
        RETURNING
          VALUE(result)                  TYPE REF TO zif_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc.

*   i n s t a n c e   e v e n t s
    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Object created</p>
      created,
      "! <p class="shorttext synchronized" lang="en">Object changed</p>
      changed,
      "! <p class="shorttext synchronized" lang="en">Task is completed</p>
      task_completed,
      "! <p class="shorttext synchronized" lang="en">User cancelled task without saving</p>
      task_cancelled.


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
      "! <p class="shorttext synchronized" lang="en">Workflow object instance key</p>
      ms_lpor TYPE sibflpor,

      "! <p class="shorttext synchronized" lang="en">Specific master data indicators</p>
      BEGIN OF ms_master_record,
        "! <p class="shorttext synchronized" lang="en">Custom specific employment status</p>
        "!
        "! <p>The individual status can be defined in table T529U. In this case the custom specific status
        "! are the same as the SAP standard status. If someone is on sabbatical the person has the standard
        "! status 'Active' (= 3) and the custom status 'Inactive' (= 1).</p><br>
        cust_status     TYPE stat1,
        "! <p class="shorttext synchronized" lang="en">SAP Standard employment status</p>
        std_status      TYPE stat2,
        "! <p class="shorttext synchronized" lang="en">Action type (Massnahme)</p>
        last_action     TYPE massn,
        "! <p class="shorttext synchronized" lang="en">Reason to action (Massnahmegrund)</p>
        reason_2_action TYPE massg,
      END OF ms_master_record,

      "! <p class="shorttext synchronized" lang="en">Log object for BAL</p>
      BEGIN OF ms_log_key,
        "! <p class="shorttext synchronized" lang="en">Object for BAL</p>
        object    TYPE balobj_d,
        "! <p class="shorttext synchronized" lang="en">Sub object for BAL</p>
        subobject TYPE balsubobj,
      END OF ms_log_key,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Type the SAP user class should be created of</p>
      mv_type_of_sap_user_cls TYPE seoclsname,

      "! <p class="shorttext synchronized" lang="en">Name of specific, application individual log class</p>
      mv_my_log_class_name    TYPE seoclsname.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get personnel number by communication type from PA0105</p>
      "!
      "! @parameter iv_comm_type          | <p class="shorttext synchronized" lang="en">Communication type in PA0105 (column USRTY)</p>
      "! @parameter iv_comm_value         | <p class="shorttext synchronized" lang="en">Value to communication type of PA0105 (column USRID)</p>
      "! @parameter iv_valid_on           | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active      | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      find_persid_by_comm_type_value
        IMPORTING
          iv_comm_type     TYPE usrty
          iv_comm_value    TYPE simple
          iv_valid_on      TYPE hr_date
          iv_search_active TYPE abap_boolean DEFAULT abap_true
        RETURNING
          VALUE(result)    TYPE pernr_d
        RAISING
          zcx_ca_wf_om_employee.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_key           | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! @parameter iv_create_sap_user_cls_of_type | <p class="shorttext synchronized" lang="en">Name of (inherited) SAP user class</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      constructor
        IMPORTING
          iv_key                         TYPE pernr_d
          iv_valid_on                    TYPE hr_date
          iv_search_active               TYPE abap_boolean
          iv_create_sap_user_cls_of_type TYPE seoclsname
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Determine email address via personnel number</p>
      "!
      "! @parameter iv_raise_exception    | <p class="shorttext synchronized" lang="en">X = Raise exception instead of flag MV_IS_ERRONEOUS</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">eMail address</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_email_addr_via_pernr
        IMPORTING
          iv_raise_exception TYPE abap_boolean DEFAULT abap_false
        RETURNING
          VALUE(result)      TYPE zca_wf_e_mail_address
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Determine SAP user Id via personnel number</p>
      "!
      "! @parameter iv_raise_exception    | <p class="shorttext synchronized" lang="en">X = Raise exception instead of flag MV_IS_ERRONEOUS</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">SAP user Id</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_sap_user_id_via_pernr
        IMPORTING
          iv_raise_exception TYPE abap_boolean DEFAULT abap_false
        RETURNING
          VALUE(result)      TYPE xubname
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Determine email address Id via personnel number</p>
      "!
      "! @parameter iv_raise_exception    | <p class="shorttext synchronized" lang="en">X = Raise exception instead of flag MV_IS_ERRONEOUS</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Windows net Id</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_windows_net_id_via_pernr
        IMPORTING
          iv_raise_exception TYPE abap_boolean DEFAULT abap_false
        RETURNING
          VALUE(result)      TYPE zca_wf_e_windows_net_id
        RAISING
          zcx_ca_wf_om_employee.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   t y p e   d e f i n i t i o n s
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Assigned tasks to the position of the employee</p>
      ty_t_tasks TYPE SORTED TABLE OF zca_wf_e_task_id WITH NON-UNIQUE KEY table_line,

      "! <p class="shorttext synchronized" lang="en">Org. assignments per org. unit</p>
      BEGIN OF ty_s_org_assignment.
        INCLUDE TYPE zca_wf_s_om_object AS s_org_unit RENAMING WITH SUFFIX _ou.
        INCLUDE TYPE zca_wf_s_om_object AS s_position RENAMING WITH SUFFIX _s.
        INCLUDE TYPE zca_wf_s_om_object AS s_job RENAMING WITH SUFFIX _c.
    TYPES:
        t_tasks TYPE ty_t_tasks,
      END   OF ty_s_org_assignment,
      "! <p class="shorttext synchronized" lang="en">Org. assignments to org. units</p>
      ty_t_org_assignments TYPE STANDARD TABLE OF ty_s_org_assignment WITH EMPTY KEY,

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
      c_my_typeid TYPE sibftypeid VALUE 'ZCL_CA_WF_OM_EMPLOYEE'  ##no_text,

      "! <p class="shorttext synchronized" lang="en">Log object for BAL and workflow status</p>
      BEGIN OF cs_log_key,
        "! <p class="shorttext synchronized" lang="en">Object for BAL</p>
        object    TYPE balobj_d  VALUE 'ZCA_WF' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Sub object for BAL</p>
        subobject TYPE balsubobj VALUE 'EMPLOYEE' ##no_text,
      END OF cs_log_key,

      "! <p class="shorttext synchronized" lang="en">Name of specific, application individual log class</p>
      c_my_log_class_name TYPE seoclsname VALUE 'ZCL_CA_WF_LOG' ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer     TYPE ty_t_buffer.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Buffer of multiple org. assignments</p>
      mt_org_assignments TYPE ty_t_org_assignments,
      "! <p class="shorttext synchronized" lang="en">Assigned tasks to the position of the employee</p>
      mt_my_tasks        TYPE ty_t_tasks.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Check whether the personnel Id is the lowest active Id</p>
      "!
      "! @parameter iv_pernr              | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @parameter iv_valid_on           | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_search_active      | <p class="shorttext synchronized" lang="en">X = Search for an active person; ' ' = any state</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      check_is_lowest_active_pers_id
        IMPORTING
          iv_pernr         TYPE pernr_d
          iv_valid_on      TYPE hr_date DEFAULT sy-datlo
          iv_search_active TYPE abap_boolean DEFAULT abap_true
        RAISING
          zcx_ca_wf_om_employee.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Build up buffer with all org. assignments an employee has</p>
      "!
      "! @raising   zcx_ca_wf_om_no_om_objects | <p class="shorttext synchronized" lang="en">WF-OM: No OM objects found to evaluation path</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      build_up_org_assignm_buffer
        RAISING
          zcx_ca_wf_om_no_om_objects
          zcx_ca_wf_om_employee
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Check general existence of personnel master data</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      check_existence_pers_master
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Create SAP user instance</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      create_sap_user_instance
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Read specific indicators of personnel master record</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      read_master_record
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Read organizational assignments from personnel master record</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      read_org_assignments
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Get org. units above the employee</p>
      "!
      "! @parameter rt_units_above             | <p class="shorttext synchronized" lang="en">Determined org. units above</p>
      "! @raising   zcx_ca_wf_om_employee      | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      "! @raising   zcx_ca_wf_om_no_om_objects | <p class="shorttext synchronized" lang="en">WF-OM: No OM objects found to evaluation path</p>
      "! @raising   zcx_ca_wf_om_org_model     | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      get_org_units_above_employee
        RETURNING
          VALUE(rt_units_above) TYPE zca_wf_t_org_model_data
        RAISING
          zcx_ca_wf_om_employee
          zcx_ca_wf_om_no_om_objects
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Get search rule instance</p>
      "!
      "! @parameter iv_search_rule_id | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule Id</p>
      "! @parameter iv_search_level   | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule level</p>
      "! @parameter ro_search_rule    | <p class="shorttext synchronized" lang="en">Search rule instance</p>
      get_search_rule_instance
        IMPORTING
          iv_search_rule_id     TYPE zca_wf_e_search_rule_id
          iv_search_level       TYPE zca_wf_e_search_rule_level
        RETURNING
          VALUE(ro_search_rule) TYPE REF TO zcl_ca_wf_om_search_rule,

      "! <p class="shorttext synchronized" lang="en">Get tasks to a position of the employee</p>
      "!
      "! @parameter is_position  | <p class="shorttext synchronized" lang="en">Key of a position</p>
      "! @parameter rt_tasks     | <p class="shorttext synchronized" lang="en">Tasks connected to the position</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
      "! @raising   zcx_ca_wf_om_no_om_objects | <p class="shorttext synchronized" lang="en">WF-OM: No OM objects found to evaluation path</p>
      get_tasks_2_position
        IMPORTING
          is_position     TYPE swhactor
        RETURNING
          VALUE(rt_tasks) TYPE ty_t_tasks
        RAISING
          zcx_ca_wf_om_no_om_objects
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Get value to communication type from PA0105</p>
      "!
      "! @parameter iv_comm_type          | <p class="shorttext synchronized" lang="en">Communication type of PA0105 (column USRTY)</p>
      "! @parameter ev_result             | <p class="shorttext synchronized" lang="en">Result must be of a concrete type; is empty if not found</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_value_to_comm_type
        IMPORTING
          iv_comm_type TYPE usrty
        EXPORTING
          ev_result    TYPE simple
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Read name details</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      read_name_details
        RAISING
          zcx_ca_wf_om_employee.

ENDCLASS.



CLASS zcl_ca_wf_om_employee IMPLEMENTATION.

  METHOD bi_object~default_attribute_value.
    "-----------------------------------------------------------------*
    "   Returns a description and/or prepared key of the object.
    "-----------------------------------------------------------------*
    "Example ==> replace the next 3 lines (comment and key preparation) as needed for your purpose
    "TEXT-DAV = Employee Xxxxxx Yyyyyy Zzzzzzz (nnnnnnnn)
    mv_default_attr = |{ TEXT-dav } { ms_data-full_name_w_id }|.

    result = REF #( mv_default_attr ).
  ENDMETHOD.                    "bi_object~default_attribute_value


  METHOD bi_object~execute_default_method ##needed.
    "-----------------------------------------------------------------*
    "   Execute default method
    "-----------------------------------------------------------------*
    TRY.
        SET PARAMETER ID 'PER' FIELD mv_key.
        CALL TRANSACTION 'PA20' WITH AUTHORITY-CHECK.

      CATCH cx_sy_authorization_error INTO DATA(lx_caught).
        MESSAGE lx_caught TYPE zcx_ca_error=>c_msgty_e.
    ENDTRY.
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
        result ?= zcl_ca_wf_om_employee=>get_instance( is_lpor = lpor ).

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


  METHOD build_up_org_assignm_buffer.
    "-----------------------------------------------------------------*
    "   Build up buffer with all org. assignments an employee has
    "-----------------------------------------------------------------*
    IF mt_org_assignments IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_om_objects_to_person) =
            zcl_ca_wf_om_org_model=>get_instance( is_key           = CONV #( mv_agent )
                                                  iv_valid_on      = mv_valid_on
                                                  iv_search_active = mv_search_active
                      )->get_org_model_data( iv_eval_path = mo_cvc_om->evaluation_path-job_assignms_2_person ).

*    OTYPE  OBJID   SEQNR LEVEL PDOWN VCOUNT PNEXT PUP PPREV
*      P   117138     1     1     2     2      0    0    0
*      S   7187       2     2*    4     1      3    1    0
*      O   10001077   4     3     0     0      0    2    0
*      S   10009001   3     2*    5     2      0    1    2
*      C   10000026   5     3     0     0      6    3    0
*      O   10002679   6     3     0     0      0    3    5

    LOOP AT lt_om_objects_to_person REFERENCE INTO DATA(lr_om_object_to_person)
                                    WHERE level EQ 2.    "Level 1 is person, level 2 an entry with org. objects
      APPEND INITIAL LINE TO mt_org_assignments REFERENCE INTO DATA(lr_org_assignment).
      DATA(lr_next_om_object) = lr_om_object_to_person.

      DO.
        CASE lr_next_om_object->otype.
          WHEN swfco_org_position.
            lr_org_assignment->s_position = CORRESPONDING #( lr_next_om_object->*
                                                                          MAPPING name       = stext
                                                                                  short_name = short ).
            lr_org_assignment->o_om_object_s = zcl_ca_wf_om_org_model=>get_instance(
                                                           is_key      = lr_org_assignment->s_om_obj_key_s
                                                           iv_valid_on = mv_valid_on
                                                           iv_search_active = mv_search_active ).

            lr_org_assignment->t_tasks = get_tasks_2_position( lr_org_assignment->s_om_obj_key_s ).

          WHEN swfco_org_job.
            lr_org_assignment->s_job = CORRESPONDING #( lr_next_om_object->*
                                                                     MAPPING name       = stext
                                                                             short_name = short ).
            lr_org_assignment->o_om_object_c = zcl_ca_wf_om_org_model=>get_instance(
                                                             is_key      = lr_org_assignment->s_om_obj_key_c
                                                             iv_valid_on = mv_valid_on
                                                             iv_search_active = mv_search_active ).

          WHEN swfco_org_orgunit.
            lr_org_assignment->s_org_unit = CORRESPONDING #( lr_next_om_object->*
                                                                          MAPPING name       = stext
                                                                                  short_name = short ).
            lr_org_assignment->o_om_object_ou = zcl_ca_wf_om_org_model=>get_instance(
                                                             is_key      = lr_org_assignment->s_om_obj_key_ou
                                                             iv_valid_on = mv_valid_on
                                                             iv_search_active = mv_search_active ).
        ENDCASE.

        IF lr_next_om_object->pdown EQ 0 AND
           lr_next_om_object->pnext EQ 0.
          EXIT.
        ENDIF.

        DATA(lv_next) = COND #( WHEN lr_next_om_object->level EQ 2
                          THEN lr_next_om_object->pdown  ELSE lr_next_om_object->pnext ).
        lr_next_om_object = REF #( lt_om_objects_to_person[ seqnr = lv_next ] ).
      ENDDO.
    ENDLOOP.
  ENDMETHOD.                    "build_up_org_assignm_buffer


  METHOD check_existence_pers_master.
    "-----------------------------------------------------------------*
    "   Check general existence of personnel master data
    "-----------------------------------------------------------------*
    SELECT FROM pa0002
         FIELDS pernr
          WHERE pernr EQ @mv_key
            AND subty EQ @space
            AND objps EQ @space
            AND sprps EQ @space
            AND endda GE @mv_valid_on
            AND begda LE @mv_valid_on
           INTO @ms_data-pernr
                UP TO 1 ROWS.                           "#EC CI_NOORDER

    ENDSELECT.
    IF ms_data-pernr IS INITIAL.
      "Personnel number & does not exist
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID 'PG' TYPE 'E' NUMBER '041' WITH |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "check_existence_pers_master


  METHOD check_is_lowest_active_pers_id.
    "-----------------------------------------------------------------*
    "   Check whether the personnel Id is the lowest active Id
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lra_active_person TYPE RANGE OF stat2,
      lv_central_person TYPE otjid.

    "Get central person, if applicable
    DATA(lv_personnel_id) = CONV otjid( |{ swfco_org_person WIDTH = 2 }{ iv_pernr }| ).  "= P 02000349
    SELECT FROM hrp1001
         FIELDS concat( 'CP', sobid )
          WHERE otjid EQ @lv_personnel_id      "an index exists for this field
            AND plvar EQ @zcl_ca_wf_om_org_model=>mv_plvar
            AND infty EQ '1001'                "Relationship
            AND subty EQ 'A209'                "bottom up (A) / filled by (209)
            AND begda LE @iv_valid_on
            AND endda GE @iv_valid_on
            AND sclas EQ 'CP'
           INTO @lv_central_person
                UP TO 1 ROWS ##no_text.                 "#EC CI_NOORDER
    ENDSELECT.
    IF sy-subrc NE 0.
      "No valid central person found to personnel Id &1
      RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>central_pernr_not_found
                                                 mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                 mv_msgv1 = CONV #( |{ iv_pernr ALPHA = OUT }| ) ).
    ENDIF.

    IF iv_search_active EQ abap_true.
      lra_active_person = VALUE #( ( sign   = zcl_ca_c_sel_options=>sign-incl
                                     option = zcl_ca_c_sel_options=>option-eq
                                     low    = zcl_ca_wf_om_cvc_employee=>employment_status-is_active ) ).
    ENDIF.

    "Select all related personnel Ids
    SELECT FROM hrp1001 AS 1001
                LEFT OUTER JOIN pa0000 AS 0000         "#EC CI_BUFFJOIN
                             ON 0000~pernr EQ 1001~sobid   AND
                                0000~endda GE @iv_valid_on AND
                                0000~begda LE @iv_valid_on
         FIELDS 1001~sobid
          WHERE 1001~otjid EQ @lv_central_person
            AND 1001~plvar EQ @zcl_ca_wf_om_org_model=>mv_plvar
            AND 1001~infty EQ '1001'               "Relationship
            AND 1001~subty EQ 'B209'               "top down (B) / filled by (209)
            AND 1001~endda GE @iv_valid_on
            AND 1001~begda LE @iv_valid_on
            AND 1001~sclas EQ @swfco_org_person    "P
            AND 0000~stat1 IN @lra_active_person   "if LRA_ACTIVE_PERSON is initial it find any activity type
            AND 0000~stat2 IN @lra_active_person
                ORDER BY 1001~sobid ASCENDING
           INTO TABLE @DATA(lt_pers_ids_2_central_person) ##no_text.
    IF sy-subrc NE 0.
      "Personnel Id &1 is inactive. No other active Ids found via CP &2.
      RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>no_pers_ids_2_central_pernr
                                                 mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                 mv_msgv1 = |{ iv_pernr ALPHA = OUT }|
                                                 mv_msgv2 = |{ lv_central_person+2 ALPHA = OUT }| ).
    ENDIF.

    DATA(lv_lowest_pernr) = VALUE pernr_d( lt_pers_ids_2_central_person[ 1 ]-sobid OPTIONAL ).

    IF iv_search_active EQ abap_true AND
       iv_pernr         NE lv_lowest_pernr.
      "Pers Id &1 is inactive or not the lowest. Please replace it by Pers Id &2
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        EXPORTING
          textid   = zcx_ca_wf_om_employee=>lowest_pernr
          mv_msgty = c_msgty_e
          mv_msgv1 = |{ iv_pernr ALPHA = OUT }|
          mv_msgv2 = |{ lv_lowest_pernr ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "check_is_lowest_active_pers_id


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    ms_lpor-typeid = to_upper( c_my_typeid ).
    ms_lpor-catid  = swfco_objtype_cl.

    IF iv_key IS INITIAL.
      RETURN.
    ENDIF.

    "Set initial validity date and prepare constants and its validations
    mv_valid_on             = iv_valid_on.
    mv_search_active        = iv_search_active.
    mv_type_of_sap_user_cls = iv_create_sap_user_cls_of_type.
    mo_cvc_employee         = zcl_ca_wf_om_cvc_employee=>get_instance( ).
    mo_cvc_om               = zcl_ca_wf_om_cvc=>get_instance( ).

    "Complete and keep several attributes
    ms_lpor-instid = mv_key = iv_key.
*    mbo_BO_PERNR-instid = CONV #( mv_key ).

    "Set log key - Instance creation in CHECK_EXISTANCE due to inheritances
    ms_log_key = cs_log_key.
    mv_my_log_class_name = c_my_log_class_name.
  ENDMETHOD.                    "constructor


  METHOD create_sap_user_instance.
    "-----------------------------------------------------------------*
    "   Create SAP user instance if available
    "-----------------------------------------------------------------*
    TRY.
        IF ms_data-bname IS INITIAL.
          RETURN.
        ENDIF.

        mo_user = zcl_ca_wf_user=>get_instance( is_lpor = VALUE #( instid = ms_data-bname
                                                                   typeid = mv_type_of_sap_user_cls
                                                                   catid  = swfco_objtype_cl ) ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       iv_class    = mv_type_of_sap_user_cls
                                                       iv_method   = 'GET_INSTANCE'
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          mv_is_erroneous = abap_true.
          mx_last_excep   = lx_error.
*          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_sap_user_instance


  METHOD find_persid_by_comm_type_value.
    "-----------------------------------------------------------------*
    "   Find personnel number by communication type from PA0105
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lra_active_person TYPE RANGE OF stat2.

    DATA(lo_cvc_employee) = zcl_ca_wf_om_cvc_employee=>get_instance( ).
    lo_cvc_employee->is_comm_type_valid( iv_comm_type ).

    DATA(lv_column_name) = SWITCH fieldname( lo_cvc_employee->is_long_id_used_in_comm_type( iv_comm_type )
                             WHEN abap_true   THEN lo_cvc_employee->column_name-usrid_long
                             WHEN abap_false  THEN lo_cvc_employee->column_name-usrid ).

    DATA(lt_where) = VALUE se16n_where(
                            ( VALUE se16n_where_line( line = |{ lv_column_name } EQ @IV_COMM_VALUE| ) ) ) ##no_text.

    IF iv_search_active EQ abap_true.
      lra_active_person = VALUE #( ( sign   = zcl_ca_c_sel_options=>sign-incl
                                     option = zcl_ca_c_sel_options=>option-eq
                                     low    = zcl_ca_wf_om_cvc_employee=>employment_status-is_active ) ).
    ENDIF.

    "Select all related personnel Ids
    SELECT FROM pa0105 AS 0105
                LEFT OUTER JOIN pa0000 AS 0000         "#EC CI_BUFFJOIN
                             ON 0000~pernr EQ 0105~pernr   AND
                                0000~endda GE @iv_valid_on AND
                                0000~begda LE @iv_valid_on
         FIELDS 0105~pernr
          WHERE (lt_where)       "an index is defined for USRID and USRID_LONG, which is why the dynamic part comes first
            AND 0105~subty EQ @iv_comm_type
            AND 0105~objps EQ @space
            AND 0105~sprps EQ @space
            AND 0105~endda GE @iv_valid_on
            AND 0105~begda LE @iv_valid_on
            AND 0000~stat2 IN @lra_active_person   "if LRA_ACTIVE_PERSON is initial it find any activity type
          ORDER BY 0105~pernr ASCENDING
           INTO TABLE @DATA(lt_pa0105).

    IF lt_pa0105 IS INITIAL.
      "No personnel number found to &1 &2 or is not valid on &3
      RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>pernr_not_found
                                                 mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                 mv_msgv1 = CONV #( iv_comm_type )
                                                 mv_msgv2 = CONV #( iv_comm_value )
                                                 mv_msgv3 = CONV #( |{ iv_valid_on DATE = USER }| ) ).
    ENDIF.

    result = lt_pa0105[ 1 ].
  ENDMETHOD.                    "find_persid_by_comm_type_value


  METHOD get_email_addr_via_pernr.
    "-----------------------------------------------------------------*
    "   Determine email address Id via personnel number
    "-----------------------------------------------------------------*
    TRY.
        get_value_to_comm_type(
                         EXPORTING
                           iv_comm_type = mo_cvc_employee->comm_type-email_address
                         IMPORTING
                           ev_result    = result ).

      CATCH zcx_ca_wf_om_employee.
        "No &1 maintained for personnel number &2 in infotype 0105
        mx_last_excep = NEW zcx_ca_wf_om_employee(
                                              textid   = zcx_ca_wf_om_employee=>subtyp_missing
                                              mv_msgty = c_msgty_e
                                              mv_msgv1 = 'Mail address'(mad)
                                              mv_msgv2 = CONV #( |{ mv_key ALPHA = OUT }| ) ).

        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).

        IF iv_raise_exception EQ abap_true.
          RAISE EXCEPTION mx_last_excep.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_email_addr_via_pernr


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor TYPE sibflpor,
      lv_key  TYPE pernr_d.

    IF is_lpor IS NOT INITIAL.
      ls_lpor       = is_lpor.
      ls_lpor-catid = swfco_objtype_cl.

      "Set key into structured definition
      IF ls_lpor-instid IS NOT INITIAL.  "Avoid destruction of type conform initial values
        lv_key = CONV #( ls_lpor-instid ).
      ENDIF.

      "Set these values in any case, e. g. to create/get an instance only with the key string
      IF ls_lpor-typeid IS INITIAL.
        ls_lpor-typeid = to_upper( zcl_ca_wf_om_employee=>c_my_typeid ).
      ENDIF.

    ELSEIF iv_key IS NOT INITIAL.
      lv_key = iv_key.
      ls_lpor = VALUE #( instid = CONV #( iv_key )
                         typeid = to_upper( COND #( WHEN is_lpor-typeid IS NOT INITIAL
                                                      THEN is_lpor-typeid
                                                      ELSE zcl_ca_wf_om_employee=>c_my_typeid ) )
                         catid  = swfco_objtype_cl ).

    ELSE.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'IV_KEY'
          mv_msgv3 = space
          mv_msgv4 = space ##no_text.
    ENDIF.

    "If the key is still not available create no instance
    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        "Is an instance already created?
        DATA(ls_buffer) = zcl_ca_wf_om_employee=>mt_buffer[ KEY primary_key
                                                                s_lpor             = ls_lpor
                                                                valid_on           = iv_valid_on
                                                                active_id_searched = iv_search_active ].
        ls_buffer-o_persistent->refresh( ).

      CATCH cx_sy_itab_line_not_found.
        check_is_lowest_active_pers_id( iv_pernr         = lv_key
                                        iv_search_active = iv_search_active
                                        iv_valid_on      = iv_valid_on ).

        "Create instance of payment approval object
        CREATE OBJECT ls_buffer-o_persistent TYPE (ls_lpor-typeid)
          EXPORTING
            iv_key           = lv_key
            iv_valid_on      = iv_valid_on
            iv_search_active = iv_search_active
            iv_create_sap_user_cls_of_type = iv_create_sap_user_cls_of_type.

        "Checks existence of object and creates default attribute = readable key with text
        ls_buffer-o_persistent->check_existence( ).
        ls_buffer-o_persistent->default_attribute_value( ).

        ls_buffer-s_lpor             = ls_buffer-o_persistent->lpor( ).
        ls_buffer-valid_on           = iv_valid_on.
        ls_buffer-active_id_searched = iv_search_active.
        INSERT ls_buffer INTO TABLE zcl_ca_wf_om_employee=>mt_buffer.
    ENDTRY.

    result ?= ls_buffer-o_persistent.
  ENDMETHOD.                    "get_instance


  METHOD get_instance_by_sap_user_id.
    "-----------------------------------------------------------------*
    "   Create instance by SAP user Id
    "-----------------------------------------------------------------*
    TRY.
        DATA(lv_pernr) = zcl_ca_wf_om_employee=>find_persid_by_comm_type_value(
                                       iv_comm_type     = zcl_ca_wf_om_cvc_employee=>comm_type-sap_user_id
                                       iv_comm_value    = iv_sap_user_id
                                       iv_search_active = iv_search_active
                                       iv_valid_on      = iv_valid_on ).

        result = zcl_ca_wf_om_employee=>get_instance(
                                      iv_create_sap_user_cls_of_type = iv_create_sap_user_cls_of_type
                                      is_lpor          = VALUE #( instid = CONV #( lv_pernr )
                                                                  typeid = CONV #( iv_create_employee_cls_of_type )
                                                                  catid  = swfco_objtype_cl )
                                      iv_search_active = iv_search_active
                                      iv_valid_on      = iv_valid_on ).

      CATCH zcx_ca_wf_om_employee INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       iv_class    = 'ZCL_CA_WF_OM_EMPLOYEE'
                                                       iv_method   = 'GET_INSTANCE_BY_SAP_USER_ID'
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_instance_by_sap_user_id


  METHOD get_instance_by_windows_net_id.
    "-----------------------------------------------------------------*
    "   Create instance by windows net Id
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_pernr              TYPE pernr_d.

    TRY.
        TRY.
            lv_pernr = zcl_ca_wf_om_employee=>find_persid_by_comm_type_value(
                                          iv_comm_type     = zcl_ca_wf_om_cvc_employee=>comm_type-windows_net_id
                                          iv_comm_value    = to_upper( iv_windows_net_id )  "TO_UPPER !!
                                          iv_valid_on      = iv_valid_on
                                          iv_search_active = iv_search_active ).

          CATCH zcx_ca_wf_om_employee.
            lv_pernr = zcl_ca_wf_om_employee=>find_persid_by_comm_type_value(
                                          iv_comm_type     = zcl_ca_wf_om_cvc_employee=>comm_type-windows_net_id
                                          iv_comm_value    = to_lower( iv_windows_net_id )  "TO_LOWER !!
                                          iv_valid_on      = iv_valid_on
                                          iv_search_active = iv_search_active ).
        ENDTRY.

        result = zcl_ca_wf_om_employee=>get_instance(
                                          iv_create_sap_user_cls_of_type = iv_create_sap_user_cls_of_type
                                          is_lpor          = VALUE #( instid = CONV #( lv_pernr )
                                                                      typeid = CONV #( iv_create_employee_cls_of_type )
                                                                      catid  = swfco_objtype_cl )
                                          iv_valid_on      = iv_valid_on
                                          iv_search_active = iv_search_active ).

      CATCH zcx_ca_wf_om_employee INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                           iv_class    = 'ZCL_CA_WF_OM_EMPLOYEE'
                                                           iv_method   = 'GET_INSTANCE_BY_WINDOWS_NET_ID'
                                                           ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_instance_by_windows_net_id


  METHOD get_org_units_above_employee.
    "-----------------------------------------------------------------*
    "   Get org. units above the employee
    "-----------------------------------------------------------------*
    IF ms_data-o_om_object_ou IS NOT BOUND.
      "Person &1 is not assigned to &2
      RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>employee_has_no_assignment_to
                                                 mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                 mv_msgv1 = CONV #( |{ mv_key ALPHA = OUT }| )
                                                 mv_msgv2 = 'an org. unit'(nou) ) ##no_text.
    ENDIF.

    rt_units_above = ms_data-o_om_object_ou->get_org_model_data(
                                          iv_eval_path = mo_cvc_om->evaluation_path-orgunit_2_org_unit ).
  ENDMETHOD.                    "get_org_units_above_employee


  METHOD get_sap_user_id_via_pernr.
    "-----------------------------------------------------------------*
    "   Determine SAP user Id via personnel number
    "-----------------------------------------------------------------*
    TRY.
        get_value_to_comm_type(
                         EXPORTING
                           iv_comm_type = mo_cvc_employee->comm_type-sap_user_id
                         IMPORTING
                           ev_result    = result ).

      CATCH zcx_ca_wf_om_employee.
        "No &1 maintained for personnel Id  &2 in infotype 0105
        mx_last_excep = NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>subtyp_missing
                                                    mv_msgty = c_msgty_e
                                                    mv_msgv1 = 'SAP user Id'(sap)
                                                    mv_msgv2 = CONV #( |{ mv_key ALPHA = OUT }| ) ).

        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).

        IF iv_raise_exception EQ abap_true.
          RAISE EXCEPTION mx_last_excep.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_sap_user_id_via_pernr


  METHOD get_search_rule_instance.
    "-----------------------------------------------------------------*
    "   Get search rule instance
    "-----------------------------------------------------------------*
    ro_search_rule = NEW zcl_ca_wf_om_search_rule( iv_search_rule_id = iv_search_rule_id
                                                   iv_search_level   = iv_search_level
                                                   iv_valid_on       = mv_valid_on ).
  ENDMETHOD.                    "get_search_rule_instance


  METHOD get_tasks_2_position.
    "-----------------------------------------------------------------*
    "   Get tasks to the position of the employee
    "-----------------------------------------------------------------*
    DATA(lt_tasks_2_pos) = zcl_ca_wf_om_org_model=>get_instance(
                                       is_key           = is_position
                                       iv_valid_on      = mv_valid_on
                                       iv_search_active = mv_search_active )->get_org_model_data(
                                                 iv_eval_path = mo_cvc_om->evaluation_path-tasks_2_position ).

    rt_tasks = VALUE #( FOR ls_task IN lt_tasks_2_pos  WHERE ( otype = swfco_org_task )  ( ls_task-objid ) ).
  ENDMETHOD.                    "get_tasks_2_position


  METHOD get_value_to_comm_type.
    "-----------------------------------------------------------------*
    "   Create instance by personnel number
    "-----------------------------------------------------------------*
    CLEAR ev_result.
    mo_cvc_employee->is_comm_type_valid( iv_comm_type ).

    DATA(lv_column_name) = SWITCH fieldname( mo_cvc_employee->is_long_id_used_in_comm_type( iv_comm_type )
                             WHEN abap_true   THEN mo_cvc_employee->column_name-usrid_long
                             WHEN abap_false  THEN mo_cvc_employee->column_name-usrid ).

    SELECT FROM pa0105
         FIELDS (lv_column_name)
          WHERE pernr EQ @mv_key
            AND subty EQ @iv_comm_type
            AND objps EQ @space
            AND sprps EQ @space
            AND endda GE @mv_valid_on
            AND begda LE @mv_valid_on                   "#EC CI_NOORDER
           INTO @ev_result
                UP TO 1 ROWS.
    ENDSELECT.

    IF ev_result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee.
    ENDIF.
  ENDMETHOD.                    "get_value_to_comm_type


  METHOD get_windows_net_id_via_pernr.
    "-----------------------------------------------------------------*
    "   Determine windows net Id via personnel number
    "-----------------------------------------------------------------*
    TRY.
        get_value_to_comm_type(
                           EXPORTING
                             iv_comm_type = mo_cvc_employee->comm_type-windows_net_id
                           IMPORTING
                             ev_result    = result ).

      CATCH zcx_ca_wf_om_employee.
*        "No &1 maintained for personnel number &2 in infotype 0105
*        mx_last_excep = NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>subtyp_missing
*                                                   mv_msgty = c_msgty_w
*                                                   mv_msgv1 = 'NT-/Windows Id'(nid)
*                                                   mv_msgv2 = CONV #( |{ mv_key ALPHA = OUT }| ) ).
*        mv_is_erroneous = abap_true.
*        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).
*
*        IF iv_raise_exception EQ abap_true.
*          RAISE EXCEPTION mx_last_excep.
*        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_windows_net_id_via_pernr


  METHOD read_master_record.
    "-----------------------------------------------------------------*
    "   Read specific indicators of personnel master record
    "-----------------------------------------------------------------*
    SELECT FROM pa0000
         FIELDS stat1 AS cust_status,     stat2 AS std_status,
                massn AS last_action,     massg AS reason_2_action
          WHERE pernr EQ @mv_key
            AND subty EQ @space
            AND objps EQ @space
            AND sprps EQ @space
            AND endda GE @mv_valid_on
            AND begda LE @mv_valid_on
           INTO CORRESPONDING FIELDS OF @ms_master_record
                UP TO 1 ROWS.                           "#EC CI_NOORDER

    ENDSELECT.
    IF ms_master_record IS INITIAL.
      "Personnel number & does not exist
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID 'PG' TYPE 'E' NUMBER '041' WITH |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "read_master_record


  METHOD read_name_details.
    "-----------------------------------------------------------------*
    "   Read name details
    "-----------------------------------------------------------------*
    TRY.
        "Get single values to the person, like title, surname and name
        SELECT FROM pa0002 AS p2
                    LEFT OUTER JOIN t522t AS t5        "#EC CI_BUFFJOIN
                          ON t5~sprsl EQ @sy-langu AND
                             t5~anred EQ p2~anred
             FIELDS p2~anred,  t5~atext,  p2~titel,  p2~titl2,
                    p2~vorna,  p2~midnm,  p2~nachn
              WHERE p2~pernr EQ @mv_key
                AND p2~subty EQ @space
                AND p2~objps EQ @space
                AND p2~sprps EQ @space
                AND p2~endda GE @mv_valid_on
                AND p2~begda LE @mv_valid_on
               INTO CORRESPONDING FIELDS OF @ms_data
                    UP TO 1 ROWS.                       "#EC CI_NOORDER
        ENDSELECT.
        IF sy-subrc NE 0.
          "No personnel number found to &1 &2 or is not valid on &3
          RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
            EXPORTING
              textid   = zcx_ca_wf_om_employee=>pernr_not_found
              mv_msgty = c_msgty_e
              mv_msgv1 = CONV #( |{ mv_key ALPHA = OUT }| )    "Variable 2 is not relevant in this case
              mv_msgv3 = CONV #( |{ mv_valid_on DATE = ENVIRONMENT }| ).
        ENDIF.

      CATCH zcx_ca_wf_om_employee INTO mx_last_excep.
        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep
                                      iv_class   = CONV #( ms_lpor-typeid )
                                      iv_method  = 'READ_NAME_DETAILS' ) ##no_text.
    ENDTRY.
  ENDMETHOD.                    "read_name_details


  METHOD read_org_assignments.
    "-----------------------------------------------------------------*
    "   Read organizational assignments from personnel master record
    "-----------------------------------------------------------------*
    SELECT FROM pa0001
         FIELDS *
          WHERE pernr EQ @mv_key
            AND subty EQ @space
            AND objps EQ @space
            AND sprps EQ @space
            AND endda GE @mv_valid_on
            AND begda LE @mv_valid_on
           INTO CORRESPONDING FIELDS OF @ms_data-s_org_assignm
                UP TO 1 ROWS.                           "#EC CI_NOORDER

    ENDSELECT.
  ENDMETHOD.                    "read_org_assignments


  METHOD zif_ca_wf_om_employee~find_manager_via_search_rule.
    "-----------------------------------------------------------------*
    "   Change validity date and refresh data
    "-----------------------------------------------------------------*
    TRY.
        ev_is_self_approving = abap_false.

        DATA(lt_units_above) = get_org_units_above_employee( ).
        DATA(lo_search_rule) = get_search_rule_instance( iv_search_rule_id = iv_search_rule_id
                                                         iv_search_level   = iv_search_level ).

        "Org. units are delivered in the right order bottom-up
        DATA(lv_return) = abap_false.
        LOOP AT lt_units_above REFERENCE INTO DATA(lr_unit_above).
          TRY.
              DATA(lo_org_unit_above) =
                        zcl_ca_wf_om_org_model=>get_instance( is_key = CORRESPONDING #( lr_unit_above->* ) ).
              DATA(lo_manager) = lo_org_unit_above->get_manager( iv_search_upwards = 0 )-o_owner.
              IF lo_manager->ms_data-pernr EQ ms_data-pernr.   "Found manager is the current employee -> search higher
                CONTINUE.
              ENDIF.

              lo_manager->is_active( ).
              "Due to anonymization reasons of the OM data avoid this check in non-productive systems
              IF zcl_ca_system_info=>is_for_production( ).
                lo_manager->is_sap_user_available( ).
              ENDIF.

              "An employee can be assigned to different org. units. Load the assignments for the org. unit here.
              lo_manager->load_assigned_om_objects( CORRESPONDING #( lr_unit_above->* ) ).
              IF lo_search_rule->is_self_approving( lo_manager ).  "normally only board members
                IF iv_return_last_before_selfappr EQ abap_true.
                  lv_return = abap_true.
                  EXIT.       "Result is set in last loop before

                ELSE.
                  ev_is_self_approving = abap_true.
                  ro_manager = mo_manager = lo_manager.
                  lv_return = abap_true.
                  EXIT.
                ENDIF.
              ENDIF.

              "Set last manager for the case the condition afterwards results in a continuation and the
              "the next found manager is a board member, but should be excluded as result.
              ro_manager = mo_manager = lo_manager.

              IF lo_search_rule->is_assigned_as_approver( mo_manager ) OR
                 lo_search_rule->has_task_4_delegation( mo_manager ).
                lv_return = abap_true.
                EXIT.         "A valid manager is found
              ENDIF.

              CONTINUE.       "Is neither the right approver nor the manager has a delegation task

            CATCH zcx_ca_wf_om_no_manager INTO DATA(lx_no_manager_found).
              mo_log->add_msg_exc( ix_excep     = lx_no_manager_found
                                   iv_probclass = mo_log->zif_ca_log~mo_log_options->problem_class-info ).
              CONTINUE.

            CATCH zcx_ca_wf_om_employee INTO DATA(lx_is_inactive).  "Manager is not active or has no active SAP user
              mo_log->add_msg_exc( ix_excep     = lx_is_inactive
                                   iv_probclass = mo_log->zif_ca_log~mo_log_options->problem_class-info ).
              CONTINUE.
          ENDTRY.
        ENDLOOP.

        mo_log->save( iv_close = abap_false ).  "Keep the log open for case of re-usage of this employee instance
        IF lv_return EQ abap_true.              "Find logged messages in TA SLG1 with ZCA_WF / EMPLOYEE
          RETURN.
        ENDIF.

        "No valid leader or superior found to org. unit &1 &2
        RAISE EXCEPTION NEW zcx_ca_wf_om_no_manager( textid   = zcx_ca_wf_om_no_manager=>no_valid_leader_found
                                                     mv_msgty = zcx_ca_wf_om_no_manager=>c_msgty_e
                                                     mv_msgv1 = CONV #( swfco_org_person )
                                                     mv_msgv2 = CONV #( |{ mv_key ALPHA = OUT }| ) ) ##no_text.

      CATCH zcx_ca_wf_om_no_manager INTO lx_no_manager_found.
        mo_log->add_msg_exc( ix_excep = lx_no_manager_found ).
        mo_log->save( iv_close = abap_false ).  "Keep the log open for case of re-usage of this employee instance
        RAISE EXCEPTION lx_no_manager_found.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          mo_log->add_msg_exc( ix_excep = lx_error ).
          mo_log->save( iv_close = abap_false ).  "Keep the log open for case of re-usage of this employee instance
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_employee~find_manager_via_search_rule


  METHOD zif_ca_wf_om_employee~compose_name_n_salutation.
    "-----------------------------------------------------------------*
    "   Read name details and compose salutation (IS SET INTO MS_DATA-FULL_NAME)
    "-----------------------------------------------------------------*
    "Compose salutation for mails / letters
    ms_data-full_name = condense( |{ COND #( WHEN ms_data-titel IS NOT INITIAL
                                                     "   Dr.        or         Mr.
                                               THEN ms_data-titel  ELSE SWITCH #( iv_incl_mr_mrs
                                                                          WHEN abap_true  THEN ms_data-atext
                                                                          WHEN abap_false THEN space ) ) } | &
                                   "       Msc             Dietmar         Gottfried            Hopp
                                  |{ ms_data-titl2 } { ms_data-vorna } { ms_data-midnm } { ms_data-nachn }| ).

    rv_name_n_id = ms_data-full_name_w_id = zcl_ca_utils=>compose_name_n_techn_id(
                                                                        iv_descr = ms_data-full_name
                                                                        iv_techn_id = ms_data-pernr ).
  ENDMETHOD.                    "zif_ca_wf_om_employee~compose_name_n_salutation


  METHOD zif_ca_wf_om_employee~get_my_manager.
    "-----------------------------------------------------------------*
    "   Get my manager
    "-----------------------------------------------------------------*
    TRY.
        ro_manager = mo_manager = zcl_ca_wf_om_org_model=>get_instance( is_key           = CONV #( mv_agent )
                                                                        iv_valid_on      = mv_valid_on
                                                                        iv_search_active = mv_search_active
                                           )->get_manager( iv_search_upwards = iv_search_upwards )-o_owner.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee(
                                      zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                     iv_class    = 'ZCL_CA_WF_OM_ORG_MODEL'
                                                     iv_method   = 'GET_MANAGER'
                                                     ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          mv_is_erroneous = abap_true.
          mo_log->add_n_save_exception( ix_catched = lx_error ).

          IF iv_raise_exception EQ abap_true.
            RAISE EXCEPTION lx_error.
          ENDIF.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_employee~get_my_manager


  METHOD zif_ca_wf_om_employee~has_left_the_company.
    "-----------------------------------------------------------------*
    "   Has the employee left the company / group?
    "-----------------------------------------------------------------*
    rv_has_left = xsdbool( NOT is_transferred( )  AND
                           NOT is_active( ) ).

    IF rv_has_left IS NOT SUPPLIED AND
       rv_has_left EQ abap_false.
      "Employee & has left
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID '5D' TYPE 'E' NUMBER '365' WITH |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "zif_ca_wf_om_employee~has_left_the_company


  METHOD zif_ca_wf_om_employee~is_assigned_to.
    "-----------------------------------------------------------------*
    "   Is the requested JOB assigned to her/his position?
    "-----------------------------------------------------------------*
    TRY.
        mo_cvc_om->is_job_id_valid(
                              EXPORTING
                                iv_job_id     = iv_job_as
                                iv_valid_on   = mv_valid_on
                              IMPORTING
                                eo_om_obj_job = DATA(lo_job) ).

        rv_is_assigned = xsdbool( ms_data-objid_c EQ iv_job_as ).

        IF rv_is_assigned IS NOT SUPPLIED AND
           rv_is_assigned EQ abap_false.
          "Person &1 is not assigned to job &2
          RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
            EXPORTING
              textid   = zcx_ca_wf_om_employee=>person_is_not_assigned_to_job
              mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
              mv_msgv1 = CONV #( |{ mv_key ALPHA = OUT }| )
              mv_msgv2 = CONV #( zcl_ca_utils=>compose_name_n_techn_id( "= Team lead (10000038)
                                                              iv_descr    = lo_job->ms_data-name
                                                              iv_techn_id = iv_job_as ) ).
        ENDIF.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                         iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                         iv_class    = 'ZCL_CA_WF_OM_EMPLOYEE'
                                                         iv_method   = 'ZIF_CA_WF_OM_EMPLOYEE~IS_ASSIGNED_TO'
                                                         ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_employee~is_assigned_to


  METHOD zif_ca_wf_om_employee~is_a_departm_lead_or_higher.
    "-----------------------------------------------------------------*
    "   Is the manager a department lead or higher?
    "-----------------------------------------------------------------*
    rv_is_a_lead = xsdbool( is_assigned_to( mo_cvc_om->job_as-regional_lead )            OR
                            is_assigned_to( mo_cvc_om->job_as-department_lead )          OR
                            is_assigned_to( mo_cvc_om->job_as-lead_enterprise_function ) OR
                            is_assigned_to( mo_cvc_om->job_as-managem_board_member )     OR
                            is_assigned_to( mo_cvc_om->job_as-group_board_member ) ).
  ENDMETHOD.                    "zif_ca_wf_om_employee~is_a_departm_lead_or_higher


  METHOD zif_ca_wf_om_employee~is_active.
    "-----------------------------------------------------------------*
    "   Is the employee active?
    "-----------------------------------------------------------------*
    rv_is_active = xsdbool( ms_master_record-std_status  EQ mo_cvc_employee->employment_status-is_active AND
                            ms_master_record-cust_status EQ mo_cvc_employee->employment_status-is_active ).

    IF rv_is_active IS NOT SUPPLIED AND
       rv_is_active EQ abap_false.
      "Personnel number &1 does not have 'active' status
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID 'PWWW' TYPE 'E' NUMBER '115' WITH |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "zif_ca_wf_om_employee~is_active


  METHOD zif_ca_wf_om_employee~is_transferred.
    "-----------------------------------------------------------------*
    "   Is the employee transferred within the company / group?
    "-----------------------------------------------------------------*
    rv_is_transferred = xsdbool( ms_master_record-last_action     EQ '49' AND   "Withdrawal / Austritt
                                 ms_master_record-reason_2_action EQ '14' ).    "Transfer to other company of the group
  ENDMETHOD.                    "zif_ca_wf_om_employee~is_transferred


  METHOD zif_ca_wf_om_employee~is_responsible_for.
    "-----------------------------------------------------------------*
    "   Is the requested TASK assigned to her/his position?
    "-----------------------------------------------------------------*
    TRY.
        mo_cvc_om->is_task_id_valid(
                                EXPORTING
                                  iv_task_id  = iv_task
                                  iv_valid_on = mv_valid_on
                                IMPORTING
                                  eo_om_obj_task = DATA(lo_task) ).
        rv_is_assigned = xsdbool( line_exists( mt_my_tasks[ table_line = iv_task ] ) ).

        IF rv_is_assigned IS NOT SUPPLIED AND
           rv_is_assigned EQ abap_false.
          "Person &1 is not assigned to job &2
          RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
            EXPORTING
              textid   = zcx_ca_wf_om_employee=>person_is_not_assigned_to_job
              mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
              mv_msgv1 = CONV #( |{ mv_key ALPHA = OUT }| )
              mv_msgv2 = CONV #( zcl_ca_utils=>compose_name_n_techn_id( "= Approving supererogations (00000001)
                                                    iv_descr    = lo_task->ms_data-name
                                                    iv_techn_id = iv_task ) ).
        ENDIF.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       iv_class    = 'ZCL_CA_WF_OM_EMPLOYEE'
                                                       iv_method   = 'ZIF_CA_WF_OM_EMPLOYEE~IS_RESPONSIBLE_FOR'
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_employee~is_responsible_for


  METHOD zif_ca_wf_om_employee~is_sap_user_available.
    "-----------------------------------------------------------------*
    "   Is the SAP user available?
    "-----------------------------------------------------------------*
    rv_is_available = xsdbool( ms_data-bname IS NOT INITIAL AND
                               mo_user       IS     BOUND ).

    IF rv_is_available IS NOT SUPPLIED AND
       rv_is_available EQ abap_false.
      "SAP user id '&1' not maintained in personnel master or does not exist
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        EXPORTING
          textid   = zcx_ca_wf_om_employee=>sap_user_not_available
          mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
          mv_msgv1 = CONV #( |{ ms_data-bname }| ).
    ENDIF.
  ENDMETHOD.                    "zif_ca_wf_om_employee~is_sap_user_available


  METHOD zif_ca_wf_om_employee~load_assigned_om_objects.
    "-----------------------------------------------------------------*
    "   Load org. assignmts for requested org. object (only O or S!)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_caught         TYPE REF TO zcx_ca_error,
      ls_org_assignment TYPE ty_s_org_assignment.

    TRY.
        build_up_org_assignm_buffer( ).

      CATCH zcx_ca_error INTO lx_caught.
        mx_last_excep = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       iv_class    = CONV #( ms_lpor-typeid )
                                                       iv_method   = 'BUILD_UP_ORG_ASSIGNM_BUFFER'
                                                       ix_error    = lx_caught ) ) ##no_text.
        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).
    ENDTRY.

    TRY.
        CASE is_org_object-otype.
          WHEN swfco_org_position.
            ls_org_assignment = VALUE #( mt_org_assignments[ objid_s = is_org_object-objid ] OPTIONAL ).

          WHEN swfco_org_orgunit.
            ls_org_assignment = VALUE #( mt_org_assignments[ objid_ou = is_org_object-objid ] OPTIONAL ).

          WHEN OTHERS.
            "Parameter '&1' has invalid value '&2'
            RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>param_invalid
                                                       mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                       mv_msgv1 = 'IS_ORG_OBJECT-OTYPE'
                                                       mv_msgv2 = CONV #( is_org_object-otype ) ) ##no_text.
        ENDCASE.

        IF ls_org_assignment IS INITIAL.
          "Check lead assignments of personnel Id &1
          RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>check_lead_assignm_of_employee
                                                     mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                     mv_msgv1 = CONV #( |{ mv_key ALPHA = OUT }| ) ) ##no_text.
        ENDIF.

        ms_data = CORRESPONDING #( BASE ( ms_data ) ls_org_assignment ).
        mt_my_tasks = ls_org_assignment-t_tasks.

      CATCH zcx_ca_error INTO lx_caught.
        mx_last_excep = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       iv_class    = CONV #( ms_lpor-typeid )
                                                       iv_method   = 'LOAD_ASSIGNED_OM_OBJECTS'
                                                       ix_error    = lx_caught ) ) ##no_text.
        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).
    ENDTRY.
  ENDMETHOD.                    "zif_ca_wf_om_employee~load_assigned_om_objects


  METHOD zif_ca_workflow~check_existence.
    "-----------------------------------------------------------------*
    "   Check existence of object
    "-----------------------------------------------------------------*
    IF mo_log IS NOT BOUND.
      mo_log ?= zcl_ca_log=>get_instance( iv_object       = ms_log_key-object
                                          iv_subobj       = ms_log_key-subobject
                                          is_lpor         = CORRESPONDING #( ms_lpor )
                                          iv_extnumber    = CONV #( mv_key )
                                          iv_del_before   = abap_true
                                          iv_del_date     = CONV #( sy-datlo + 365 )
                                          iv_log_cls_name = mv_my_log_class_name ) ##no_text.
    ENDIF.

    CLEAR ms_data.
    check_existence_pers_master( ).
    read_master_record( ).
    read_org_assignments( ).

    "Read name and compose salutation
    read_name_details( ).
    compose_name_n_salutation( ).

    mv_agent          = |{ swfco_org_person WIDTH = 2 }{ mv_key }|.
    ms_data-pernr     = mv_key.
    ms_data-mail_addr = get_email_addr_via_pernr( ).
    ms_data-net_id    = get_windows_net_id_via_pernr( ).
    ms_data-bname     = get_sap_user_id_via_pernr( ).

    load_assigned_om_objects( VALUE #( otype = swfco_org_orgunit
                                       objid = ms_data-orgeh ) ).

    create_sap_user_instance( ).
  ENDMETHOD.                    "zif_ca_workflow~check_existence


  METHOD zif_ca_workflow~get_task_descr.
    "-----------------------------------------------------------------*
    "   Assemble task short text
    "-----------------------------------------------------------------*
    "Example ==> see also method BI_OBJECT~DEFAULT_ATTRIBUTE_VALUE
    "= TEXT-DAV = Employee Xxxxxx Yyyyyy Zzzzzzz (nnnnnnnn) - Description
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
