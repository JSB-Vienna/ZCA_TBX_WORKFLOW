"! <p class="shorttext synchronized" lang="en">WF-OM: BC Employee in Org. management (workflow-capable)</p>
CLASS zcl_ca_wf_om_employee DEFINITION PUBLIC
                                       CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_workflow,            " !!! Includes IF_WORKFLOW = BI_OBJECT + BI_PERSISTENT
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
      mv_default_attr    FOR zif_ca_workflow~mv_default_attr.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Employment status</p>
      BEGIN OF cs_employment_status,
        is_leaving  TYPE stat2 VALUE pfrd3_c_stat2_leave,      "= 0
        is_inactive TYPE stat2 VALUE pfrd3_c_stat2_inactive,   "= 1
        is_retired  TYPE stat2 VALUE pfrd3_c_stat2_retire,     "= 2
        is_active   TYPE stat2 VALUE pfrd3_c_stat2_active,     "= 3
      END   OF cs_employment_status.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">BC Extended user</p>
      mo_user         TYPE REF TO zcl_ca_wf_user READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">WF-OM: BC Manager in Org. management (workflow-capable)</p>
      mo_manager      TYPE REF TO zcl_ca_wf_om_employee READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Instance of last occurred exception</p>
      mx_last_excep   TYPE REF TO zcx_ca_wf_om_employee READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Object data</p>
      ms_data         TYPE zca_wf_s_employee READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Compatible HCM person for usage in workflow expressions</p>
      mv_agent        TYPE swp_agent READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Key attribute - Personnel number</p>
      mv_key          TYPE pernr_d READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">X = Errors occurred while determine additional data</p>
      mv_is_erroneous TYPE abap_boolean READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current validity date the data were read</p>
      mv_valid_on     TYPE hr_date READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create instance by LPOR or by personnel number</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @parameter iv_valid_on  | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance
        IMPORTING
          is_lpor       TYPE sibflpor OPTIONAL
          iv_key        TYPE pernr_d  OPTIONAL
          iv_valid_on   TYPE hr_date  DEFAULT sy-datlo
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance by windows net Id</p>
      "!
      "! @parameter iv_windows_net_id | <p class="shorttext synchronized" lang="en">Windows net Id</p>
      "! @parameter iv_valid_on       | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_user_cls_name  | <p class="shorttext synchronized" lang="en">Name of (inherited) user / employee class</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param      | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc      | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance_by_windows_net_id
        IMPORTING
          iv_windows_net_id TYPE zca_wf_e_windows_net_id
          iv_valid_on       TYPE hr_date DEFAULT sy-datlo
          iv_user_cls_name  TYPE seoclsname DEFAULT 'ZCL_CA_WF_OM_EMPLOYEE' ##no_text
        RETURNING
          VALUE(result)     TYPE REF TO zcl_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance by SAP user Id</p>
      "!
      "! @parameter iv_sap_user_id   | <p class="shorttext synchronized" lang="en">SAP user id</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_user_cls_name | <p class="shorttext synchronized" lang="en">Name of (inherited) user / employee class</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance_by_sap_user_id
        IMPORTING
          iv_sap_user_id   TYPE xubname
          iv_valid_on      TYPE hr_date DEFAULT sy-datlo
          iv_user_cls_name TYPE seoclsname DEFAULT 'ZCL_CA_WF_OM_EMPLOYEE' ##no_text
        RETURNING
          VALUE(result)    TYPE REF TO zcl_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance by lowest Personnell Number</p>
      "!
      "! @parameter iv_pernr         | <p class="shorttext synchronized" lang="en">Personnell number</p>
      "! @parameter iv_valid_on      | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter iv_user_cls_name | <p class="shorttext synchronized" lang="en">Name of (inherited) user / employee class</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Created instance or found in buffer</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance_by_lowest_pernr
        IMPORTING
          iv_pernr         TYPE pernr_d
          iv_valid_on      TYPE hr_date DEFAULT sy-datlo
          iv_user_cls_name TYPE seoclsname DEFAULT 'ZCL_CA_WF_OM_EMPLOYEE' ##no_text
        RETURNING
          VALUE(result)    TYPE REF TO zcl_ca_wf_om_employee
        RAISING
          zcx_ca_param
          zcx_ca_dbacc.


*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @parameter iv_valid_on  | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      constructor
        IMPORTING
          iv_key      TYPE pernr_d
          iv_valid_on TYPE hr_date DEFAULT sy-datlo
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Change validity date -&gt; this REFRESHES (!) the data</p>
      "!
      "! @parameter iv_valid_on           | <p class="shorttext synchronized" lang="en">New validity date</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      change_validity_date_n_refresh
        IMPORTING
          iv_valid_on TYPE hr_date DEFAULT sy-datlo
        RAISING
          zcx_ca_wf_om_employee,

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

      "! <p class="shorttext synchronized" lang="en">Get status of personnel master</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Status of personnel master (use MS_EMPL.. to compare)</p>
      get_master_record_status
        RETURNING
          VALUE(result) TYPE stat2,

      "! <p class="shorttext synchronized" lang="en">Get my manager</p>
      "!
      "! @parameter iv_raise_exception    | <p class="shorttext synchronized" lang="en">X = Raise exception instead of flag MV_IS_ERRONEOUS</p>
      "! @parameter ro_manager            | <p class="shorttext synchronized" lang="en">Manager</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_my_manager
        IMPORTING
          iv_raise_exception TYPE abap_boolean DEFAULT abap_false
        RETURNING
          VALUE(ro_manager)  TYPE REF TO zcl_ca_wf_om_employee
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Read name details and compose salutation</p>
      "!
      "! @parameter iv_incl_mr_mrs | <p class="shorttext synchronized" lang="en">X = Include Mr or Mrs (no impact on the title)</p>
      compose_name_n_salutation
        IMPORTING
          iv_incl_mr_mrs TYPE abap_boolean DEFAULT abap_false,

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
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants and value checks for Org. managment Employee</p>
      mo_employee_options TYPE REF TO zcl_ca_wf_om_c_employee,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Workflow object instance key</p>
      ms_lpor             TYPE sibflpor,

      "! <p class="shorttext synchronized" lang="en">Specific master data indicators</p>
      BEGIN OF ms_master_record,
        "! <p class="shorttext synchronized" lang="en">Lock indicator</p>
        lock_indicator TYPE sprps,
        "! <p class="shorttext synchronized" lang="en">Employment Status</p>
        status         TYPE stat2,
      END OF ms_master_record,

      "! <p class="shorttext synchronized" lang="en">Log object for BAL</p>
      BEGIN OF ms_log_key,
        "! <p class="shorttext synchronized" lang="en">Object for BAL</p>
        object    TYPE balobj_d,
        "! <p class="shorttext synchronized" lang="en">Sub object for BAL</p>
        subobject TYPE balsubobj,
      END OF ms_log_key,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Name of specific, application individual log class</p>
      mv_my_log_class_name TYPE seoclsname.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get personnel number by communication type from PA0105</p>
      "!
      "! @parameter iv_comm_type          | <p class="shorttext synchronized" lang="en">Communication type in PA0105 (column USRTY)</p>
      "! @parameter iv_comm_value         | <p class="shorttext synchronized" lang="en">Value to communication type of PA0105 (column USRID)</p>
      "! @parameter iv_valid_on           | <p class="shorttext synchronized" lang="en">Validity date</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Personnel number</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_pers_no_by_comm_type_value
        IMPORTING
          iv_comm_type  TYPE usrty
          iv_comm_value TYPE simple
          iv_valid_on   TYPE hr_date
        RETURNING
          VALUE(result) TYPE pernr_d
        RAISING
          zcx_ca_wf_om_employee.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check general existence of personnel master data</p>
      check_existence_pers_master
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Create SAP user instance</p>
      create_sap_user_instance
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Determine leading personnel number</p>
      "!
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Leading personnel number</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      determine_leading_personnel_no
        RETURNING
          VALUE(result) TYPE pernr_d
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Determine assigned org. unit and its manager to employee</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_assigned_om_objects
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Read specific indicators of personnel master record</p>
      "!
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      get_master_record_indicators
        RAISING
          zcx_ca_wf_om_employee,

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
                                       WITH UNIQUE KEY primary_key COMPONENTS s_lpor.

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

ENDCLASS.



CLASS zcl_ca_wf_om_employee IMPLEMENTATION.


  METHOD bi_object~default_attribute_value.
    "-----------------------------------------------------------------*
    "   Returns a description and/or prepared key of the object.
    "-----------------------------------------------------------------*
    "Example ==> replace the next 3 lines (comment and key preparation) as needed for your purpose
    "TEXT-DAV = Employee Xxxxxx Yyyyyy Zzzzzzz (nnnnnnnn)
    mv_default_attr = condense( |({ mv_key ALPHA = OUT })| ).
    mv_default_attr = |{ TEXT-dav } { ms_data-full_name } { mv_default_attr }|.

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
    DELETE mt_buffer WHERE s_lpor EQ ms_lpor.
  ENDMETHOD.                    "bi_object~release


  METHOD bi_persistent~find_by_lpor.
    "-----------------------------------------------------------------*
    "   Create business class instance
    "-----------------------------------------------------------------*
    TRY.
        result = zcl_ca_wf_om_employee=>get_instance( is_lpor = lpor ).

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


  METHOD get_windows_net_id_via_pernr.
    "-----------------------------------------------------------------*
    "   Determine windows net Id via personnel number
    "-----------------------------------------------------------------*
    TRY.
        get_value_to_comm_type(
                           EXPORTING
                             iv_comm_type = mo_employee_options->comm_type-windows_net_id
                           IMPORTING
                             ev_result    = result ).

      CATCH zcx_ca_wf_om_employee.
        "No &1 maintained for personnel number &2 in infotype 0105
        mx_last_excep = NEW zcx_ca_wf_om_employee(
                                              textid   = zcx_ca_wf_om_employee=>subtyp_missing
                                              mv_msgty = c_msgty_w
                                              mv_msgv1 = 'NT-/Windows Id'(nid)
                                              mv_msgv2 = CONV #( |{ mv_key ALPHA = OUT }| ) ).

        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).

        IF iv_raise_exception EQ abap_true.
          RAISE EXCEPTION mx_last_excep.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_windows_net_id_via_pernr


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
                         typeid = to_upper( zcl_ca_wf_om_employee=>c_my_typeid )
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
                                                                s_lpor = ls_lpor ].
        ls_buffer-o_persistent->refresh( ).

      CATCH cx_sy_itab_line_not_found.
        "Create instance of payment approval object
        CREATE OBJECT ls_buffer-o_persistent TYPE (ls_lpor-typeid)
          EXPORTING
            iv_key = lv_key.

        "Checks existence of object and creates default attribute = readable key with text
        ls_buffer-o_persistent->check_existence( ).
        ls_buffer-o_persistent->default_attribute_value( ).

        ls_buffer-s_lpor = ls_buffer-o_persistent->lpor( ).
        INSERT ls_buffer INTO TABLE zcl_ca_wf_om_employee=>mt_buffer.
    ENDTRY.

    result ?= ls_buffer-o_persistent.
  ENDMETHOD.                    "get_instance


  METHOD get_instance_by_sap_user_id.
    "-----------------------------------------------------------------*
    "   Create instance by SAP user Id
    "-----------------------------------------------------------------*
    TRY.
        DATA(lv_pernr) = zcl_ca_wf_om_employee=>get_pers_no_by_comm_type_value(
                                             iv_comm_type  = zcl_ca_wf_om_c_employee=>comm_type-sap_user_id
                                             iv_comm_value = iv_sap_user_id
                                             iv_valid_on   = iv_valid_on ).

        result = zcl_ca_wf_om_employee=>get_instance( is_lpor     = VALUE #( instid = CONV #( lv_pernr )
                                                                             typeid = CONV #( iv_user_cls_name )
                                                                             catid  = swfco_objtype_cl )
                                                      iv_valid_on = iv_valid_on ).

      CATCH zcx_ca_wf_om_employee.
        "No personnel number found to &1 &2 or is not valid on &3
        RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
          EXPORTING
            textid   = zcx_ca_wf_om_employee=>pernr_not_found
            mv_msgty = c_msgty_e
            mv_msgv1 = 'SAP user Id'(sap)
            mv_msgv2 = CONV #( iv_sap_user_id )
            mv_msgv3 = CONV #( |{ iv_valid_on DATE = ENVIRONMENT }| ).
    ENDTRY.
  ENDMETHOD.                    "get_instance_by_sap_user_id


  METHOD check_existence_pers_master.
    "-----------------------------------------------------------------*
    "   Check general existence of personnel master data
    "-----------------------------------------------------------------*
    SELECT FROM pa0002
         FIELDS pernr
          WHERE pernr EQ @mv_key
            AND endda GE @mv_valid_on
            AND begda LE @mv_valid_on
           INTO @ms_data-pernr
                UP TO 1 ROWS.

    ENDSELECT.
    IF ms_data-pernr IS INITIAL.
      "Personnel number & does not exist
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID 'PG' TYPE 'E' NUMBER '041'
        WITH
            |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "check_existence_pers_master


  METHOD get_instance_by_windows_net_id.
    "-----------------------------------------------------------------*
    "   Create instance by windows net Id
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_pernr              TYPE pernr_d.

    TRY.
        TRY.
            lv_pernr = zcl_ca_wf_om_employee=>get_pers_no_by_comm_type_value(
                                           iv_comm_type  = zcl_ca_wf_om_c_employee=>comm_type-windows_net_id
                                           iv_comm_value = iv_windows_net_id
                                           iv_valid_on   = iv_valid_on ).

          CATCH zcx_ca_wf_om_employee.
            lv_pernr = zcl_ca_wf_om_employee=>get_pers_no_by_comm_type_value(
                                          iv_comm_type  = zcl_ca_wf_om_c_employee=>comm_type-windows_net_id
                                          iv_comm_value = to_upper( iv_windows_net_id )  "TO_UPPER !!
                                          iv_valid_on   = iv_valid_on ).
        ENDTRY.

        result = zcl_ca_wf_om_employee=>get_instance( is_lpor     = VALUE #( instid = CONV #( lv_pernr )
                                                                             typeid = CONV #( iv_user_cls_name )
                                                                             catid  = swfco_objtype_cl )
                                                      iv_valid_on = iv_valid_on ).

      CATCH zcx_ca_wf_om_employee.
        "No personnel number found to &1 &2 or is not valid on &3
        RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
          EXPORTING
            textid   = zcx_ca_wf_om_employee=>pernr_not_found
            mv_msgty = c_msgty_e
            mv_msgv1 = 'NT-/Windows Id'(nid)
            mv_msgv2 = CONV #( iv_windows_net_id )
            mv_msgv3 = CONV #( |{ iv_valid_on DATE = ENVIRONMENT }| ).
    ENDTRY.
  ENDMETHOD.                    "get_instance_by_windows_net_id


  METHOD change_validity_date_n_refresh.
    "-----------------------------------------------------------------*
    "   Change validity date and refresh data
    "-----------------------------------------------------------------*
    mv_valid_on = iv_valid_on.
    refresh( ).
  ENDMETHOD.                    "change_validity_date_n_refresh


  METHOD compose_name_n_salutation.
    "-----------------------------------------------------------------*
    "   Compose name inclusive salutation (IS SET INTO MS_DATA-FULL_NAME)
    "-----------------------------------------------------------------*
    "Compose salutation for mails / letters
    ms_data-full_name = condense( |{ COND #( WHEN ms_data-titel IS NOT INITIAL
                                                     "   Dr.        or         Mr.
                                               THEN ms_data-titel  ELSE SWITCH #( iv_incl_mr_mrs
                                                                          WHEN abap_true  THEN ms_data-atext
                                                                          WHEN abap_false THEN space ) ) } | &
                                   "       Msc             Dietmar         Gottfried            Hopp
                                  |{ ms_data-titl2 } { ms_data-vorna } { ms_data-midnm } { ms_data-nachn }| ).

    ms_data-full_name_w_id = |{ ms_data-full_name } ({ condense( |{ ms_data-pernr ALPHA = OUT }| ) })|.
  ENDMETHOD.                    "compose_name_n_salutation


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
                AND p2~endda GE @mv_valid_on
                AND p2~begda LE @mv_valid_on
               INTO CORRESPONDING FIELDS OF @ms_data
                    UP TO 1 ROWS.
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


  METHOD create_sap_user_instance.
    "-----------------------------------------------------------------*
    "   Create SAP user instance if available
    "-----------------------------------------------------------------*
    TRY.
        IF ms_data-bname IS INITIAL.
          RETURN.
        ENDIF.

        mo_user = zcl_ca_wf_user=>get_instance( iv_key = ms_data-bname ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_employee( zcx_ca_error=>create_exception(
                                                       iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                       iv_class    = 'ZCL_CA_WF_USER'
                                                       iv_method   = 'GET_INSTANCE'
                                                       ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          mv_is_erroneous = abap_true.
          mx_last_excep   = lx_error.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_sap_user_instance


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


  METHOD get_my_manager.
    "-----------------------------------------------------------------*
    "   Get my manager
    "-----------------------------------------------------------------*
    TRY.
        ro_manager = mo_manager =
               NEW zcl_ca_wf_om_org_model( is_org_object = ms_data-s_om_obj_key_ou
                                           iv_valid_on   = mv_valid_on )->get_manager( )-o_owner.

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
  ENDMETHOD.                    "get_my_manager


  METHOD zif_ca_workflow~check_existence.
    "-----------------------------------------------------------------*
    "   Check existence of object
    "-----------------------------------------------------------------*
    mo_log ?= zcl_ca_log=>get_instance( iv_object       = ms_log_key-object
                                        iv_subobj       = ms_log_key-subobject
                                        is_lpor         = CORRESPONDING #( ms_lpor )
                                        iv_extnumber    = CONV #( mv_key )
                                        iv_del_before   = abap_true
                                        iv_del_date     = CONV #( sy-datlo + 365 )
                                        iv_log_cls_name = mv_my_log_class_name ) ##no_text.

    CLEAR ms_data.
    check_existence_pers_master( ).
    get_master_record_indicators( ).

    "Read name and compose salutation
    read_name_details( ).
    compose_name_n_salutation( ).

    mv_agent          = |{ swfco_org_person WIDTH = 2 }{ mv_key }|.
    ms_data-pernr     = mv_key.
    ms_data-mail_addr = get_email_addr_via_pernr( ).
*    ms_data-net_id    = get_windows_net_id_via_pernr( ).
    ms_data-bname     = get_sap_user_id_via_pernr( ).

    get_assigned_om_objects( ).

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


  METHOD get_master_record_indicators.
    "-----------------------------------------------------------------*
    "   Read specific indicators of personnel master record
    "-----------------------------------------------------------------*
    SELECT FROM pa0000
         FIELDS sprps AS lock_indicator,
                stat2 AS status
          WHERE pernr EQ @mv_key
            AND endda GE @mv_valid_on
            AND begda LE @mv_valid_on
           INTO CORRESPONDING FIELDS OF @ms_master_record
                         UP TO 1 ROWS.

    ENDSELECT.
    IF ms_master_record IS INITIAL.
      "Personnel number & does not exist
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID 'PG' TYPE 'E' NUMBER '041'
        WITH
            |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "get_master_record_indicators


  METHOD get_master_record_status.
    "-----------------------------------------------------------------*
    "   Get status of personnel master
    "-----------------------------------------------------------------*
    result = ms_master_record-status.
  ENDMETHOD.                    "get_master_record_status


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
    mv_valid_on = iv_valid_on.
    mo_employee_options = zcl_ca_wf_om_c_employee=>get_instance( ).

    "Complete and keep several attributes
    ms_lpor-instid = mv_key = iv_key.
*    mbo_BO_PERNR-instid = CONV #( mv_key ).

    "Set log key - Instance creation in CHECK_EXISTANCE due to inheritances
    ms_log_key = cs_log_key.
    mv_my_log_class_name = c_my_log_class_name.
  ENDMETHOD.                    "constructor


  METHOD determine_leading_personnel_no.
    "-----------------------------------------------------------------*
    "   Determine leading personnel number
    "-----------------------------------------------------------------*
    SELECT usrid INTO  @DATA(lv_leading_pers_no)
                 FROM  pa0105
                       UP TO 1 ROWS
                 WHERE pernr EQ @mv_key
                   AND usrty EQ '9105'
                   AND endda GE @mv_valid_on
                   AND begda LE @mv_valid_on.           "#EC CI_NOORDER
    ENDSELECT.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee.
    ENDIF.

    result = CONV #( lv_leading_pers_no ).
  ENDMETHOD.                    "determine_leading_personnel_no


  METHOD get_assigned_om_objects.
    "-----------------------------------------------------------------*
    "   Determine assigned org. unit and its manager to employee
    "-----------------------------------------------------------------*
    TRY.
        DATA(lo_om_person) = NEW zcl_ca_wf_om_org_model( is_org_object = VALUE #( otype = swfco_org_person
                                                                                  objid = mv_key )
                                                         iv_valid_on   = mv_valid_on ).
        DATA(lt_om_objects_to_person) = lo_om_person->get_org_model_data( iv_eval_path = 'P-S-O' ) ##no_text.

        LOOP AT lt_om_objects_to_person REFERENCE INTO DATA(lr_om_object_to_person).
          CASE lr_om_object_to_person->otype.
            WHEN swfco_org_position.
              ms_data-otype_s      = lr_om_object_to_person->otype.
              ms_data-objid_s      = lr_om_object_to_person->objid.
              ms_data-name_s       = lr_om_object_to_person->stext.
              ms_data-short_name_s = lr_om_object_to_person->short.

            WHEN swfco_org_orgunit.
              ms_data-otype_ou      = lr_om_object_to_person->otype.
              ms_data-objid_ou      = lr_om_object_to_person->objid.
              ms_data-name_ou       = lr_om_object_to_person->stext.
              ms_data-short_name_ou = lr_om_object_to_person->short.
          ENDCASE.
        ENDLOOP.

      CATCH zcx_ca_wf_om_org_model INTO DATA(lx_catched).
        mx_last_excep = CAST zcx_ca_wf_om_employee(
                                  zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_wf_om_employee=>c_zcx_ca_wf_om_employee
                                                   iv_class    = CONV #( ms_lpor-typeid )
                                                   iv_method   = 'GET_ASSIGNED_OM_OBJECTS'
                                                   ix_error    = lx_catched ) ) ##no_text.
        mv_is_erroneous = abap_true.
        mo_log->add_n_save_exception( ix_catched = mx_last_excep ).
    ENDTRY.
  ENDMETHOD.                    "get_assigned_om_objects


  METHOD get_email_addr_via_pernr.
    "-----------------------------------------------------------------*
    "   Determine email address Id via personnel number
    "-----------------------------------------------------------------*
    TRY.
        get_value_to_comm_type(
                         EXPORTING
                           iv_comm_type = mo_employee_options->comm_type-email_address
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


  METHOD get_pers_no_by_comm_type_value.
    "-----------------------------------------------------------------*
    "   Get personnel number by communication type from PA0105
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_column_name       TYPE fieldname.

    DATA(lo_employee_options) = zcl_ca_wf_om_c_employee=>get_instance( ).
    lo_employee_options->is_comm_type_valid( iv_comm_type ).

    CASE iv_comm_type.
      WHEN lo_employee_options->comm_type-sap_user_id     OR
           lo_employee_options->comm_type-windows_net_id.
        lv_column_name = lo_employee_options->column_name-usrid.

      WHEN lo_employee_options->comm_type-email_address.
        lv_column_name = lo_employee_options->column_name-usrid_long.
    ENDCASE.

    DATA(lt_where) = VALUE se16n_where(
                            ( VALUE se16n_where_line( line = |{ lv_column_name } EQ @IV_COMM_VALUE| ) ) ) ##no_text.

    " POET_20240226 - adaptions HCM Central Person lowest active personnel number shall be taken
    SELECT 0105~pernr FROM pa0105 AS 0105
                  INNER JOIN pa0000 AS 0000            "#EC CI_BUFFJOIN
                          ON 0000~pernr EQ 0105~pernr
                WHERE 0105~usrty EQ @iv_comm_type
                  AND 0105~endda GE @iv_valid_on
                  AND 0105~begda LE @iv_valid_on
                  AND 0000~endda GE @iv_valid_on
                  AND 0000~begda LE @iv_valid_on
                  AND 0000~stat2 EQ @cs_employment_status-is_active
                  AND (lt_where)
             ORDER BY 0105~pernr ASCENDING
           INTO TABLE @DATA(lt_pa0105).

    IF lt_pa0105 IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee.
    ENDIF.

    result = lt_pa0105[ 1 ].
  ENDMETHOD.                    "get_pers_nbr_by_comm_type_value


  METHOD get_sap_user_id_via_pernr.
    "-----------------------------------------------------------------*
    "   Determine SAP user Id via personnel number
    "-----------------------------------------------------------------*
    TRY.
        get_value_to_comm_type(
                         EXPORTING
                           iv_comm_type = mo_employee_options->comm_type-sap_user_id
                         IMPORTING
                           ev_result    = result ).

      CATCH zcx_ca_wf_om_employee.
        "No &1 maintained for personnel number &2 in infotype 0105
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


  METHOD get_value_to_comm_type.
    "-----------------------------------------------------------------*
    "   Create instance by personnel number
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_column_name       TYPE fieldname.

    CLEAR ev_result.
    mo_employee_options->is_comm_type_valid( iv_comm_type ).

    CASE iv_comm_type.
      WHEN mo_employee_options->comm_type-sap_user_id    OR
           mo_employee_options->comm_type-windows_net_id.
        lv_column_name = mo_employee_options->column_name-usrid.

      WHEN mo_employee_options->comm_type-email_address.
        lv_column_name = mo_employee_options->column_name-usrid_long.
    ENDCASE.

    SELECT (lv_column_name) INTO  @ev_result
                            FROM  pa0105
                                  UP TO 1 ROWS
                            WHERE pernr EQ @mv_key
                              AND usrty EQ @iv_comm_type
                              AND endda GE @mv_valid_on
                              AND begda LE @mv_valid_on. "#EC CI_NOORDER
    ENDSELECT.

    IF ev_result IS INITIAL.
      DATA(lv_leading_pernr) = determine_leading_personnel_no( ).

      SELECT (lv_column_name) INTO  @ev_result
                              FROM  pa0105
                                    UP TO 1 ROWS
                              WHERE pernr EQ @lv_leading_pernr
                                AND usrty EQ @iv_comm_type
                                AND endda GE @mv_valid_on
                                AND begda LE @mv_valid_on. "#EC CI_NOORDER
      ENDSELECT.

      IF ev_result IS INITIAL.
        RAISE EXCEPTION TYPE zcx_ca_wf_om_employee.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_value_to_comm_type


  METHOD is_personnel_master_active.
    "-----------------------------------------------------------------*
    "   Is the personnel master record in status active?
    "-----------------------------------------------------------------*
    IF rv_is_active IS SUPPLIED.
      rv_is_active = xsdbool( ms_master_record-status EQ cs_employment_status-is_active ).

    ELSEIF ms_master_record-status NE cs_employment_status-is_active.
      "Personnel number &1 does not have 'active' status
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID 'PWWW' TYPE 'E' NUMBER '115'
           WITH |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "is_personnel_master_active


  METHOD is_personnel_master_locked.
    "-----------------------------------------------------------------*
    "   Is the personnel master record locked?
    "-----------------------------------------------------------------*
    IF rv_is_locked IS SUPPLIED.
      rv_is_locked = ms_master_record-lock_indicator.

    ELSEIF ms_master_record-lock_indicator NE abap_false.
      "Personnel number & is locked
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        MESSAGE ID '5A' TYPE 'E' NUMBER '198'
           WITH |{ mv_key ALPHA = OUT }|.
    ENDIF.
  ENDMETHOD.                    "is_personnel_master_locked


  METHOD is_sap_user_available.
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
  ENDMETHOD.                    "is_sap_user_available


  METHOD get_instance_by_lowest_pernr.
    "-----------------------------------------------------------------*
    "   Create instance by Lowest Personnell Number
    "-----------------------------------------------------------------*
    "Get central person, if applicable
    SELECT FROM hrp1001
         FIELDS sobid
          WHERE otype EQ @swfco_org_person        "P
            AND objid EQ @iv_pernr
            AND begda LE @iv_valid_on
            AND endda GE @iv_valid_on
            AND sclas EQ 'CP'
           INTO @DATA(lv_central_person)
                UP TO 1 ROWS.
    ENDSELECT.
    IF sy-subrc NE 0.
      "No valid central person found to personnel number &1
      RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>central_pernr_not_found
                                                 mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                 mv_msgv1 = CONV #( |{ iv_pernr ALPHA = OUT }| ) ).
    ENDIF.

    "Select all related personnel numbers
    SELECT FROM hrp1001 AS 1001
                LEFT OUTER JOIN pa0000 AS 0000         "#EC CI_BUFFJOIN
                             ON 0000~pernr EQ 1001~sobid
         FIELDS 1001~sobid
          WHERE 1001~otype EQ 'CP'
            AND 1001~objid EQ @lv_central_person
            AND 1001~endda GE @iv_valid_on
            AND 1001~begda LE @iv_valid_on
            AND 1001~sclas EQ @swfco_org_person                 "P
            AND 0000~endda GE @iv_valid_on
            AND 0000~begda LE @iv_valid_on
            AND 0000~stat2 EQ @cs_employment_status-is_active
                ORDER BY 1001~sobid ASCENDING
           INTO TABLE @DATA(lt_pers_ids_2_central_person).
    IF sy-subrc NE 0.
      "No valid personnel numbers found to central person &1
      RAISE EXCEPTION NEW zcx_ca_wf_om_employee( textid   = zcx_ca_wf_om_employee=>no_pers_ids_2_central_pernr
                                                 mv_msgty = zcx_ca_wf_om_employee=>c_msgty_e
                                                 mv_msgv1 = CONV #( |{ lv_central_person ALPHA = OUT }| ) ).
    ENDIF.

    DATA(lv_lowest_pernr) = VALUE pernr_d( lt_pers_ids_2_central_person[ 1 ]-sobid OPTIONAL ).

    IF iv_pernr NE lv_lowest_pernr.
      "Central Person: Please maintain lowest personnel number (&1) instead!
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        EXPORTING
          textid   = zcx_ca_wf_om_employee=>lowest_pernr
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( |{ lv_lowest_pernr ALPHA = OUT }| ).
    ENDIF.

    result = zcl_ca_wf_om_employee=>get_instance( is_lpor = VALUE #( instid = CONV #( iv_pernr )
                                                                     typeid = CONV #( iv_user_cls_name )
                                                                     catid  = swfco_objtype_cl )
                                                  iv_valid_on = iv_valid_on ).
  ENDMETHOD.                    "get_instance_by_lowest_pernr

ENDCLASS.
