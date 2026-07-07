"! <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule</p>
CLASS zcl_ca_wf_om_search_rule DEFINITION PUBLIC
                                          CREATE PUBLIC
                                          FINAL.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "!
    "! @parameter iv_search_rule_id | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule Id</p>
    "! @parameter iv_search_level   | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule level</p>
    "! @parameter iv_valid_on       | <p class="shorttext synchronized" lang="en">Validity date</p>
    METHODS constructor
      IMPORTING
        iv_search_rule_id TYPE zca_wf_e_search_rule_id
        iv_search_level   TYPE zca_wf_e_search_rule_level
        iv_valid_on       TYPE hr_date DEFAULT sy-datlo.

    "! <p class="shorttext synchronized" lang="en">Is employee an approver according to the job assignment?</p>
    "!
    "! @parameter io_employee              | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee in Org. management</p>
    "! @parameter rv_is_assigned           | <p class="shorttext synchronized" lang="en">X = Is assigned as approver</p>
    "! @raising   zcx_ca_wf_om_search_rule | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule exception</p>
    METHODS is_assigned_as_approver
      IMPORTING
        io_employee           TYPE REF TO zif_ca_wf_om_employee
      RETURNING
        VALUE(rv_is_assigned) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_search_rule.

    "! <p class="shorttext synchronized" lang="en">Is employee allowed to approve her-/himself?</p>
    "!
    "! @parameter io_employee              | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee in Org. management</p>
    "! @parameter rv_can_approve           | <p class="shorttext synchronized" lang="en">X = Can approve her-/himself</p>
    "! @raising   zcx_ca_wf_om_search_rule | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule exception</p>
    METHODS is_self_approving
      IMPORTING
        io_employee           TYPE REF TO zif_ca_wf_om_employee
      RETURNING
        VALUE(rv_can_approve) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_search_rule.

    "! <p class="shorttext synchronized" lang="en">Has employee a task assigned for approval delegation?</p>
    "!
    "! @parameter io_employee              | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee in Org. management</p>
    "! @parameter rv_has_task              | <p class="shorttext synchronized" lang="en">X = Has task for approval delegation</p>
    "! @raising   zcx_ca_wf_om_search_rule | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule exception</p>
    METHODS has_task_4_delegation
      IMPORTING
        io_employee        TYPE REF TO zif_ca_wf_om_employee
      RETURNING
        VALUE(rv_has_task) TYPE abap_boolean
      RAISING
        zcx_ca_wf_om_search_rule.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Valid delegation tasks to rule Id and level</p>
      ty_t_tasks TYPE SORTED TABLE OF zca_wf_e_task_id WITH NON-UNIQUE KEY table_line.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Initial OM object Id for comparison in selections</p>
      cv_initial_object_id TYPE hrobjid VALUE IS INITIAL.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Constants and value checks for select option tables</p>
      mo_cvc_so              TYPE REF TO zcl_ca_c_sel_options,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Valid approving jobs</p>
      mra_defined_approver   TYPE zca_wf_t_om_range_4_job,
      "! <p class="shorttext synchronized" lang="en">Valid self-approving jobs</p>
      mra_self_approver      TYPE zca_wf_t_om_range_4_job,
      "! <p class="shorttext synchronized" lang="en">Valid delegation tasks</p>
      mt_delegation_tasks    TYPE ty_t_tasks,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule Id</p>
      mv_search_rule_id      TYPE zca_wf_e_search_rule_id,
      "! <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule level</p>
      mv_search_level        TYPE zca_wf_e_search_rule_level,
      "! <p class="shorttext synchronized" lang="en">Validity date</p>
      mv_valid_on            TYPE hr_date,
      "! <p class="shorttext synchronized" lang="en">X = Self-approving jobs already selected</p>
      mv_self_apprv_jobs_sel TYPE abap_boolean VALUE abap_false,
      "! <p class="shorttext synchronized" lang="en">X = Delegating tasks already selected</p>
      mv_deleg_tasks_sel     TYPE abap_boolean VALUE abap_false.

*   i n s t a n c e   m e t h o d s
    "! <p class="shorttext synchronized" lang="en">Get valid approving jobs to rule Id and level</p>
    "!
    "! @raising   zcx_ca_wf_om_search_rule | <p class="shorttext synchronized" lang="en">CA-WF: OM Search rule exception</p>
    METHODS get_approving_jobs
      RAISING
        zcx_ca_wf_om_search_rule.

    "! <p class="shorttext synchronized" lang="en">Get valid self-approving jobs to rule Id and level</p>
    METHODS get_self_approving_jobs.

    "! <p class="shorttext synchronized" lang="en">Get valid delegation tasks to rule Id and level</p>
    METHODS get_delegation_tasks.

ENDCLASS.                     "zcl_ca_wf_om_search_rule  DEFINITION


CLASS zcl_ca_wf_om_search_rule IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    mv_search_rule_id = iv_search_rule_id.
    mv_search_level   = iv_search_level.
    mv_valid_on       = iv_valid_on.

    mo_cvc_so = zcl_ca_c_sel_options=>get_instance( ).
  ENDMETHOD.                    "constructor


  METHOD get_approving_jobs.
    "-----------------------------------------------------------------*
    "   Get valid approving jobs to rule Id and level
    "-----------------------------------------------------------------*
    IF mra_defined_approver IS NOT INITIAL.
      RETURN.
    ENDIF.

    SELECT FROM zcawf_om_srlassm AS lam
                INNER JOIN hrp1000 AS mas
                        ON mas~objid EQ lam~approving_job AND
                           mas~otype EQ @swfco_org_job
                INNER JOIN zcawf_om_srjob AS job
                        ON job~job_id EQ lam~approving_job
         FIELDS lam~approving_job AS low,  @mo_cvc_so->sign-incl AS sign,  @mo_cvc_so->option-eq AS option
          WHERE lam~srule_id EQ @mv_search_rule_id
            AND lam~sr_level EQ @mv_search_level
            AND lam~begda    LE @mv_valid_on
            AND lam~endda    GE @mv_valid_on
            AND mas~begda    LE @mv_valid_on
            AND mas~endda    GE @mv_valid_on
            AND job~begda    LE @mv_valid_on
            AND job~endda    GE @mv_valid_on
           INTO CORRESPONDING FIELDS OF TABLE @mra_defined_approver.

    "They must all be found otherwise some decisions are not possible
    get_self_approving_jobs( ).
    APPEND LINES OF mra_self_approver TO mra_defined_approver.
    SORT mra_defined_approver BY low.
    DELETE ADJACENT DUPLICATES FROM mra_defined_approver.

    IF mra_defined_approver IS INITIAL.
      "No valid &1 found to rule &2 and level &3 valid on &4
      RAISE EXCEPTION NEW zcx_ca_wf_om_search_rule( textid   = zcx_ca_wf_om_search_rule=>no_valid_jobs_or_tasks_found
                                                    mv_msgty = zcx_ca_wf_om_search_rule=>c_msgty_e
                                                    mv_msgv1 = 'approver jobs'(apj)
                                                    mv_msgv2 = CONV #( mv_search_rule_id )
                                                    mv_msgv3 = CONV #( mv_search_level )
                                                    mv_msgv4 = CONV #( |{ mv_valid_on DATE = ENVIRONMENT }| ) ).
    ENDIF.
  ENDMETHOD.                    "get_approving_jobs


  METHOD get_self_approving_jobs.
    "-----------------------------------------------------------------*
    "   Get valid self-approving jobs to rule Id and level
    "-----------------------------------------------------------------*
    IF mv_self_apprv_jobs_sel EQ abap_true.
      RETURN.
    ENDIF.

    SELECT FROM zcawf_om_srlassm AS lam
                INNER JOIN hrp1000 AS mas
                        ON mas~objid EQ lam~self_approving_job AND
                           mas~otype EQ @swfco_org_job
                INNER JOIN zcawf_om_srjob AS job
                        ON job~job_id EQ lam~self_approving_job
         FIELDS lam~self_approving_job AS low,  @mo_cvc_so->sign-incl AS sign,  @mo_cvc_so->option-eq AS option
          WHERE lam~srule_id EQ @mv_search_rule_id
            AND lam~sr_level EQ @mv_search_level
            AND lam~begda    LE @mv_valid_on
            AND lam~endda    GE @mv_valid_on
            AND lam~self_approving_job NE @cv_initial_object_id
            AND mas~begda    LE @mv_valid_on
            AND mas~endda    GE @mv_valid_on
            AND job~begda    LE @mv_valid_on
            AND job~endda    GE @mv_valid_on
           INTO CORRESPONDING FIELDS OF TABLE @mra_self_approver.

    "No result is an allowed result -> foreign key definition 1 : CN
    mv_self_apprv_jobs_sel = abap_true.
  ENDMETHOD.                    "get_self_approving_jobs


  METHOD get_delegation_tasks.
    "-----------------------------------------------------------------*
    "   Get valid delegation tasks to rule Id and level
    "-----------------------------------------------------------------*
    IF mv_deleg_tasks_sel EQ abap_true.
      RETURN.
    ENDIF.

    SELECT FROM zcawf_om_srlassm AS lam
                INNER JOIN hrp1000 AS mas
                        ON mas~objid EQ lam~task_2_delegate AND
                           mas~otype EQ @swfco_org_task
                INNER JOIN zcawf_om_srtask AS tsk
                        ON tsk~task_id EQ lam~task_2_delegate
         FIELDS lam~task_2_delegate
          WHERE lam~srule_id EQ @mv_search_rule_id
            AND lam~sr_level EQ @mv_search_level
            AND lam~begda    LE @mv_valid_on
            AND lam~endda    GE @mv_valid_on
            AND lam~task_2_delegate NE @cv_initial_object_id
            AND mas~begda    LE @mv_valid_on
            AND mas~endda    GE @mv_valid_on
            AND tsk~begda    LE @mv_valid_on
            AND tsk~endda    GE @mv_valid_on
           INTO TABLE @mt_delegation_tasks.

    "No result is an allowed result -> foreign key definition 1 : CN
    mv_deleg_tasks_sel = abap_true.
  ENDMETHOD.                    "get_delegation_tasks


  METHOD is_assigned_as_approver.
    "-----------------------------------------------------------------*
    "   Is employee an approver according to the job assignment?
    "-----------------------------------------------------------------*
    get_approving_jobs( ).
    rv_is_assigned = xsdbool( io_employee->ms_data-s_om_obj_key_c-objid IN mra_defined_approver ).
  ENDMETHOD.                    "is_assigned_as_approver


  METHOD is_self_approving.
    "-----------------------------------------------------------------*
    "   Is employee allowed to approve her-/himself?
    "-----------------------------------------------------------------*
    get_self_approving_jobs( ).
    rv_can_approve = xsdbool( mra_self_approver IS NOT INITIAL AND
                              io_employee->ms_data-s_om_obj_key_c-objid IN mra_self_approver ).
  ENDMETHOD.                    "is_self_approving


  METHOD has_task_4_delegation.
    "-----------------------------------------------------------------*
    "   Has employee a task assigned for approval delegation?
    "-----------------------------------------------------------------*
    get_delegation_tasks( ).

    rv_has_task = abap_false.
    LOOP AT mt_delegation_tasks INTO DATA(lv_delegating_task).
      rv_has_task = io_employee->is_responsible_for( lv_delegating_task ).
      IF rv_has_task EQ abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "has_task_4_delegation

ENDCLASS.                     "zcl_ca_wf_om_search_rule  IMPLEMENTATION


