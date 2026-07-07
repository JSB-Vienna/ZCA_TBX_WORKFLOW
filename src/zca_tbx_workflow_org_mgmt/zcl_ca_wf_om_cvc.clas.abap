"! <p class="shorttext synchronized" lang="en">WF-OM: Constants and value checks for org. management</p>
CLASS zcl_ca_wf_om_cvc DEFINITION PUBLIC
                                  FINAL
                                  CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Evaluation path</p>
      BEGIN OF evaluation_path,
        job_assignms_2_person   TYPE wegid VALUE 'P-S-C-O' ##no_text,
        job_assignms_2_position TYPE wegid VALUE 'ORG_STEL' ##no_text,
        job_assignms_2_org_unit TYPE wegid VALUE 'ORG_STEL' ##no_text,
        manager_2_org_unit      TYPE wegid VALUE 'BOSSONLY' ##no_text,
        orgunit_2_org_unit      TYPE wegid VALUE 'O-O' ##no_text,
        orgunit_2_ou_below      TYPE wegid VALUE 'O-O_DOWN' ##no_text,
        person_2_org_unit       TYPE wegid VALUE 'P-S-O' ##no_text,
        position_2_person       TYPE wegid VALUE 'S_NACH_P' ##no_text,
        staff_2_org_unit        TYPE wegid VALUE 'SBES' ##no_text,
        tasks_2_position        TYPE wegid VALUE 'S_TASKS' ##no_text,
      END OF evaluation_path,

      "! <p class="shorttext synchronized" lang="en">Scope of determination</p>
      BEGIN OF scope,
        manager      TYPE zca_wf_e_scope VALUE '1' ##no_text,
        members_only TYPE zca_wf_e_scope VALUE '2' ##no_text,
        all          TYPE zca_wf_e_scope VALUE '3' ##no_text,
      END OF scope,

      "! <p class="shorttext synchronized" lang="en">Assigned job</p>
      BEGIN OF job_as,
        "! <p class="shorttext synchronized" lang="en">No job assigned / Keine Stelle zugeordnet</p>
        not_assigned             TYPE zca_wf_e_job_id VALUE '00000000' ##no_text,
        "! <p class="shorttext synchronized" lang="en">District manager / Leiter der BZL</p>
        district_manager         TYPE zca_wf_e_job_id VALUE '00000233' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Group board member / Vorstandsmitglied</p>
        group_board_member       TYPE zca_wf_e_job_id VALUE '10000025' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Management board member / Mitglied der Geschäftsführung</p>
        managem_board_member     TYPE zca_wf_e_job_id VALUE '10000026' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Lead enterprise function / Leiter Konzernfunktion</p>
        lead_enterprise_function TYPE zca_wf_e_job_id VALUE '10000027' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Department lead / Abteilungsleiter</p>
        department_lead          TYPE zca_wf_e_job_id VALUE '10000028' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Team lead / Teamleiter</p>
        team_lead                TYPE zca_wf_e_job_id VALUE '10000029' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Group manager / Gruppenleiter</p>
        group_manager            TYPE zca_wf_e_job_id VALUE '10000030' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Shift coordination / Schichtkoordination</p>
        shift_coordination       TYPE zca_wf_e_job_id VALUE '10000031' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Shift supervisor / Schichtführung</p>
        shift_supervisor         TYPE zca_wf_e_job_id VALUE '10000032' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Regional lead / Leitung der Region</p>
        regional_lead            TYPE zca_wf_e_job_id VALUE '10000033' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Lead project responsibles / Leitung Projektverantwortliche</p>
        project_responsible_lead TYPE zca_wf_e_job_id VALUE '10000034' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Lead technicians / Leitung Techniker*innen</p>
        technicians_lead         TYPE zca_wf_e_job_id VALUE '10000035' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Scheduler / Disponent*in</p>
        scheduler                TYPE zca_wf_e_job_id VALUE '10000036' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Project responsible / Projektverantwortliche*r</p>
        project_responsible      TYPE zca_wf_e_job_id VALUE '10000037' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Commercial assistance / Kaufmännische Assistenz</p>
        commercial_assistance    TYPE zca_wf_e_job_id VALUE '10000038' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Vacant technician / Vakante*r Techniker*in (NN)</p>
        vacant_technician        TYPE zca_wf_e_job_id VALUE '10000039' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Vacant trainee / Vakante*r Lehrling/Auszubildende*r (NN)</p>
        vacant_trainee           TYPE zca_wf_e_job_id VALUE '10000040' ##no_text,
        "! <p class="shorttext synchronized" lang="en">EVN Employee / EVN-Mitarbeiter</p>
        employee                 TYPE zca_wf_e_job_id VALUE '10000041' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Support / staff unit leader / Stabstellenleiter</p>
        staff_unit_lead          TYPE zca_wf_e_job_id VALUE '10000050' ##no_text,
        "! <p class="shorttext synchronized" lang="en">CR Service center lead / CR Servicecenter Leitung</p>
        service_center_lead      TYPE zca_wf_e_job_id VALUE '10000051' ##no_text,
      END OF job_as,

      "! <p class="shorttext synchronized" lang="en">Assigned task</p>
      BEGIN OF task,
        "! <p class="shorttext synchronized" lang="en">Approving supererogations / Mehrleistungen genehmigen</p>
        approving_supererogations TYPE zca_wf_e_task_id VALUE '00000001' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Approving travel expense</p>
        approving_travel_expenses TYPE zca_wf_e_task_id VALUE '00000002' ##no_text,
      END OF task.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_om_cvc.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Is the passed job Id valid?</p>
      "!
      "! @parameter iv_job_id              | <p class="shorttext synchronized" lang="en">Job Id => use const ZCL_CA_WF_OM_CVC=>JOB_AS-*</p>
      "! @parameter iv_valid_on            | <p class="shorttext synchronized" lang="en">Object is valid on</p>
      "! @parameter eo_om_obj_job          | <p class="shorttext synchronized" lang="en">Org. object instance if additionally needed</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">X = Job Id is valid</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_job_id_valid
        IMPORTING
          iv_job_id     TYPE zca_wf_e_job_id
          iv_valid_on   TYPE hr_date
        EXPORTING
          eo_om_obj_job TYPE REF TO zif_ca_wf_om_org_model
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Is the passed scope valid?</p>
      "!
      "! @parameter iv_scope               | <p class="shorttext synchronized" lang="en">Scope</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">X = Scope is valid</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_scope_valid
        IMPORTING
          iv_scope      TYPE zca_wf_e_scope
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model,

      "! <p class="shorttext synchronized" lang="en">Is the passed task Id valid?</p>
      "!
      "! @parameter iv_task_id             | <p class="shorttext synchronized" lang="en">Task Id => use const ZCL_CA_WF_OM_CVC=>TASK_AS-*</p>
      "! @parameter iv_valid_on            | <p class="shorttext synchronized" lang="en">Object is valid on</p>
      "! @parameter eo_om_obj_task         | <p class="shorttext synchronized" lang="en">Org. object instance if additionally needed</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">X = Task Id is valid</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_task_id_valid
        IMPORTING
          iv_task_id     TYPE zca_wf_e_task_id
          iv_valid_on    TYPE hr_date
        EXPORTING
          eo_om_obj_task TYPE REF TO zif_ca_wf_om_org_model
        RETURNING
          VALUE(result)  TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      mo_instance       TYPE REF TO zcl_ca_wf_om_cvc.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Check value against fixed_values</p>
      "!
      "! @parameter value                  | <p class="shorttext synchronized" lang="en">Value under test</p>
      "! @parameter param_name             | <p class="shorttext synchronized" lang="en">Name of field/parameter for output in error message</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      check_against_fixed_values
        IMPORTING
          value      TYPE simple
          param_name TYPE csequence
        RAISING
          zcx_ca_wf_om_org_model.

ENDCLASS.



CLASS zcl_ca_wf_om_cvc IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_wf_om_cvc=>mo_instance IS NOT BOUND.
      zcl_ca_wf_om_cvc=>mo_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_wf_om_cvc=>mo_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_job_id_valid.
    "-----------------------------------------------------------------*
    "   Is the passed job Id valid?
    "-----------------------------------------------------------------*
    TRY.
        eo_om_obj_job = zcl_ca_wf_om_org_model=>get_instance( is_key = VALUE #( otype = swfco_org_job
                                                                                objid = iv_job_id ) ).
        result = abap_true.

      CATCH zcx_ca_dbacc.
        result = abap_false.

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                               iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                               ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.

    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_JOB_AS'
          mv_msgv2 = CONV #( iv_job_id ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_job_id_valid


  METHOD is_scope_valid.
    "-----------------------------------------------------------------*
    "   Is the passed scope valid?
    "-----------------------------------------------------------------*
    result = xsdbool( iv_scope BETWEEN scope-manager AND scope-all ).

    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_SCOPE'
          mv_msgv2 = CONV #( iv_scope ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_scope_valid


  METHOD is_task_id_valid.
    "-----------------------------------------------------------------*
    "   Is the passed task Id valid?
    "-----------------------------------------------------------------*
    TRY.
        "It checks only the existence. The result is not needed here.
        eo_om_obj_task = zcl_ca_wf_om_org_model=>get_instance( is_key = VALUE #( otype = swfco_org_task
                                                                                 objid = iv_task_id ) ).
        result = abap_true.

      CATCH zcx_ca_dbacc.
        result = abap_false.

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( zcx_ca_error=>create_exception(
                                               iv_excp_cls = zcx_ca_wf_om_org_model=>c_zcx_ca_wf_om_org_model
                                               ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.

    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_TASK_AS'
          mv_msgv2 = CONV #( iv_task_id ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_task_id_valid


  METHOD check_against_fixed_values.
    "-----------------------------------------------------------------*
    "   Check value against fixed_values
    "-----------------------------------------------------------------*
    TRY.
        NEW zcl_ca_ddic( iv_data       = value
                         iv_param_name = param_name )->check_fixed_values( iv_value       = value
                                                                           iv_raise_excep = abap_true ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_om_org_model( lx_catched ).
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.                    "check_against_fixed_values

ENDCLASS.
