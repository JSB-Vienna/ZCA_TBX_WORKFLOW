"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow: Wrapped WAPI function modules</p>
CLASS zcl_ca_wf_wapi_utils DEFINITION PUBLIC
                                      CREATE PUBLIC.

  PUBLIC SECTION.
*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Determine WAPI error message and raise exception</p>
      "!
      "! @parameter it_msg_stru | <p class="shorttext synchronized" lang="en">Returned messages of WAPI call</p>
      "! @parameter iv_function | <p class="shorttext synchronized" lang="en">Name of WAPI FM</p>
      check_sap_wapi_result
        IMPORTING
          it_msg_stru TYPE swr_msgtab
          iv_function TYPE rs38l_fnam,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM WORKITEM_COMPLETE (saves also container)</p>
      "!
      "! @parameter iv_wi_id     | <p class="shorttext synchronized" lang="en">Work item Id</p>
      "! @parameter iv_act_agent | <p class="shorttext synchronized" lang="en">Actual agent (user id)</p>
      "! @parameter iv_langu     | <p class="shorttext synchronized" lang="en">Name of WAPI FM</p>
      "! @parameter iv_do_commit | <p class="shorttext synchronized" lang="en">X = Do commit</p>
      "! @parameter io_wi_cnt    | <p class="shorttext synchronized" lang="en">Instance of WI container</p>
      "! @parameter iv_xml_cnt   | <p class="shorttext synchronized" lang="en">WI container as XML stream (like IFS_XML_CONTAINER)</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Status after completion</p>
      complete_workitem
        IMPORTING
          iv_wi_id      TYPE sww_wiid
          iv_act_agent  TYPE syuname   DEFAULT sy-uname
          iv_langu      TYPE sylangu   DEFAULT sy-langu
          iv_do_commit  TYPE abap_bool DEFAULT abap_true
          io_wi_cnt     TYPE REF TO if_swf_cnt_container OPTIONAL
          iv_xml_cnt    TYPE xstring   OPTIONAL
        RETURNING
          VALUE(result) TYPE sww_wistat,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM CREATE_EVENT_EXTENDED (for BOR + classes!)</p>
      "!
      "! @parameter is_lpor        | <p class="shorttext synchronized" lang="en">Object instance</p>
      "! @parameter iv_event       | <p class="shorttext synchronized" lang="en">Event name</p>
      "! @parameter iv_evt_langu   | <p class="shorttext synchronized" lang="en">Event language</p>
      "! @parameter iv_evt_creator | <p class="shorttext synchronized" lang="en">Event creator</p>
      "! @parameter iv_do_commit   | <p class="shorttext synchronized" lang="en">X = Do commit</p>
      "! @parameter io_evt_cnt     | <p class="shorttext synchronized" lang="en">Instance of event container</p>
      "! @parameter iv_xml_cnt     | <p class="shorttext synchronized" lang="en">WI container as XML stream (like IFS_XML_CONTAINER)</p>
      create_event_extended
        IMPORTING
          is_lpor        TYPE sibflporb
          iv_event       TYPE sibfevent
          iv_evt_langu   TYPE syst_langu DEFAULT sy-langu
          iv_evt_creator TYPE syst_uname DEFAULT sy-uname
          iv_do_commit   TYPE abap_bool  DEFAULT abap_true
          io_evt_cnt     TYPE REF TO if_swf_cnt_container OPTIONAL
          iv_xml_cnt     TYPE xstring OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM SAP_WAPI_WORKITEM_RECIPIENTS</p>
      "!
      "! <p>The FM SAP_WAPI_WORKITEM_RECIPIENTS is not able to return the active substitutes, which is why this
      "! method does not call the WAPI but the essential method of where the respective flag can be provided.</p>
      "!
      "! @parameter iv_wi_id              | <p class="shorttext synchronized" lang="en">Work item Id</p>
      "! @parameter iv_include_substitute | <p class="shorttext synchronized" lang="en">X = Return active substitutes of actual recipients</p>
      "! @parameter iv_do_commit          | <p class="shorttext synchronized" lang="en">X = Do commit</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Result with found workitems</p>
      get_recipients_to_workitem
        IMPORTING
          iv_wi_id              TYPE sww_wiid
          iv_include_substitute TYPE abap_bool  DEFAULT abap_false
          iv_do_commit          TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(result)         TYPE tswhactor,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM SAP_WAPI_CREATE_WORKLIST (= Workplace items)</p>
      "!
      "! @parameter iv_user                 | <p class="shorttext synchronized" lang="en">SAP user id</p>
      "! @parameter it_task_filter          | <p class="shorttext synchronized" lang="en">Filter for task Ids</p>
      "! @parameter it_object_filter        | <p class="shorttext synchronized" lang="en">Filter for object keys</p>
      "! @parameter it_status_filter        | <p class="shorttext synchronized" lang="en">Filter for work item status</p>
      "! @parameter it_wi_creation_filter   | <p class="shorttext synchronized" lang="en">Filter for creation time as time stamp</p>
      "! @parameter iv_read_task_text       | <p class="shorttext synchronized" lang="en">X = Read task text</p>
      "! @parameter iv_langu                | <p class="shorttext synchronized" lang="en">Language for reading texts</p>
      "! @parameter iv_translate_wi_text    | <p class="shorttext synchronized" lang="en">X = Translate work item text</p>
      "! @parameter it_wi_text_filter       | <p class="shorttext synchronized" lang="en">Work item text filter</p>
      "! @parameter iv_passive_substitution | <p class="shorttext synchronized" lang="en">X = Return also work items of passive substitutions</p>
      "! @parameter iv_time_zone            | <p class="shorttext synchronized" lang="en">Time zone of the current user</p>
      "! @parameter iv_do_commit            | <p class="shorttext synchronized" lang="en">X = Do commit</p>
      "! @parameter result                  | <p class="shorttext synchronized" lang="en">Result with found workitems</p>
      create_worklist_to_user
        IMPORTING
          iv_user                 TYPE syuname      DEFAULT sy-uname
          it_task_filter          TYPE swrttask     OPTIONAL
          it_object_filter        TYPE sibflporbt   OPTIONAL
          it_status_filter        TYPE swrtstatus   OPTIONAL
          it_wi_creation_filter   TYPE swrtrcreatmp OPTIONAL
          iv_read_task_text       TYPE abap_bool    DEFAULT space
          iv_langu                TYPE syst_langu   DEFAULT sy-langu
          iv_translate_wi_text    TYPE abap_bool    DEFAULT space
          it_wi_text_filter       TYPE swrtrwitext  OPTIONAL
          iv_passive_substitution TYPE abap_bool    DEFAULT space
          iv_time_zone            TYPE systzonlo    DEFAULT sy-zonlo
          iv_do_commit            TYPE abap_bool    DEFAULT abap_true
        RETURNING
          VALUE(result)           TYPE swrtwihdr,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM SAP_WAPI_WORKITEMS_TO_OBJECT</p>
      "!
      "! @parameter is_lpor               | <p class="shorttext synchronized" lang="en">Object instance</p>
      "! @parameter iv_status_sel_variant | <p class="shorttext synchronized" lang="en">Sel. variant for WI status (use const CL_SWF_UTL_STATUS=>C*)</p>
      "! @parameter iv_top_lvl_only       | <p class="shorttext synchronized" lang="en">X = Select only top level items</p>
      "! @parameter iv_only_top_lvl_out   | <p class="shorttext synchronized" lang="en">X = Return only top level items</p>
      "! @parameter is_time_range         | <p class="shorttext synchronized" lang="en">Selection in this time range</p>
      "! @parameter iv_read_text          | <p class="shorttext synchronized" lang="en">X = Reading also text elements</p>
      "! @parameter iv_langu              | <p class="shorttext synchronized" lang="en">Language for reading texts</p>
      "! @parameter iv_use_auto_tasks     | <p class="shorttext synchronized" lang="en">X = Use automatically determined task list</p>
      "! @parameter iv_log_del_tasks      | <p class="shorttext synchronized" lang="en">X = Consider also (logically) deleted elements</p>
      "! @parameter it_task_filter        | <p class="shorttext synchronized" lang="en">Task filter</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">Result with found workitems</p>
      get_workitems_to_object
        IMPORTING
          is_lpor               TYPE sibflporb
          iv_status_sel_variant TYPE swr_stavar DEFAULT cl_swf_utl_status=>co_variant_active     "only active WF + WIs
          iv_top_lvl_only       TYPE abap_bool  DEFAULT abap_false
          iv_only_top_lvl_out   TYPE abap_bool  DEFAULT abap_false
          is_time_range         TYPE swr_timint OPTIONAL
          iv_read_text          TYPE abap_bool  DEFAULT abap_false
          iv_langu              TYPE syst_langu DEFAULT sy-langu
          iv_use_auto_tasks     TYPE abap_bool  DEFAULT abap_true
          iv_log_del_tasks      TYPE abap_bool  DEFAULT abap_false
          it_task_filter        TYPE swrttask   OPTIONAL
        RETURNING
          VALUE(result)         TYPE swrtwihdr,

      "! <p class="shorttext synchronized" lang="en">Wrapped WAPI FM READ_CONTAINER</p>
      "!
      "! @parameter iv_wi_id       | <p class="shorttext synchronized" lang="en">Work item Id</p>
      "! @parameter iv_langu       | <p class="shorttext synchronized" lang="en">Name of WAPI FM</p>
      "! @parameter iv_buff_access | <p class="shorttext synchronized" lang="en">X = Get buffered container</p>
      "! @parameter result         | <p class="shorttext synchronized" lang="en">Container instance</p>
      read_container
        IMPORTING
          iv_wi_id       TYPE sww_wiid
          iv_langu       TYPE syst_langu DEFAULT sy-langu
          iv_buff_access TYPE abap_bool  DEFAULT abap_true
        RETURNING
          VALUE(result)  TYPE REF TO if_swf_cnt_container,

      "! <p class="shorttext synchronized" lang="en">Start workflow</p>
      "!
      "! @parameter iv_wf_task_id          | <p class="shorttext synchronized" lang="en">Workflow definition Id</p>
      "! @parameter iv_initial_user        | <p class="shorttext synchronized" lang="en">Initiator</p>
      "! @parameter iv_wf_container        | <p class="shorttext synchronized" lang="en">WF container (use CL_SWF_IFS_CONVERSION_BASE for conversion)</p>
      "! @parameter it_wf_simple_container | <p class="shorttext synchronized" lang="en">WF simple container</p>
      "! @parameter it_agents              | <p class="shorttext synchronized" lang="en">Initial agents</p>
      "! @parameter iv_check_authority     | <p class="shorttext synchronized" lang="en">X = Check whether the user is authorized to start a workflow</p>
      "! @parameter iv_langu               | <p class="shorttext synchronized" lang="en">Language for text objects</p>
      "! @parameter iv_do_commit           | <p class="shorttext synchronized" lang="en">X = Do commit; ' ' = Commit is required by consumer</p>
      "! @parameter iv_start_asynchronous  | <p class="shorttext synchronized" lang="en">X = Start workflow asynchronous</p>
      "! @parameter iv_desired_start_date  | <p class="shorttext synchronized" lang="en">Start date</p>
      "! @parameter iv_desired_start_time  | <p class="shorttext synchronized" lang="en">Start time</p>
      "! @parameter iv_desired_start_zonlo | <p class="shorttext synchronized" lang="en">Time zone for start time</p>
      "! @parameter ev_wf_id               | <p class="shorttext synchronized" lang="en">Workflow instance ID</p>
      "! @parameter ev_new_status          | <p class="shorttext synchronized" lang="en">New status of started workflow</p>
      start_workflow
        IMPORTING
          iv_wf_task_id          TYPE sww_wftask
          iv_initial_user        TYPE syst_uname   DEFAULT sy-uname
          iv_wf_container        TYPE xstring      OPTIONAL
          it_wf_simple_container TYPE swrtcont     OPTIONAL
          it_agents              TYPE swrtagent    OPTIONAL
          iv_check_authority     TYPE abap_boolean DEFAULT abap_true
          iv_langu               TYPE syst_langu   DEFAULT sy-langu
          iv_do_commit           TYPE abap_boolean DEFAULT abap_true
          iv_start_asynchronous  TYPE abap_boolean DEFAULT abap_false
          iv_desired_start_date  TYPE swr_datein   OPTIONAL
          iv_desired_start_time  TYPE swr_timein   OPTIONAL
          iv_desired_start_zonlo TYPE syst_zonlo   DEFAULT sy-zonlo
        EXPORTING
          ev_wf_id               TYPE sww_wfid
          ev_new_status          TYPE swr_wistat.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_wf_wapi_utils IMPLEMENTATION.

  METHOD check_sap_wapi_result.
    "-----------------------------------------------------------------*
    "   Determine WAPI error message and raise exception
    "-----------------------------------------------------------------*
    LOOP AT it_msg_stru REFERENCE INTO DATA(lr_wf_msg)
                        WHERE msgty CA zcx_ca_error=>c_msgty_eax.
      DATA(lx_error) = CAST zcx_ca_workflow( zcx_ca_intern=>create_exception(
                                                         iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                         iv_function = iv_function
                                                         is_msg      = CORRESPONDING #( lr_wf_msg->* ) ) ).
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "check_sap_wapi_result


  METHOD complete_workitem.
    "-----------------------------------------------------------------*
    "   Complete a workitem and, if necessary, save changed container
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru TYPE swr_msgtab,
      lv_xml_cont TYPE xstring.

    IF iv_xml_cnt IS NOT INITIAL.
      lv_xml_cont = iv_xml_cnt.

    ELSEIF io_wi_cnt IS BOUND.
      cl_swf_ifs_conversion_base=>to_ifs_xml(
                                        EXPORTING
                                          source_container = CAST #( io_wi_cnt )
                                        IMPORTING
                                          ifs_xml_stream   = lv_xml_cont
                                          error_handle     = DATA(lx_catched) ).

      IF lx_catched IS BOUND.
        DATA(lx_error) = CAST zcx_ca_workflow( zcx_ca_intern=>create_exception(
                                                             iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                             iv_class    = 'CL_SWF_IFS_CONVERSION_BASE'
                                                             iv_method   = 'TO_IFS_XML'
                                                             ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'SAP_WAPI_WORKITEM_COMPLETE'
      EXPORTING
        workitem_id       = iv_wi_id
        actual_agent      = iv_act_agent
        language          = iv_langu
        do_commit         = iv_do_commit
        ifs_xml_container = lv_xml_cont
      IMPORTING
        new_status        = result
      TABLES
        message_struct    = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_WORKITEM_COMPLETE' ) ##no_text.
  ENDMETHOD.                    "complete_workitem


  METHOD create_event_extended.
    "-----------------------------------------------------------------*
    "   Wrapped WAPI FM CREATE_EVENT_EXTENDED (for BOR + classes!)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru TYPE swr_msgtab,
      lv_xml_cont TYPE xstring.

    IF iv_xml_cnt IS NOT INITIAL.
      lv_xml_cont = iv_xml_cnt.

    ELSEIF io_evt_cnt IS BOUND.
      cl_swf_ifs_conversion_base=>to_ifs_xml(
                                        EXPORTING
                                          source_container = CAST #( io_evt_cnt )
                                        IMPORTING
                                          ifs_xml_stream   = lv_xml_cont
                                          error_handle     = DATA(lx_catched) ).

      IF lx_catched IS BOUND.
        DATA(lx_error) = CAST zcx_ca_workflow( zcx_ca_intern=>create_exception(
                                                           iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                           iv_class    = 'CL_SWF_IFS_CONVERSION_BASE'
                                                           iv_method   = 'TO_IFS_XML'
                                                           ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.

    "The FM SAP_WAPI_START_WORKFLOW does a COMMIT in any case, but this is not allowed in any situation.
    "E. g. during an action in RAP or during COMMIT phase itself. This is why the FM is called in a
    "separated destination in case of IV_DO_COMMIT is FALSE.

*    DATA(destination_for_wf_event) = SWITCH rfcdest( iv_do_commit
*                                         WHEN abap_true  THEN space
*                                         WHEN abap_false THEN cl_swf_utl_rfc_services=>get_name( ) ).

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT_EXTENDED'
*      DESTINATION destination_for_wf_event
      EXPORTING
        catid             = is_lpor-catid
        typeid            = is_lpor-typeid
        instid            = is_lpor-instid
        event             = CONV string( iv_event )
        commit_work       = iv_do_commit
        event_language    = iv_evt_langu
        user              = iv_evt_creator
        ifs_xml_container = lv_xml_cont
      TABLES
        message_struct    = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_CREATE_EVENT_EXTENDED' ) ##no_text.
  ENDMETHOD.                    "create_event_extended


  METHOD create_worklist_to_user.
    "-----------------------------------------------------------------*
    "   Determine work items to user incl. his/her substitutions
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru          TYPE swr_msgtab.

    CALL FUNCTION 'SAP_WAPI_CREATE_WORKLIST'
      EXPORTING
        user                 = iv_user
        im_task_filter       = it_task_filter
        im_object_filter     = it_object_filter
        im_status_filter     = it_status_filter
        im_wicrea_filter     = it_wi_creation_filter
        read_task_text       = iv_read_task_text
        language             = iv_langu
        translate_wi_text    = iv_translate_wi_text
        im_witext_filter     = it_wi_text_filter
        passive_substitution = iv_passive_substitution
        time_zone            = iv_time_zone
        do_commit            = iv_do_commit
      TABLES
        worklist             = result
        message_struct       = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_CREATE_WORKLIST' ) ##no_text.
  ENDMETHOD.                    "create_worklist_to_user


  METHOD get_recipients_to_workitem.
    "-----------------------------------------------------------------*
    "   Find recipients to a specific workitem
    "-----------------------------------------------------------------*
    TRY.
        result = cl_swf_run_wim_factory=>find_by_wiid( im_wiid = iv_wi_id
                                           )->if_swf_run_wim~get_agents( im_include_substitute = iv_include_substitute
                                                                         im_do_commit          = iv_do_commit ).

      CATCH cx_swf_run_wim INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_intern( zcx_ca_intern=>create_exception(
                                                                     iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                                     iv_class    = 'IF_SWF_RUN_WIM_INTERNAL'
                                                                     iv_method   = 'IF_SWF_RUN_WIM~GET_AGENTS'
                                                                     ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_recipients_to_workitem


  METHOD get_workitems_to_object.
    "-----------------------------------------------------------------*
    "   Find workitem to given
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru          TYPE swr_msgtab.

    CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
      EXPORTING
        object_por               = is_lpor
        top_level_items          = iv_top_lvl_only
        selection_status_variant = iv_status_sel_variant
        time                     = is_time_range
        text                     = iv_read_text
        output_only_top_level    = iv_only_top_lvl_out
        language                 = iv_langu
        determine_task_filter    = iv_use_auto_tasks
        removed_objects          = iv_log_del_tasks
      TABLES
        task_filter              = it_task_filter
        worklist                 = result
        message_struct           = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_WORKITEMS_TO_OBJECT' ) ##no_text.
  ENDMETHOD.                    "get_workitems_to_object


  METHOD read_container.
    "-----------------------------------------------------------------*
    "   Determine WAPI error message and raise exception
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_catched  TYPE REF TO cx_swf_ifs_exception,
      lx_error    TYPE REF TO zcx_ca_workflow,
      lt_msg_stru TYPE swr_msgtab,
      lv_xml_cnt  TYPE xstring,
      lv_task_id  TYPE swd_step_t.

    CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
      EXPORTING
        workitem_id       = iv_wi_id
        language          = iv_langu
        buffered_access   = iv_buff_access
      IMPORTING
        ifs_xml_container = lv_xml_cnt
      TABLES
        message_struct    = lt_msg_stru.

    "Check result and raise exception
    check_sap_wapi_result( it_msg_stru = lt_msg_stru
                           iv_function = 'SAP_WAPI_READ_CONTAINER' ) ##no_text.

    "Create container instance using the task id
    SELECT SINGLE wi_rh_task INTO lv_task_id
                             FROM  swwwihead
                             WHERE wi_id EQ iv_wi_id.

    TRY.
        "Convert XML container into container instance
        cl_swf_cnt_factory=>create_task_container(
                                            EXPORTING
                                              im_task_id        = lv_task_id
                                            IMPORTING
                                              ex_task_container = result ).

      CATCH cx_swf_ifs_exception INTO lx_catched.
        lx_error ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                     iv_class    = 'CL_SWF_CNT_FACTORY'
                                                     iv_method   = 'CREATE_TASK_CONTAINER'
                                                     ix_error    = lx_catched )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.

    cl_swf_ifs_conversion_base=>import_from_ifs_xml(
                                                EXPORTING
                                                  ifs_xml_stream   = lv_xml_cnt
                                                  target_container = result
                                                IMPORTING
                                                  error_handle     = lx_catched ).
    IF lx_catched IS BOUND AND
       lx_catched->t100_msg-msgty CA zcx_ca_error=>c_msgty_eax.   "Warnings will also be returned
      lx_error ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                   iv_class    = 'CL_SWF_IFS_CONVERSION_BASE'
                                                   iv_method   = 'IMPORT_FROM_IFS_XML'
                                                   ix_error    = lx_catched )  ##no_text.
    ENDIF.
  ENDMETHOD.                    "read_container


  METHOD start_workflow.
    "-----------------------------------------------------------------*
    "   Start workflow
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_msg_stru          TYPE swr_msgtab.

    "The FM SAP_WAPI_START_WORKFLOW does a COMMIT in any case, but this is not allowed in any situation.
    "E. g. during an action in RAP or during COMMIT phase itself. This is why the FM is called in a
    "separated destination in case of IV_DO_COMMIT is FALSE.

    TRY.
        DATA(destination_for_wf_start) = cl_swf_utl_rfc_services=>get_name( ).

        CALL FUNCTION 'SAP_WAPI_START_WORKFLOW'
          DESTINATION destination_for_wf_start
          EXPORTING
            task                = iv_wf_task_id
            language            = iv_langu
            do_commit           = iv_do_commit
            check_authority     = iv_check_authority
            user                = iv_initial_user
            start_asynchronous  = iv_start_asynchronous
            desired_start_date  = iv_desired_start_date
            desired_start_time  = iv_desired_start_time
            desired_start_zonlo = iv_desired_start_zonlo
            ifs_xml_container   = iv_wf_container
          IMPORTING
            workitem_id         = ev_wf_id
            new_status          = ev_new_status
          TABLES
            input_container     = it_wf_simple_container[]
            message_struct      = lt_msg_stru
            agents              = it_agents[].

        "Check result and raise exception
        check_sap_wapi_result( it_msg_stru = lt_msg_stru
                               iv_function = 'SAP_WAPI_START_WORKFLOW' ) ##no_text.

      CATCH cx_swf_run_wim_syst_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_workflow( zcx_ca_intern=>create_exception(
                                                               iv_excp_cls = zcx_ca_workflow=>c_zcx_ca_workflow
                                                               iv_class    = 'CL_SWF_UTL_RFC_SERVICES'
                                                               iv_method   = 'GET_NAME'
                                                               ix_error    = lx_catched ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "start_workflow

ENDCLASS.
