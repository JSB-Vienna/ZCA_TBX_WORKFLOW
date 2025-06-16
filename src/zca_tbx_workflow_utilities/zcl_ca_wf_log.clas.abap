"! <p class="shorttext synchronized" lang="en">CA-TBX: Workflow specific Business Application Logging (BAL)</p>
CLASS zcl_ca_wf_log DEFINITION PUBLIC
                               INHERITING FROM zcl_ca_log
                               CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      zif_ca_wf_log.

*   a l i a s e s
    ALIASES:
      add_n_save_message            FOR  zif_ca_wf_log~add_n_save_message,
      add_n_save_exception          FOR  zif_ca_wf_log~add_n_save_exception,
      add_n_save_msg_task_cancelled FOR  zif_ca_wf_log~add_n_save_msg_task_cancelled,
      add_n_save_msg_task_completed FOR  zif_ca_wf_log~add_n_save_msg_task_completed,
      add_n_save_msg_wf_cancelled   FOR  zif_ca_wf_log~add_n_save_msg_wf_cancelled.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Adjust profile for workflow application logs</p>
      adjust_profile_for_wf_apps
        IMPORTING
          iv_title       TYPE string OPTIONAL
          iv_use_grid    TYPE abap_bool DEFAULT abap_true
          iv_disp_srcpos TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(es_prof) TYPE bal_s_prof,

      "! <p class="shorttext synchronized" lang="en">Get instance for the active work item id</p>
      "!
      "! <p><strong>This method creates also a logging instance if no work items were found!!</strong></p>
      "!
      "! @parameter iv_object       | <p class="shorttext synchronized" lang="en">Application Log: Object Name (Application Code)</p>
      "! @parameter iv_subobj       | <p class="shorttext synchronized" lang="en">Application Log: Subobject</p>
      "! @parameter is_lpor         | <p class="shorttext synchronized" lang="en">Business object/class key - relevant for buffering</p>
      "! @parameter iv_wf_task_id   | <p class="shorttext synchronized" lang="en">Workflow definition Id (WS...) as optional filter</p>
      "! @parameter iv_log_cls_name | <p class="shorttext synchronized" lang="en">Name of (inherited) application log class</p>
      "! @parameter ro_log          | <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      get_instance_for_active_wi
        IMPORTING
          iv_object       TYPE balobj_d
          iv_subobj       TYPE balsubobj
          is_lpor         TYPE sibflporb
          iv_wf_task_id   TYPE sww_task OPTIONAL
          iv_log_cls_name TYPE seoclsname DEFAULT 'ZCL_CA_WF_LOG' ##no_text
        RETURNING
          VALUE(ro_log)   TYPE REF TO zif_ca_wf_log.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_object        | <p class="shorttext synchronized" lang="en">Application Log: Object Name (Application Code)</p>
      "! @parameter iv_subobj        | <p class="shorttext synchronized" lang="en">Application Log: Subobject</p>
      "! @parameter is_lpor          | <p class="shorttext synchronized" lang="en">Business object/class key - relevant for buffering</p>
      "! @parameter iv_add_key       | <p class="shorttext synchronized" lang="en">Additional key (for several logs to one business object)</p>
      "! @parameter iv_extnumber     | <p class="shorttext synchronized" lang="en">Application Log: External ID</p>
      "! @parameter iv_repid         | <p class="shorttext synchronized" lang="en">ABAP Program: Current Main Program</p>
      "! @parameter iv_tcode         | <p class="shorttext synchronized" lang="en">ABAP System Field: Current Transaction Code</p>
      "! @parameter iv_def_msgid     | <p class="shorttext synchronized" lang="en">Default message class</p>
      "! @parameter iv_def_probclass | <p class="shorttext synchronized" lang="en">Default problem class (use const. C_PROBCLAS_*)</p>
      "! @parameter iv_mode          | <p class="shorttext synchronized" lang="en">Application Log: Operating mode  (use const. C_MODE_*)</p>
      "! @parameter iv_del_before    | <p class="shorttext synchronized" lang="en">Application Log: Keep log until expiry</p>
      "! @parameter iv_del_date      | <p class="shorttext synchronized" lang="en">Application Log: Expiration Date</p>
      constructor
        IMPORTING
          iv_object        TYPE balobj_d
          iv_subobj        TYPE balsubobj
          is_lpor          TYPE sibflporb
          iv_add_key       TYPE zca_d_log_add_key OPTIONAL
          iv_extnumber     TYPE balnrext   OPTIONAL
          VALUE(iv_repid)  TYPE syrepid    DEFAULT sy-cprog
          VALUE(iv_tcode)  TYPE syst_tcode DEFAULT sy-tcode
          iv_def_msgid     TYPE symsgid    OPTIONAL
          iv_def_probclass TYPE balprobcl  DEFAULT zcl_ca_c_log=>problem_class-very_important
          iv_mode          TYPE balmode    DEFAULT 'D'
          iv_del_before    TYPE abap_bool  DEFAULT abap_false
          iv_del_date      TYPE aldate_del OPTIONAL,

      zif_ca_wf_log~display REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Save message with specific settings for workflow</p>
      save_for_workflow.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">My own type</p>
      c_my_typeid          TYPE seoclsname        VALUE 'ZCL_CA_WF_LOG' ##no_text.

ENDCLASS.



CLASS zcl_ca_wf_log IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor(
                EXPORTING
                  iv_object        = iv_object
                  iv_subobj        = iv_subobj
                  is_lpor          = is_lpor
                  iv_add_key       = iv_add_key
                  iv_extnumber     = iv_extnumber
                  iv_repid         = iv_repid
                  iv_tcode         = iv_tcode
                  iv_def_msgid     = iv_def_msgid
                  iv_def_probclass = iv_def_probclass
                  iv_mode          = iv_mode
                  iv_del_before    = iv_del_before
                  iv_del_date      = iv_del_date ).
  ENDMETHOD.                    "constructor


  METHOD adjust_profile_for_wf_apps.
    "-----------------------------------------------------------------*
    "   Adjust profile for workflow application logs
    "-----------------------------------------------------------------*
    "Get display profile for single log default
    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = es_prof.

    es_prof-show_all = abap_false.
    es_prof-use_grid = iv_use_grid.

    "Open all messages
    es_prof-show_all = abap_true.

    "Column optimization doesn't work correctly, therefore set column width explicit
    es_prof-cwidth_opt = abap_true.

    "Hide and redefine  t r e e   c o l u m n s  (upper pane)
    "Set columns for object-, sub-object description and Log number to invisible
    MODIFY es_prof-lev1_fcat: FROM VALUE #( no_out = abap_true )
                              TRANSPORTING no_out
                              WHERE ref_field EQ 'T_OBJECT'
                                 OR ref_field EQ 'T_SUBOBJ'
                                 OR ref_field EQ 'LOGNUMBER' ##no_text,

    "Replace header of column 'External number' with 'Work item description' and set new width
                              FROM VALUE #( outputlen = '90'
                                            coltext   = 'Work item short description'(wds) )
                              TRANSPORTING outputlen  coltext
                              WHERE ref_field EQ 'EXTNUMBER' ##no_text,

    "Replace header of column 'Transaction' with 'Work item Id' and set new width
                              FROM VALUE #( outputlen = '20'
                                            coltext   = 'Work item Id'(wid) )
                              TRANSPORTING outputlen  coltext
                              WHERE ref_field EQ 'ALTCODE' ##no_text,

                              FROM VALUE #( outputlen = '20' )
                              TRANSPORTING outputlen
                              WHERE ref_field EQ 'T_ALMODE' ##no_text,

    "Replace header of column 'Program' with 'Task Id'
                              FROM VALUE #( outputlen = '20'
                                            coltext   = 'Task Id'(tid) )
                              TRANSPORTING outputlen  coltext
                              WHERE ref_field EQ 'ALPROG' ##no_text.

    "Adapt  columns in  m e s s a g e   l i n e s  (lower pane)
    "Change column width for columns 'Message', c_fname_class, 'Method' and 'Line No.'
    MODIFY es_prof-mess_fcat: FROM VALUE #( outputlen = '100' )
                              TRANSPORTING outputlen
                              WHERE ref_field EQ 'T_MSG' ##no_text,

                              FROM VALUE #( outputlen = '40'
                                            hotspot   = abap_true )
                              TRANSPORTING outputlen  hotspot
                              WHERE ref_field EQ c_fname_class,

                              FROM VALUE #( outputlen = '60'
                                            hotspot   = abap_true )
                              TRANSPORTING outputlen  hotspot
                              WHERE ref_field EQ c_fname_method,

                              FROM VALUE #( outputlen = '8'
                                            hotspot   = abap_true )
                              TRANSPORTING outputlen  hotspot
                              WHERE ref_field EQ c_fname_line.

    "Source code position is defined as HOTSPOT. Set call back routine
    es_prof-clbk_ucom = VALUE #( userexitf = c_fm_name_cb_ucom
                                 userexitt = 'F' ) ##no_text.  "= Function module

    "Display source code position only if the user has any development authorities
    DATA(lv_disp_srcpos) = iv_disp_srcpos.
    IF zcl_ca_log=>intern_has_user_developm_auth( ) EQ abap_false.
      lv_disp_srcpos = abap_false.
    ENDIF.

    IF lv_disp_srcpos EQ abap_true.        "display error positions
      "Enhance field catalog with columns for the source code position
      APPEND: VALUE #( ref_table  = c_fname_s_excep_srcpos
                       ref_field  = c_fname_class
                       col_pos    = 2
                       colddictxt = abap_true
                       hotspot    = abap_true
                       outputlen  = 10
                       is_extern  = abap_true ) TO es_prof-mess_fcat ##no_text,

              VALUE #( ref_table  = c_fname_s_excep_srcpos
                       ref_field  = c_fname_method
                       colddictxt = abap_true
                       hotspot    = abap_true
                       outputlen  = 10
                       col_pos    = 3
                       is_extern  = abap_true ) TO es_prof-mess_fcat ##no_text,

              VALUE #( ref_table  = c_fname_s_excep_srcpos
                       ref_field  = c_fname_line
                       colddictxt = abap_true
                       hotspot    = abap_true
                       outputlen  = 10
                       col_pos    = 4
                       is_extern  = abap_true ) TO es_prof-mess_fcat ##no_text.

      "define callback routine to read external data
      es_prof-clbk_read = VALUE #( userexitt = 'F' "function
                                   userexitf = c_fm_name_cb_read ) ##no_text.
    ENDIF.

    "Change sorting to date / time descending
    LOOP AT es_prof-lev1_sort ASSIGNING FIELD-SYMBOL(<ls_sort>)
                              WHERE ref_field EQ 'ALDATE'
                                 OR ref_field EQ 'ALTIME' ##no_text.
      <ls_sort>-up   = abap_false.
      <ls_sort>-down = abap_true.
    ENDLOOP.
    IF sy-subrc NE 0.
      APPEND: VALUE #( ref_table = 'BAL_S_SHOW'
                       ref_field = 'ALDATE'
                       spos      = '01'
                       down      = abap_true ) TO es_prof-mess_sort ##no_text,

              VALUE #( ref_table = 'BAL_S_SHOW'
                       ref_field = 'ALTIME'
                       spos      = '02'
                       down      = abap_true ) TO es_prof-mess_sort ##no_text.
    ENDIF.

    "Delete sorting on problem class
    CLEAR es_prof-lev2_sort.
  ENDMETHOD.                    "adjust_profile_wf


  METHOD get_instance_for_active_wi.
    "-----------------------------------------------------------------*
    "   Get log instance for active work item
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_workitem_id       TYPE sww_wiid.

    IF ro_log IS NOT SUPPLIED.
      "Parameter &1 is not supplied / has no receiving field, but is needed
      RAISE EXCEPTION TYPE zcx_ca_log
        EXPORTING
          textid   = zcx_ca_log=>param_not_supplied
          mv_msgv1 = 'RO_LOG' ##no_text.
    ENDIF.

    IF is_lpor-instid IS INITIAL.
      RETURN.
    ENDIF.

    IF is_lpor-typeid NE zif_ca_c_wf_bos=>cbo_workitem-typeid.
      "Are there any active work items to the requested object
      DATA(lt_active_wis_to_object) =
                zcl_ca_wf_wapi_utils=>get_workitems_to_object( is_lpor             = is_lpor
                                                               iv_only_top_lvl_out = abap_false ).
      IF lt_active_wis_to_object IS NOT INITIAL.
        "Get full workitem headers to have filter column TOP_TASK available
        SELECT * FROM  swwwihead
                       FOR ALL ENTRIES IN @lt_active_wis_to_object
                 WHERE wi_id EQ @lt_active_wis_to_object-wi_id
                 INTO  TABLE @DATA(lt_workitems).

        "Are there any enqueued in the current process
        DATA(lt_workitem_ids) = cl_swf_run_transaction_manager=>get_instance( )->get_enqueued_workitems( ).

        LOOP AT lt_workitems REFERENCE INTO DATA(lr_workitem).
          IF iv_wf_task_id         IS NOT INITIAL AND       "is a filter provided
             lr_workitem->top_task NE iv_wf_task_id.
            CONTINUE.
          ENDIF.

          "Is the work item currently enqueued?
          lv_workitem_id = VALUE #( lt_workitem_ids[ table_line = lr_workitem->wi_id ] OPTIONAL ).

          IF lv_workitem_id IS NOT INITIAL.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF lv_workitem_id IS INITIAL.
          lv_workitem_id = lr_workitem->wi_id.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA(lv_mode) = zcl_ca_c_log=>operating_mode-dialog.
    IF lr_workitem IS BOUND AND
       lr_workitem->wi_type EQ swfco_wi_batch.
      lv_mode = zcl_ca_c_log=>operating_mode-batch.
    ENDIF.

    ro_log ?= zcl_ca_log=>get_instance( iv_object       = iv_object
                                        iv_subobj       = iv_subobj
                                        is_lpor         = is_lpor
                                        iv_add_key      = CONV #( lv_workitem_id )
                                        iv_mode         = lv_mode
                                        iv_del_before   = abap_true
                                        iv_del_date     = CONV #( sy-datlo + 182 )
                                        iv_log_cls_name = iv_log_cls_name ) ##no_text.
  ENDMETHOD.                    "get_instance_for_active_wi


  METHOD save_for_workflow.
    "-----------------------------------------------------------------*
    "   Save message with specific settings for workflow
    "-----------------------------------------------------------------*
    save( iv_close       = abap_false
          iv_commit      = abap_false
   "May check transaction state with CL_SYSTEM_TRANSACTION_STATE=>GET_IN_UPDATE_TASK or
   "CL_SYSTEM_TRANSACTION_STATE=>GET_ON_END_OF_TRANSACTION whether the IN_UPD_TASK should be set or not.
   " ! ! !  The in this method called FM BAL_DB_SAVE use already the first method, but in case of a RAP-/Fiori
   "call it seems that it is not recognized and dumps.
   "The big question is now, how can we know if a RAP-/Fiori application is in a UPDATE TASK or not. And is there
   "a difference between managed and unmanaged applications.
   "For the reason to use this class in workflow and Fiori applications this flag is set to FALSE.
          iv_in_upd_task = abap_false ).
  ENDMETHOD.                    "save_for_workflow


  METHOD zif_ca_wf_log~add_n_save_exception.
    "-----------------------------------------------------------------*
    "   Add and save exception without closing log
    "-----------------------------------------------------------------*
    IF ix_catched IS NOT BOUND.
      RETURN.
    ENDIF.

    add_msg_exc( ix_excep = ix_catched
                 iv_all   = abap_true ).

    save_for_workflow( ).

    IF rx_error IS SUPPLIED.
      rx_error = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                     iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                     iv_class    = iv_class
                                                     iv_method   = iv_method
                                                     ix_error    = ix_catched ) ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "zif_ca_wf_log~add_n_save_exception


  METHOD zif_ca_wf_log~add_n_save_message.
    "-----------------------------------------------------------------*
    "   Add and save exception without closing log
    "-----------------------------------------------------------------*
    IF is_message IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_message) = is_message.

    IF ls_message-msgty IS INITIAL.
      ls_message-msgty = c_msgty_i.
    ENDIF.

    IF ls_message-msgid IS INITIAL.
      ls_message-msgid = '38'.
    ENDIF.

    IF ls_message-msgno IS INITIAL.
      ls_message-msgno = '001'.
    ENDIF.

    add_msg( iv_msgty = ls_message-msgty
             iv_msgid = ls_message-msgid
             iv_msgno = ls_message-msgno
             iv_msgv1 = ls_message-msgv1
             iv_msgv2 = ls_message-msgv2
             iv_msgv3 = ls_message-msgv3
             iv_msgv4 = ls_message-msgv4 ).

    save_for_workflow( ).

    MESSAGE ID     ls_message-msgid
            TYPE   c_msgty_s
            NUMBER ls_message-msgno
            WITH   ls_message-msgv1  ls_message-msgv2
                   ls_message-msgv3  ls_message-msgv4 DISPLAY LIKE ls_message-msgty.
  ENDMETHOD.                    "zif_ca_wf_log~add_n_save_exception


  METHOD zif_ca_wf_log~add_n_save_msg_task_cancelled.
    "-----------------------------------------------------------------*
    "   Log message for task cancellation by user
    "-----------------------------------------------------------------*
    add_msg( iv_msgty = c_msgty_s
             iv_msgid = 'SY'
             iv_msgno = '556' ) ##no_text.

    save_for_workflow( ).

    "Action canceled by user
    MESSAGE s556(sy).
  ENDMETHOD.                    "zif_ca_wf_log~add_n_save_msg_task_cancelled


  METHOD zif_ca_wf_log~add_n_save_msg_task_completed.
    "-----------------------------------------------------------------*
    "   Log message for successful task completion
    "-----------------------------------------------------------------*
    add_msg( iv_msgty = c_msgty_s
             iv_msgid = 'SODQ'
             iv_msgno = '513'
             iv_msgv1 = iv_task_descr ) ##no_text.

    save_for_workflow( ).

    "Task &1 completed successfully
    MESSAGE s513(sodq) WITH iv_task_descr.
  ENDMETHOD.                    "zif_ca_wf_log~add_n_save_msg_task_completed


  METHOD zif_ca_wf_log~add_n_save_msg_wf_cancelled.
    "-----------------------------------------------------------------*
    "   Log message that workflow was cancelled due to user action
    "-----------------------------------------------------------------*
    DATA(lv_object_key_for_output) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( is_lpor ).

    add_msg( iv_msgty = c_msgty_w
             iv_msgid = 'ZCA_WF_BASE'
             iv_msgno = '014'
             iv_msgv1 = lv_object_key_for_output
             iv_msgv2 = is_lpor-typeid
             iv_msgv3 = sy-uname ) ##no_text.

    save_for_workflow( ).

    "Workflow to object &1 (&2) was cancelled by user &3
    MESSAGE s014(zca_wf_base) WITH lv_object_key_for_output  is_lpor-typeid  sy-uname
                              DISPLAY LIKE c_msgty_i.
  ENDMETHOD.                    "zif_ca_wf_log~add_n_save_msg_wf_cancelled


  METHOD zif_ca_wf_log~display.
    "-----------------------------------------------------------------*
    "   Display current messages
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_profile           TYPE bal_s_prof.

    IF is_profile IS NOT INITIAL.
      "use imported profile
      ls_profile = is_profile.
    ELSE.
      "get display profile
      ls_profile = zcl_ca_wf_log=>adjust_profile_for_wf_apps( iv_title       = CONV #( iv_title )
                                                              iv_use_grid    = iv_use_grid
                                                              iv_disp_srcpos = iv_disp_srcpos ).
    ENDIF.

    "Display logs
    zcl_ca_log=>display_for_reference_obj( io_parent      = io_parent
                                           iv_title       = iv_title
                                           is_lpor        = ms_lpor
                                           iv_add_key     = mv_add_key
                                           iv_popup       = abap_false
                                           iv_opt_cwidth  = abap_true
                                           iv_disp_srcpos = abap_false
                                           is_profile     = ls_profile ).
  ENDMETHOD.                    "zif_ca_wf_log~display

ENDCLASS.
