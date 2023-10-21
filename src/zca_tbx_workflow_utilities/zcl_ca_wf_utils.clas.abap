"! <p class="shorttext synchronized" lang="en">CA-TBX: Workflow specific utilities / conversions</p>
CLASS zcl_ca_wf_utils DEFINITION PUBLIC
                                 FINAL
                                 CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Link action: Execute</p>
      c_action_execute TYPE string      VALUE `EXECUTE`  ##no_text,
      "! <p class="shorttext synchronized" lang="en">Link action: Display</p>
      c_action_display TYPE string      VALUE `DISPLAY`  ##no_text.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Assemble base URL for access to Fiori Launchpad</p>
      "!
      "! @parameter iv_icf_node_path | <p class="shorttext synchronized" lang="en">Use constants /UI2/IF_START_URL=&gt&CO_F*</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_base_url_launchpad
        IMPORTING
          iv_icf_node_path TYPE string DEFAULT /ui2/if_start_url=>co_flp
        RETURNING
          VALUE(result)    TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for Fiori inbox displaying all work items</p>
      "!
      "! @parameter iv_wi_id         | <p class="shorttext synchronized" lang="en">Workitem Id</p>
      "! @parameter iv_icf_node_path | <p class="shorttext synchronized" lang="en">Use constants /UI2/IF_START_URL=&gt&CO_F*</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_fiori_for_wi
        IMPORTING
          iv_wi_id         TYPE sww_wiid
          iv_icf_node_path TYPE string DEFAULT /ui2/if_start_url=>co_flp
        RETURNING
          VALUE(result)    TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for Fiori inbox displaying all work items</p>
      "!
      "! @parameter iv_icf_node_path | <p class="shorttext synchronized" lang="en">Use constants /UI2/IF_START_URL=&gt&CO_F*</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_fiori_inbox_all
        IMPORTING
          iv_icf_node_path TYPE string DEFAULT /ui2/if_start_url=>co_flp
        RETURNING
          VALUE(result)    TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for Fiori My EVN inbox displaying work items of scenario ZMY_INBOX</p>
      "!
      "! @parameter iv_icf_node_path | <p class="shorttext synchronized" lang="en">Use constants /UI2/IF_START_URL=&gt&CO_F*</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">URL for branch into Fiori inbox</p>
      "! @raising   zcx_ca_wf_utils  | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_fiori_zmy_inbox
        IMPORTING
          iv_icf_node_path TYPE string DEFAULT /ui2/if_start_url=>co_flp
        RETURNING
          VALUE(result)    TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for transaction call in SAP GUI</p>
      "!
      "! <p>This method is an adapted copy of CL_SWN_URL_GENERATOR=>GET_WI_ACTION_URL_WINGUI, which executes
      "! a workitem. There are some wrapper transactions, like MMPURUICALLME29N to release purchase orders.
      "! Those programs behind the transactions can be copied and adapted for your own purpose.</p>
      "! <p>Then call this method to create a corresponding URL to call the transaction e. g. out of a mail.</p>
      "! <p>The following hint is copied from the original method mentioned above.</p>
      "! <p>Note 1556749:<br>
      "! If anything is changed in the generation of this URL the code in CL_SWN_HTTP_SHORTCUT must be
      "! reviewed and maybe adapted. For security reasons some restrictive checks were implemented in this
      "! class to validate and restrict the parameters which are used to generate the SAP Shortcut. These
      "! checks and therefore the SAP Shortcut generation may fail, when changing the code in this method.</p>
      "!
      "! @parameter iv_sicf_service_path | <p class="shorttext synchronized" lang="en">Path in SICF (of your service) as part of the URL</p>
      "! <p>E. g. /sap/public/bc/workflow/shortcut ==> pass this value <strong>WITHOUT a question mark</strong> at the end!!!!</p>
      "! @parameter iv_transaction_code  | <p class="shorttext synchronized" lang="en">Transaction code</p>
      "! @parameter it_parameters        | <p class="shorttext synchronized" lang="en">Parameter / Value pairs to provide transaction parameters</p>
      "! @parameter iv_okcode            | <p class="shorttext synchronized" lang="en">Function code to be executed; ONLI is report execution</p>
      "! @parameter iv_user              | <p class="shorttext synchronized" lang="en">User</p>
      "! @parameter iv_langu             | <p class="shorttext synchronized" lang="en">Language</p>
      "! @raising   zcx_ca_wf_utils      | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_gui_tcode_call
        IMPORTING
          iv_sicf_service_path TYPE string
          iv_transaction_code  TYPE syst_tcode
          it_parameters        TYPE swwtproper     OPTIONAL
          iv_okcode            TYPE syst_ucomm     DEFAULT 'ONLI'
          iv_user              TYPE syst_uname     DEFAULT sy-uname
          iv_langu             TYPE syst_langu     DEFAULT sy-langu
        RETURNING
          VALUE(result)        TYPE swn_attachurl
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Assemble URL for WEBGUI Transaction</p>
      "!
      "! @parameter iv_tcode         | <p class="shorttext synchronized" lang="en">Transaction code</p>
      "! @parameter iv_icf_node_path | <p class="shorttext synchronized" lang="en">Use constants /UI2/IF_START_URL=&gt&CO_F*</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">URL for WebGUI </p>
      "! @raising zcx_ca_wf_utils    | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      assemble_url_webgui_tcode_call
        IMPORTING
          iv_tcode         TYPE syst_tcode
          iv_icf_node_path TYPE string DEFAULT /ui2/if_start_url=>co_flp
        RETURNING
          VALUE(result)    TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Check BAPI return parameter for errors</p>
      "!
      "! @parameter it_return | <p class="shorttext synchronized" lang="en">Table with messages after BAPI call</p>
      "! @parameter result    | <p class="shorttext synchronized" lang="en">Found message of type E, A or X, else it is empty</p>
      check_bapi_return_for_errors
        IMPORTING
          it_return     TYPE bapiret2_t
        RETURNING
          VALUE(result) TYPE bapi_msg,

      "! <p class="shorttext synchronized" lang="en">Convert WF actor into user Id (cut of org.type)</p>
      "!
      "! @parameter iv_actor | <p class="shorttext synchronized" lang="en">Workflow actor with object type US</p>
      "! @parameter result   | <p class="shorttext synchronized" lang="en">SAP user ID</p>
      convert_actor_2_user
        IMPORTING
          iv_actor      TYPE swp_agent
        RETURNING
          VALUE(result) TYPE xubname,

      "! <p class="shorttext synchronized" lang="en">Convert user Id into actor (add US)</p>
      "!
      "! @parameter iv_uname | <p class="shorttext synchronized" lang="en">SAP user ID</p>
      "! @parameter result   | <p class="shorttext synchronized" lang="en">Workflow actor with object type US</p>
      convert_user_2_actor
        IMPORTING
          iv_uname      TYPE xubname
        RETURNING
          VALUE(result) TYPE swp_agent,

      "! <p class="shorttext synchronized" lang="en">Create BOR Instance from key values</p>
      "!
      "! @parameter iv_type_id | <p class="shorttext synchronized" lang="en">Name of BOR object</p>
      "! @parameter iv_key_1   | <p class="shorttext synchronized" lang="en">Key value 1</p>
      "! @parameter iv_key_2   | <p class="shorttext synchronized" lang="en">Key value 2</p>
      "! @parameter iv_key_3   | <p class="shorttext synchronized" lang="en">Key value 3</p>
      "! @parameter iv_key_4   | <p class="shorttext synchronized" lang="en">Key value 4</p>
      "! @parameter iv_key_5   | <p class="shorttext synchronized" lang="en">Key value 5</p>
      "! @parameter iv_key_6   | <p class="shorttext synchronized" lang="en">Key value 6</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">BOR instance</p>
      create_bor_inst_from_key_vals
        IMPORTING
          iv_type_id    TYPE sibftypeid
          iv_key_1      TYPE sibfinstid
          iv_key_2      TYPE sibfinstid OPTIONAL
          iv_key_3      TYPE sibfinstid OPTIONAL
          iv_key_4      TYPE sibfinstid OPTIONAL
          iv_key_5      TYPE sibfinstid OPTIONAL
          iv_key_6      TYPE sibfinstid OPTIONAL
        RETURNING
          VALUE(result) TYPE sibflporb,

      "! <p class="shorttext synchronized" lang="en">Create a shortcut attachm. for TA or report call in SAP GUI</p>
      "!
      "! @parameter iv_title        | <p class="shorttext synchronized" lang="en">Name for attachment</p>
      "! @parameter iv_action       | <p class="shorttext synchronized" lang="en">Action type for TA SWNWIEX only (use const. C_ACTION_*)</p>
      "! @parameter iv_tcode        | <p class="shorttext synchronized" lang="en">Transaction code, may be with leading *</p>
      "! @parameter iv_report       | <p class="shorttext synchronized" lang="en">Report name</p>
      "! @parameter it_params       | <p class="shorttext synchronized" lang="en">Parameter / Value pairs to provide transaction call</p>
      "! @parameter iv_ucomm        | <p class="shorttext synchronized" lang="en">User command to start transaction (NOT for reports!)</p>
      "! @parameter iv_custom       | <p class="shorttext synchronized" lang="en">Customer addition for shortcut; attached at the end</p>
      "! @parameter iv_user_id      | <p class="shorttext synchronized" lang="en">User Id</p>
      "! @parameter iv_langu        | <p class="shorttext synchronized" lang="en">Language</p>
      "! @parameter iv_client       | <p class="shorttext synchronized" lang="en">Client</p>
      "! @parameter iv_window_size  | <p class="shorttext synchronized" lang="en">Window size (maximized or normal)</p>
      "! @parameter iv_logon_id     | <p class="shorttext synchronized" lang="en">For RFC login</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Shortcut as attachment for a BCS mail</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      create_shortcut_as_mail_attchm
        IMPORTING
          iv_title       TYPE text80         DEFAULT 'Excute workitem'(ewi)
          iv_action      TYPE string         DEFAULT c_action_execute
          iv_tcode       TYPE syst_tcode     DEFAULT '*SWNWIEX'  ##no_text   "Execute workitem
          iv_report      TYPE syrepid        OPTIONAL
          it_params      TYPE swwtproper     OPTIONAL
          iv_ucomm       TYPE syst_ucomm     OPTIONAL
          iv_custom      TYPE text255        OPTIONAL
          iv_user_id     TYPE syst_uname     DEFAULT sy-uname
          iv_langu       TYPE syst_langu     DEFAULT sy-langu
          iv_client      TYPE syst_mandt     DEFAULT sy-mandt
          iv_window_size TYPE char40         DEFAULT 'Normal window'    ##no_text
          iv_logon_id    TYPE char50         OPTIONAL
        RETURNING
          VALUE(result)  TYPE soli_tab
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Create BO SOFM with decision reason to provide events, etc.</p>
      create_sofm_with_decis_reason
        IMPORTING
          iv_decision_title  TYPE csequence OPTIONAL
          iv_decision_reason TYPE string
          iv_langu           TYPE syst_langu DEFAULT sy-langu
        RETURNING
          VALUE(result)      TYPE swotobjid
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Find active workitem (single task) to a workflow id</p>
      "!
      "! @parameter iv_wf_id        | <p class="shorttext synchronized" lang="en">Active WORKFLOWitem Id</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Active workitem Id</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      find_active_wi_by_wf_id
        IMPORTING
          iv_wf_id      TYPE sww_wfid
        RETURNING
          VALUE(result) TYPE sww_wiid
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Get currently active workitem and its container</p>
      "!
      "! @parameter iv_raise_excep  | <p class="shorttext synchronized" lang="en">1=Raise exception if no WI was found; 0=Check result values</p>
      "! @parameter ev_wi_id        | <p class="shorttext synchronized" lang="en">Active workitem Id</p>
      "! @parameter eo_wi_cnt       | <p class="shorttext synchronized" lang="en">Container to active workitem</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      get_active_wi_n_container
        IMPORTING
          iv_raise_excep TYPE abap_bool DEFAULT abap_true
        EXPORTING
          ev_wi_id       TYPE sww_wiid
          eo_wi_cnt      TYPE REF TO if_swf_cnt_container
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Get text of SOFM object (only 1st line)</p>
      "!
      "! @parameter it_attach_objs | <p class="shorttext synchronized" lang="en">Pass the workflow container element "_Attach_Objects"</p>
      "! @parameter result         | <p class="shorttext synchronized" lang="en">First line of SOFM content</p>
      get_decision_reason_from_sofm
        IMPORTING
          it_attach_objs TYPE sibflporbt
        RETURNING
          VALUE(result)  TYPE zca_d_reason_decision,

      "! <p class="shorttext synchronized" lang="en">Check if the workflow admin is active to refresh the agents</p>
      "!
      "! @parameter iv_workitem_id  | <p class="shorttext synchronized" lang="en">Active workitem Id</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">X = Workflow administration is active for this workitem</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      is_wf_admin_function_active
        IMPORTING
          iv_workitem_id TYPE sww_wiid
        RETURNING
          VALUE(result)  TYPE abap_boolean
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Prepare object key for output / display</p>
      "!
      "! @parameter is_lpor | <p class="shorttext synchronized" lang="en">Business class / object key</p>
      "! @parameter result  | <p class="shorttext synchronized" lang="en">For output prepared key values</p>
      prepare_object_key_for_ouput
        IMPORTING
          is_lpor       TYPE sibflporb
        RETURNING
          VALUE(result) TYPE zca_d_key_for_output,

      "! <p class="shorttext synchronized" lang="en">Wait some seonds for something, e. g for posting completed</p>
      "!
      "! @parameter iv_seconds | <p class="shorttext synchronized" lang="en">Number of seconds to wait</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">1 = Waiting time is over</p>
      wait_n_seconds
        IMPORTING
          iv_seconds    TYPE int2
        RETURNING
          VALUE(result) TYPE abap_bool.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Parameter to call specific workitem</p>
      c_link_fiori_spec_wi TYPE string            VALUE `&/detail/&1/&2/TaskCollection(SAP__Origin='&1',InstanceID='&2')` ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Assembled URL</p>
      mv_url               TYPE string.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Add further parameter to current URL</p>
      "!
      "! @parameter iv_name         | <p class="shorttext synchronized" lang="en">Parameter name</p>
      "! @parameter iv_value        | <p class="shorttext synchronized" lang="en">Unconverted parameter value</p>
      "! @parameter iv_curr_url     | <p class="shorttext synchronized" lang="en">Current URL, otherwise kept in class</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Completetd URL</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      add_param_to_fiori_url
        IMPORTING
          iv_name       TYPE string
          iv_value      TYPE data
          iv_curr_url   TYPE string OPTIONAL
        RETURNING
          VALUE(result) TYPE string
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Create type conform field for conversions</p>
      "!
      "! @parameter iv_ref_struct   | <p class="shorttext synchronized" lang="en">Reference structure name</p>
      "! @parameter iv_ref_field    | <p class="shorttext synchronized" lang="en">Reference field name</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">Type conform field reference</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      create_type_conform_field
        IMPORTING
          iv_ref_struct TYPE swc_refstr
          iv_ref_field  TYPE swc_reffld
        RETURNING
          VALUE(result) TYPE REF TO data
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Returns number of lines of a table</p>
      "!
      "! @parameter it_table | <p class="shorttext synchronized" lang="en">Description</p>
      get_num_of_lines
        IMPORTING
          it_table      TYPE ANY TABLE
        RETURNING
          VALUE(result) TYPE i,

      "! <p class="shorttext synchronized" lang="en">Prepare Business Object key for output</p>
      "!
      "! @parameter is_lpor         | <p class="shorttext synchronized" lang="en">Business Object key</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">For output prepared key values</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      prepare_bo_key_for_output
        IMPORTING
          is_lpor       TYPE sibflporb
        RETURNING
          VALUE(result) TYPE sibfboriid
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Prepare Business Class key for output</p>
      "!
      "! @parameter is_lpor         | <p class="shorttext synchronized" lang="en">Business Class key</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">For output prepared key values</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      prepare_cl_key_for_output
        IMPORTING
          is_lpor       TYPE sibflporb
        RETURNING
          VALUE(result) TYPE sibfboriid
        RAISING
          zcx_ca_wf_utils,

      "! <p class="shorttext synchronized" lang="en">Prepare object key from structured key field</p>
      "!
      "! @parameter iv_ref_struct   | <p class="shorttext synchronized" lang="en">Reference structure name</p>
      "! @parameter iv_key_string   | <p class="shorttext synchronized" lang="en">Business Class key as character string</p>
      "! @parameter result          | <p class="shorttext synchronized" lang="en">For output prepared key values</p>
      "! @raising   zcx_ca_wf_utils | <p class="shorttext synchronized" lang="en">Common WF exception: Service method error</p>
      prepare_key_from_structure
        IMPORTING
          iv_ref_struct TYPE swc_refstr
          iv_key_string TYPE sibfboriid
        RETURNING
          VALUE(result) TYPE sibfboriid
        RAISING
          zcx_ca_wf_utils.

ENDCLASS.



CLASS ZCL_CA_WF_UTILS IMPLEMENTATION.


  METHOD add_param_to_fiori_url.
    "-----------------------------------------------------------------*
    "   Add further parameter to current URL
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_url   TYPE string,
      lv_value TYPE char50,
      lv_param TYPE string.

    IF iv_curr_url IS NOT INITIAL.
      lv_url = iv_curr_url.
    ELSE.
      lv_url = mv_url.
    ENDIF.

    TRY.
        zcl_ca_conv=>internal_2_external(
                                    EXPORTING
                                      internal_value = iv_value
                                    IMPORTING
                                      external_value = lv_value ).

        lv_param = condense( iv_name && `=` && lv_value ) ##no_text.

        IF lv_url CA `?` ##no_text.
          lv_url = lv_url && `&` && lv_param ##no_text.
        ELSE.
          lv_url = lv_url && `?` && lv_param ##no_text.
        ENDIF.

        result = mv_url = lv_url.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                                 iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                                 iv_class    = 'ZCL_CA_CONV'
                                                                 iv_method   = 'INTERNAL_2_EXTERNAL'
                                                                 ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "add_param_to_fiori_url


  METHOD assemble_base_url_launchpad.
    "-----------------------------------------------------------------*
    "   Assemble base URL for access to Fiori Launchpad
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_param       TYPE string,
      lv_protocol    TYPE string,
      lv_host        TYPE string,
      lv_port        TYPE string,
      lv_langu       TYPE laiso,
      lv_access_mode TYPE c.

    CLEAR mv_url.
    cl_http_server=>get_location(
                            EXPORTING
                              application  = iv_icf_node_path
                            IMPORTING
                              host         = lv_host
                              port         = lv_port
                              out_protocol = lv_protocol ).
    IF lv_protocol IS INITIAL OR
       lv_host     IS INITIAL OR
       lv_port     IS INITIAL.
      "Server location could not be determined / assembled
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid   = zcx_ca_wf_utils=>server_loc_incompl
          mv_msgty = c_msgty_e
          mv_msgv1 = CONV #( iv_icf_node_path ).
    ENDIF.

    mv_url = |{ lv_protocol }://{ lv_host }:{ lv_port }{ iv_icf_node_path }|.
    TRANSLATE mv_url TO LOWER CASE.

    add_param_to_fiori_url( iv_name = `sap-client`
                            iv_value = sy-mandt ) ##no_text.

    add_param_to_fiori_url( iv_name  = `sap-language`
                            iv_value = sy-langu ) ##no_text.

    GET PARAMETER ID 'ACCESSIBILITY_MODE' FIELD lv_access_mode.
    IF lv_access_mode IS NOT INITIAL.
      add_param_to_fiori_url( iv_name  = `sap-accessibility`
                              iv_value = abap_true ) ##no_text.
    ENDIF.

    result = mv_url.
  ENDMETHOD.                    "assemble_base_url_launchpad


  METHOD assemble_url_fiori_for_wi.
    "-----------------------------------------------------------------*
    "   Assemble URL for Fiori inbox displaying all work items
    "
    "   Maintenance of the system aliases (set system id and client
    "   for at least software version /IWPGW/BWF) is in TA SPRO here:
    "   SAP NetWeaver -> UI Technologies -> SAP Fiori -> Initial Setup
    "   -> Connection Settings (Front...) -> Define SAP System Alias
    "-----------------------------------------------------------------*
    assemble_base_url_launchpad( iv_icf_node_path ).

    add_param_to_fiori_url( iv_name  = `#WorkflowTask-displayInbox?allItems`
                            iv_value = `true` ) ##no_text.

    "Determine
    DATA(lo_dest_finder) = /iwfnd/cl_destin_finder=>get_destination_finder( ).
    DATA(lv_client)      = cl_abap_syst=>get_client( ).
    LOOP AT lo_dest_finder->get_system_aliases_list( ) ASSIGNING FIELD-SYMBOL(<ls_alias>)
                                                       WHERE software_version EQ '/IWPGW/BWF'
                                                         AND target_sysid     EQ sy-sysid
                                                         AND target_client    EQ lv_client ##no_text.

      DATA(lv_param) = replace( val  = c_link_fiori_spec_wi
                                sub  = `&1`   with = <ls_alias>-system_alias
                                occ  = 0 ).  "= all occurrences
      lv_param = replace( val  = lv_param
                          sub  = `&2`   with = iv_wi_id
                          occ  = 0 ) ##no_text.  "= all occurrences
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      "No system alias found for service group '&1' SID '&2' client '&3'
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid   = zcx_ca_wf_utils=>no_alias_found
          mv_msgty = c_msgty_e
          mv_msgv1 = '/IWPGW/BWF'
          mv_msgv2 = CONV #( sy-sysid )
          mv_msgv3 = CONV #( lv_client ) ##no_text.
    ENDIF.

    result = mv_url = mv_url && lv_param.
  ENDMETHOD.                    "assemble_url_fiori_for_wi


  METHOD assemble_url_fiori_inbox_all.
    "-----------------------------------------------------------------*
    "   Assemble URL for Fiori inbox displaying all work items
    "-----------------------------------------------------------------*
    assemble_base_url_launchpad( iv_icf_node_path ).

    add_param_to_fiori_url( iv_name  = `#WorkflowTask-displayInbox?allItems`
                            iv_value = `true` ) ##no_text.
    result = mv_url.
  ENDMETHOD.                    "assemble_url_fiori_inbox_all


  METHOD assemble_url_gui_tcode_call.
    "-----------------------------------------------------------------*
    "   Assemble URL for transaction call in SAP GUI
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_parameters TYPE string,
      lv_langu_ext  TYPE char2.       "Language in external format

    "Is service path passed without a question mark
    IF iv_sicf_service_path CA '?'.
      RAISE EXCEPTION TYPE zcx_ca_wf_utils.
    ENDIF.

    "assemble the parameter string
    LOOP AT it_parameters INTO DATA(ls_param).
      IF lv_parameters IS INITIAL.
        lv_parameters = |{ ls_param-name }={ ls_param-value }|.
      ELSE.
        lv_parameters = |{ lv_parameters };{ ls_param-name }={ ls_param-value }|.
      ENDIF.
    ENDLOOP.

    lv_parameters = |{ lv_parameters };DYNP_OKCODE={ iv_okcode }| ##no_text.

    "make sure language is DE not D
    WRITE iv_langu TO lv_langu_ext.

    "escape critical strings
    lv_parameters = escape( val    = lv_parameters
                        format = cl_abap_format=>e_url_full ).

    "get the host part of the URL
    DATA(lv_url_host) = cl_swn_url_generator=>get_bsp_host( ).
    IF lv_url_host IS INITIAL.
      "Configuration incomplete to generate host URL
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid   = zcx_ca_wf_utils=>configuration_incomplete
          mv_msgty = c_msgty_e.
    ENDIF.

    "append parameters
    result = |{ lv_url_host }{ iv_sicf_service_path }?sysid={ sy-sysid }| ##no_text.              "this system

    result = |{ result }&client={ sy-mandt }|       &&   "our client here (note 941377)
             |&uname={ iv_user }|                   &&   "user
             |&langu={ lv_langu_ext }|              &&   "language
             |&transaction={ iv_transaction_code }| &&   "transaction code
             |&param={ lv_parameters }| ##no_text.       "transaction parameter
  ENDMETHOD.                    "assemble_url_gui_tcode_call


  METHOD assemble_url_webgui_tcode_call.
    "-----------------------------------------------------------------*
    "   Assemble link to call transaction via WebGUI
    "-----------------------------------------------------------------*
    assemble_base_url_launchpad( iv_icf_node_path ).

    mv_url = |{ mv_url }#StartTransaction-{ iv_tcode CASE = LOWER }?sap-ui-tech-hint=GUI| ##no_text.

    result = mv_url.
  ENDMETHOD.


  METHOD check_bapi_return_for_errors.
    "-----------------------------------------------------------------*
    "   Check BAPI return parameter for errors
    "-----------------------------------------------------------------*
    CLEAR result.
    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>)
                      WHERE type CA 'EAX' ##no_text.
      EXIT.
    ENDLOOP.
    IF sy-subrc EQ 0.
      result = <ls_return>-message.
    ENDIF.
  ENDMETHOD.                    "check_bapi_return_for_errors


  METHOD convert_actor_2_user.
    "-----------------------------------------------------------------*
    "   Convert WF actor into user Id (cut of org.type)
    "-----------------------------------------------------------------*
    IF iv_actor(2) EQ swfco_org_user.
      result = iv_actor+2.
    ENDIF.
  ENDMETHOD.                    "convert_actor_2_user


  METHOD convert_user_2_actor.
    "-----------------------------------------------------------------*
    "   Convert user Id into actor (add US)
    "-----------------------------------------------------------------*
    result = |{ swfco_org_user }{ iv_uname }|.
  ENDMETHOD.                    "convert_user_2_actor


  METHOD create_bor_inst_from_key_vals.
    "-----------------------------------------------------------------*
    "   Create BOR Instance from key values
    "-----------------------------------------------------------------*
    result = VALUE #( catid  = swfco_objtype_bor
                      typeid = iv_type_id
                      instid = |{ iv_key_1 }{ iv_key_2 }{ iv_key_3 }{ iv_key_4 }{ iv_key_5 }{ iv_key_6 }| ).
  ENDMETHOD.                    "create_bor_inst_from_key_vals


  METHOD create_shortcut_as_mail_attchm.
    "-----------------------------------------------------------------*
    "   Create a shortcut attachm. for TA or report call in SAP GUI
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_params TYPE text255,
      lv_custom TYPE text255.

    IF iv_tcode  IS INITIAL AND
       iv_report IS INITIAL.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid   = zcx_ca_wf_utils=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_TCODE'
          mv_msgv2 = 'IV_REPORT'
          mv_msgv3 = space
          mv_msgv4 = space ##no_text.

    ELSEIF iv_tcode EQ '*SWNWIEX' ##no_text.
      "Is called for a shortcut to execute a workitem

      "Is workitem Id provided
      LOOP AT it_params INTO DATA(ls_param_wi_id)
                        WHERE name EQ 'p_wi_id'
                           OR name EQ 'P_WI_ID' ##no_text.
        EXIT.
      ENDLOOP.
      IF sy-subrc NE 0.
        "Parameter '&1' is not specified
        RAISE EXCEPTION TYPE zcx_ca_wf_utils
          EXPORTING
            textid   = zcx_ca_wf_utils=>param_not_supplied
            mv_msgty = c_msgty_e
            mv_msgv1 = 'Tcode=SWNWIEX, but parameter P_WI_ID is missing'(e01).
      ENDIF.

      lv_params = |p_action={ iv_action }| ##no_text.
      lv_params = |{ lv_params };{ ls_param_wi_id-name }={ ls_param_wi_id-value }|.
      lv_params = |{ lv_params };p_appl=NOTIF;DYNP_OKCODE=ONLI| ##no_text.

      "Add custom tags for shortcut
      lv_custom = |[Workflow]{ cl_abap_char_utilities=>cr_lf }| &&
*                  |Email={ iv_email }{ cl_abap_char_utilities=>cr_lf }| &&
                  |WI_ID={ ls_param_wi_id-value }| ##no_text.

    ELSE.
      LOOP AT it_params INTO DATA(ls_param).
        IF lv_params IS INITIAL.
          lv_params = |{ ls_param-name }={ ls_param-value }|.
        ELSE.
          lv_params = |{ lv_params };{ ls_param-name }={ ls_param-value }|.
        ENDIF.
      ENDLOOP.

      IF iv_report IS NOT INITIAL.
        lv_params = |{ lv_params };DYNP_OKCODE=ONLI| ##no_text.    "= F8 = report execution

      ELSEIF iv_ucomm IS NOT INITIAL.
        lv_params = |{ lv_params };DYNP_OKCODE={ iv_ucomm }| ##no_text.
      ENDIF.
    ENDIF.

    IF iv_custom IS NOT INITIAL.
      lv_custom = iv_custom.
    ENDIF.

    CALL FUNCTION 'SWN_CREATE_SHORTCUT'
      EXPORTING
        i_title                 = iv_title
        i_transaction           = iv_tcode
        i_report                = iv_report
        i_parameter             = lv_params
        i_user                  = iv_user_id
        i_language              = iv_langu
        i_client                = iv_client
        i_windowsize            = iv_window_size
        i_saplogon_id           = iv_logon_id
        i_custom                = iv_custom
      IMPORTING
        shortcut_table          = result
*       shortcut_string         =     " Shortcut als String
      EXCEPTIONS
        inconsistent_parameters = 1
        OTHERS                  = 2.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                               iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                               iv_function = 'SWN_CREATE_SHORTCUT'
                                                               iv_subrc    = sy-subrc ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_shortcut_as_mail_attchm


  METHOD create_sofm_with_decis_reason.
    "-----------------------------------------------------------------*
    "   Create BO SOFM with decision reason to provide events, etc.
    "-----------------------------------------------------------------*
    CALL FUNCTION 'SWU_INTERN_CREATE_NOTE_OBJECT'
      EXPORTING
        im_title         = CONV so_obj_des( iv_decision_title )
        im_text          = iv_decision_reason
        im_language      = iv_langu
      IMPORTING
        ex_objid         = result
      EXCEPTIONS
        system_failure   = 1
        no_authorization = 2
        OTHERS           = 3.
    IF sy-subrc NE 0.
      DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                               iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                               iv_function = 'SWU_INTERN_CREATE_NOTE_OBJECT'
                                                               iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "create_sofm_with_reason


  METHOD create_type_conform_field.
    "-----------------------------------------------------------------*
    "   Create type conform field for conversions
    "-----------------------------------------------------------------*
    TRY.
        DATA(lv_ref_field_name) = COND #( WHEN iv_ref_struct IS NOT INITIAL
                                            THEN |{ iv_ref_struct }-{ iv_ref_field }|
                                            ELSE iv_ref_field ).

        DATA(lo_elem_descr) = CAST cl_abap_elemdescr( NEW zcl_ca_ddic( iv_name = lv_ref_field_name )->mo_type_desc ).

        CREATE DATA result TYPE HANDLE lo_elem_descr.

      CATCH zcx_ca_param INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                                 iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                                 iv_class    = 'ZCL_CA_DDIC'
                                                                 iv_method   = 'CONSTRUCTOR'
                                                                 ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_type_conform_field


  METHOD find_active_wi_by_wf_id.
    "-----------------------------------------------------------------*
    "   Find active work item (single task) to a workflow id
    "-----------------------------------------------------------------*
    DATA(lo_sel_options) = zcl_ca_c_sel_options=>get_instance( ).
    DATA(lo_dep_wis) = cl_swf_utl_get_dependant_wis=>get_instance( iv_wf_id ).

    lo_dep_wis->set_status_filter( VALUE #(   sign   = lo_sel_options->sign-excl
                                              option = lo_sel_options->option-eq
                                            ( low    = swfco_wi_status_waiting )
                                            ( low    = swfco_wi_status_ready )
                                            ( low    = swfco_wi_status_selected )
                                            ( low    = swfco_wi_status_started )
                                            ( low    = swfco_wi_status_committed )
                                            ( low    = swfco_wi_status_error ) ) ).

    lo_dep_wis->set_witype_filter( VALUE #(   sign   = lo_sel_options->sign-excl
                                              option = lo_sel_options->option-eq
                                            ( low    = swfco_wi_normal )
                                            ( low    = swfco_wi_batch ) ) ).

    DATA(lt_act_wis) = lo_dep_wis->get_workitems( ).
    CASE lines( lt_act_wis ).
      WHEN 0.
        "Found no active workflow task to workflow Id 1&
        RAISE EXCEPTION TYPE zcx_ca_wf_utils
          EXPORTING
            textid   = zcx_ca_wf_utils=>no_act_wis_found
            mv_msgv1 = CONV #( |{ iv_wf_id ALPHA = OUT }| ).

      WHEN 1.
        DATA(ls_act_wi) = lt_act_wis[ 1 ].

      WHEN OTHERS.
        "No unique result for workflow Id 1&
        RAISE EXCEPTION TYPE zcx_ca_wf_utils
          EXPORTING
            textid   = zcx_ca_wf_utils=>no_unique_result
            mv_msgv1 = CONV #( |{ iv_wf_id ALPHA = OUT }| ).
    ENDCASE.

    "Check status and raise exception if in status error
    IF ls_act_wi-wi_stat EQ swfco_wi_status_error.
      "Found active work item is in status 'ERROR'
      RAISE EXCEPTION TYPE zcx_ca_wf_utils
        EXPORTING
          textid = zcx_ca_wf_utils=>act_wi_in_stat_err.
    ENDIF.

    result = ls_act_wi-wi_id.
  ENDMETHOD.                    "find_active_wi_by_wf_id


  METHOD get_active_wi_n_container.
    "-----------------------------------------------------------------*
    "   Get currently active workitem and its container
    "-----------------------------------------------------------------*
    CLEAR: ev_wi_id,
           eo_wi_cnt.

    cl_swf_evt_requester=>get_workitem(
                                  IMPORTING
                                    ex_workitem_id = DATA(lv_wi_id) ).
    ev_wi_id = CONV #( lv_wi_id ).

    "The value is also initial, if it is called in a side methods of a workflow step!!
    IF ev_wi_id IS INITIAL.
      CASE iv_raise_excep.
        WHEN abap_false.
          RETURN.

        WHEN abap_true.
          "Currently no workitem is executed or is called by a side method
          RAISE EXCEPTION TYPE zcx_ca_wf_utils
            EXPORTING
              textid   = zcx_ca_wf_utils=>no_wi_found
              mv_msgty = 'I' ##no_text.
      ENDCASE.
    ENDIF.

    TRY.
        eo_wi_cnt = zcl_ca_wf_wapi_utils=>read_container( iv_wi_id = ev_wi_id ).

      CATCH zcx_ca_param
            cx_swf_ifs_exception INTO DATA(lx_error).
        IF iv_raise_excep EQ abap_true.
          DATA(lx_wf_utils) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                              iv_excp_cls   = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                              iv_class      = 'ZCL_CA_WF_WAPI_UTILS'
                                                              iv_method     = 'READ_CONTAINER'
                                                              ix_error      = lx_error ) )  ##no_text.
          IF lx_wf_utils IS BOUND.
            RAISE EXCEPTION lx_wf_utils.
          ENDIF.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "get_active_wi_n_container


  METHOD get_decision_reason_from_sofm.
    "-----------------------------------------------------------------*
    "   Get text of SOFM object, e. g. Note to decision
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_obj_content       TYPE soli_tab.

    LOOP AT it_attach_objs REFERENCE INTO DATA(lr_attach_obj)
                           WHERE typeid EQ 'SOFM' ##no_text.
      "Loop over all objects to find the last one of the right type - so NO exit!
    ENDLOOP.

    IF lr_attach_obj IS BOUND.
      DATA(ls_document) = CONV sood4( lr_attach_obj->instid ).

      CALL FUNCTION 'SO_DOCUMENT_REPOSITORY_MANAGER'
        EXPORTING
          method   = 'GETCONTENT'
        TABLES
          objcont  = lt_obj_content
        CHANGING
          document = ls_document ##no_text.

      result = VALUE #( lt_obj_content[ 1 ] OPTIONAL ).
    ENDIF.
  ENDMETHOD.                    "get_decision_reason_from_sofm


  METHOD get_num_of_lines.
    "-----------------------------------------------------------------*
    "   Returns number of lines of a table
    "-----------------------------------------------------------------*
    result = lines( it_table ).
  ENDMETHOD.                    "get_num_of_lines


  METHOD is_wf_admin_function_active.
    "-----------------------------------------------------------------*
    "   Check if the workflow administration is active for this workitem
    "-----------------------------------------------------------------*
    TRY.
        result = cl_swf_run_wim_factory=>find_by_wiid( im_wiid = iv_workitem_id )->get_context( )-adm_active.

      CATCH cx_swf_run_wim INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                                 iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                                 iv_class    = 'CL_SWF_RUN_WIM_FACTORY'
                                                                 iv_method   = 'FIND_BY_WIID'
                                                                 ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "is_wf_admin_function_active


  METHOD prepare_bo_key_for_output.
    "-----------------------------------------------------------------*
    "   Prepare Business Object key for output
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_name_for_excep TYPE seomtdkey,
      lv_key_val_output TYPE sibfinstid.

    FIELD-SYMBOLS:
      <lv_key_string> TYPE data,
      <lv_key_value>  TYPE data.

    TRY.
        DATA(lo_key_handler) = cl_swo_key_handling=>get_instance( ).
        ls_name_for_excep = VALUE #( clsname = 'CL_SWO_KEY_HANDLING'
                                     mtdname = 'GET_LOGICAL_KEYFIELDS' ) ##no_text.
        lo_key_handler->get_logical_keyfields(
                                        EXPORTING
                                          i_object_type = CONV #( is_lpor-typeid )
                                        IMPORTING
                                          e_fields      = DATA(lt_key_fields) ).

        SORT lt_key_fields BY editorder.
        ls_name_for_excep = VALUE #( clsname = 'ZCL_CA_CONV'
                                     mtdname = 'INTERNAL_2_EXTERNAL' ) ##no_text.
        LOOP AT lt_key_fields REFERENCE INTO DATA(lr_key_field).
          DATA(lr_key_value) = zcl_ca_wf_utils=>create_type_conform_field( iv_ref_struct = lr_key_field->refstruct
                                                                           iv_ref_field  = lr_key_field->reffield ).
          ASSIGN lr_key_value->* TO <lv_key_value>.
          ASSIGN is_lpor-instid+lr_key_field->offset(lr_key_field->ddlength) TO <lv_key_string>.
          <lv_key_value> = <lv_key_string>.

          zcl_ca_conv=>internal_2_external(
                                      EXPORTING
                                        internal_value = <lv_key_value>
                                      IMPORTING
                                        external_value = lv_key_val_output ).

          result = condense( |{ result } { lv_key_val_output }| ).
        ENDLOOP.

      CATCH cx_swo_runtime_error
            zcx_ca_conv INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                               iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                               iv_class    = ls_name_for_excep-clsname
                                                               iv_method   = ls_name_for_excep-mtdname
                                                               ix_error    = lx_catched ) ).
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "prepare_bo_key_for_output


  METHOD prepare_cl_key_for_output.
    "-----------------------------------------------------------------*
    "   Prepare Business Class key for output
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_name_for_excep TYPE seomtdkey,
      lv_key_val_output TYPE sibfinstid.

    FIELD-SYMBOLS:
      <lv_key_string> TYPE data,
      <lv_key_value>  TYPE data.

    TRY.
        ls_name_for_excep = VALUE #( clsname = 'CL_SWF_UTL_DEF_SERVICES'
                                     mtdname = 'GET_OBJECT_KEYFIELDS' ) ##no_text.
        cl_swf_utl_def_services=>get_object_keyfields(
                                                EXPORTING
                                                  im_clstype   = is_lpor-catid
                                                  im_clsname   = is_lpor-typeid
                                                IMPORTING
                                                  ex_container = DATA(lo_keyfld_container) ).

        IF lo_keyfld_container IS NOT BOUND.
          RAISE EXCEPTION TYPE zcx_ca_wf_utils.
        ENDIF.

        lo_keyfld_container->get_iterator(
                                    IMPORTING
                                      ex_iterator = DATA(lo_iterator)
                                      ex_element  = DATA(lo_cnt_element) ).

        DATA(lv_offset) = 0.
        WHILE lo_cnt_element IS BOUND.

          ls_name_for_excep = VALUE #( clsname = 'IF_SWF_CNT_ELEMENT'
                                       mtdname = 'GET_TYPE' ) ##no_text.
          lo_cnt_element->get_type(
                              IMPORTING
                                refstruct        = DATA(lv_ref_struct)
                                reffield         = DATA(lv_ref_field)
                                typekind         = DATA(lv_type_kind)
                                length           = DATA(lv_length)
                                exception_return = DATA(lv_exception) ).

          IF lv_exception IS BOUND.
            RAISE EXCEPTION lv_exception.
          ENDIF.

          IF lv_type_kind EQ swfex_typekind_flatstruct.
            result = zcl_ca_wf_utils=>prepare_key_from_structure( iv_ref_struct = lv_ref_struct
                                                                  iv_key_string = is_lpor-instid ).

          ELSEIF lv_type_kind CA swfex_typekind_character_f.   "any character type -> C, N, D + T
            DATA(lr_key_value) = zcl_ca_wf_utils=>create_type_conform_field( iv_ref_struct = lv_ref_struct
                                                                             iv_ref_field  = lv_ref_field ).
            ASSIGN lr_key_value->* TO <lv_key_value>.

            <lv_key_value> = is_lpor-instid+lv_offset(lv_length).
            lv_offset = lv_offset + lv_length.

            ls_name_for_excep = VALUE #( clsname = 'ZCL_CA_CONV'
                                         mtdname = 'INTERNAL_2_EXTERNAL' ) ##no_text.
            zcl_ca_conv=>internal_2_external(
                                        EXPORTING
                                          internal_value = <lv_key_value>
                                        IMPORTING
                                          external_value = lv_key_val_output ).

            result = condense( |{ result } { lv_key_val_output }| ).

          ELSE.
            RAISE EXCEPTION TYPE zcx_ca_wf_utils.
          ENDIF.

          lo_cnt_element = lo_iterator->get_next( ).
        ENDWHILE.

      CATCH zcx_ca_error
            cx_swf_cnt_element INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                                 iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                                 iv_class    = ls_name_for_excep-clsname
                                                                 iv_method   = ls_name_for_excep-mtdname
                                                                 ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "prepare_cl_key_for_output


  METHOD prepare_key_from_structure.
    "-----------------------------------------------------------------*
    "   Prepare object key from structured key field
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_s_key_value    TYPE REF TO data,
      lv_key_val_output TYPE sibfinstid.

    FIELD-SYMBOLS:
      <ls_key_value>     TYPE data,
      <lv_key_component> TYPE data.

    TRY.
        DATA(lo_struct_descr) = CAST cl_abap_structdescr( NEW zcl_ca_ddic( iv_name = iv_ref_struct )->mo_type_desc ).
        CREATE DATA lr_s_key_value TYPE HANDLE lo_struct_descr.
        ASSIGN lr_s_key_value->* TO <ls_key_value>.

        <ls_key_value> = CONV #( iv_key_string ).

        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <ls_key_value> TO <lv_key_component>.
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.

          zcl_ca_conv=>internal_2_external(
                                      EXPORTING
                                        internal_value = <lv_key_component>
                                      IMPORTING
                                        external_value = lv_key_val_output ).

          result = condense( |{ result } { lv_key_val_output }| ).
        ENDDO.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_wf_utils( zcx_ca_error=>create_exception(
                                                                 iv_excp_cls = zcx_ca_wf_utils=>c_zcx_ca_wf_utils
                                                                 iv_class    = 'ZCL_CA_CONV'
                                                                 iv_method   = 'INT_2_EXT'
                                                                 ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "prepare_key_from_structure


  METHOD prepare_object_key_for_ouput.
    "-----------------------------------------------------------------*
    "   Prepare object key for output / display
    "-----------------------------------------------------------------*
    TRY.
        CASE is_lpor-catid.
          WHEN swfco_objtype_bor.
            result = zcl_ca_wf_utils=>prepare_bo_key_for_output( is_lpor ).

          WHEN swfco_objtype_cl.
            result = zcl_ca_wf_utils=>prepare_cl_key_for_output( is_lpor ).

          WHEN OTHERS.
            RAISE EXCEPTION TYPE zcx_ca_wf_utils.
        ENDCASE.

      CATCH cx_root INTO DATA(lx_catched).
        result = is_lpor-instid.
    ENDTRY.
  ENDMETHOD.                    "prepare_object_for_ouput


  METHOD wait_n_seconds.
    "-----------------------------------------------------------------*
    "   Wait some seconds for something, e. g for posting completed
    "-----------------------------------------------------------------*
    WAIT UP TO iv_seconds SECONDS.
    result = abap_true.
  ENDMETHOD.                    "wait_n_seconds


  METHOD assemble_url_fiori_zmy_inbox.
    "-----------------------------------------------------------------*
    "   Assemble URL for Fiori inbox displaying all work items
    "-----------------------------------------------------------------*
    assemble_base_url_launchpad( iv_icf_node_path ).

    add_param_to_fiori_url( iv_name  = `#WorkflowTask-displayInbox?&scenarioId`
                            iv_value = `ZMY_INBOX` ) ##no_text.
    result = mv_url.
  ENDMETHOD.                    "assemble_url_fiori_my_evn_inbox
ENDCLASS.
