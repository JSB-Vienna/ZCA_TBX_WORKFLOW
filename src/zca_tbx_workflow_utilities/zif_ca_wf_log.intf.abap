"! <p class="shorttext synchronized" lang="en">CA-TBX: Workflow specific Business Application Logging (BAL)</p>
INTERFACE zif_ca_wf_log PUBLIC.
* i n t e r f a c e s
  INTERFACES:
    zif_ca_log.

* a l i a s e s
  ALIASES:
    add_msg               FOR  zif_ca_log~add_msg,
    add_msg_bapiret2      FOR  zif_ca_log~add_msg_bapiret2,
    add_msg_bapiret2_tab  FOR  zif_ca_log~add_msg_bapiret2_tab,
    add_msg_exc           FOR  zif_ca_log~add_msg_exc,
    add_msg_syst          FOR  zif_ca_log~add_msg_syst,
    close                 FOR  zif_ca_log~close,
    display               FOR  zif_ca_log~display,
    get_msg_count         FOR  zif_ca_log~get_msg_count,
    get_msg_list_bapiret2 FOR  zif_ca_log~get_msg_list_bapiret2,
    get_profile           FOR  zif_ca_log~get_profile,
    save                  FOR  zif_ca_log~save,
    set_ext_number        FOR  zif_ca_log~set_ext_number.

* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Add and save exception without closing log</p>
    "!
    "! @parameter ix_catched | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter iv_class   | <p class="shorttext synchronized" lang="en">Name of executed class (addition to IV_METHOD)</p>
    "! @parameter iv_method  | <p class="shorttext synchronized" lang="en">Name of executed method (addition to IV_CLASS)</p>
    "! @parameter rx_error   | <p class="shorttext synchronized" lang="en">Converted exception to further use</p>
    add_n_save_exception
      IMPORTING
        ix_catched      TYPE REF TO cx_root
        iv_class        TYPE seoclsname OPTIONAL
        iv_method       TYPE seocpdname OPTIONAL
      RETURNING
        VALUE(rx_error) TYPE REF TO zcx_ca_param,

    "! <p class="shorttext synchronized" lang="en">Add and save message without closing log</p>
    "!
    "! <p>The method set default values for <em>MSGTY, MSGID and MSGNO</em> in case they are empty. These are:</p>
    "! <ul>
    "!   <li>The message contents only four placeholder: <em>&1 &2 &3 &4</em></li>
    "!   <li>MSGTY = <em>'I'</em></li>
    "!   <li>MSGID = <em>'38'</em></li>
    "!   <li>MSGNO = <em>'001'</em></li>
    "! </ul>
    "!
    "! @parameter is_message | <p class="shorttext synchronized" lang="en">Message details</p>
    add_n_save_message
      IMPORTING
        is_message TYPE dcmessage,

    "! <p class="shorttext synchronized" lang="en">Add and save message workflow task is completed successfully</p>
    "!
    "! @parameter iv_task_descr | <p class="shorttext synchronized" lang="en">Short description of task (case sensitive)</p>
    add_n_save_msg_task_completed
      IMPORTING
        iv_task_descr TYPE text50,

    "! <p class="shorttext synchronized" lang="en">Add and save message workflow task was cancelled by user</p>
    add_n_save_msg_task_cancelled,

    "! <p class="shorttext synchronized" lang="en">Add and save message workflow was cancelled by user</p>
    "!
    "! @parameter is_lpor | <p class="shorttext synchronized" lang="en">Process key</p>
    add_n_save_msg_wf_cancelled
      IMPORTING
        is_lpor TYPE sibflporb.

ENDINTERFACE.
