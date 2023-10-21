"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow: Extentd. SAP standard interface IF_WORKFLOW</p>
INTERFACE zif_ca_workflow PUBLIC.
*   i n t e r f a c e s
  INTERFACES:
    if_workflow,
    if_salv_c_bool_sap.

* a l i a s e s
  ALIASES:
*   BI_OBJECT methods
    default_attribute_value FOR bi_object~default_attribute_value,
    execute_default_method  FOR bi_object~execute_default_method,
    release                 FOR bi_object~release,
*   BI_PERSISTENT methods
    find_by_lpor            FOR bi_persistent~find_by_lpor,
    lpor                    FOR bi_persistent~lpor,
    refresh                 FOR bi_persistent~refresh,
*   Flags for usage in WF definitions
    true                    FOR if_salv_c_bool_sap~true,
    false                   FOR if_salv_c_bool_sap~false.

* i n s t a n c e   a t t r i b u t e s
  DATA:
*   o b j e c t   r e f e r e n c e s
    "! <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
    mo_log          TYPE REF TO zif_ca_wf_log READ-ONLY,

*   s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Default attribute with prepared object key</p>
    mv_default_attr TYPE text80 READ-ONLY.

*   i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Check existence of object</p>
    "!
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
    check_existence DEFAULT IGNORE
      RAISING
        zcx_ca_param
        zcx_ca_dbacc,

    "! <p class="shorttext synchronized" lang="en">Assemble task short text</p>
    "!
    "! @parameter iv_task_desc | <p class="shorttext synchronized" lang="en">Complementing description/action/function</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Completed task short description</p>
    get_task_descr DEFAULT IGNORE
      IMPORTING
        iv_task_desc  TYPE text80
      RETURNING
        VALUE(result) TYPE witext,

    "! <p class="shorttext synchronized" lang="en">Raise an instance event</p>
    "!
    "! @parameter iv_event       | <p class="shorttext synchronized" lang="en">Event name</p>
    "! @parameter io_evt_cnt     | <p class="shorttext synchronized" lang="en">Instance of event container</p>
    "! @parameter iv_evt_creator | <p class="shorttext synchronized" lang="en">Event creator (user that triggered the event)</p>
    "! @parameter iv_do_commit   | <p class="shorttext synchronized" lang="en">X = Do commit here</p>
    "! @raising   zcx_ca_param   | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
    raise_event DEFAULT IGNORE
      IMPORTING
        iv_event       TYPE sibfevent
        io_evt_cnt     TYPE REF TO if_swf_cnt_container OPTIONAL
        iv_evt_creator TYPE syst_uname DEFAULT sy-uname
        iv_do_commit   TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_ca_param.

ENDINTERFACE.
