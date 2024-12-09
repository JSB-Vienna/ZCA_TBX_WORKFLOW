"! <p class="shorttext synchronized" lang="en">CA-TBX: Factory + buffering of Business Classes (= Workflow)</p>
INTERFACE zif_ca_wf_bcs_factory PUBLIC.
* i n s t a n c e   m e t h o d s
  METHODS:
    "! <p class="shorttext synchronized" lang="en">Create a workflow instance by its type and key</p>
    "!
    "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">(Concatenated) Object key</p>
    "! @parameter iv_type      | <p class="shorttext synchronized" lang="en">Name of the class to be created</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Created workflow instance</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
    "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
    create_by_key
      IMPORTING
        iv_key        TYPE sibfinstid
        iv_type       TYPE sibftypeid
      RETURNING
        VALUE(result) TYPE REF TO zif_ca_workflow
      RAISING
        zcx_ca_dbacc
        zcx_ca_param,

    "! <p class="shorttext synchronized" lang="en">Create an instance by the workflow instance key</p>
    "!
    "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Workflow instance key</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Created workflow instance</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
    "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">CA-TBX exception: Database access</p>
    create_by_lpor
      IMPORTING
        is_lpor       TYPE sibflpor
      RETURNING
        VALUE(result) TYPE REF TO zif_ca_workflow
      RAISING
        zcx_ca_dbacc
        zcx_ca_param,

    "! <p class="shorttext synchronized" lang="en">Extract key from input of consumer</p>
    "!
    "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">Key as character string</p>
    "! @parameter iv_instid    | <p class="shorttext synchronized" lang="en">Key from LPOR-INSTID</p>
    "! @parameter result       | <p class="shorttext synchronized" lang="en">Created workflow instance</p>
    "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">CA-TBX exception: Parameter error (INHERIT from this excep!)</p>
    extract_key_from_input
      IMPORTING
        iv_key        TYPE sibfinstid OPTIONAL
        iv_instid     TYPE sibfinstid OPTIONAL
      RETURNING
        VALUE(result) TYPE sibfinstid
      RAISING
        zcx_ca_param,

    "! <p class="shorttext synchronized" lang="en">Delete instance from buffer</p>
    "!
    "! @parameter io_wf_object | <p class="shorttext synchronized" lang="en">Workflow instance</p>
    release_from_buffer
      IMPORTING
        io_wf_object TYPE REF TO zif_ca_workflow.

ENDINTERFACE.
