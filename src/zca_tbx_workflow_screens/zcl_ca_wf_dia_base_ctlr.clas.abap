"! <p class="shorttext synchronized" lang="en">CA-TBX: Workflow: Base controller class for dialogs</p>
CLASS zcl_ca_wf_dia_base_ctlr DEFINITION PUBLIC
                                         INHERITING FROM zcl_ca_scr_fw_window_ctlr
                                         CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Main method, that controls the entire processing</p>
      "!
      "! @parameter io_log           | <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      "! @parameter iv_mode          | <p class="shorttext synchronized" lang="en">Screen mode (use ZCL_CA_C_SCR_FW=>MODE-*)</p>
      "! @parameter iv_dialog_name   | <p class="shorttext synchronized" lang="en">Dialog name (e. g. helpful for screen control)</p>
      "! @parameter iv_open_as       | <p class="shorttext synchronized" lang="en">Open as screen or popup -&gt; use ZCL_CA_C_SCR_FW=>OPEN_AS-*</p>
      "! @parameter is_popup_corners | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      constructor
        IMPORTING
          io_log           TYPE REF TO zif_ca_wf_log OPTIONAL
          iv_mode          TYPE syucomm DEFAULT zcl_ca_c_scr_fw=>mode-modify
          iv_dialog_name   TYPE zca_d_dialog_name OPTIONAL
          iv_open_as       TYPE abap_boolean DEFAULT zcl_ca_c_scr_fw=>open_as-screen
          is_popup_corners TYPE zca_s_scr_fw_popup_corners OPTIONAL
        RAISING
          zcx_ca_param.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      mo_log      TYPE REF TO zif_ca_wf_log.

*   i n s t a n c e   m e t h o d s
    METHODS:
      on_closed REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_wf_dia_base_ctlr IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    "Call super constructor
    super->constructor( iv_mode          = iv_mode
                        iv_dialog_name   = iv_dialog_name
                        iv_open_as       = iv_open_as
                        is_popup_corners = is_popup_corners ).

    mo_log = io_log.
  ENDMETHOD.                    "constructor


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Release class objects
    "-----------------------------------------------------------------*
    super->on_closed( ).

    FREE mo_log.
  ENDMETHOD.                    "on_closed

ENDCLASS.
