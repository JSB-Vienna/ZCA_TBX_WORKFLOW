"! <p class="shorttext synchronized" lang="en">CA-TBX: Workflow: Base controller class for subscreens</p>
"!
"! <p>Use this class for any sub screen in your workflow applications to be able to log any messages
"! during runtime.</p>
CLASS zcl_ca_wf_sscr_base_ctlr DEFINITION PUBLIC
                                          INHERITING FROM zcl_ca_scr_fw_screen_ctlr
                                          CREATE PUBLIC
                                          ABSTRACT.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_frame_label | <p class="shorttext synchronized" lang="en">Frame label</p>
      "! @parameter io_log         | <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      constructor
        IMPORTING
          iv_frame_label TYPE text70 OPTIONAL
          io_log         TYPE REF TO zif_ca_wf_log OPTIONAL.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      mo_log         TYPE REF TO zif_ca_wf_log.

*   i n s t a n c e   m e t h o d s
    METHODS:
      handle_pbo REDEFINITION,

      on_closed REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_wf_sscr_base_ctlr IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( iv_frame_label ).

    mo_log = io_log.
  ENDMETHOD.                    "constructor


  METHOD handle_pbo.
    "-----------------------------------------------------------------*
    "   Handle Process Before Output - but set no GUI status!
    "-----------------------------------------------------------------*
    IF mv_frame_label IS NOT INITIAL.
      exchange_data(
              EXPORTING
                iv_event = iv_event
                iv_fname = |{ c_frame_label }{ mv_dynnr }|
              CHANGING
                cv_data  = mv_frame_label ).
    ENDIF.
  ENDMETHOD.                    "handle_pbo


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->on_closed( ).
    FREE mo_log.
  ENDMETHOD.                    "on_closed

ENDCLASS.
