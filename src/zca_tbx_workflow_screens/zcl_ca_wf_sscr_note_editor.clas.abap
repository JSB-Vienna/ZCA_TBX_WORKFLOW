"! <p class="shorttext synchronized" lang="en">CA-TBX: Workflow: SSCR 0900 - Notes editor</p>
CLASS zcl_ca_wf_sscr_note_editor DEFINITION PUBLIC
                                            INHERITING FROM zcl_ca_wf_sscr_base_ctlr
                                            CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter io_log              | <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      "! @parameter iv_text_object      | <p class="shorttext synchronized" lang="en">Text object (must be defined in SE75)</p>
      "! @parameter iv_text_id          | <p class="shorttext synchronized" lang="en">Text id (must be defined in SE75)</p>
      "! @parameter iv_text_key         | <p class="shorttext synchronized" lang="en">Text key, typically the id of an business object</p>
      "! @parameter iv_text_langu       | <p class="shorttext synchronized" lang="en">Text language</p>
      "! @parameter iv_read_immediately | <p class="shorttext synchronized" lang="en">X = Read text immediately with instance creation</p>
      "! @parameter iv_must_exist       | <p class="shorttext synchronized" lang="en">X = Raise exception if the text doesn't exist</p>
      "! @parameter iv_frame_descr      | <p class="shorttext synchronized" lang="en">Frame description</p>
      "! @raising   zcx_ca_param        | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      constructor
        IMPORTING
          io_log              TYPE REF TO zif_ca_wf_log OPTIONAL
          iv_text_object      TYPE tdobject
          iv_text_id          TYPE tdid
          iv_text_key         TYPE tdobname  OPTIONAL
          iv_text_langu       TYPE spras     DEFAULT sy-langu
          iv_read_immediately TYPE abap_bool DEFAULT abap_true
          iv_must_exist       TYPE abap_bool DEFAULT abap_false
          iv_frame_descr      TYPE text70    OPTIONAL
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get notes editor</p>
      "!
      "! @parameter ro_notes_editor | <p class="shorttext synchronized" lang="en">Note editor (enter a note and/or display notes history)</p>
      get_notes_editor
        RETURNING
          VALUE(ro_notes_editor) TYPE REF TO zcl_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Set visibility of the notes editor (ONLY AT PBO ALLOWED!)</p>
      "!
      "! @parameter iv_event      | <p class="shorttext synchronized" lang="en">Screen event PBO</p>
      "! @parameter iv_is_visible | <p class="shorttext synchronized" lang="en">X = Notes editor is visible</p>
      "! @raising   zcx_ca_param  | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      set_visibility
        IMPORTING
          iv_event      TYPE syst_ucomm
          iv_is_visible TYPE abap_boolean DEFAULT abap_true
        RAISING
          zcx_ca_param.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Frame program for screens</p>
      c_frame_prog_screens TYPE syrepid   VALUE 'SAPLZCA_WF_BASIC_SCREENS' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Name of custom container that contains the editor control</p>
      c_ccont_name_editor  TYPE fieldname VALUE 'CCONT_NOTE_EDITOR' ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Custom container for embedding viewer</p>
      mo_cc_note_editor TYPE REF TO cl_gui_custom_container,
      "! <p class="shorttext synchronized" lang="en">Note editor (enter a note and/or display notes history)</p>
      mo_note_editor    TYPE REF TO zcl_ca_note_editor.

*   i n s t a n c e   m e t h o d s
    METHODS:
      handle_pbo REDEFINITION,

      on_closed REDEFINITION,

      on_process_fcode REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">My view name</p>
      c_my_view_name       TYPE char20            VALUE 'SSCR_NOTE_EDITOR' ##no_text.

ENDCLASS.



CLASS ZCL_CA_WF_SSCR_NOTE_EDITOR IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( iv_frame_label = iv_frame_descr
                        io_log         = io_log ).

    mv_repid       = c_frame_prog_screens.
    mv_dynnr       = '0900'.
    mv_screen_name = c_my_view_name.

    IF mv_frame_label IS INITIAL.
      mv_frame_label = 'Notes'(not).
    ENDIF.

    mo_note_editor = NEW #( iv_text_object      = iv_text_object
                            iv_text_id          = iv_text_id
                            iv_text_key         = iv_text_key
                            iv_text_langu       = iv_text_langu
                            iv_read_immediately = iv_read_immediately
                            iv_must_exist       = iv_must_exist ).
  ENDMETHOD.                    "constructor


  METHOD set_visibility.
    "-----------------------------------------------------------------*
    "   Set visibility of the notes editor
    "-----------------------------------------------------------------*
    IF iv_event NE mo_scr_options->event-pbo.
      RETURN.     "Only during PBO allowed
    ENDIF.

    CASE iv_is_visible.
      WHEN abap_true.
        mo_scr_fld_attr->make_visible( screen_field_name  = 'GS_FRAME_LABEL-D0900' ) ##no_text.
        mo_cc_note_editor->set_visible( iv_is_visible ).

      WHEN abap_false.
        mo_scr_fld_attr->hide( screen_field_name  = 'GS_FRAME_LABEL-D0900' ) ##no_text.
        mo_cc_note_editor->set_visible( iv_is_visible ).
    ENDCASE.
  ENDMETHOD.                    "set_visibility


  METHOD handle_pbo.
    "-----------------------------------------------------------------*
    "   Handle Process Before Output
    "-----------------------------------------------------------------*
    IF mo_cc_note_editor IS BOUND.
      RETURN.
    ENDIF.

    TRY.
        super->handle_pbo( iv_event ).

        mo_cc_note_editor = zcl_ca_cfw_util=>create_custom_container( iv_cnt_name = c_ccont_name_editor ).

        CASE mo_screen->mv_mode.
          WHEN mo_screen->mo_scr_options->mode-modify.
            mo_note_editor->create_display_change_control( io_parent = mo_cc_note_editor ).

          WHEN mo_screen->mo_scr_options->mode-display.
            mo_note_editor->create_display_control( io_parent = mo_cc_note_editor ).
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        mo_log->add_n_save_exception( lx_catched ).
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "handle_pbo


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Release fields and instances for garbage collection
    "-----------------------------------------------------------------*
    IF mo_cc_note_editor IS BOUND.
      mo_cc_note_editor->free( ).
    ENDIF.

    FREE: mo_cc_note_editor,
          mo_note_editor.

    super->on_closed( ).
  ENDMETHOD.                    "on_closed


  METHOD get_notes_editor.
    "-----------------------------------------------------------------*
    "   Get notes editor
    "-----------------------------------------------------------------*
    ro_notes_editor = mo_note_editor.
  ENDMETHOD.                    "Get_notes_editor


  METHOD on_process_fcode.
    "-----------------------------------------------------------------*
    "   Handle function code
    "-----------------------------------------------------------------*
    "No checks in display mode
    IF mo_screen->mv_mode EQ mo_screen->mo_scr_options->mode-display.
      RETURN.
    ENDIF.

    TRY.
        CASE iv_fcode.
          WHEN mo_fcodes->save.
            mo_note_editor->save_note( ).
        ENDCASE.

      CATCH zcx_ca_param INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE c_msgty_e.
    ENDTRY.
  ENDMETHOD.                    "on_process_fcode
ENDCLASS.
