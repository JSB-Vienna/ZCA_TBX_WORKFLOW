"! <p class="shorttext synchronized" lang="en">CA-TBX: Note editor for enter notes + display note history</p>
CLASS zcl_ca_note_editor DEFINITION PUBLIC
                                    CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Sort order</p>
      BEGIN OF cs_sort_order,
        ascending  TYPE seu_order        VALUE 'A' ##no_text,
        descending TYPE seu_order        VALUE 'D' ##no_text,
      END   OF cs_sort_order.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Splitter container that contain the editors</p>
      mo_splitter           TYPE REF TO cl_gui_splitter_container READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Editor control to change / capture a note</p>
      mo_editor_for_changes TYPE REF TO cl_gui_textedit READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Editor control to display notes</p>
      mo_editor_for_display TYPE REF TO cl_gui_textedit READ-ONLY,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Changed / captured note</p>
      mt_note_changes       TYPE tline_tab READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Notes in display</p>
      mt_note_display       TYPE tline_tab READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Key of note / text module</p>
      ms_note_key           TYPE stxh_key READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_text_object      | <p class="shorttext synchronized" lang="en">Text object (must be defined in SE75)</p>
      "! @parameter iv_text_id          | <p class="shorttext synchronized" lang="en">Text id (must be defined in SE75)</p>
      "! @parameter iv_text_key         | <p class="shorttext synchronized" lang="en">Text key, e. g. the id of a business object</p>
      "! <p>The key can also be provided later in method SAVE_NOTE.</p>
      "! @parameter iv_text_langu       | <p class="shorttext synchronized" lang="en">Text language</p>
      "! @parameter iv_read_immediately | <p class="shorttext synchronized" lang="en">X = Read text immediately with instance creation</p>
      "! @parameter iv_must_exist       | <p class="shorttext synchronized" lang="en">X = Raise exception if the text doesn't exist</p>
      "! @raising   zcx_ca_note_editor  | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      constructor
        IMPORTING
          iv_text_object      TYPE tdobject
          iv_text_id          TYPE tdid
          iv_text_key         TYPE tdobname  OPTIONAL
          iv_text_langu       TYPE spras     DEFAULT sy-langu
          iv_read_immediately TYPE abap_bool DEFAULT abap_true
          iv_must_exist       TYPE abap_bool DEFAULT abap_false
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Convert note for display into format of supplied parameter</p>
      "!
      "! @parameter et_note | <p class="shorttext synchronized" lang="en">Text as table of line type TDLINE</p>
      "! @parameter ev_note | <p class="shorttext synchronized" lang="en">Text as string</p>
      convert_note_for_display
        EXPORTING
          et_note TYPE swnttextline
          ev_note TYPE string,

      "! <p class="shorttext synchronized" lang="en">Convert note for saving into format of text modules</p>
      "!
      "! @parameter it_note | <p class="shorttext synchronized" lang="en">Text as table of line type TDLINE</p>
      "! @parameter iv_note | <p class="shorttext synchronized" lang="en">Text as string</p>
      convert_note_for_saving
        IMPORTING
          it_note        TYPE swnttextline OPTIONAL
          iv_note        TYPE string OPTIONAL
        RETURNING
          VALUE(rt_note) TYPE tline_tab,

      "! <p class="shorttext synchronized" lang="en">Create a control enter a note</p>
      "!
      "! @parameter io_parent             | <p class="shorttext synchronized" lang="en">Parent container that should contain the editors</p>
      "! @parameter iv_hide_toolbar       | <p class="shorttext synchronized" lang="en">X = Hide tool bar</p>
      "! @parameter iv_hide_statusbar     | <p class="shorttext synchronized" lang="en">X = Hide status bar</p>
      "! @parameter iv_max_chars          | <p class="shorttext synchronized" lang="en">A possible text is restricted to this number of digits</p>
      "! @parameter iv_wordwrap_mode      | <p class="shorttext synchronized" lang="en">0=off; 1=Wrap at window border; 2=Wrap at fixed position</p>
      "! @parameter iv_wordwrap_position  | <p class="shorttext synchronized" lang="en">Position in case word wrap mode 2</p>
      "! @parameter iv_wordwrap_to_linebreak_mode | <p class="shorttext synchronized" lang="en">0=Preserve word wraps; 1=Change word wrap to line break</p>
      "! @raising   zcx_ca_note_editor    | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      create_change_control
        IMPORTING
          io_parent                     TYPE REF TO cl_gui_container
          iv_hide_toolbar               TYPE abap_bool DEFAULT abap_true
          iv_hide_statusbar             TYPE abap_bool DEFAULT abap_false
          iv_max_chars                  TYPE i OPTIONAL
          iv_wordwrap_mode              TYPE i DEFAULT cl_gui_textedit=>wordwrap_at_windowborder
          iv_wordwrap_position          TYPE i DEFAULT -1
          iv_wordwrap_to_linebreak_mode TYPE i DEFAULT cl_gui_textedit=>false
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Create a control to display notes</p>
      "!
      "! @parameter io_parent             | <p class="shorttext synchronized" lang="en">Parent container that should contain the editors</p>
      "! @parameter iv_hide_toolbar       | <p class="shorttext synchronized" lang="en">X = Hide tool bar</p>
      "! @parameter iv_hide_statusbar     | <p class="shorttext synchronized" lang="en">X = Hide status bar</p>
      "! @parameter iv_comment_identifier | <p class="shorttext synchronized" lang="en">Identifier for comment lines</p>
      "! @parameter iv_highlight_comments | <p class="shorttext synchronized" lang="en">X = Highlight comment lines</p>
      "! @parameter iv_wordwrap_mode      | <p class="shorttext synchronized" lang="en">0=off; 1=Wrap at window border; 2=Wrap at fixed position</p>
      "! @parameter iv_wordwrap_position  | <p class="shorttext synchronized" lang="en">Position in case word wrap mode 2</p>
      "! @parameter iv_wordwrap_to_linebreak_mode | <p class="shorttext synchronized" lang="en">0=Preserve word wraps; 1=Change word wrap to line break</p>
      "! @raising   zcx_ca_note_editor    | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      create_display_control
        IMPORTING
          io_parent                     TYPE REF TO cl_gui_container
          iv_hide_toolbar               TYPE abap_bool DEFAULT abap_true
          iv_hide_statusbar             TYPE abap_bool DEFAULT abap_true
          iv_comment_identifier         TYPE txted_string DEFAULT cl_gui_textedit=>abap_commentline_identifier
          iv_highlight_comments         TYPE abap_bool DEFAULT abap_true
          iv_wordwrap_mode              TYPE i DEFAULT cl_gui_textedit=>wordwrap_at_windowborder
          iv_wordwrap_position          TYPE i DEFAULT -1
          iv_wordwrap_to_linebreak_mode TYPE i DEFAULT cl_gui_textedit=>false
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Create a control for display notes (l) AND enter a note (r)</p>
      "!
      "! @parameter io_parent             | <p class="shorttext synchronized" lang="en">Parent container that should contain the editors</p>
      "! @parameter iv_hide_toolbar       | <p class="shorttext synchronized" lang="en">X = Hide tool bar</p>
      "! @parameter iv_hide_statusbar     | <p class="shorttext synchronized" lang="en">X = Hide status bar</p>
      "! @parameter iv_max_chars          | <p class="shorttext synchronized" lang="en">A possible text is restricted to this number of digits</p>
      "! @parameter iv_width_display      | <p class="shorttext synchronized" lang="en">Width in percent for displaying editor</p>
      "! @parameter iv_comment_identifier | <p class="shorttext synchronized" lang="en">Identifier for comment lines</p>
      "! @parameter iv_highlight_comments | <p class="shorttext synchronized" lang="en">X = Hightlight comment lines</p>
      "! @parameter iv_wordwrap_mode      | <p class="shorttext synchronized" lang="en">0=off; 1=Wrap at window border; 2=Wrap at fixed position</p>
      "! @parameter iv_wordwrap_position  | <p class="shorttext synchronized" lang="en">Position in case word wrap mode 2</p>
      "! @parameter iv_wordwrap_to_linebreak_mode | <p class="shorttext synchronized" lang="en">0=Preserve word wraps; 1=Change word wrap to line break</p>
      "! @raising   zcx_ca_note_editor    | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      create_display_change_control
        IMPORTING
          io_parent                     TYPE REF TO cl_gui_container
          iv_hide_toolbar               TYPE abap_bool DEFAULT abap_true
          iv_hide_statusbar             TYPE abap_bool DEFAULT abap_true
          iv_max_chars                  TYPE i OPTIONAL
          iv_width_display              TYPE i DEFAULT 65
          iv_comment_identifier         TYPE txted_string DEFAULT cl_gui_textedit=>abap_commentline_identifier
          iv_highlight_comments         TYPE abap_bool DEFAULT abap_true
          iv_wordwrap_mode              TYPE i DEFAULT cl_gui_textedit=>wordwrap_at_windowborder
          iv_wordwrap_position          TYPE i DEFAULT -1
          iv_wordwrap_to_linebreak_mode TYPE i DEFAULT cl_gui_textedit=>false
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Prepare note for change/creation with SAVE_NOTE</p>
      "!
      "! @parameter iv_note            | <p class="shorttext synchronized" lang="en">New note as string</p>
      "! @parameter iv_is_obligatory   | <p class="shorttext synchronized" lang="en">X = Note is obligatory</p>
      "! @parameter iv_raise_exception | <p class="shorttext synchronized" lang="en">X = Raise exception, if note is obligatory</p>
      "! @parameter iv_add_user_n_time | <p class="shorttext synchronized" lang="en">X = Add user name and time stamp</p>
      "! @raising   zcx_ca_note_editor | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      get_n_prepare_for_save
        IMPORTING
          iv_note               TYPE string    OPTIONAL
          iv_is_obligatory      TYPE abap_bool DEFAULT abap_false
          iv_raise_exception    TYPE abap_bool DEFAULT abap_true
          iv_add_user_n_time    TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(rv_is_modified) TYPE abap_bool
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Save note changes</p>
      "!
      "! @parameter iv_text_key         | <p class="shorttext synchronized" lang="en">Text key (OVERWRITES the one passed into the CONSTRUCTOR!!)</p>
      "! @parameter iv_note             | <p class="shorttext synchronized" lang="en">Note as text stream</p>
      "! @parameter iv_is_obligatory    | <p class="shorttext synchronized" lang="en">X = Note is obligatory</p>
      "! @parameter iv_raise_exception  | <p class="shorttext synchronized" lang="en">X = Raise exception, if note is obligatory</p>
      "! @parameter iv_add_user_n_time  | <p class="shorttext synchronized" lang="en">X = Add user name and time stamp</p>
      "! @parameter iv_sort_order       | <p class="shorttext synchronized" lang="en">Sort order -&gt; descending = most recent note at the beginning</p>
      "! @parameter iv_save_immediately | <p class="shorttext synchronized" lang="en">X = Save immediately; ' ' = save asynchronous in background</p>
      "! @parameter rv_is_modified      | <p class="shorttext synchronized" lang="en">X = Note is changed</p>
      "! @raising   zcx_ca_note_editor  | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      save_note
        IMPORTING
          iv_text_key           TYPE tdobname  OPTIONAL
          iv_note               TYPE string    OPTIONAL
          iv_is_obligatory      TYPE abap_bool DEFAULT abap_false
          iv_raise_exception    TYPE abap_bool DEFAULT abap_true
          iv_add_user_n_time    TYPE abap_bool DEFAULT abap_true
          iv_sort_order         TYPE seu_order DEFAULT zcl_ca_note_editor=>cs_sort_order-descending
          iv_save_immediately   TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(rv_is_modified) TYPE abap_bool
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Set user id and full name (instead of SYST variables)</p>
      "!
      "! @parameter iv_user_id   | <p class="shorttext synchronized" lang="en">SAP user id</p>
      set_user
        IMPORTING
          iv_user_id TYPE syuname.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Note / text header record</p>
      ms_note_header           TYPE thead,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = It is a new note</p>
      mv_is_a_new_note         TYPE abap_bool VALUE abap_false,
      "! <p class="shorttext synchronized" lang="en">Sign to recognize which lines should be highlighted</p>
      mv_sign_for_highlighting TYPE txted_string VALUE cl_gui_textedit=>abap_commentline_identifier,
      "! <p class="shorttext synchronized" lang="en">X = Comment should be highlighted</p>
      mv_highlighting_comment  TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">SAP user id</p>
      mv_user_id               TYPE syuname.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Read note from DB</p>
      "!
      "! @parameter iv_must_exist      | <p class="shorttext synchronized" lang="en">X = Raise exception if the text doesn't exist</p>
      "! @raising   zcx_ca_note_editor | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      get_note
        IMPORTING
          iv_must_exist TYPE abap_bool DEFAULT abap_false
        RAISING
          zcx_ca_note_editor,

      "! <p class="shorttext synchronized" lang="en">Prepare note for display with method CREATE_DISPLAY_*</p>
      "!
      "! @parameter iv_comment_identifier | <p class="shorttext synchronized" lang="en">Identifier for comment lines</p>
      "! @parameter iv_highlight_comments | <p class="shorttext synchronized" lang="en">X = Hightlight comment lines</p>
      "! @raising   zcx_ca_note_editor    | <p class="shorttext synchronized" lang="en">Error while handling a note</p>
      prepare_n_set_for_display
        IMPORTING
          iv_comment_identifier TYPE txted_string DEFAULT cl_gui_textedit=>abap_commentline_identifier
          iv_highlight_comments TYPE abap_bool DEFAULT abap_true
        RAISING
          zcx_ca_note_editor.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.                     "zcl_ca_note_editor  DEFINITION


CLASS zcl_ca_note_editor IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error             TYPE REF TO zcx_ca_note_editor.

    "Check existence text object
    CALL FUNCTION 'CHECK_TEXT_OBJECT'
      EXPORTING
        object = iv_text_object
      EXCEPTIONS
        object = 1
        OTHERS = 2.
    IF sy-subrc NE 0.
      lx_error ?= zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_note_editor=>c_zcx_ca_note_editor
                                                  iv_function = 'CHECK_TEXT_OBJECT'
                                                  iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Check existence text id
    CALL FUNCTION 'CHECK_TEXT_ID'
      EXPORTING
        id     = iv_text_id
        object = iv_text_object
      EXCEPTIONS
        id     = 1
        OTHERS = 2.
    IF sy-subrc NE 0.
      lx_error ?= zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_note_editor=>c_zcx_ca_note_editor
                                                  iv_function = 'CHECK_TEXT_ID'
                                                  iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Check language
    CALL FUNCTION 'CHECK_TEXT_LANGUAGE'
      EXPORTING
        language = iv_text_langu
      EXCEPTIONS
        language = 1
        OTHERS   = 2.
    IF sy-subrc NE 0.
      lx_error ?= zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_note_editor=>c_zcx_ca_note_editor
                                                  iv_function = 'CHECK_TEXT_LANGUAGE'
                                                  iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Keep values
    ms_note_key-tdobject = iv_text_object.
    ms_note_key-tdid     = iv_text_id.
    ms_note_key-tdname   = iv_text_key.
    ms_note_key-tdspras  = iv_text_langu.

    "Read text
    IF iv_read_immediately EQ abap_true   AND
       ms_note_key-tdname  IS NOT INITIAL.
      get_note( iv_must_exist ).

    ELSEIF ms_note_key-tdname IS INITIAL.
      mv_is_a_new_note = abap_true.
    ENDIF.
  ENDMETHOD.                    "constructor


  METHOD convert_note_for_display.
    "-----------------------------------------------------------------*
    "   Convert note for display into format of supplied parameter
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_note     TYPE tdline,
      lv_note_len TYPE i.

    IF mt_note_display IS INITIAL.
      RETURN.
    ENDIF.

    "Convert text into editor lines. This converts format type '*' or '/' in CR_LFs
    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        language    = ms_note_key-tdspras
      TABLES
        itf_text    = mt_note_display
        text_stream = et_note.

    IF ev_note IS SUPPLIED.
      lv_note_len = lines( et_note ) * 132.
      CALL FUNCTION 'SCMS_FTEXT_TO_STRING'
        EXPORTING
          length    = lv_note_len
        IMPORTING
          ftext     = ev_note
        TABLES
          ftext_tab = et_note.
    ENDIF.
  ENDMETHOD.                    "convert_note_for_display


  METHOD convert_note_for_saving.
    "-----------------------------------------------------------------*
    "   Convert note for saving into table TLINE format (= Format of text modules)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_note               TYPE swnttextline.

    "Convert string into table
    IF iv_note IS NOT INITIAL.
      CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
        EXPORTING
          text      = iv_note
        TABLES
          ftext_tab = lt_note.
    ENDIF.

    "Convert note
    IF it_note IS NOT INITIAL.
      lt_note = it_note.
    ENDIF.

    IF lt_note IS NOT INITIAL.
      "Convert editor lines into text (module) format. This eliminates CR LFs.
      CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
        EXPORTING
          language    = ms_note_key-tdspras
        TABLES
          text_stream = lt_note
          itf_text    = rt_note.
    ENDIF.
  ENDMETHOD.                    "convert_note_for_saving


  METHOD create_change_control.
    "-----------------------------------------------------------------*
    "   Create a control enter a note
    "-----------------------------------------------------------------*
    "Create text editor for displaying text
    mo_editor_for_changes =
        zcl_ca_cfw_util=>create_text_edit_control(
                                          io_parent                     = io_parent
                                          iv_display_only               = abap_false
                                          iv_hide_statusbar             = iv_hide_statusbar
                                          iv_hide_toolbar               = iv_hide_toolbar
                                          iv_max_chars                  = iv_max_chars
                                          iv_wordwrap_mode              = iv_wordwrap_mode
                                          iv_wordwrap_position          = iv_wordwrap_position
                                          iv_wordwrap_to_linebreak_mode = iv_wordwrap_to_linebreak_mode ).
  ENDMETHOD.                    "create_change_control


  METHOD create_display_control.
    "-----------------------------------------------------------------*
    "   Create a control to display notes
    "-----------------------------------------------------------------*
    "Create text editor for displaying text
    mo_editor_for_display =
        zcl_ca_cfw_util=>create_text_edit_control(
                                          io_parent                     = io_parent
                                          iv_display_only               = abap_true
                                          iv_hide_statusbar             = iv_hide_statusbar
                                          iv_hide_toolbar               = iv_hide_toolbar
                                          iv_wordwrap_mode              = iv_wordwrap_mode
                                          iv_wordwrap_position          = iv_wordwrap_position
                                          iv_wordwrap_to_linebreak_mode = iv_wordwrap_to_linebreak_mode ).

    "Prepare text for display
    prepare_n_set_for_display( iv_comment_identifier = iv_comment_identifier
                               iv_highlight_comments = iv_highlight_comments ).
  ENDMETHOD.                    "create_display_control


  METHOD create_display_change_control.
    "-----------------------------------------------------------------*
    "   Create a control for display notes (l) AND enter a note (r)
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lr_pane              TYPE REF TO cl_gui_container.

    "Create splitter container
    mo_splitter = zcl_ca_cfw_util=>create_splitter_container( io_parent  = io_parent
                                                              iv_rows    = 1
                                                              iv_columns = 2 ).
    "Set column to 65 % of width
    zcl_ca_cfw_util=>set_column_width( io_splitter = mo_splitter
                                       iv_id       = 1
                                       iv_width    = iv_width_display ).

    "Get left cell for displaying existing note
    lr_pane = mo_splitter->get_container( row    = 1
                                          column = 1 ).
    "Create text editor for displaying note and set note into editor
    create_display_control( io_parent                     = lr_pane
                            iv_hide_statusbar             = iv_hide_statusbar
                            iv_hide_toolbar               = iv_hide_toolbar
                            iv_wordwrap_mode              = iv_wordwrap_mode
                            iv_wordwrap_position          = iv_wordwrap_position
                            iv_wordwrap_to_linebreak_mode = iv_wordwrap_to_linebreak_mode
                            iv_comment_identifier         = iv_comment_identifier
                            iv_highlight_comments         = iv_highlight_comments ).

    "Get left cell for change/create note
    lr_pane = mo_splitter->get_container( row    = 1
                                          column = 2 ).
    "Create text editor for creating note
    create_change_control( io_parent                     = lr_pane
                           iv_hide_statusbar             = iv_hide_statusbar
                           iv_hide_toolbar               = iv_hide_toolbar
                           iv_max_chars                  = iv_max_chars
                           iv_wordwrap_mode              = iv_wordwrap_mode
                           iv_wordwrap_position          = iv_wordwrap_position
                           iv_wordwrap_to_linebreak_mode = iv_wordwrap_to_linebreak_mode ).
  ENDMETHOD.                    "create_display_change_control


  METHOD get_note.
    "-----------------------------------------------------------------*
    "   Read note from DB
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error             TYPE REF TO zcx_ca_note_editor.

    "Get text from DB
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = ms_note_key-tdid
        language                = ms_note_key-tdspras
        name                    = ms_note_key-tdname
        object                  = ms_note_key-tdobject
      IMPORTING
        header                  = ms_note_header
      TABLES
        lines                   = mt_note_display
      EXCEPTIONS
        not_found               = 1
        id                      = 2
        language                = 3
        name                    = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc EQ 1.
      "Keep in mind that text does still not exist
      mv_is_a_new_note = abap_true.

      "Raise exception if the text must already exist
      IF iv_must_exist EQ abap_true.
        lx_error ?= zcx_ca_error=>create_exception(
                                      iv_excp_cls = zcx_ca_note_editor=>c_zcx_ca_note_editor
                                      iv_function = 'READ_TEXT'
                                      iv_subrc    = sy-subrc ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.

    ELSEIF sy-subrc GT 1.
      lx_error ?= zcx_ca_error=>create_exception(
                                iv_excp_cls = zcx_ca_note_editor=>c_zcx_ca_note_editor
                                iv_function = 'READ_TEXT'
                                iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "get_note


  METHOD get_n_prepare_for_save.
    "-----------------------------------------------------------------*
    "   Prepare note for change/creation with SAVE_NOTE
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_note        TYPE swnttextline,
      ls_note        TYPE tline,
      lv_is_modified TYPE i.

    "Is note changed outside?
    lv_is_modified = cl_gui_textedit=>false.
    IF iv_note IS NOT INITIAL.
      lv_is_modified = cl_gui_textedit=>true.

    ELSEIF mo_editor_for_changes IS BOUND.
      "Get text from changing control in a table
      mo_editor_for_changes->get_text_as_stream(
                                            IMPORTING
                                              text                   = lt_note
                                              is_modified            = lv_is_modified
                                            EXCEPTIONS
                                              error_dp               = 1
                                              error_cntl_call_method = 2
                                              OTHERS                 = 3 ).
      IF sy-subrc NE 0.
        DATA(lx_error) = CAST zcx_ca_intern(
                                 zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                                  iv_class    = 'CL_GUI_TEXTEDIT'
                                                                  iv_method   = 'GET_TEXT_AS_STREAM'
                                                                  iv_subrc    = sy-subrc ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
      ENDIF.
    ENDIF.

    "Convert IS_MODIFIED into EVN boolean and prepare text saving
    CASE lv_is_modified.
      WHEN cl_gui_textedit=>false.
        rv_is_modified = abap_false.

      WHEN cl_gui_textedit=>true.
        rv_is_modified = abap_true.
        mt_note_changes = convert_note_for_saving( it_note = lt_note
                                                   iv_note = iv_note ).
    ENDCASE.

    "Is a note obligatory?
    IF iv_is_obligatory EQ abap_true  AND
       mt_note_changes  IS INITIAL.
      CASE iv_raise_exception.
        WHEN abap_false.
          "Returning value is already set
          RETURN.

        WHEN abap_true.
          "Set focus on change editor
          mo_editor_for_changes->set_focus( mo_editor_for_changes ).
          "It is obligatory to enter a note
          RAISE EXCEPTION TYPE zcx_ca_note_editor
            EXPORTING
              textid   = zcx_ca_note_editor=>note_is_obligatory
              mv_msgty = zcx_ca_note_editor=>c_msgty_e.
      ENDCASE.
    ENDIF.

    "If nothing was captured or the standard info is not requested
    IF rv_is_modified     EQ abap_false OR
       iv_add_user_n_time EQ abap_false.
      "Leave method
      RETURN.
    ENDIF.

    "Create line with admin infos, like who has written and when
    SET LANGUAGE ms_note_key-tdspras.        "Set output environment to requested language
    "Set date and time
    GET TIME.

    "Set user id if not set somewhere else
    IF mv_user_id IS INITIAL.
      mv_user_id = sy-uname.
    ENDIF.

    TRY.
        DATA(lv_full_name) = zcl_ca_wf_user=>get_instance( iv_key = mv_user_id )->ms_address-fullname.
        ls_note-tdline = condense( |{ mv_sign_for_highlighting } { sy-datlo DATE = ENVIRONMENT } / | &
                                   |{ sy-uzeit TIME = ENVIRONMENT } { lv_full_name } ({ mv_user_id })| ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        lv_full_name = lx_catched->get_text( ).
    ENDTRY.

    SET LANGUAGE space.  "Reset output environment to user specific settings

    "Set paragraph format for this line
    ls_note-tdformat = '*'.

    "Insert admin line before captured text
    INSERT ls_note INTO  mt_note_changes
                   INDEX 1.
  ENDMETHOD.                    "get_n_prepare_for_save


  METHOD prepare_n_set_for_display.
    "-----------------------------------------------------------------*
    "   Prepare note for display with method CREATE_DISPLAY_*
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error             TYPE REF TO zcx_ca_intern,
      lt_note              TYPE swnttextline,
      lv_highlight_comment TYPE i.

    "Convert text for display
    convert_note_for_display(
                        IMPORTING
                          et_note = lt_note ).
    "Set current text
    mo_editor_for_display->set_text_as_stream(
                                          EXPORTING
                                            text            = lt_note
                                          EXCEPTIONS
                                            error_dp        = 1
                                            error_dp_create = 2
                                            OTHERS          = 3 ).
    IF sy-subrc NE 0.
      lx_error ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                   iv_class    = 'CL_GUI_TEXTEDIT'
                                                   iv_method   = 'SET_TEXT_AS_STREAM'
                                                   iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Set '*' as comment attribute
    mv_sign_for_highlighting = iv_comment_identifier.
    mo_editor_for_display->set_comments_string(
                                          EXPORTING
                                            comments_string        = mv_sign_for_highlighting
                                          EXCEPTIONS
                                            error_cntl_call_method = 1
                                            OTHERS                 = 2 ).
    IF sy-subrc NE 0.
      lx_error ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                   iv_class    = 'CL_GUI_TEXTEDIT'
                                                   iv_method   = 'SET_COMMENTS_STRING'
                                                   iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.

    "Highlight comment lines
    mv_highlighting_comment = iv_highlight_comments.
    IF mv_highlighting_comment  EQ abap_true   AND
       mv_sign_for_highlighting IS NOT INITIAL.
      lv_highlight_comment = mo_editor_for_display->true.
    ELSE.
      lv_highlight_comment = mo_editor_for_display->false.
    ENDIF.
    mo_editor_for_display->set_highlight_comments_mode(
                                                  EXPORTING
                                                    highlight_comments_mode = lv_highlight_comment
                                                  EXCEPTIONS
                                                    error_cntl_call_method  = 1
                                                    invalid_parameter       = 2
                                                    OTHERS                  = 3 ).
    IF sy-subrc NE 0.
      lx_error ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                   iv_class    = 'CL_GUI_TEXTEDIT'
                                                   iv_method   = 'SET_HIGHLIGHT_COMMENTS_MODE'
                                                   iv_subrc    = sy-subrc ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "prepare_n_set_for_display


  METHOD save_note.
    "-----------------------------------------------------------------*
    "   Save note changes
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_intern           TYPE REF TO zcx_ca_intern,
      ls_text             TYPE tline,
      lv_insert           TYPE abap_bool,
      lv_save_immediately TYPE abap_bool.

    "Prepare text for saving
    rv_is_modified = get_n_prepare_for_save( iv_note            = iv_note
                                             iv_is_obligatory   = iv_is_obligatory
                                             iv_raise_exception = iv_raise_exception
                                             iv_add_user_n_time = iv_add_user_n_time ).
    IF rv_is_modified EQ abap_false.
      RETURN.
    ENDIF.

    "Add new text to old text
    ls_text-tdformat = '*'.
    CASE iv_sort_order.
      WHEN cs_sort_order-descending.
        APPEND ls_text TO mt_note_changes.
        APPEND LINES OF mt_note_display TO mt_note_changes.

      WHEN cs_sort_order-ascending.
        APPEND ls_text TO mt_note_display.
        APPEND LINES OF mt_note_changes TO mt_note_display.
        mt_note_changes = mt_note_display.
    ENDCASE.

    "Is it a new text
    GET TIME.
    IF mv_is_a_new_note EQ abap_true.
      lv_insert = mv_is_a_new_note.
      ms_note_header-tdobject = ms_note_key-tdobject.
      ms_note_header-tdid     = ms_note_key-tdid.

      IF ms_note_key-tdname IS INITIAL AND
         iv_text_key        IS INITIAL.
        "At least one of the following parameters must be passed: &1 &2 &3 &4
        RAISE EXCEPTION TYPE zcx_ca_note_editor
          EXPORTING
            textid   = zcx_ca_note_editor=>at_least_one
            mv_msgty = zcx_ca_note_editor=>c_msgty_e
            mv_msgv1 = 'IV_TEXT_KEY in CONSTRUCTOR or'
            mv_msgv2 = 'IV_TEXT_KEY in SAVE_NOTE' ##no_text.

      ELSEIF iv_text_key IS NOT INITIAL.
        ms_note_header-tdname = ms_note_key-tdname = iv_text_key.

      ELSE.
        ms_note_header-tdname = ms_note_key-tdname.
      ENDIF.

      ms_note_header-tdfuser  = sy-uname.       "Created by
      ms_note_header-tdfdate  = sy-datlo.       "Date created
      ms_note_header-tdftime  = sy-uzeit.       "Time Created

    ELSE.
      ms_note_header-tdluser  = sy-uname.       "Last changed by
      ms_note_header-tdldate  = sy-datlo.       "Changed On
      ms_note_header-tdltime  = sy-uzeit.       "Last Changed at
    ENDIF.

    "Set language if necessary
    IF ms_note_header-tdspras IS INITIAL.
      ms_note_header-tdspras = ms_note_key-tdspras.
    ENDIF.

    "Save text in DB
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = ms_note_header
        insert          = lv_insert
        savemode_direct = iv_save_immediately
        owner_specified = abap_true
      IMPORTING
        newheader       = ms_note_header
      TABLES
        lines           = mt_note_changes
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
    IF sy-subrc BETWEEN 1 AND 4.
      DATA(lx_error) =
             CAST zcx_ca_note_editor(
                          zcx_ca_error=>create_exception( iv_excp_cls = zcx_ca_note_editor=>c_zcx_ca_note_editor
                                                          iv_function = 'SAVE_TEXT'
                                                          iv_subrc    = sy-subrc ) ) ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.

    ELSEIF sy-subrc GE 5.
      lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                    iv_function = 'SAVE_TEXT'
                                                    iv_subrc    = sy-subrc ) ##no_text.
      IF lx_intern IS BOUND.
        RAISE EXCEPTION lx_intern.
      ENDIF.
    ENDIF.

    IF iv_save_immediately EQ abap_false.       "Use settings of text object
      "Register text change for saving in update task so that it is only saved with COMMIT WORK afterwards.
      CALL FUNCTION 'COMMIT_TEXT'
        EXPORTING
          object   = ms_note_header-tdobject
          name     = ms_note_header-tdname
          id       = ms_note_header-tdid
          language = ms_note_header-tdspras.
    ENDIF.

    "Prepare text for next display and set flag is not new
    mt_note_display = mt_note_changes.
    CLEAR mt_note_changes.
    mv_is_a_new_note = abap_false.

    "Actualize text in corresponding controls
    "Set enhanced new text in display control
    IF mo_editor_for_display IS BOUND.
      prepare_n_set_for_display( iv_comment_identifier = mv_sign_for_highlighting
                                 iv_highlight_comments = mv_highlighting_comment ).
    ENDIF.

    "Clear text and undo buffer of changing control
    IF mo_editor_for_changes IS BOUND.
      mo_editor_for_changes->delete_text(
                                    EXCEPTIONS
                                      error_cntl_call_method = 1
                                      OTHERS                 = 2 ).
      IF sy-subrc NE 0.
        lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                      iv_class    = 'CL_GUI_TEXTEDIT'
                                                      iv_method   = 'DELETE_TEXT'
                                                      iv_subrc    = sy-subrc ) ##no_text.
        IF lx_intern IS BOUND.
          RAISE EXCEPTION lx_intern.
        ENDIF.
      ENDIF.
      mo_editor_for_changes->empty_undo_buffer(
                                           EXCEPTIONS
                                             error_cntl_call_method = 1
                                             OTHERS                 = 2 ).
      IF sy-subrc NE 0.
        lx_intern ?= zcx_ca_intern=>create_exception( iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                      iv_class    = 'CL_GUI_TEXTEDIT'
                                                      iv_method   = 'EMPTY_UNDO_BUFFER'
                                                      iv_subrc    = sy-subrc ) ##no_text.
        IF lx_intern IS BOUND.
          RAISE EXCEPTION lx_intern.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "save_note


  METHOD set_user.
    "-----------------------------------------------------------------*
    "   Set user id and full name (instead of SYST variables)
    "-----------------------------------------------------------------*
    mv_user_id   = iv_user_id.
  ENDMETHOD.                    "set_user

ENDCLASS.                     "zcl_ca_note_editor  IMPLEMENTATION


