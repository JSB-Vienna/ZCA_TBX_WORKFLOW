"! <p class="shorttext synchronized" lang="en">CA-TBX: SSCR 0905 + 0906 - Archive content</p>
CLASS zcl_ca_wf_sscr_archive_content DEFINITION PUBLIC
                                                INHERITING FROM zcl_ca_wf_sscr_base_ctlr
                                                CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! <p>To reduce the toolbar of the viewer to specific buttons call method GET_VIEWER_DEFAULT_BUTTON of
      "! this class to get all buttons. Delete those you don't want to offer and pass the rest to parameter
      "! IT_BUTTONS of this method DISPLAY.</p>
      "!
      "! @parameter io_log                 | <p class="shorttext synchronized" lang="en">Workflow specific Business Application Logging (BAL)</p>
      "! @parameter io_archive_content     | <p class="shorttext synchronized" lang="en">ArchiveLink + DMS content of a business object</p>
      "! @parameter it_buttons             | <p class="shorttext synchronized" lang="en">Visible toolbar buttons (see comment to this method)</p>
      "! @parameter iv_no_toolbar          | <p class="shorttext synchronized" lang="en">X = Hide toolbar</p>
      "! @parameter iv_no_gos_toolbar      | <p class="shorttext synchronized" lang="en">X = Hide GOS toolbar</p>
      "! @parameter iv_hide_viewer         | <p class="shorttext synchronized" lang="en">X = Hide archive viewer, this prevent the external display</p>
      "! @parameter iv_external_display    | <p class="shorttext synchronized" lang="en">X = External display, only an empty subscreen is displayed</p>
      "! @parameter iv_frame_label         | <p class="shorttext synchronized" lang="en">Frame label</p>
      "! @raising   zcx_ca_archive_content | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while handling Archive content</p>
      constructor
        IMPORTING
          io_log              TYPE REF TO zif_ca_wf_log OPTIONAL
          io_archive_content  TYPE REF TO zcl_ca_archive_content
          it_buttons          TYPE ttb_button OPTIONAL
          iv_no_toolbar       TYPE abap_boolean DEFAULT abap_false
          iv_no_gos_toolbar   TYPE abap_boolean DEFAULT abap_false
          iv_hide_viewer      TYPE abap_boolean DEFAULT abap_false
          iv_external_display TYPE abap_boolean DEFAULT abap_false
          iv_frame_label      TYPE text70 OPTIONAL
        RAISING
          zcx_ca_archive_content,

      "! <p class="shorttext synchronized" lang="en">Handle event: Switch to archive viewer and/or refresh view</p>
      on_new_document_stored
        FOR EVENT new_document_stored OF zcl_ca_archive_content
        IMPORTING
          sender
          refresh_with_opt
          counter_before
          counter_now.

*   i n s t a n c e   e v e n t s
    EVENTS:
      "! <p class="shorttext synchronized" lang="en">Switch to archive viewer</p>
      switch_2_archive_viewer,
      "! <p class="shorttext synchronized" lang="en">Switch to external viewer</p>
      switch_2_external_viewer.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Screens</p>
      BEGIN OF cs_screen_id,
        archive_content_viewer     TYPE syst_dynnr VALUE '0905',
        blind_screen_2_hide_viewer TYPE syst_dynnr VALUE '0906',
      END   OF cs_screen_id,

      "! <p class="shorttext synchronized" lang="en">Frame program for screens</p>
      c_frame_prog_screens        TYPE syrepid   VALUE 'SAPLZCA_WF_BASIC_SCREENS' ##no_text,
      "! <p class="shorttext synchronized" lang="en">Name of custom container that contains the editor control</p>
      c_ccont_name_archive_viewer TYPE fieldname VALUE 'CCONT_ARCHIVE_CONTENT' ##no_text.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Custom container for embedding viewer</p>
      mo_cust_container       TYPE REF TO cl_gui_custom_container,
      "! <p class="shorttext synchronized" lang="en">ArchiveLink + DMS content of a business object</p>
      mo_archive_content      TYPE REF TO zcl_ca_archive_content,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Displayed docs after last refresh to add only the new docs</p>
      mt_docs_display_at_last TYPE zca_tt_archive_docs,
      "! <p class="shorttext synchronized" lang="en">Visible toolbar buttons</p>
      mt_buttons              TYPE ttb_button,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Hide toolbar</p>
      mv_no_toolbar           TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X = Hide GOS toolbar</p>
      mv_no_gos_toolbar       TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X=Open Doc. viewer in ext. screen; ' '=in this subscreen</p>
      mv_external_display     TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X = Hide archive viewer, this prevent the external display</p>
      mv_hide_viewer          TYPE abap_boolean.

*   i n s t a n c e   m e t h o d s
    METHODS:
      handle_pbo REDEFINITION,

      on_closed REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">My view name</p>
      c_my_view_name       TYPE char20            VALUE 'SSCR_ARCHIVE_CONTENT' ##no_text.

ENDCLASS.



CLASS zcl_ca_wf_sscr_archive_content IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( iv_frame_label = iv_frame_label
                        io_log         = io_log ).

    mv_repid            = c_frame_prog_screens.
    mv_screen_name      = c_my_view_name.

    mo_archive_content  = io_archive_content.
    mt_buttons          = it_buttons.
    mv_no_toolbar       = iv_no_toolbar.
    mv_no_gos_toolbar   = iv_no_gos_toolbar.
    "The order of these comparisons ranks hiding the viewer higher than the external display
    "External display is not allowed when the viewer should explicitly be hidden
    mv_external_display = SWITCH #( iv_hide_viewer
                            WHEN abap_true  THEN abap_false
                            WHEN abap_false THEN iv_external_display ).
    "Hide viewer if external display is requested
    mv_hide_viewer      = SWITCH #( mv_external_display
                            WHEN abap_true  THEN abap_true
                            WHEN abap_false THEN iv_hide_viewer ).

    IF ( mv_external_display EQ abap_true     OR
         mv_hide_viewer      EQ abap_false ) AND
         mo_archive_content  IS NOT BOUND.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_archive_content
        EXPORTING
          textid   = zcx_ca_archive_content=>param_invalid
          mv_msgty = zcx_ca_archive_content=>c_msgty_e
          mv_msgv1 = 'MO_ARCHIVE_CONTENT'
          mv_msgv2 = 'INITIAL' ##no_text.
    ENDIF.

    IF mv_hide_viewer EQ abap_true.
      mv_dynnr = cs_screen_id-blind_screen_2_hide_viewer.

    ELSE.
      mv_dynnr = cs_screen_id-archive_content_viewer.

      IF mv_frame_label IS INITIAL.
        mv_frame_label = 'Archived content'(arc).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "constructor


  METHOD handle_pbo.
    "-----------------------------------------------------------------*
    "   Handle Process Before Output
    "-----------------------------------------------------------------*
    TRY.
        CASE mv_hide_viewer.
          WHEN abap_true.
            IF mv_external_display EQ abap_true.
              mo_archive_content->display( it_buttons        = mt_buttons
                                           iv_no_toolbar     = mv_no_toolbar
                                           iv_no_gos_toolbar = mv_no_gos_toolbar ).
            ENDIF.

          WHEN abap_false.
            IF mo_cust_container IS BOUND.
              RETURN.
            ENDIF.

            super->handle_pbo( iv_event ).

            mo_cust_container = zcl_ca_cfw_util=>create_custom_container( iv_cnt_name = c_ccont_name_archive_viewer ).
            mt_docs_display_at_last = mo_archive_content->mt_docs.
            SET HANDLER on_new_document_stored FOR mo_archive_content.
            mo_archive_content->display( io_parent         = mo_cust_container
                                         it_buttons        = mt_buttons
                                         iv_no_toolbar     = mv_no_toolbar
                                         iv_no_gos_toolbar = mv_no_gos_toolbar ).
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        mo_log->add_n_save_exception( ix_catched = lx_catched
                                      iv_class   = 'ZCL_CA_WF_SSCR_ARCHIVE_CONTENT'
                                      iv_method  = 'HANDLE_PBO' ) ##no_text.
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "handle_pbo


  METHOD on_closed.
    "-----------------------------------------------------------------*
    "   Release fields and instances for garbage collection
    "-----------------------------------------------------------------*
    IF mo_archive_content IS BOUND.
      TRY.
          mo_archive_content->close_windows( ).

        CATCH zcx_ca_archive_content INTO DATA(lx_catched).
          MESSAGE lx_catched TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.

    IF mo_cust_container IS BOUND.
      mo_cust_container->free( ).
    ENDIF.

    FREE: mo_cust_container,
          mo_archive_content,
          mt_buttons,
          mv_no_toolbar,
          mv_no_gos_toolbar,
          mv_external_display,
          mv_hide_viewer.

    super->on_closed( ).
  ENDMETHOD.                    "on_closed


  METHOD on_new_document_stored.
    "-----------------------------------------------------------------*
    "   Handle event: Switch to archive viewer and/or refresh the view
    "-----------------------------------------------------------------*
    mo_archive_content = sender.
    IF mv_dynnr EQ cs_screen_id-blind_screen_2_hide_viewer.
      "Since this instance will be destroyed by the following event, delete this registration
      SET HANDLER on_new_document_stored FOR mo_archive_content ACTIVATION abap_false.
      "The switch of the screens can only be handled by the main screen -> so ask him :)?
      RAISE EVENT switch_2_archive_viewer.

    ELSE.
      LOOP AT mo_archive_content->mt_docs INTO DATA(lo_document).
        IF line_exists( mt_docs_display_at_last[ table_line = lo_document ] ).
          CONTINUE.
        ENDIF.

        TRY.
            "Attach new document to the list of the displayed
            mo_archive_content->mo_viewer->appe_ao_doc(
                                  iv_arc_id      = lo_document->ms_data-archiv_id
                                  iv_doc_id      = lo_document->ms_data-arc_doc_id
                                  iv_object_type = lo_document->ms_data-sap_object
                                  iv_object_id   = lo_document->ms_data-object_id
                                  iv_ar_object   = lo_document->ms_data-ar_object
                                  iv_title       = COND #( WHEN lo_document->ms_data-descr IS NOT INITIAL
                                                             THEN lo_document->ms_data-descr
                                                           WHEN lo_document->ms_data-filename IS NOT INITIAL
                                                             THEN lo_document->ms_data-filename
                                                           ELSE lo_document->ms_doc_type_descr-objecttext )
                                  "as it is prepared before ZCL_CA_ARCHIVE_CONTENT=>DISPLAY
                                  iv_window_id   = lo_document->ms_data-object_id+10(10)
                                  iv_no_refresh  = abap_false ).

          CATCH cx_dv_exception INTO DATA(lx_catched).
            MESSAGE lx_catched TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.                    "on_new_document_stored

ENDCLASS.
