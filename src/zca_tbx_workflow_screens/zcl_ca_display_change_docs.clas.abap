"! <p class="shorttext synchronized" lang="en">CA-TBX: Display change documents as popup or inplace</p>
CLASS zcl_ca_display_change_docs DEFINITION PUBLIC
                                            INHERITING FROM zcl_ca_salv_wrapper
                                            CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter iv_list_title    | <p class="shorttext synchronized" lang="en">Short description (is set as ALV header and as 2nd title)</p>
      "! @parameter iv_prg_variants  | <p class="shorttext synchronized" lang="en">Program name for saving layout variants</p>
      "! @parameter iv_layout_handle | <p class="shorttext synchronized" lang="en">ID to differ between different ALV calls in one program</p>
      "! @parameter io_container     | <p class="shorttext synchronized" lang="en">Container instance, if ALV is used inplace with controls</p>
      "! @parameter iv_cnt_name      | <p class="shorttext synchronized" lang="en">Name of the ALV control/container</p>
      "! @parameter is_popup_corners | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      constructor
        IMPORTING
          iv_list_title    TYPE lvc_title                  DEFAULT 'Change documents'(lti)
          io_container     TYPE REF TO cl_gui_container    OPTIONAL
          iv_cnt_name      TYPE csequence                  OPTIONAL
          iv_prg_variants  TYPE sycprog                    DEFAULT sy-cprog
          iv_layout_handle TYPE slis_handl                 DEFAULT 'CHDO' ##no_text
          is_popup_corners TYPE zca_s_scr_fw_popup_corners OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Display change documents for object</p>
      "!
      "! @parameter iv_object_class   | <p class="shorttext synchronized" lang="en">Object class</p>
      "! @parameter iv_object_id      | <p class="shorttext synchronized" lang="en">Single object key (concatenated incl. client!)</p>
      "! @parameter ira_object_ids    | <p class="shorttext synchronized" lang="en">Multiple object keys (concatenated incl. client!)</p>
      "! @parameter iv_date_of_change | <p class="shorttext synchronized" lang="en">Creation date from</p>
      "! @parameter iv_time_of_change | <p class="shorttext synchronized" lang="en">Creation time from</p>
      "! @parameter iv_date_until     | <p class="shorttext synchronized" lang="en">Creation date to</p>
      "! @parameter iv_time_until     | <p class="shorttext synchronized" lang="en">Creation time to</p>
      "! @parameter ira_transaction   | <p class="shorttext synchronized" lang="en">Transactions</p>
      "! @raising   zcx_ca_dbacc      | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      "! @raising   zcx_ca_param      | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      display_for_object
        IMPORTING
          iv_object_class   TYPE cdobjectcl
          iv_object_id      TYPE cdobjectv OPTIONAL
          ira_object_ids    TYPE cdobjectv_range_tab OPTIONAL
          iv_date_of_change TYPE cddatum OPTIONAL
          iv_time_of_change TYPE cduzeit OPTIONAL
          iv_date_until     TYPE cddatum DEFAULT '99991231'
          iv_time_until     TYPE cduzeit DEFAULT '235959'
          ira_transaction   TYPE cdtcode_range_tab OPTIONAL
        RAISING
          zcx_ca_dbacc
          zcx_ca_param,

      process REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
**     o b j e c t   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mo_...               TYPE REF TO x..
*
**     d a t a   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mr_...               TYPE REF TO x..
*
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Determined change documents</p>
      mt_change_docs_raw TYPE cdredcd_tab,
      "! <p class="shorttext synchronized" lang="en">Determined and prepared change documents</p>
      mt_change_docs     TYPE zca_t_change_documents,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      ms_popup_corners   TYPE zca_s_scr_fw_popup_corners,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">X = Open as popup</p>
      mv_open_as_popup   TYPE abap_boolean.

*   i n s t a n c e   m e t h o d s
    METHODS:
      prepare_alv REDEFINITION,

      "! <p class="shorttext synchronized" lang="en">Create key field from technical description</p>
      "!
      "! @parameter ir_table_field | <p class="shorttext synchronized" lang="en">Technical table field description</p>
      "! @parameter rr_key_field   | <p class="shorttext synchronized" lang="en">Reference of key field</p>
      "! @raising   zcx_ca_param   | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      create_key_field
        IMPORTING
          ir_table_field      TYPE REF TO dfies
        RETURNING
          VALUE(rr_key_field) TYPE REF TO data
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Get fields with technical details of a table</p>
      "!
      "! @parameter iv_table_name   | <p class="shorttext synchronized" lang="en">Table name</p>
      "! @parameter rt_table_fields | <p class="shorttext synchronized" lang="en">Fields of the table with technical details</p>
      "! @raising   zcx_ca_param    | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      get_table_fields
        IMPORTING
          iv_table_name          TYPE cdtabname
        RETURNING
          VALUE(rt_table_fields) TYPE ddfields
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Convert key string of the change doc. into defined portions</p>
      "!
      "! @parameter iv_cd_key       | <p class="shorttext synchronized" lang="en">Change document key as string</p>
      "! @parameter it_table_fields | <p class="shorttext synchronized" lang="en">Fields of the table with technical details</p>
      "! @parameter rv_cd_key_prep  | <p class="shorttext synchronized" lang="en">Prepare change document key</p>
      "! @raising   zcx_ca_param    | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      prepare_change_doc_key
        IMPORTING
          iv_cd_key             TYPE cdtabkey
          it_table_fields       TYPE ddfields
        RETURNING
          VALUE(rv_cd_key_prep) TYPE cdtabkey
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Prepare change document entries for display</p>
      "!
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      prepare_result
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Determine and set users full name</p>
      "!
      "! @parameter iv_sap_user_id | <p class="shorttext synchronized" lang="en">SAP user Id</p>
      "! @parameter rv_full_name   | <p class="shorttext synchronized" lang="en">Users full name</p>
      set_users_full_name
        IMPORTING
          iv_sap_user_id      TYPE xubname
        RETURNING
          VALUE(rv_full_name) TYPE emnam.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_CA_DISPLAY_CHANGE_DOCS IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( ir_table              = REF #( mt_change_docs )
                        iv_list_title         = iv_list_title
                        iv_register_events    = abap_true
                        iv_layout_restriction = if_salv_c_layout=>restrict_user_dependant
                        iv_prg_variants       = iv_prg_variants
                        iv_layout_handle      = iv_layout_handle
                        io_container          = io_container
                        iv_cnt_name           = iv_cnt_name ).

    mv_open_as_popup = xsdbool( io_container IS NOT BOUND ).
    ms_popup_corners = is_popup_corners.
  ENDMETHOD.                    "constructor


  METHOD create_key_field.
    "-----------------------------------------------------------------*
    "   Create key field from technical description
    "-----------------------------------------------------------------*
    TRY.
        IF ir_table_field->rollname IS NOT INITIAL.
          CREATE DATA rr_key_field TYPE (ir_table_field->rollname).
        ELSE.
          CREATE DATA rr_key_field TYPE   (ir_table_field->datatype)
                                   LENGTH ir_table_field->intlen.
        ENDIF.

      CATCH cx_sy_create_data_error INTO DATA(lx_catched).
        DATA(lx_error) = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                           iv_class    = 'ZCL_CA_DISPLAY_CHANGE_DOCS'
                                                           iv_method   = 'CREATE_KEY_FIELD'
                                                           ix_error    = lx_catched ) ) ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "create_key_field


  METHOD display_for_object.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    TRY.
        IF iv_object_class IS INITIAL.
          "Parameter '&1' has invalid value '&2'
          RAISE EXCEPTION TYPE zcx_ca_param
            EXPORTING
              textid   = zcx_ca_param=>param_not_supplied
              mv_msgty = c_msgty_e.
        ENDIF.

        IF iv_object_id   IS INITIAL AND
           ira_object_ids IS INITIAL.
          "At least one of the following parameters must be passed: &1 &2 &3 &4
          RAISE EXCEPTION TYPE zcx_ca_param
            EXPORTING
              textid   = zcx_ca_param=>at_least_one
              mv_msgty = c_msgty_e.
        ENDIF.

        CALL FUNCTION 'CHANGEDOCUMENT_READ_ALL'
          EXPORTING
            i_objectclass              = iv_object_class
            i_objectid                 = iv_object_id
            i_date_of_change           = iv_date_of_change
            i_time_of_change           = iv_time_of_change
            i_date_until               = iv_date_until
            i_time_until               = iv_time_until
            it_objectid                = ira_object_ids
            it_tcode                   = ira_transaction
          IMPORTING
            et_cdred                   = mt_change_docs_raw
          EXCEPTIONS
            no_position_found          = 1
            missing_input_objectclass  = 2
            missing_input_header       = 3
            wrong_access_to_archive    = 4
            time_zone_conversion_error = 5
            read_too_many_entries      = 6
            OTHERS                     = 7.
        CASE sy-subrc.
          WHEN 0.
            process( ).

          WHEN 1.
            "No data was found for the specified selection criteria
            RAISE EXCEPTION TYPE zcx_ca_dbacc
              EXPORTING
                textid   = zcx_ca_dbacc=>no_data
                mv_msgty = zcx_ca_dbacc=>c_msgty_s.

          WHEN OTHERS.
            DATA(lx_error) = CAST zcx_ca_param(
                                  zcx_ca_error=>create_exception(
                                                   iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                   iv_function = 'CHANGEDOCUMENT_READ_ALL'
                                                   iv_subrc    = sy-subrc ) ) ##no_text.
            IF lx_error IS BOUND.
              RAISE EXCEPTION lx_error.
            ENDIF.
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "display_for_object


  METHOD get_table_fields.
    "-----------------------------------------------------------------*
    "   Get fields with technical details of a table
    "-----------------------------------------------------------------*
    DATA(lo_stru_desc) = CAST cl_abap_structdescr(
                                    NEW zcl_ca_ddic( iv_name = iv_table_name )->mo_type_desc ).
    rt_table_fields = lo_stru_desc->get_ddic_field_list( ).
  ENDMETHOD.                    "get_table_fields


  METHOD prepare_alv.
    "-----------------------------------------------------------------*
    "   Prepare ALV
    "-----------------------------------------------------------------*
    "Define available functions
    DATA(lo_funcs) = mo_salv->get_functions( ).
    lo_funcs->set_sort_asc( abap_false ).
    lo_funcs->set_sort_desc( abap_false ).
    lo_funcs->set_detail( abap_false ).

    "Get column controller
    DATA(lo_cols) = mo_salv->get_columns( ).
    lo_cols->set_key_fixation( ).
    lo_cols->set_column_position( columnname = 'UDATE'
                                  position   = 1 ) ##no_text.
    lo_cols->set_column_position( columnname = 'UTIME'
                                  position   = 2 ) ##no_text.
    lo_cols->set_column_position( columnname = 'TABKEY'
                                  position   = 3 ) ##no_text.
    lo_cols->set_column_position( columnname = 'TABNAME'
                                  position   = 4 ) ##no_text.

    "Adapt / set columns
    LOOP AT mt_cols ASSIGNING FIELD-SYMBOL(<ls_col>).
      "Casting to make further functionalities available
      DATA(lo_col) = CAST cl_salv_column_table( <ls_col>-r_column ).

      CASE <ls_col>-columnname.
        WHEN 'UDATE'   OR
             'UTIME'   OR
             'TABKEY' ##no_text.
          lo_col->set_key( abap_true ).
          lo_col->set_key_presence_required( abap_true ).

        WHEN 'F_OLD' ##no_text.
          lo_col->set_color( VALUE #( col = col_total
                                           int = 1
                                           inv = 0 ) ).

        WHEN 'F_NEW' ##no_text.
          lo_col->set_color( VALUE #( col = col_positive
                                           int = 1
                                           inv = 0 ) ).

        WHEN 'CHNGIND' ##no_text.
          lo_col->set_technical( ).

        WHEN 'CHNGIND_TX' ##no_text.
          lo_col->set_short_text( 'Chg. ind.'(chi) ).
      ENDCASE.
    ENDLOOP.

    "Get sort controller and set groups
    DATA(lo_sorts) = mo_salv->get_sorts( ).

    lo_sorts->add_sort( columnname = 'UDATE' ##no_text
                        position   = 1
                        sequence   = if_salv_c_sort=>sort_down ).

    lo_sorts->add_sort( columnname = 'UTIME' ##no_text
                        position   = 2
                        sequence   = if_salv_c_sort=>sort_down ).

    lo_sorts->add_sort( columnname = 'TABKEY' ##no_text
                        position   = 3
                        sequence   = if_salv_c_sort=>sort_up
                        group      = if_salv_c_sort=>group_with_underline ).
  ENDMETHOD.                    "prepare_alv


  METHOD prepare_change_doc_key.
    "-----------------------------------------------------------------*
    "   Convert key string of the change document into defined portions
    "-----------------------------------------------------------------*
    "Local data definitions
    FIELD-SYMBOLS:
      "Inline declaration is NOT ALLOWED for this type of assignment!!!
      <lv_key_value> TYPE data,           "Key value from key string
      <lv_key_field> TYPE data.           "Key value in type conform field

    DATA:
      lv_key_value_prep    TYPE symsgv.

    DATA(lv_offset) = 0.
    LOOP AT it_table_fields REFERENCE INTO DATA(lr_table_field)
                            WHERE keyflag EQ abap_true.
      "Create a type conform field for further preparation
      DATA(lr_key_field) = create_key_field( lr_table_field ).
      ASSIGN lr_key_field->* TO <lv_key_field>.

      "Transfer key value into type conform field
      "HINT: GUIDs of length RAW16 can simply be moved into CHAR32, any other variant has to be converted
      "via class CL_SYSTEM_UUID!!
      DATA(lv_length) = COND int4( WHEN lr_table_field->inttype EQ cl_abap_typedescr=>typekind_hex "some GUIDs are in Hex
                                     THEN lr_table_field->outputlen
                                     ELSE lr_table_field->leng ).
      ASSIGN iv_cd_key+lv_offset(lv_length) TO <lv_key_value>.
      <lv_key_field> = <lv_key_value>.

      TRY.
          zcl_ca_conv=>internal_2_external(
                                      EXPORTING
                                        internal_value = <lv_key_field>
                                      IMPORTING
                                        external_value = lv_key_value_prep ).

        CATCH zcx_ca_conv.
          lv_key_value_prep = <lv_key_field>.
      ENDTRY.

      rv_cd_key_prep = condense( |{ rv_cd_key_prep } { lv_key_value_prep }| ).
      lv_offset += lv_length.
    ENDLOOP.
  ENDMETHOD.                    "prepare_change_doc_key


  METHOD prepare_result.
    "-----------------------------------------------------------------*
    "   Prepare change document entries
    "-----------------------------------------------------------------*
    "Get descriptions to fixed values of the change indicator
    DATA(lt_change_ind_descr) = NEW zcl_ca_ddic( iv_name = 'CDCHNGIND' )->get_fixed_values( ) ##no_text.

    "Group raw data by table name and key to prepare the key value and set the field label for techn. field
    "name. This grouping is also to increase the performance for this a bit more elaborate preparation.
    LOOP AT mt_change_docs_raw REFERENCE INTO DATA(lr_cd_raw_grp_entry)
                               GROUP BY ( table_name = lr_cd_raw_grp_entry->tabname ) ASCENDING AS TEXT
                                                                                      INTO DATA(ls_cd_raw_grp).
      "Get fields of the table
      DATA(lt_table_fields) = get_table_fields( ls_cd_raw_grp-table_name ).

      "Complete change documents with ...
      LOOP AT GROUP ls_cd_raw_grp REFERENCE INTO DATA(lr_cd_raw_grp_member).
        DATA(ls_change_doc) = CORRESPONDING zca_s_change_document( lr_cd_raw_grp_member->* ).

        "... the field label text, ...
        ls_change_doc-scrtext_l = VALUE #( lt_table_fields[ fieldname = ls_change_doc-fname ]-scrtext_l OPTIONAL ).

        "... the description of the change indicator and ...
        ls_change_doc-chngind_tx = VALUE #( lt_change_ind_descr[ low = ls_change_doc-chngind ]-ddtext OPTIONAL ).

        "... users full name.
        ls_change_doc-fullname = set_users_full_name( ls_change_doc-username ).

        "Last but not least prepare the key string into the defined portions.
        ls_change_doc-tabkey = prepare_change_doc_key( iv_cd_key       = lr_cd_raw_grp_member->tabkey
                                                       it_table_fields = lt_table_fields ).
        APPEND ls_change_doc TO mt_change_docs.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.                    "prepare_result


  METHOD process.
    "-----------------------------------------------------------------*
    "   Display change documents
    "-----------------------------------------------------------------*
    TRY.
        IF mv_open_as_popup EQ abap_true.
          IF ms_popup_corners IS INITIAL.
            ms_popup_corners-starting_at_x = 10.
            ms_popup_corners-ending_at_x   = 210.
            ms_popup_corners-starting_at_y = 8.
            ms_popup_corners-ending_at_y   = 28.
          ENDIF.

          mo_salv->set_screen_popup( start_column = ms_popup_corners-starting_at_x
                                     end_column   = ms_popup_corners-ending_at_x
                                     start_line   = ms_popup_corners-starting_at_y
                                     end_line     = ms_popup_corners-ending_at_y ).
        ENDIF.

        prepare_alv( ).

        prepare_result( ).

        mo_salv->display( ).

      CATCH cx_salv_error INTO DATA(lx_salv_error).
        MESSAGE lx_salv_error TYPE c_msgty_e.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "process


  METHOD set_users_full_name.
    "-----------------------------------------------------------------*
    "   Determine and set users full name
    "-----------------------------------------------------------------*
    TRY.
        "Class ZCL_CA_WF_USER buffers the existing users itself
        DATA(lo_user) = zcl_ca_wf_user=>get_instance( iv_key = iv_sap_user_id ).
        rv_full_name = lo_user->ms_address-fullname.

      CATCH zcx_ca_param
            zcx_ca_dbacc INTO DATA(lx_catched).
        rv_full_name = lx_catched->get_text( ).
    ENDTRY.
  ENDMETHOD.                    "set_users_full_name
ENDCLASS.
