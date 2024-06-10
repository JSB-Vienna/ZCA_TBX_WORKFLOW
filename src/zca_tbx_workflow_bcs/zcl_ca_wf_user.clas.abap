"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow: BC Extended user Id (master data)</p>
CLASS zcl_ca_wf_user DEFINITION PUBLIC
                                CREATE PROTECTED.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message,
      zif_ca_workflow.            " !!! Includes IF_WORKFLOW = BI_OBJECT + BI_PERSISTENT

*   a l i a s e s
    ALIASES:
*     BI_OBJECT methods
      default_attr_value   FOR bi_object~default_attribute_value,
      execute_def_method   FOR bi_object~execute_default_method,
      release              FOR bi_object~release,
*     BI_PERSISTENT methods
      find_by_lpor         FOR bi_persistent~find_by_lpor,
      lpor                 FOR bi_persistent~lpor,
      refresh              FOR bi_persistent~refresh,
*     ZIF_CA_WORKFLOW methods
      check_existence      FOR zif_ca_workflow~check_existence,
      get_task_descr       FOR zif_ca_workflow~get_task_descr,
      raise_event          FOR zif_ca_workflow~raise_event,
      mo_log               FOR zif_ca_workflow~mo_log,
      mv_default_attr      FOR zif_ca_workflow~mv_default_attr.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     b u s i n e s s   o b j e c t s
      "! <p class="shorttext synchronized" lang="en">BO type USR01 - SAP user address data</p>
      mbo_usr01     TYPE sibflporb VALUE zif_ca_c_wf_bos=>cbo_usr01 READ-ONLY ##no_text,
      "! <p class="shorttext synchronized" lang="en">BO type USR01 - SAP user master data / methods</p>
      mbo_user      TYPE sibflporb VALUE zif_ca_c_wf_bos=>cbo_user READ-ONLY ##no_text,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">BAPI reference structure for addresses (contact person)</p>
      ms_address    TYPE bapiaddr3 READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">User: Logon Data Transfer Structure</p>
      ms_logon_data TYPE bapilogond READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      mv_key        TYPE xubname READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Compatible user Id for usage in workflow expressions</p>
      mv_agent      TYPE swp_agent READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Personnel number</p>
      mv_pernr      TYPE pernr_d READ-ONLY.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Create instance</p>
      "!
      "! @parameter is_lpor      | <p class="shorttext synchronized" lang="en">Business object/class key</p>
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Common WF object: Extended BO USR01 informations</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance
        IMPORTING
          is_lpor       TYPE sibflpor OPTIONAL
          iv_key        TYPE xubname  DEFAULT sy-uname
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_user
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Create instance for an workflow agent (type US only)</p>
      "!
      "! @parameter iv_wf_agent  | <p class="shorttext synchronized" lang="en">Workflow agent (type + user id)</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Common WF object: BC extending BO USR01 + BO USER infos</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      get_instance_from_wf_agent
        IMPORTING
          iv_wf_agent   TYPE swp_agent
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_user
        RAISING
          zcx_ca_param
          zcx_ca_dbacc.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
      "!
      "! @parameter iv_key       | <p class="shorttext synchronized" lang="en">User Name in User Master Record</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      constructor
        IMPORTING
          iv_key TYPE xubname
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Checks whether the user is a dialog user</p>
      "!
      "! @parameter result       | <p class="shorttext synchronized" lang="en">X = User is dialog user</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      is_dialog_user
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Checks whether the user is locked locally and/or globally</p>
      "!
      "! @parameter result       | <p class="shorttext synchronized" lang="en">X = User is valid at passed date</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      is_locked_locally_or_globally
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Validation check - combination of some of the IS_... methods</p>
      "!
      "! @parameter iv_date      | <p class="shorttext synchronized" lang="en">Date</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">X = User is valid</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      is_valid
        IMPORTING
          iv_date       TYPE systdatlo DEFAULT sy-datlo
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Checks whether the user is valid up to given date</p>
      "!
      "! @parameter iv_date      | <p class="shorttext synchronized" lang="en">Date</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">X = User is valid at passed date</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      is_valid_at_given_date
        IMPORTING
          iv_date       TYPE syst_datlo DEFAULT sy-datlo
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_param.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Macro handler for USR01</p>
      mo_wfmacs_usr01 TYPE REF TO zcl_ca_wf_exec_macros,
      "! <p class="shorttext synchronized" lang="en">Macro handler for USER</p>
      mo_wfmacs_user  TYPE REF TO zcl_ca_wf_exec_macros,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Users locks</p>
      ms_is_locked    TYPE bapislockd,
      "! <p class="shorttext synchronized" lang="en">Workflow instance key</p>
      ms_lpor         TYPE sibflpor.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Set personnel number late</p>
      "!
      "! @parameter iv_pernr | <p class="shorttext synchronized" lang="en">Personnel number</p>
      set_pernr
        IMPORTING
          iv_pernr TYPE pernr_d.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Buffered instance</p>
      BEGIN OF ty_s_buffer.
        INCLUDE TYPE sibflpor AS s_lpor.
    TYPES:
        o_persistent TYPE REF TO zif_ca_workflow,
      END   OF ty_s_buffer,
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      ty_t_buffer TYPE HASHED TABLE OF ty_s_buffer
                                       WITH UNIQUE KEY primary_key COMPONENTS s_lpor.

*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Type Id</p>
      c_my_typeid          TYPE sibftypeid        VALUE 'ZCL_CA_WF_USER'  ##no_text.

*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Instance buffer</p>
      mt_buffer     TYPE ty_t_buffer.

ENDCLASS.



CLASS zcl_ca_wf_user IMPLEMENTATION.

  METHOD bi_object~default_attribute_value ##needed.
    "-----------------------------------------------------------------*
    "   Returns a description and/or prepared key of the object.
    "-----------------------------------------------------------------*
    mv_default_attr = |{ ms_address-fullname } ({ mv_key })|.
    result = REF #( mv_default_attr ).
  ENDMETHOD.                    "bi_object~default_attribute_value


  METHOD bi_object~execute_default_method ##needed.
    "-----------------------------------------------------------------*
    "   Execute default method
    "-----------------------------------------------------------------*
    SET PARAMETER ID 'XUS' FIELD mv_key.
    CALL TRANSACTION 'SU01D' WITH AUTHORITY-CHECK
                              AND SKIP FIRST SCREEN.
  ENDMETHOD.                    "bi_object~execute_default_method


  METHOD bi_object~release ##needed.
    "-----------------------------------------------------------------*
    "   Release instance
    "-----------------------------------------------------------------*
    DELETE mt_buffer WHERE s_lpor EQ ms_lpor.
  ENDMETHOD.                    "bi_object~release


  METHOD bi_persistent~find_by_lpor.
    "-----------------------------------------------------------------*
    "   Create workflow / workitem instance
    "-----------------------------------------------------------------*
    TRY.
        result ?= zcl_ca_wf_user=>get_instance( is_lpor = lpor ).

      CATCH zcx_ca_error INTO DATA(lx_catched).
        "As long as no exceptions are declared for this method, this is
        "currently the best solution.
        MESSAGE lx_catched TYPE c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "bi_persistent~find_by_lpor


  METHOD bi_persistent~lpor.
    "-----------------------------------------------------------------*
    "   Return instance key
    "-----------------------------------------------------------------*
    result = ms_lpor.
  ENDMETHOD.                    "bi_persistent~lpor


  METHOD bi_persistent~refresh ##needed.
    "-----------------------------------------------------------------*
    "   Refresh instance
    "-----------------------------------------------------------------*

  ENDMETHOD.                    "bi_persistent~refresh


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    ms_lpor-typeid = to_upper( c_my_typeid ).
    ms_lpor-catid  = swfco_objtype_cl.

    IF iv_key IS INITIAL.
      RETURN.
    ENDIF.

    "Complete and keep several attributes
    ms_lpor-instid = mv_key = iv_key.
  ENDMETHOD.                    "constructor


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      ls_lpor TYPE sibflpor,
      lv_key  TYPE xubname.

    IF is_lpor IS NOT INITIAL.
      ls_lpor       = is_lpor.
      ls_lpor-catid = swfco_objtype_cl.

      "Set key into structured definition
      IF is_lpor-instid IS NOT INITIAL.  "Avoid destruction of type conform initial values
        lv_key = CONV #( is_lpor-instid ).
      ENDIF.

      "Set these values in any case, e. g. to create/get an instance only with the key string
      IF ls_lpor-typeid IS INITIAL.
        ls_lpor-typeid = to_upper( zcl_ca_wf_user=>c_my_typeid ).
      ENDIF.

    ELSEIF iv_key IS NOT INITIAL.
      lv_key  = iv_key.
      ls_lpor = VALUE #( instid = CONV #( iv_key )
                         typeid = to_upper( zcl_ca_wf_user=>c_my_typeid )
                         catid  = swfco_objtype_cl ).

    ELSE.
      "At least one of the following parameters must be passed: &1 &2 &3 &4
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>at_least_one
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_LPOR'
          mv_msgv2 = 'IV_KEY'
          mv_msgv3 = space
          mv_msgv4 = space ##no_text.
    ENDIF.

    "If approval id is still not available create no instance
    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        "Is an instance already created?
        DATA(ls_buffer) = VALUE #( zcl_ca_wf_user=>mt_buffer[ KEY primary_key
                                                                  s_lpor = ls_lpor ] ).

        ls_buffer-o_persistent->refresh( ).

      CATCH cx_sy_itab_line_not_found.
        "Create instance of payment approval object
        CREATE OBJECT ls_buffer-o_persistent TYPE (ls_lpor-typeid)
          EXPORTING
            iv_key = lv_key.

        "Checks existence of object and creates default attribute = readable key with text
        ls_buffer-o_persistent->check_existence( ).
        ls_buffer-o_persistent->default_attribute_value( ).

        ls_buffer-s_lpor = ls_buffer-o_persistent->lpor( ).
        INSERT ls_buffer INTO TABLE zcl_ca_wf_user=>mt_buffer.
    ENDTRY.

    result ?= ls_buffer-o_persistent.
  ENDMETHOD.                    "get_instance


  METHOD get_instance_from_wf_agent.
    "-----------------------------------------------------------------*
    "   Get instance from workflow agent (type US only)
    "-----------------------------------------------------------------*
    IF iv_wf_agent(2) NE swfco_org_user.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_param
        EXPORTING
          textid   = zcx_ca_param=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IV_WF_AGENT(2)'
          mv_msgv2 = CONV #( iv_wf_agent(2) ).
    ENDIF.

    result = zcl_ca_wf_user=>get_instance( iv_key = iv_wf_agent+2(12) ).
  ENDMETHOD.                    "get_instance_from_wf_agent


  METHOD is_dialog_user.
    "-----------------------------------------------------------------*
    "   Checks whether the user is a dialog user
    "-----------------------------------------------------------------*
    result = abap_false.
    IF ms_logon_data-ustyp EQ 'A' ##no_text.
      result = abap_true.
    ENDIF.

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "User &1 is not a dialog user
      RAISE EXCEPTION TYPE zcx_ca_param
        MESSAGE e199(cts_organizer_msg) WITH mv_key.
    ENDIF.
  ENDMETHOD.                    "is_dialog_user


  METHOD is_locked_locally_or_globally.
    "-----------------------------------------------------------------*
    "   Checks whether the user is locked locally and/or globally
    "-----------------------------------------------------------------*
    result = abap_false.
    IF ms_is_locked-local_lock EQ 'L' OR
       ms_is_locked-glob_lock  EQ 'L' ##no_text.
      result = abap_true.
    ENDIF.

    IF result IS NOT SUPPLIED AND
       result EQ abap_true.
      "User &1 is locked
      RAISE EXCEPTION TYPE zcx_ca_param
        MESSAGE e066(siag_msg) WITH mv_key.
    ENDIF.
  ENDMETHOD.                    "is_locked_locally_or_globally


  METHOD is_valid.
    "-----------------------------------------------------------------*
    "   Checks whether the user is valid up to given date
    "-----------------------------------------------------------------*
    TRY.
        result = abap_true.
        is_valid_at_given_date( iv_date ).
        is_dialog_user( ).
        is_locked_locally_or_globally( ).

      CATCH zcx_ca_param INTO DATA(lx_catched).
        result = COND #( WHEN result IS SUPPLIED
                           THEN abap_false
                           ELSE THROW zcx_ca_param( textid   = lx_catched->ms_t100key
                                                    previous = lx_catched->previous
                                                    mv_msgty = lx_catched->mv_msgty
                                                    mv_msgv1 = CONV #( mv_key ) ) ).
    ENDTRY.
  ENDMETHOD.                    "is_valid


  METHOD is_valid_at_given_date.
    "-----------------------------------------------------------------*
    "   Check whether the user is valid at a specific date
    "-----------------------------------------------------------------*
    result = abap_true.
    "User is always valid if no date is maintained
    IF ms_logon_data-gltgb IS NOT INITIAL                  AND
       iv_date             NOT BETWEEN ms_logon_data-gltgv AND
                                       ms_logon_data-gltgb.
      result = abap_false.
    ENDIF.

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "User is not valid anymore
      DATA(lx_error) = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                           iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                           iv_class    = 'ZCL_CA_WF_USER'
                                                           iv_method   = 'CHECK_VALIDITY'
                                                           "Specified user name &1 is invalid
                                                           is_msg      = VALUE #( msgty = c_msgty_e
                                                                                  msgid = 'HRASR00_WC'
                                                                                  msgno = '010'
                                                                                  msgv1 = CONV #( mv_key ) )
                                                           iv_subrc    = 4  ) )  ##no_text.
      IF lx_error IS BOUND.
        RAISE EXCEPTION lx_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "is_valid_at_given_data


  METHOD set_pernr.
    "-----------------------------------------------------------------*
    "   Set personnel number late
    "-----------------------------------------------------------------*
    mv_pernr = iv_pernr.
  ENDMETHOD.                    "set_pernr


  METHOD zif_ca_workflow~check_existence.
    "-----------------------------------------------------------------*
    "   Check existence of the object and preparing other data
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_return  TYPE bapiret2_t.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username      = mv_key
        cache_results = abap_true
      IMPORTING
        address       = ms_address
        logondata     = ms_logon_data
        islocked      = ms_is_locked
      TABLES
        return        = lt_return.

    IF line_exists( lt_return[ type   = c_msgty_e
                               id     = '01'
                               number = '124' ] ).
      DATA(lx_dbacc) = CAST zcx_ca_dbacc( zcx_ca_error=>create_exception(
                                                               iv_excp_cls = zcx_ca_dbacc=>c_zcx_ca_dbacc
                                                               iv_function = 'BAPI_USER_GET_DETAIL'
                                                               it_return   = lt_return ) )  ##no_text.
      IF lx_dbacc IS BOUND.
        RAISE EXCEPTION lx_dbacc.
      ENDIF.

    ELSE.
      DATA(lx_param) = CAST zcx_ca_param( zcx_ca_error=>create_exception(
                                                               iv_excp_cls = zcx_ca_param=>c_zcx_ca_param
                                                               iv_function = 'BAPI_USER_GET_DETAIL'
                                                               it_return   = lt_return ) )  ##no_text.
      IF lx_param IS BOUND.
        RAISE EXCEPTION lx_param.
      ENDIF.
    ENDIF.

    mv_agent = |{ swfco_org_user WIDTH = 2 }{ mv_key }|.

    mbo_usr01-instid = mv_key.
    mbo_user-instid  = mv_key.
  ENDMETHOD.                    "zif_ca_workflow~check_existence

ENDCLASS.
