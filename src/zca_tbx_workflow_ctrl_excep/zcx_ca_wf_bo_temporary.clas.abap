"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Temporary error</p>
CLASS zcx_ca_wf_bo_temporary DEFINITION
  PUBLIC
  INHERITING FROM cx_bo_temporary
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .
    INTERFACES if_xo_const_message .

    ALIASES c_msgty_e
      FOR if_xo_const_message~error .
    ALIASES c_msgty_i
      FOR if_xo_const_message~info .
    ALIASES c_msgty_s
      FOR if_xo_const_message~success .
    ALIASES c_msgty_w
      FOR if_xo_const_message~warning .
    ALIASES ms_default_textid
      FOR if_t100_message~default_textid .
    ALIASES ms_t100key
      FOR if_t100_message~t100key .
    ALIASES mv_msgty
      FOR if_t100_dyn_msg~msgty .
    ALIASES mv_msgv1
      FOR if_t100_dyn_msg~msgv1 .
    ALIASES mv_msgv2
      FOR if_t100_dyn_msg~msgv2 .
    ALIASES mv_msgv3
      FOR if_t100_dyn_msg~msgv3 .
    ALIASES mv_msgv4
      FOR if_t100_dyn_msg~msgv4 .

    CONSTANTS:
      BEGIN OF zcx_ca_wf_bo_temporary,
        msgid TYPE symsgid VALUE 'ZCA_WF_BASE',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'CLASS_NAME',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_wf_bo_temporary .
    CONSTANTS:
      BEGIN OF any_other_msg,
        msgid TYPE symsgid VALUE 'S1',
        msgno TYPE symsgno VALUE '897',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE 'MV_MSGV4',
      END OF any_other_msg .
    CONSTANTS:
      BEGIN OF error_func_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_func_call .
    CONSTANTS:
      BEGIN OF error_meth_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_meth_call .
    CONSTANTS:
      BEGIN OF error_subr_call,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_subr_call .
    CONSTANTS:
      BEGIN OF error_unknown_comp,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_unknown_comp .
    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_wf_bo_temporary TYPE seoclsname VALUE 'ZCX_CA_WF_BO_TEMPORARY' ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">Message error type for comparison</p>
    CONSTANTS c_msgty_eax TYPE char3 VALUE 'EAX' ##NO_TEXT.
    "! <p class="shorttext synchronized" lang="en">All messages passed from BAPI call</p>
    DATA mt_return TYPE bapiret2_t READ-ONLY .
*     s i n g l e   v a l u e s
    "! <p class="shorttext synchronized" lang="en">Return code</p>
    DATA mv_subrc TYPE syst_subrc .

    "! <p class="shorttext synchronized" lang="en">Create exception instance</p>
    "!
    "! @parameter iv_excp_cls   | <p class="shorttext synchronized" lang="en">Type of new exception</p>
    "! @parameter iv_function   | <p class="shorttext synchronized" lang="en">Name of function module or program (addition to subroutine)</p>
    "! @parameter iv_class      | <p class="shorttext synchronized" lang="en">Name of executed class (addition to IV_METHOD)</p>
    "! @parameter iv_method     | <p class="shorttext synchronized" lang="en">Name of executed method (addition to IV_CLASS)</p>
    "! @parameter iv_subroutine | <p class="shorttext synchronized" lang="en">Name of subroutine</p>
    "! @parameter iv_msgty      | <p class="shorttext synchronized" lang="en">Message type</p>
    "! @parameter is_msg        | <p class="shorttext synchronized" lang="en">Message details (pass ONLY if not from SYST!!)</p>
    "! @parameter is_return     | <p class="shorttext synchronized" lang="en">Message details from BAPI call</p>
    "! @parameter it_return     | <p class="shorttext synchronized" lang="en">All messages from BAPI call</p>
    "! @parameter iv_subrc      | <p class="shorttext synchronized" lang="en">Return code</p>
    "! @parameter ix_error      | <p class="shorttext synchronized" lang="en">Catched exception</p>
    "! @parameter rx_excep      | <p class="shorttext synchronized" lang="en">Created exception instance</p>
    CLASS-METHODS create_exception
      IMPORTING
        !iv_excp_cls    TYPE seoclsname DEFAULT c_zcx_ca_wf_bo_temporary
        !iv_function    TYPE rs38l_fnam OPTIONAL
        !iv_class       TYPE seoclsname OPTIONAL
        !iv_method      TYPE seocmpname OPTIONAL
        !iv_subroutine  TYPE formname OPTIONAL
        !iv_msgty       TYPE syst_msgty DEFAULT c_msgty_e
        !is_msg         TYPE dcmessage OPTIONAL
        !is_return      TYPE bapiret2 OPTIONAL
        !it_return      TYPE bapiret2_t OPTIONAL
        VALUE(iv_subrc) TYPE sysubrc OPTIONAL
        !ix_error       TYPE REF TO cx_root OPTIONAL
      RETURNING
        VALUE(rx_excep) TYPE REF TO zcx_ca_wf_bo_temporary .
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor
      IMPORTING
        !textid         LIKE if_t100_message=>t100key OPTIONAL
        !previous       LIKE previous OPTIONAL
        !class_name     TYPE seoclname OPTIONAL
        !instance       TYPE REF TO bi_object OPTIONAL
        !collection     TYPE sibfexctab OPTIONAL
        !log_no         TYPE balognr OPTIONAL
        !log_msg_no     TYPE balmnr OPTIONAL
        !force_dataflow TYPE xfeld OPTIONAL
        !mt_return      TYPE bapiret2_t OPTIONAL
        !mv_subrc       TYPE syst_subrc OPTIONAL
        !mv_msgty       TYPE symsgty OPTIONAL
        !mv_msgv1       TYPE symsgv OPTIONAL
        !mv_msgv2       TYPE symsgv OPTIONAL
        !mv_msgv3       TYPE symsgv OPTIONAL
        !mv_msgv4       TYPE symsgv OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Set catched exception instance later</p>
    "!
    "! @parameter ix_error | <p class="shorttext synchronized" lang="en">Catched exception</p>
    METHODS set_previous_late
      IMPORTING
        !ix_error TYPE REF TO cx_root .
  PROTECTED SECTION.


  PRIVATE SECTION.
    CLASS-DATA:
      "! <p class="shorttext synchronized" lang="en">Structure for saving original message</p>
      ms_other_msg TYPE scx_t100key.
ENDCLASS.



CLASS zcx_ca_wf_bo_temporary IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous       = previous
        class_name     = class_name
        instance       = instance
        collection     = collection
        log_no         = log_no
        log_msg_no     = log_msg_no
        force_dataflow = force_dataflow.
    me->mt_return = mt_return .
    me->mv_subrc = mv_subrc .
    me->mv_msgty = mv_msgty .
    me->mv_msgv1 = mv_msgv1 .
    me->mv_msgv2 = mv_msgv2 .
    me->mv_msgv3 = mv_msgv3 .
    me->mv_msgv4 = mv_msgv4 .
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.


  METHOD create_exception.
    "-----------------------------------------------------------------*
    "   Create exception instance of an inheriting exception class
    "   and tries to use the original message. Otherwise a common
    "   message will be used.
    "-----------------------------------------------------------------*
    "Extract message from input
    DATA(ls_return) = zcx_ca_error=>extract_message( iv_msgty  = iv_msgty
                                                     iv_subrc  = iv_subrc
                                                     is_msg    = is_msg
                                                     is_return = is_return
                                                     it_return = it_return
                                                     ix_error  = ix_error ).

    "If no error message was found, e. g. in BAPI messages, leave
    IF ls_return IS INITIAL AND
       iv_subrc  EQ 0.
      RETURN.
    ENDIF.

    "Message is incomplete -> provide a common message
    IF ls_return-id     IS INITIAL OR
       ls_return-number IS INITIAL.
      IF iv_function IS NOT INITIAL.
        "Exception triggered in function module &1 (RC = &2)
        ls_return-id         = error_func_call-msgid.
        ls_return-number     = error_func_call-msgno.
        ls_return-message_v1 = iv_function.

      ELSEIF iv_method IS NOT INITIAL.
        "Exception triggered in class->method &1 (RC = &2)
        ls_return-id         = error_meth_call-msgid.
        ls_return-number     = error_meth_call-msgno.
        ls_return-message_v1 = iv_class && '=>' && iv_method.

      ELSEIF iv_subroutine IS NOT INITIAL.
        "Exception triggered in subroutine &1 with return code &2
        ls_return-id         = error_subr_call-msgid.
        ls_return-number     = error_subr_call-msgno.
        ls_return-message_v1 = iv_subroutine && '(' && iv_function && ')'.
      ELSE.
        "Exception triggered in unknown component (&1) (RC = &2)
        ls_return-id         = error_unknown_comp-msgid.
        ls_return-number     = error_unknown_comp-msgno.
        ls_return-message_v1 = 'not passed'(e01).
      ENDIF.

      ls_return-message_v2 = condense( CONV symsgv( iv_subrc ) ).
    ENDIF.

    "Set values into a common structure
    CLEAR ms_other_msg.
    ms_other_msg-msgid = ls_return-id.
    ms_other_msg-msgno = ls_return-number.
    IF ls_return-message_v1 IS NOT INITIAL.
      ms_other_msg-attr1 = 'MV_MSGV1' ##no_text.
    ENDIF.
    IF ls_return-message_v2 IS NOT INITIAL.
      ms_other_msg-attr2 = 'MV_MSGV2' ##no_text.
    ENDIF.
    IF ls_return-message_v3 IS NOT INITIAL.
      ms_other_msg-attr3 = 'MV_MSGV3' ##no_text.
    ENDIF.
    IF ls_return-message_v4 IS NOT INITIAL.
      ms_other_msg-attr4 = 'MV_MSGV4' ##no_text.
    ENDIF.

    "Create exception with available message
    CREATE OBJECT rx_excep TYPE (iv_excp_cls)
      EXPORTING
        textid     = ms_other_msg
        previous   = ix_error
        class_name = iv_class
        mt_return  = it_return
        mv_subrc   = iv_subrc
        mv_msgty   = iv_msgty
        mv_msgv1   = ls_return-message_v1
        mv_msgv2   = ls_return-message_v2
        mv_msgv3   = ls_return-message_v3
        mv_msgv4   = ls_return-message_v4.
  ENDMETHOD.                    "create_exception


  METHOD set_previous_late.
    "-----------------------------------------------------------------*
    "   Set catched exception after creation of the exception
    "-----------------------------------------------------------------*
    previous = ix_error.
  ENDMETHOD.                    "set_previous_late
ENDCLASS.
