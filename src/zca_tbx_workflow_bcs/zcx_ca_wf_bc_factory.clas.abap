"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while creating a BC instance</p>
CLASS zcx_ca_wf_bc_factory DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_param
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF no_or_multiple_key_fields,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '123',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_or_multiple_key_fields .

    CONSTANTS:
      BEGIN OF is_no_workflow_class,
        msgid TYPE symsgid VALUE 'WD',
        msgno TYPE symsgno VALUE '360',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF is_no_workflow_class .

    "! <p class="shorttext synchronized" lang="en">My own name</p>
    CONSTANTS c_zcx_ca_wf_bcs_factory TYPE seoclsname VALUE 'ZCX_CA_WF_BCS_FACTORY' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        !textid      LIKE if_t100_message=>t100key OPTIONAL
        !previous    LIKE previous OPTIONAL
        !mt_return   TYPE bapiret2_t OPTIONAL
        !mv_subrc    TYPE syst_subrc OPTIONAL
        !mv_msgty    TYPE symsgty OPTIONAL
        !mv_msgv1    TYPE symsgv OPTIONAL
        !mv_msgv2    TYPE symsgv OPTIONAL
        !mv_msgv3    TYPE symsgv OPTIONAL
        !mv_msgv4    TYPE symsgv OPTIONAL
        !mv_severity TYPE t_severity OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_ca_wf_bc_factory IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous    = previous
        mt_return   = mt_return
        mv_subrc    = mv_subrc
        mv_msgty    = mv_msgty
        mv_msgv1    = mv_msgv1
        mv_msgv2    = mv_msgv2
        mv_msgv3    = mv_msgv3
        mv_msgv4    = mv_msgv4
        mv_severity = mv_severity.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
