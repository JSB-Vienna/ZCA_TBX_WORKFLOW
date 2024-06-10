CLASS zcx_ca_wf_om_employee DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_wf_om_org_management
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF zcx_ca_wf_om_employee,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_wf_om_employee .
    CONSTANTS:
      BEGIN OF subtyp_missing,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF subtyp_missing .
    CONSTANTS:
      BEGIN OF pernr_not_found,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF pernr_not_found .
    CONSTANTS:
      BEGIN OF central_pernr_not_found,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '019',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF central_pernr_not_found .
    CONSTANTS:
      BEGIN OF no_pers_ids_2_central_pernr,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '020',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_pers_ids_2_central_pernr .
    CONSTANTS:
      BEGIN OF sap_user_not_available,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '015',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF sap_user_not_available .
    CONSTANTS:
      BEGIN OF lowest_pernr,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '018',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF lowest_pernr .
    CONSTANTS c_zcx_ca_wf_om_employee TYPE seoclsname VALUE 'ZCX_CA_WF_OM_EMPLOYEE' ##NO_TEXT.

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



CLASS zcx_ca_wf_om_employee IMPLEMENTATION.


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
      if_t100_message~t100key = zcx_ca_wf_om_employee .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
