CLASS zcx_ca_wf_om_org_model DEFINITION
  PUBLIC
  INHERITING FROM zcx_ca_wf_om_org_management
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF is_not_a_leader_of_this_ou,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '021',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF is_not_a_leader_of_this_ou .
    CONSTANTS:
      BEGIN OF zcx_ca_wf_om_org_model,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_wf_om_org_model .
    CONSTANTS:
      BEGIN OF sap_id_not_found,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF sap_id_not_found .
    CONSTANTS:
      BEGIN OF error_occurred,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF error_occurred .
    CONSTANTS:
      BEGIN OF no_owner_to_orgunit,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_owner_to_orgunit .
    CONSTANTS:
      BEGIN OF org_unit_has_no_manager,
        msgid TYPE symsgid VALUE 'HRHAP00_TEMPLATE',
        msgno TYPE symsgno VALUE '810',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF org_unit_has_no_manager .
    CONSTANTS:
      BEGIN OF no_valid_pernr_found,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '006',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_valid_pernr_found .
    CONSTANTS:
      BEGIN OF too_less_data,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '007',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF too_less_data .
    CONSTANTS:
      BEGIN OF no_valid_leader_found,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '008',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_valid_leader_found .
    CONSTANTS:
      BEGIN OF object_type_not_allowed,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF object_type_not_allowed .
    CONSTANTS:
      BEGIN OF a_person_can_t_have_members,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '016',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF a_person_can_t_have_members .
    CONSTANTS:
      BEGIN OF no_people_found_to_scope,
        msgid TYPE symsgid VALUE 'ZCA_WF_OM',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE 'MV_MSGV3',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_people_found_to_scope .
    CONSTANTS c_zcx_ca_wf_om_org_model TYPE seoclsname VALUE 'ZCX_CA_WF_OM_ORG_MODEL' ##NO_TEXT.

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



CLASS zcx_ca_wf_om_org_model IMPLEMENTATION.


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
      if_t100_message~t100key = zcx_ca_wf_om_org_model .
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
