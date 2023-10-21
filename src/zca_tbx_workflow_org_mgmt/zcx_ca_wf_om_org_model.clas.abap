class ZCX_CA_WF_OM_ORG_MODEL definition
  public
  inheriting from ZCX_CA_WF_OM_ORG_MANAGEMENT
  create public .

public section.

  constants:
    begin of ZCX_CA_WF_OM_ORG_MODEL,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_WF_OM_ORG_MODEL .
  constants:
    begin of SAP_ID_NOT_FOUND,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '002',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SAP_ID_NOT_FOUND .
  constants:
    begin of ERROR_OCCURRED,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ERROR_OCCURRED .
  constants:
    begin of NO_OWNER_TO_ORGUNIT,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '005',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value '',
    end of NO_OWNER_TO_ORGUNIT .
  constants:
    begin of NO_VALID_PERNR_FOUND,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_VALID_PERNR_FOUND .
  constants:
    begin of TOO_LESS_DATA,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '007',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TOO_LESS_DATA .
  constants:
    begin of NO_VALID_LEADER_FOUND,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_VALID_LEADER_FOUND .
  constants:
    begin of OBJECT_TYPE_NOT_ALLOWED,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of OBJECT_TYPE_NOT_ALLOWED .
  constants:
    begin of A_PERSON_CAN_T_HAVE_MEMBERS,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '016',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of A_PERSON_CAN_T_HAVE_MEMBERS .
  constants:
    begin of NO_PEOPLE_FOUND_TO_SCOPE,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '017',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value '',
    end of NO_PEOPLE_FOUND_TO_SCOPE .
  constants C_ZCX_CA_WF_OM_ORG_MODEL type SEOCLSNAME value 'ZCX_CA_WF_OM_ORG_MODEL' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_WF_OM_ORG_MODEL IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_WF_OM_ORG_MODEL .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
