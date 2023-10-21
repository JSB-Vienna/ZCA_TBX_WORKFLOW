class ZCX_CA_WF_OM_EMPLOYEE definition
  public
  inheriting from ZCX_CA_WF_OM_ORG_MANAGEMENT
  create public .

public section.

  constants:
    begin of ZCX_CA_WF_OM_EMPLOYEE,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_WF_OM_EMPLOYEE .
  constants:
    begin of SUBTYP_MISSING,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SUBTYP_MISSING .
  constants:
    begin of PERNR_NOT_FOUND,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value '',
    end of PERNR_NOT_FOUND .
  constants:
    begin of SAP_USER_NOT_AVAILABLE,
      msgid type symsgid value 'ZCA_WF_OM',
      msgno type symsgno value '015',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SAP_USER_NOT_AVAILABLE .
  constants C_ZCX_CA_WF_OM_EMPLOYEE type SEOCLSNAME value 'ZCX_CA_WF_OM_EMPLOYEE' ##NO_TEXT.

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



CLASS ZCX_CA_WF_OM_EMPLOYEE IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = ZCX_CA_WF_OM_EMPLOYEE .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
