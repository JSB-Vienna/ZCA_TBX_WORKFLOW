"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow exception: Creating a new instance</p>
class ZCX_CA_WF_BO_CREATE_ERROR definition
  public
  inheriting from ZCX_CA_WF_BO_ERROR
  create public .

public section.

  constants:
    begin of ZCX_CA_WF_BO_CREATE_ERROR,
      msgid type symsgid value 'ZCA_WF_BASE',
      msgno type symsgno value '004',
      attr1 type scx_attrname value 'CLASS_NAME',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_CA_WF_BO_CREATE_ERROR .
  constants C_ZCX_CA_WF_BO_CREATE_ERROR type SEOCLSNAME value 'ZCX_CA_WF_BO_CREATE_ERROR' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !CLASS_NAME type SEOCLNAME optional
      !INSTANCE type ref to BI_OBJECT optional
      !COLLECTION type SIBFEXCTAB optional
      !LOG_NO type BALOGNR optional
      !LOG_MSG_NO type BALMNR optional
      !FORCE_DATAFLOW type XFELD optional
      !PARAMETERS type SIBFPARTAB optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_CA_WF_BO_CREATE_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
CLASS_NAME = CLASS_NAME
INSTANCE = INSTANCE
COLLECTION = COLLECTION
LOG_NO = LOG_NO
LOG_MSG_NO = LOG_MSG_NO
FORCE_DATAFLOW = FORCE_DATAFLOW
PARAMETERS = PARAMETERS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_WF_BO_CREATE_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
