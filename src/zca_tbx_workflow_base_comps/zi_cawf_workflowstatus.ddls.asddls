@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA-WF: Workflow status (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICAWFSTATUS',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true,
  buffering: { status: #ACTIVE,
               type: #FULL
               }
  }
@ObjectModel: {
  representativeKey: 'WorkflowStatus',
  semanticKey: [ 'LogObject',
                 'LogSubobject',
                 'WorkflowStatus' ],
  usageType: { dataClass: #META,
               sizeCategory: #S,
               serviceQuality: #A
               }
  }
@Metadata: {
  ignorePropagatedAnnotations: true,
  allowExtensions: true
  }
define view ZI_CAWF_WorkflowStatus
  as select from zcawf_status

  association [1..1] to ZI_CA_BAL_Object           as _BAL_Object          on  $projection.LogObject = _BAL_Object.LogObject

  association [1..1] to ZI_CAWF_ActiveWFAppl       as _ActiveWFApplication on  $projection.LogObject    = _ActiveWFApplication.LogObject
                                                                           and $projection.LogSubobject = _ActiveWFApplication.LogSubobject

  association [0..*] to ZI_CAWF_WorkflowStatusText as _Text                on  $projection.LogObject      = _Text.LogObject
                                                                           and $projection.LogSubobject   = _Text.LogSubobject
                                                                           and $projection.WorkflowStatus = _Text.WorkflowStatus
{
      @ObjectModel.foreignKey.association: '_BAL_Object'
  key object    as LogObject,
      @ObjectModel.foreignKey.association: '_ActiveWFApplication'
  key subobject as LogSubobject,
      @ObjectModel.text.association: '_Text'
  key wf_status as WorkflowStatus,

      //Associations
      _BAL_Object,
      _ActiveWFApplication,
      _Text
}

