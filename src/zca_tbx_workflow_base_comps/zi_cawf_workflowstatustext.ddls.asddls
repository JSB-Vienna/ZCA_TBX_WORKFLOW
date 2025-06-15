@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA-WF: Workflow status description (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICAWFSTATUSTX',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true,
  buffering: { status: #ACTIVE,
               type: #FULL
               }
  }
@ObjectModel: {
  dataCategory: #TEXT,
  representativeKey: 'WorkflowStatus',
  semanticKey: [ 'Language',
                 'LogObject',
                 'LogSubobject',
                 'WorkflowStatus' ],
  usageType: { dataClass: #META,
               sizeCategory: #S,
               serviceQuality: #A
               }
  }
@Search.searchable: true
@Metadata: {
  ignorePropagatedAnnotations: true,
  allowExtensions: true
  }
define view ZI_CAWF_WorkflowStatusText
  as select from zcawf_statustx
        
  association [0..1] to I_Language             as _Language            on  $projection.Language = _Language.Language

  association [1..1] to ZI_CA_BAL_Object       as _BAL_Object          on  $projection.LogObject = _BAL_Object.LogObject

  association [1..1] to ZI_CAWF_ActiveWFAppl   as _ActiveWFApplication on  $projection.LogObject    = _ActiveWFApplication.LogObject
                                                                       and $projection.LogSubobject = _ActiveWFApplication.LogSubobject

  association [1..1] to ZI_CAWF_WorkflowStatus as _WorkflowStatus      on  $projection.LogObject      = _WorkflowStatus.LogObject
                                                                       and $projection.LogSubobject   = _WorkflowStatus.LogSubobject
                                                                       and $projection.WorkflowStatus = _WorkflowStatus.WorkflowStatus
{
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
  key langu     as Language,
      @ObjectModel.foreignKey.association: '_BAL_Object'
  key object    as LogObject,
      @ObjectModel.foreignKey.association: '_ActiveWFApplication'
  key subobject as LogSubobject,
      @ObjectModel.foreignKey.association: '_WorkflowStatus'
  key wf_status as WorkflowStatus,
      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.8
      descr_l   as DescriptionLong,
      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.8
      descr_s   as DescriptionShort,

      //Associations
      _Language,
      _BAL_Object,
      _ActiveWFApplication,
      _WorkflowStatus
}

where
  langu = $session.system_language   
