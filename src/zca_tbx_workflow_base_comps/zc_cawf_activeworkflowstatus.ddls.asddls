@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA-WF: Active workflow status'
@VDM: {
  viewType: #CONSUMPTION,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZCCAWFACTWFSTAT',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true
  }
define view ZC_CAWF_ActiveWorkflowStatus

  as select from ZI_CAWF_WorkflowStatus
  
{
  key LogObject                      as Log_Object,
  key LogSubobject                   as Log_Subobject,
  key WorkflowStatus                 as Workflow_Status,
      _ActiveWFApplication.Is_Active as Is_Active
}

where
  _ActiveWFApplication.Is_Active = 'X'
