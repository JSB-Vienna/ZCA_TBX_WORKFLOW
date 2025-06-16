@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA-WF: Active workflow application (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICAWFACTVAPPL',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true,
  buffering: { status: #ACTIVE,
               type: #FULL
               }
  }
@ObjectModel: {
  compositionRoot: true,
  representativeKey: 'LogSubobject',
  semanticKey: [ 'LogObject',
                 'LogSubobject' ],
  usageType: { dataClass: #META,
               sizeCategory: #S,
               serviceQuality: #A
               }
  }
@Metadata: {
  ignorePropagatedAnnotations: true,
  allowExtensions: true
  }
define view ZI_CAWF_ActiveWFAppl
  as select from zcawf_actvappl

  association [1..1] to ZI_CA_BAL_Object    as _BAL_Object    on  $projection.LogObject = _BAL_Object.LogObject

  association [1..1] to ZI_CA_BAL_Subobject as _BAL_Subobject on  $projection.LogObject    = _BAL_Subobject.LogObject
                                                              and $projection.LogSubobject = _BAL_Subobject.LogSubobject

{
      @ObjectModel.foreignKey.association: '_BAL_Object'
  key object    as LogObject,
      @ObjectModel.foreignKey.association: '_BAL_Subobject'
  key subobject as LogSubobject,
      @Semantics.booleanIndicator: true
      is_active as Is_Active,

      /* Associations */
      _BAL_Object,
      _BAL_Subobject
}
