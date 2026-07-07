@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA: BAL Subobject (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICABALSUBOBJ',
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
define view ZI_CA_BAL_Subobject
  as select from balsub

  association [1..1] to ZI_CA_BAL_Object        as _BAL_Object on  $projection.LogObject = _BAL_Object.LogObject

  association [0..*] to ZI_CA_BAL_SubobjectText as _Text       on  $projection.LogObject    = _Text.LogObject
                                                               and $projection.LogSubobject = _Text.LogSubobject

{
      @ObjectModel.foreignKey.association: '_BAL_Object'
  key object    as LogObject,
  key subobject as LogSubobject,

      /* Associations */
      _BAL_Object,
      _Text
}
