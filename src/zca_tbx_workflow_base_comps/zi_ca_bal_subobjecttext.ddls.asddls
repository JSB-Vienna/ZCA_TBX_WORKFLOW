@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA: BAL Subobject description (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICABALSUBOBJTX',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true,
  buffering: { status: #ACTIVE,
               type: #FULL
               }
  }
@ObjectModel: {
  dataCategory: #TEXT,
  representativeKey: 'LogSubobject',
  semanticKey: [ 'Language',
                 'LogObject',
                 'LogSubobject' ],
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
define view ZI_CA_BAL_SubobjectText
  as select from balsubt

  association [0..1] to I_Language          as _Language      on  $projection.Language = _Language.Language

  association [1..1] to ZI_CA_BAL_Object    as _BAL_Object    on  $projection.LogObject = _BAL_Object.LogObject

  association [1..1] to ZI_CA_BAL_Subobject as _BAL_Subobject on  $projection.LogObject    = _BAL_Subobject.LogObject
                                                              and $projection.LogSubobject = _BAL_Subobject.LogSubobject

{
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
  key spras     as Language,
      @ObjectModel.foreignKey.association: '_BAL_Object'
  key object    as LogObject,
      @ObjectModel.foreignKey.association: '_BAL_Subobject'
  key subobject as LogSubobject,
      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.8
      subobjtxt as BAL_Subobject_Text,

      /* Associations */
      _Language,
      _BAL_Object,
      _BAL_Subobject
}
