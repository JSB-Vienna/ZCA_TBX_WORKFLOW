@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA: BAL Object description (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICABALOBJTX',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true,
  buffering: { status: #ACTIVE,
               type: #FULL
               }
  }
@ObjectModel: {
  dataCategory: #TEXT,
  representativeKey: 'LogObject',
  semanticKey: [ 'Language',
                 'LogObject' ],
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
define view ZI_CA_BAL_ObjectText
  as select from balobjt

  association [0..1] to I_Language       as _Language   on $projection.Language = _Language.Language

  association [1..1] to ZI_CA_BAL_Object as _BAL_Object on $projection.LogObject = _BAL_Object.LogObject

{
      @Semantics.language: true
      @ObjectModel.foreignKey.association: '_Language'
  key spras  as Language,
      @ObjectModel.foreignKey.association: '_BAL_Object'
  key object as LogObject,
      @Semantics.text: true
      @Search.defaultSearchElement: true
      @Search.ranking: #HIGH
      @Search.fuzzinessThreshold: 0.8
      objtxt as BAL_Object_Text,

      /* Associations */
      _Language,
      _BAL_Object
}
