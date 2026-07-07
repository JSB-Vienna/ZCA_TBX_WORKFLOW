@ClientHandling: {
  type: #CLIENT_INDEPENDENT,
  algorithm: #NONE
  }
@EndUserText.label: 'CA: BAL Object (basic)'
@VDM: {
  viewType: #BASIC,
  lifecycle.contract.type: #NONE
  }
@AccessControl.authorizationCheck: #NOT_REQUIRED
@AbapCatalog: {
  sqlViewName: 'ZICABALOBJ',
  dataMaintenance: #DISPLAY_ONLY,
  compiler.compareFilter: true,
  preserveKey: true,
  buffering: { status: #ACTIVE,
               type: #FULL
               }
  }
@ObjectModel: {
  compositionRoot: true,
  representativeKey: 'LogObject',
  semanticKey: 'LogObject',
  usageType: { dataClass: #META,
               sizeCategory: #S,
               serviceQuality: #A
               }
  }
@Metadata: {
  ignorePropagatedAnnotations: true,
  allowExtensions: true
  }
define view ZI_CA_BAL_Object
  as select from balobj

  association [0..*] to ZI_CA_BAL_ObjectText as _Text on $projection.LogObject = _Text.LogObject

{
  key object                as LogObject,
      abap_language_version as AbapLanguageVersion,

      /* Associations */
      _Text
}
