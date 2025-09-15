@EndUserText.label: 'Remote ECC Batch Stock (action-enabled)'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_ECC_MCHB'
@Metadata.allowExtensions: true
define root view entity ZI_ECCMchbST
  as select from zst_mchb_stub
{

  @UI.lineItem: [ 
  -- pos 1
  { position: 10,  label: 'Material' },
  
  -- action button
  { type: #FOR_ACTION,
    dataAction: 'TransferStock',
    label: 'Transfer to SLoc' }
  ]
  
  @Consumption.valueHelpDefinition: [ {
    entity: { name: 'ZI_EccMaterial', element: 'matnr' }
  } ]
  key matnr,
  
  @UI.lineItem: [ { position: 20,  label: 'Plant' } ]
  key plant,
  
  @UI.lineItem: [ { position: 30,  label: 'SLoc' } ]
  key lgort,
  
  @UI.lineItem: [ { position: 40,  label: 'Batch' } ]  
  key charg,

  @UI.lineItem: [ { position: 50,  label: 'Unrestricted' } ]
  @Semantics.quantity.unitOfMeasure: 'uom'
  clabs,

  @UI.lineItem: [ { position: 60,  label: 'UoM' } ]
  uom
}
