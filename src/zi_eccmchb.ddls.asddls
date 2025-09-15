@EndUserText.label: 'RFC_READ_TABLE on MCHB'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_ECC_MCHB'
@AbapCatalog.extensibility.extensible : false

define custom entity ZI_EccMchb
{
  @UI.lineItem: [ { position: 10,  label: 'Material' } ]
  @Consumption.valueHelpDefinition: [ {
    entity: { name: 'ZI_EccMaterial', element: 'matnr' }
  } ]
  key matnr : abap.char(40);

  @UI.lineItem: [ { position: 20,  label: 'Plant' } ]
  key plant : abap.char(4);

  @UI.lineItem: [ { position: 30,  label: 'SLoc' } ]
  key lgort : abap.char(4);

  @UI.lineItem: [ { position: 40,  label: 'Batch' } ]
  key charg : abap.char(10);

  @UI.lineItem: [ { position: 50,  label: 'Unrestricted' } ]
  @Semantics.quantity.unitOfMeasure: 'uom'
  clabs : abap.quan(13,3);

  @UI.lineItem: [ { position: 60,  label: 'UoM' } ]
  uom   : abap.unit(3);
}
