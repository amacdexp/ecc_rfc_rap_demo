@EndUserText.label: 'Remote ECC Storage Locations (via RFC)'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_ECC_STLOC'
@AbapCatalog.extensibility.extensible : false

define custom entity ZI_EccStorageLocation
{
  @UI.lineItem: [ { position: 10, label: 'Plant' } ]
  key plant : abap.char(4);

  @UI.lineItem: [ { position: 20, label: 'Storage Location' } ]
  key lgort : abap.char(4);

  @UI.lineItem: [ { position: 30, label: 'Description' } ]
  lgobe : abap.char(16);
}
