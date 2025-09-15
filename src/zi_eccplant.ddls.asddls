@EndUserText.label: 'Remote ECC Plants (via RFC)'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_ECC_PLANTS'
@AbapCatalog.extensibility.extensible : false

define custom entity ZI_EccPlant
{
  @UI.lineItem: [ { position: 10, label: 'Plant' } ]
  key plant : werks_d;

  @UI.lineItem: [ { position: 20, label: 'Plant Name' } ]
  name : name1_gp;
}
