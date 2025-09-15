@EndUserText.label: 'Remote ECC Materials (via BAPI)'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_GET_ECC_MATERIALS'
@AbapCatalog.extensibility.extensible : false

define custom entity ZI_EccMaterial
{
  @UI.lineItem: [ { position: 10, label: 'Material' } ]
  @Search.defaultSearchElement: true
  key matnr :  abap.char(40); 

  @UI.lineItem: [ { position: 20, label: 'Description' } ]
  @Search.defaultSearchElement: false
  maktx : abap.char(40); 
}
