@EndUserText.label: 'Parameters for Stock Transfer'
@Metadata.allowExtensions: true
define abstract entity Z_A_StockTransfer_Params
{
  @EndUserText.label: 'Target Storage Location'
  TargetStorageLocation : abap.char(4);

}
