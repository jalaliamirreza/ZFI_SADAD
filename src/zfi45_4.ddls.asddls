@AbapCatalog.sqlViewName: 'ZFI45_4'
@ClientDependent: true
@AbapCatalog.compiler.compareFilter: true
@EndUserText.label: 'ZFI45_4'
define view ZZFI45_4 as 
SELECT FROM ZFI45_3
{
LIFNR,MAX(IBAN) AS IBAN
}
GROUP BY LIFNR
