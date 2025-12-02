@AbapCatalog.sqlViewName: 'ZFI45_2'
@ClientDependent: true
@AbapCatalog.compiler.compareFilter: true
@EndUserText.label: 'ZFI45_2'
define view ZZFI45_2 as 
SELECT FROM BSEG
{
BELNR,LIFNR,MAX(BVTYP) AS BVTYP
}
WHERE BVTYP<>''
GROUP BY BELNR,LIFNR
 


  
