@AbapCatalog.sqlViewName: 'ZVOUCHER'
@ClientDependent: true
@AbapCatalog.compiler.compareFilter: true
@EndUserText.label: 'ZVOUCHER'
define view ZZVOUCHER as 
select from bkpf H
inner join bseg D on H.belnr=D.belnr and H.bukrs=D.bukrs and  H.gjahr=D.gjahr
inner join skat C on C.saknr=D.hkont and C.ktopl='BARZ'
{
H.mandt,H.bukrs,D.umskz,D.buzei,H.belnr,H.gjahr,H.monat,H.bldat,budat,D.hkont,C.txt20,D.kunnr,D.lifnr,kostl,
case when D.shkzg='S' then FLOOR(D.dmbtr*100) else 0 end as BED_IRR,
case when D.shkzg='H' then FLOOR(D.dmbtr*100) else 0 end as BES_IRR,
case when D.shkzg='S' then (case when H.waers='IRR' then FLOOR(D.wrbtr*100) else D.wrbtr end)  else 0 end as BED,
case when D.shkzg='H' then (case when H.waers='IRR' then FLOOR(D.wrbtr*100) else D.wrbtr end)  else 0 end as BES,H.waers,
D.augbl,D.vbeln,D.vbel2,D.ebeln,D.ebelp,D.bschl,
D.valut,D.sgtxt,D.werks,D.aufnr,D.matnr,D.bvtyp,
case when D.shkzg='S' then D.menge else D.menge*-1 end as QTY,
D.meins,D.awkey,H.blart,cputm,xblnr,D.koart,usnam,tcode,FLOOR(kursf*1000) as KURSF
}
where H.bstat <> 'M' and H.bstat <> 'S'
--WHERE AND H.BELNR='9800758127'


