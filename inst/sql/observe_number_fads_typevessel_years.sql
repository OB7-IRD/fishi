-------IMZILEN TAHA -- 22/08/2023
------- OB7 - IRD/MARBEC

WITH A AS (select o.label1 AS ocean,
					ct1.iso3code AS flag,
					CASE
					WHEN vt.code::numeric IN (1,2,3)	THEN 'BB'
					WHEN vt.code::numeric IN (4,5,6)	THEN 'PS'
					WHEN vt.code::numeric IN (7)		THEN 'LL'
					WHEN vt.code::numeric IN (10)		THEN 'SV'
					ELSE 'OTH'
					END AS vessel_type,
					extract(year FROM r.date)::integer AS fishing_year,
					v.label1 AS vessel_name,
					oo.code as object_code_activity,
					oo.label1 as object_label_activity,
					tbo.code as buoy_code_activity,
					tbo.label1 as buoy_label_activity,
					tb.code as buoy_id
					FROM
					ps_common.trip AS t
					INNER JOIN common.ocean AS o
					ON t.ocean = o.topiaid
					LEFT OUTER JOIN common.harbour AS h
					ON t.landingharbour = h.topiaid
					INNER JOIN common.country AS ct2
					ON h.country = ct2.topiaid
					INNER JOIN common.vessel AS v
					ON t.vessel = v.topiaid
					INNER JOIN common.vesseltype AS vt
					ON v.vesseltype = vt.topiaid
					INNER JOIN common.country AS ct1
					ON v.flagcountry = ct1.topiaid
					INNER JOIN ps_logbook.route AS r
					ON r.trip = t.topiaid
					INNER JOIN ps_logbook.activity AS a
					ON a.route = r.topiaid
					--LEFT OUTER JOIN common.fpazone AS f
					--ON a.currentfpazone = f.topiaid
					LEFT OUTER JOIN ps_common.schooltype st 
					ON a.schooltype = st.homeid 
					LEFT OUTER JOIN ps_common.vesselactivity AS va
					ON a.vesselactivity = va.topiaid
					LEFT OUTER JOIN ps_logbook.floatingobject AS fo
					ON fo.activity = a.topiaid
					LEFT OUTER JOIN ps_common.objectoperation AS oo
					ON fo.objectoperatiON = oo.topiaid
					LEFT OUTER JOIN ps_logbook.transmittingbuoy AS tb
					ON tb.floatingobject = fo.topiaid
					LEFT OUTER JOIN ps_common.transmittingbuoyoperation AS tbo
					ON tb.transmittingbuoyoperation = tbo.topiaid
					
					WHERE o.label1='Atlantic' AND ct1.iso3code='FRA'AND (oo.code = '1' OR tbo.code ='3') 
),
B AS (SELECT fishing_year , count(*) as No_buoys_total
FROM A
WHERE buoy_code_activity = '3' AND fishing_year between 2013 AND 2022
group by fishing_year
),
C AS (SELECT fishing_year , count(distinct vessel_name) No_ps, count(*) as No_buoys_ps
FROM A
WHERE  vessel_type='PS' AND buoy_code_activity = '3' AND fishing_year between 2013 AND 2022 
group by fishing_year
),
D AS (SELECT fishing_year ,count(distinct vessel_name) No_supply, count(*) as No_buoys_supply
FROM A
WHERE buoy_code_activity = '3' AND fishing_year between 2013 AND 2022 AND vessel_type='SV'
group by fishing_year)

SELECT B.fishing_year,C.No_ps,C.No_buoys_ps,COALESCE(D.No_supply,0) No_supply,COALESCE(D.No_buoys_supply,0) No_buoys_supply,B.No_buoys_total
FROM B
LEFT JOIN C
ON B.fishing_year = C.fishing_year  
LEFT JOIN D
ON B.fishing_year = D.fishing_year  
order by fishing_year
