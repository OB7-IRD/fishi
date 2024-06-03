-------------------------------------------------------------------------------------------
-- FADS - OBSERVE
-------------------------------------------------------------------------------------------
-- Generic extraction of the number of fads by year
-------------------------------------------------------------------------------------------
-- Taha imzilen <taha.imzilen@ird.fr>
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 22/08/2023 -- v1.0 -- TI -- initial version
-- 23/06/2024 -- v2.0 -- CL -- adapt to fishi
-------------------------------------------------------------------------------------------
SELECT 
	o.label1 AS ocean
	,ct1.iso3code AS flag
	,CASE
		WHEN vt.code::numeric IN (1,2,3)	THEN 'BB'
		WHEN vt.code::numeric IN (4,5,6)	THEN 'PS'
		WHEN vt.code::numeric IN (7)		THEN 'LL'
		WHEN vt.code::numeric IN (10)		THEN 'SV'
		ELSE 'OTH'
	END AS vessel_type
	,EXTRACT(year FROM r.date)::integer AS fishing_year
	,v.label1 AS vessel_name
	,oo.code as object_code_activity
	,oo.label1 as object_label_activity
	,tbo.code as buoy_code_activity
	,tbo.label1 as buoy_label_activity
	,tb.code as buoy_id

FROM
	ps_common.trip AS t
	INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
	LEFT OUTER JOIN common.harbour AS h ON t.landingharbour = h.topiaid
	INNER JOIN common.country AS ct2 ON h.country = ct2.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
	INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
	INNER JOIN ps_logbook.activity AS a ON a.route = r.topiaid
	LEFT OUTER JOIN ps_common.schooltype st ON a.schooltype = st.homeid 
	LEFT OUTER JOIN ps_common.vesselactivity AS va ON a.vesselactivity = va.topiaid
	LEFT OUTER JOIN ps_logbook.floatingobject AS fo ON fo.activity = a.topiaid
	LEFT OUTER JOIN ps_common.objectoperation AS oo ON fo.objectoperatiON = oo.topiaid
	LEFT OUTER JOIN ps_logbook.transmittingbuoy AS tb ON tb.floatingobject = fo.topiaid
	LEFT OUTER JOIN ps_common.transmittingbuoyoperation AS tbo ON tb.transmittingbuoyoperation = tbo.topiaid
					
WHERE 
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct1.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
	AND (oo.code = '1' OR tbo.code ='3') 
;