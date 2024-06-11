-------------------------------------------------------------------------------------------
-- CATCHES - SCHOOL TYPES - OBSERVE
-------------------------------------------------------------------------------------------
-- Generic extraction of the catches by school type from Observe
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2024-05-29 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
SELECT
	r.date AS activity_date
	,o.label1 AS ocean_label
	,CASE
		WHEN vt.code::numeric IN (1,2,3)	THEN 'BB'
		WHEN vt.code::numeric IN (4,5,6)	THEN 'PS'
		WHEN vt.code::numeric IN (7)		THEN 'LL'
		WHEN vt.code::numeric IN (10)		THEN 'SV'
		ELSE 'OTH'
	END AS gear
	,ct.iso3code AS fleet
	,ct.iso3code AS flag
	,s.code AS species_code
	,c.weight::numeric AS total_catch_weight 
	,st.homeid as school_code
	,o.code::numeric AS ocean_code
	,vt.code::numeric AS vessel_type_code
	,ct.code::numeric AS country_code
	
FROM
	ps_logbook.catch c
	INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
	INNER JOIN ps_logbook.route r ON a.route = r.topiaid
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN ps_common.schooltype st ON a.schooltype = st.topiaid 
	INNER JOIN common.species s ON c.species = s.topiaid
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid
	
WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;