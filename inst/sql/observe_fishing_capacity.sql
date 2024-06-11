-------------------------------------------------------------------------------------------
-- CAPACITY - CARRYING - OBSERVE
-------------------------------------------------------------------------------------------
 -- Generic extraction of the fishing capacity of the fleet from Observe
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2024-05-29 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
SELECT
	r.date AS activity_date
	,v.keelcode::numeric AS keel_code
	,v.capacity::numeric AS catch 
	,o.code::numeric AS ocean_code
	,ct.code::numeric AS country_code
	,vt.code::numeric AS vessel_type_code
	
FROM
	ps_logbook.catch c
	INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
	INNER JOIN ps_logbook.route r ON a.route = r.topiaid
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
	
WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;