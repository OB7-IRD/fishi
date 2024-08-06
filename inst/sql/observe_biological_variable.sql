-------------------------------------------------------------------------------------------
-- BIOLOGICAL VARIABLE - OBSERVE
-------------------------------------------------------------------------------------------
-- Generic extraction of each biological variable sampled from Observe
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2023 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
SELECT
	EXTRACT(YEAR FROM r.date) AS fishing_year
	,sm.weight 
	,sm.length 
	,sx.code AS sex
	,sp.faocode AS species_code
	,sm.count 
	
FROM ps_observation.route r
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid
	INNER JOIN ps_observation.activity a ON a.route = r.topiaid
	INNER JOIN ps_observation.set s ON s.activity = a.topiaid
	INNER JOIN ps_observation.sample sa ON sa.set = s.topiaid
	INNER JOIN ps_observation.samplemeasure sm ON sm.sample = sa.topiaid
	INNER JOIN common.sex sx ON sm.sex = sx.topiaid 
	INNER JOIN common.species sp ON (sm.species = sp.topiaid) 
	
WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
