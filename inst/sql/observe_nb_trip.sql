SELECT
	EXTRACT(YEAR FROM r.date) as sampling_year
	,r.date as fish_sampling_date
	,t.enddate as landing_date
	,v.label1 as vessel_name
	,v.code::numeric AS boat_code
	,ct.iso3code AS fleet
	,o.code::numeric as ocean_id
	,ct.code::numeric as country_id
	,c.well as vessel_well_number
FROM ps_observation.route r
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid
	INNER JOIN ps_observation.activity a on (a.route = r.topiaid)
	INNER JOIN ps_observation.set s on (s.activity = a.topiaid)
	INNER JOIN ps_observation.catch c on (c.set = s.topiaid)
WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
