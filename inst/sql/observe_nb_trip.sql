SELECT
	EXTRACT(YEAR FROM r.date) as sampling_year
	,r.date as fish_sampling_date
	,t.enddate as landing_date
	,v.label1 as vessel_name
	,v.code::numeric AS boat_code
	,ct.iso3code AS fleet
	,w.well AS vessel_well_number
	,o.code::numeric as ocean_id
	,ct.code::numeric as country
FROM ps_logbook.catch c
	INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
	INNER JOIN ps_logbook.route r ON a.route = r.topiaid
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN ps_logbook.wellactivity wa ON a.topiaid = wa.activity
	INNER JOIN ps_logbook.well w ON wa.well = w.topiaid 
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
	INNER JOIN common.species s ON (c.species = s.topiaid)
WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
