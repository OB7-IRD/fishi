select
	r.date as activity_date
	,v.keelcode::numeric as keel_code
	,c.weight::numeric as catch 
	,o.code::numeric as ocean_id
	,ct.code::numeric as country_id
	,vt.code::numeric as vessel_type_id
from
	ps_logbook.catch c
	INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
	INNER JOIN ps_logbook.route r ON a.route = r.topiaid
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
where
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;

