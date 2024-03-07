SELECT
	o.code::numeric as ocean_id
	,o.label1::text as ocean_name
	,r.date::date as activity_date
	,v.code::numeric as vessel_id
	,vt.code::numeric as vessel_type_id
	,vt.label1::text as vessel_type_name
	,ct.code::numeric as country_id 
from
	ps_logbook.activity a 
	INNER JOIN ps_logbook.route r ON a.route = r.topiaid
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
	INNER JOIN common.ocean o ON t.ocean = o.topiaid
	INNER JOIN common.vessel v ON t.vessel = v.topiaid
	INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
WHERE
	EXTRACT (YEAR FROM t.landingdate) IN (?time_period)
	AND o.code IN (?ocean)
	AND c.code IN (?country)
	AND vst.code IN (?vessel_type)
;
