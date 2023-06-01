select
	r.date as activity_date
	,o.label1 AS ocean_name
	,CASE
	WHEN vt.code::numeric IN (1,2,3)	THEN 'BB'
	WHEN vt.code::numeric IN (4,5,6)	THEN 'PS'
	WHEN vt.code::numeric IN (7)		THEN 'LL'
	WHEN vt.code::numeric IN (10)		THEN 'SV'
	ELSE 'OTH'
	END AS gear
	,ct.iso3code as fleet
	,ct.iso3code as flag
	,s.code as c_esp
	,c.weight::numeric as catch 
	,CASE
	WHEN st.code::numeric IN (0) THEN 'IND'
	WHEN st.code::numeric IN (1) THEN 'BO'
	WHEN st.code::numeric IN (2) THEN 'BL'
	END AS l4c_tban
	,o.code::numeric as ocean_id
	,vt.code::numeric as vessel_type_id
	,ct.code::numeric as country_id
from
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
where
	EXTRACT(year FROM r.date) IN (?time_period)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;