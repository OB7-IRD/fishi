SELECT
	t.topiaid 
	,o.label1 AS ocean_label
	,t.startdate AS departure
	,t.enddate AS arrival
	,t.enddate AS landing_date
	,v.label1 AS vessel_label
	,v.code::numeric AS boat_code
	,ct.iso3code AS fleet
	,o.code::numeric AS ocean_code
	,ct.code::numeric AS country_code
	,h1.code AS port_code
	,h1.label1 AS port_label
	,EXTRACT(YEAR FROM t.enddate) AS landing_year
	,t.landingtotalweight
	,t.localmarkettotalweight
	,t.landingtotalweight + t.localmarkettotalweight AS total_landing
	,t.topiaid AS sample_number
	,vt.code AS vessel_type_code
	,h1.label1 AS port_departure
	,h2.label1 AS port_arrival

FROM ps_observation.sample s2 
	INNER JOIN ps_observation.set s ON (s.topiaid = s2.set)
	INNER JOIN ps_observation.activity a ON (s.activity = a.topiaid)
	INNER JOIN ps_observation.route r ON (a.route = r.topiaid)
	INNER JOIN ps_common.trip t ON (r.trip = t.topiaid)
	INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
	INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
	INNER JOIN common.country ct ON (v.flagcountry = ct.topiaid )
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
	INNER JOIN common.harbour AS h1 ON t.departureharbour = h1.topiaid
	INNER JOIN common.harbour AS h2 ON t.landingharbour = h2.topiaid

WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND EXTRACT(year FROM t.enddate) IN (?landing_year)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
