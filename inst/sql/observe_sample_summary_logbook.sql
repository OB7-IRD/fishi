SELECT
	t.topiaid
	,o.label1 as ocean_name
	,t.startdate AS departure
	,t.enddate AS arrival
	,t.enddate as landing_date
	,v.label1 as vessel_name
	,v.code::numeric AS boat_code
	,ct.iso3code AS fleet
	,o.code::numeric as ocean_id
	,ct.code::numeric as country_id
	,h1.code as harbour_id
	,h1.label1 as harbour_name
	,EXTRACT(YEAR FROM t.enddate) as landing_year
	,t.landingtotalweight
	,t.localmarkettotalweight
	,t.landingtotalweight + t.localmarkettotalweight as total_landing
	,s.number as sample_number
	,s.well as vessel_well_number
	,CASE
		WHEN vt.code::numeric IN (1,2,3)	THEN 'BB'
		WHEN vt.code::numeric IN (4,5,6)	THEN 'PS'
		WHEN vt.code::numeric IN (7)		THEN 'LL'
		WHEN vt.code::numeric IN (10)		THEN 'SV'
		ELSE 'OTH'
	END AS vessel_type
	,h1.label1 as port_departure
	,h2.label1 as port_arrival
	
FROM ps_common.trip AS t
	INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
	INNER JOIN common.harbour AS h1 ON t.departureharbour = h1.topiaid
	INNER JOIN common.harbour AS h2 ON t.landingharbour = h2.topiaid
	INNER JOIN common.country AS ct2 ON h1.country = ct2.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct ON v.flagcountry = ct.topiaid
	INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
	INNER JOIN ps_logbook.activity AS a ON a.route = r.topiaid
	LEFT OUTER JOIN ps_logbook.sample s on (t.topiaid = s.trip)
	
WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND EXTRACT(year FROM t.enddate) IN (?landing_year)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
