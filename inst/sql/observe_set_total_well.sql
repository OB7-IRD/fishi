SELECT
	t.topiaid
	,lp.label2 as program
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
	,s.well as vessel_well_number
	,vt.code as vessel_type_code
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
	INNER JOIN ps_common.program lp ON (t.logbookprogram = lp.topiaid)
	INNER join ps_logbook.sample s ON (s.trip = t.topiaid)
	
WHERE
	lp.label2 in ('AVDTH Atlantique (IRD)', 
					  'AVDTH Indien (IRD)', 
					  'Saisies en cours Abidjan (IRD)')
	and EXTRACT(year FROM r.date) IN (?time_period)
	AND EXTRACT(year FROM t.enddate) IN (?landing_year)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
