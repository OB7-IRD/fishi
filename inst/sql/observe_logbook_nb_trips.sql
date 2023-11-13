SELECT
	lp.label2 AS program
	,o.label1 AS ocean
	,ct.iso3code AS flag
	,v.label1 AS vessel_name
	,CASE
		WHEN vt.code::NUMERIC IN (1, 2, 3) THEN 'BB'
		WHEN vt.code::NUMERIC IN (4, 5, 6) THEN 'PS'
		WHEN vt.code::NUMERIC IN (7) THEN 'LL'
		WHEN vt.code::NUMERIC IN (10) THEN 'SV'
		ELSE 'OTH'
		END AS vessel_type
	,t.enddate AS arrival
	,h.label1 AS port_arrival
	,o.code::numeric as ocean_id
	,ct.code::numeric as country_id
	,h.code as harbour_id
	,v.code::numeric AS boat_code
	,w.well as vessel_well_number
FROM
	ps_common.trip AS t
	INNER JOIN common.ocean AS o on t.ocean = o.topiaid
	INNER JOIN common.harbour AS h ON t.landingharbour = h.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct ON v.flagcountry = ct.topiaid
	INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
--	INNER JOIN ps_logbook.activity AS a ON a.route = r.topiaid
	INNER JOIN ps_common.program lp ON (t.logbookprogram = lp.topiaid)
	INNER JOIN ps_logbook.well w ON (w.trip = t.topiaid)
WHERE
	lp.label2 IN ('AVDTH Atlantique (IRD)', 
				  'AVDTH Indien (IRD)', 
				  'Saisies en cours Abidjan (IRD)')
		AND EXTRACT(year FROM t.enddate) IN (?time_period)
;
