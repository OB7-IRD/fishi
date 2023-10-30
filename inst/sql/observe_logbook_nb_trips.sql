SELECT
	t.topiaid,
	lp.label2 AS program,
	o.label1 AS ocean,
	ct1.iso3code AS flag,
	v.label1 AS vessel_name,
	CASE
		WHEN vt.code::NUMERIC IN (1, 2, 3) THEN 'BB'
		WHEN vt.code::NUMERIC IN (4, 5, 6) THEN 'PS'
		WHEN vt.code::NUMERIC IN (7) THEN 'LL'
		WHEN vt.code::NUMERIC IN (10) THEN 'SV'
		ELSE 'OTH'
		END AS vessel_type,
	t.enddate AS activity_date,
	t.startdate AS departure,
	h1.label1 AS port_departure, 
	t.enddate AS arrival,
	h2.label1 AS port_arrival, 
	ct2.iso3code AS landing_country,
	v.cfrid

FROM
	ps_common.trip AS t
	INNER JOIN common.ocean AS o on t.ocean = o.topiaid
	INNER JOIN common.harbour AS h1 ON t.departureharbour = h1.topiaid
	INNER JOIN common.harbour AS h2 ON t.landingharbour = h2.topiaid
	INNER JOIN common.country AS ct2 ON h1.country = ct2.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
	INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
	INNER JOIN ps_logbook.activity AS a ON a.route = r.topiaid
	INNER JOIN ps_common.program lp ON (t.logbookprogram = lp.topiaid)
WHERE
	lp.label2 IN ('AVDTH Atlantique (IRD)', 
				  'AVDTH Indien (IRD)', 
				  'Saisies en cours Abidjan (IRD)')
		AND EXTRACT(year FROM t.enddate) IN (?time_period)
;
