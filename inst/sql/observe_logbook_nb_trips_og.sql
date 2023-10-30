SELECT
	program,
	ocean,
	flag,
	vessel_type,
	landing_year,
	port_arrival,
	count(*) AS nb_trips
FROM
	(-- START nb_trips
	SELECT
		program,
		ocean,
		flag,
		vessel_type,
		landing_year,
		vessel_name,
		departure,
		port_departure,
		arrival - departure AS nb_days_in_trip,
		arrival,
		port_arrival,
		(SELECT
			min(t2.startdate) - arrival
		FROM
			ps_common.trip AS t2
			INNER JOIN common.vessel AS v2 ON t2.vessel = v2.topiaid
			INNER JOIN ps_common.program lp ON (t2.logbookprogram = lp.topiaid)
		WHERE
			v2.label1 = trips.vessel_name
			AND lp.label2 IN ('AVDTH Atlantique (IRD)', 'AVDTH Indien (IRD)', 'Saisies en cours Abidjan (IRD)')
			AND t2.startdate >= trips.arrival) AS nb_days_with_next_trip,
		(SELECT
			min(t2.startdate)
		FROM
			ps_common.trip AS t2
			INNER JOIN common.vessel AS v2 ON t2.vessel = v2.topiaid
			INNER JOIN ps_common.program lp ON (t2.logbookprogram = lp.topiaid)
		WHERE
			v2.label1 = trips.vessel_name
			AND lp.label2 IN ('AVDTH Atlantique (IRD)', 'AVDTH Indien (IRD)', 'Saisies en cours Abidjan (IRD)')
				AND t2.startdate >= trips.arrival) AS next_departure
	FROM
		( -- start Trips 
		SELECT
			t.topiaid,
			lp.label2 AS program,
			o.label1 AS ocean,
			ct1.iso3code AS flag,
			CASE
				WHEN vt.code::NUMERIC IN (1, 2, 3) THEN 'BB'
				WHEN vt.code::NUMERIC IN (4, 5, 6) THEN 'PS'
				WHEN vt.code::NUMERIC IN (7) THEN 'LL'
				WHEN vt.code::NUMERIC IN (10) THEN 'SV'
				ELSE 'OTH'
				END AS vessel_type,
			EXTRACT(YEAR FROM t.enddate)::integer AS landing_year,
			t.startdate AS departure,
			h1.label1 AS port_departure, 
			t.enddate AS arrival,
			h2.label1 AS port_arrival, 
			r.date AS activity_date,
			ct2.iso3code AS landing_country,
			v.cfrid,
			v.label1 AS vessel_name
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
			lp.label2 IN ('AVDTH Atlantique (IRD)', 'AVDTH Indien (IRD)', 'Saisies en cours Abidjan (IRD)')
				AND EXTRACT(year from t.enddate) = 2023) AS trips
		-- END Trips
	GROUP BY
		program,
		ocean,
		flag,
		vessel_type,
		vessel_name,
		landing_year,
		departure,
		port_departure,
		arrival,
		port_arrival
	ORDER BY
		ocean,
		flag,
		vessel_type,
		vessel_name,
		landing_year,
		departure,
		port_departure,
		arrival,
		port_arrival) AS nb_trips
		-- END NB TRIPS
GROUP BY
	program,
	ocean,
	flag,
	vessel_type,
	landing_year,
	port_arrival