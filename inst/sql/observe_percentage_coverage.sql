SELECT 
	ocean_label
	,flag
	,vessel_type
	,arrival_port
	,arrival_year
	,COUNT( distinct well_with_tuna) as nb_wells_with_tuna
	,COUNT( distinct well_with_sample) as nb_wells_with_sample
--	,ROUND(100*COUNT(distinct well_with_sample)/nullif(COUNT( distinct well_with_tuna),0),2) as percent_well_sampled
FROM
(SELECT
	ocean_label
	,flag 
	,vessel_type
	,arrival_port 
	,arrival_year
	,well_with_tuna.well_id as well_with_tuna
	,well_with_sample.well_id as well_with_sample
	FROM
	(SELECT
		t.topiaid
		,lp.label2 as program
		,o.label1 AS ocean_label
		,ct1.iso3code AS flag
		,CASE
			WHEN vt.code::numeric IN (1,2,3)	THEN 'BB'
			WHEN vt.code::numeric IN (4,5,6)	THEN 'PS'
			WHEN vt.code::numeric IN (7)		THEN 'LL'
			WHEN vt.code::numeric IN (10)		THEN 'SV'
			ELSE 'OTH'
		END AS vessel_type
		,v.cfrid
		,v.label1 AS vessel_name
		,EXTRACT(year FROM t.enddate)::integer AS arrival_year
		,t.startdate as departure_date
		,h1.label1 as departure_port
		,t.enddate as arrival_date
		,h2.label1 as arrival_port
		,t.landingtotalweight
		,t.landingtotalweight total_landings_to_cannery
		,w.well as well_id
	FROM ps_common.trip AS t
		INNER JOIN ps_logbook.route r ON t.topiaid = r.trip
		INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
		INNER JOIN common.harbour AS h1 ON t.departureharbour = h1.topiaid
		INNER JOIN common.harbour AS h2 ON t.landingharbour = h2.topiaid
		INNER JOIN common.country AS ct2 ON h1.country = ct2.topiaid
		INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
		INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
		INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
		INNER JOIN ps_common.program lp ON (t.logbookprogram = lp.topiaid)
		LEFT OUTER JOIN ps_logbook.well w on (w.trip = t.topiaid)
	) AS well_with_tuna
	LEFT OUTER JOIN(
	SELECT
		t.topiaid
		,s.well as well_id
	FROM
		ps_common.trip AS t
		INNER JOIN ps_logbook.route r ON t.topiaid = r.trip
		INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
		INNER JOIN common.harbour AS h1 ON t.departureharbour = h1.topiaid
		INNER JOIN common.harbour AS h2 ON t.landingharbour = h2.topiaid
		INNER JOIN common.country AS ct2 ON h1.country = ct2.topiaid
		INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
		INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
		INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
		INNER JOIN ps_common.program lp ON (t.logbookprogram = lp.topiaid)
		LEFT OUTER JOIN ps_logbook.sample s on (t.topiaid = s.trip)
		INNER JOIN ps_common.sampletype st ON st.topiaid = s.sampletype
		) AS well_with_sample
	ON well_with_tuna.topiaid = well_with_sample.topiaid 
	AND well_with_tuna.well_id = well_with_sample.well_id
		) AS percent_wells_sampled
group by ocean_label, flag, vessel_type, arrival_port, arrival_year