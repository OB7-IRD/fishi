SELECT
	o.label1 AS ocean
	,o.code AS ocean_code
	,p.label1 AS program
	,v.label1 AS vessel
	,v.code AS vessel_code
	,c.iso3code AS flag
	,v.fleetcountry AS fleet_country
	,ob.lastname::text AS observer_name
	,t.startdate::date AS trip_start_date
	,t.enddate::date AS trip_end_date
	,r.date::date AS observation_date
	,a.time AS observation_time
	,va.label1 AS vessel_activity
	,va.code AS vessel_activity_code
	,a.latitude AS latitude
	,a.longitude AS longitude
	,(CASE WHEN st.code='0' THEN 'UNK' 
	ELSE (CASE WHEN st.code='1' THEN 'FOB' 
	ELSE (CASE WHEN st.code='2' THEN 'FSC' END) END) END) AS school_type
	,t.topiaid AS trip_id
	,s.topiaid AS set_id

FROM ps_common.trip t
	INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
	INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
	INNER JOIN common.person ob ON (t.observer = ob.topiaid)
	INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
	INNER JOIN common.country c ON (v.flagcountry = c.topiaid)
	INNER JOIN ps_observation.route r ON (r.trip = t.topiaid)
	INNER JOIN ps_observation.activity a ON (a.route = r.topiaid)
	INNER JOIN ps_common.vesselactivity va ON (a.vesselactivity = va.topiaid)
	LEFT OUTER JOIN ps_observation.set s ON (s.activity = a.topiaid)
	LEFT OUTER JOIN ps_common.schooltype st ON (s.schooltype = st.topiaid)
	LEFT OUTER JOIN ps_common.reasonfornullset rns ON (s.reasonfornullset = rns.topiaid)
	LEFT OUTER JOIN common.fpazone fpa ON (a.currentfpazone = fpa.topiaid)

WHERE
	extract(year from r.date) IN (?time_period)
	AND c.iso3code IN (?flag)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
	AND p.topiaid IN ('fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234', -- DCF (IRD)
					  'fr.ird.referential.ps.common.Program#1308048349668#0.7314513252652438', -- DCF (TAAF)
				  	  'fr.ird.referential.ps.common.Program#1363095174385#0.011966550987014823', -- Moratoire ICCAT 2013-2015 (IRD)
				  	  'fr.ird.referential.ps.common.Program#1373642516190#0.998459307142491') -- OCUP
	AND va.code LIKE '6' 
;
