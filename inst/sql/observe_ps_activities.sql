SELECT
	o.label1 AS ocean
	,o.code::numeric AS ocean_code
	,p.label1 AS program
	,v.label1 AS vessel
	,v.code::numeric AS vessel_code
	,vt.code::numeric AS vessel_id
	,c.code::numeric AS country_code
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
	,oo.code AS operation_on_object_code
	,oo.label1 AS operation_on_object
	,fo.computedwhenarrivingsimplifiedobjecttype AS fob_type_when_arriving
	,fo.computedwhenleavingsimplifiedobjecttype AS fob_type_when_leaving
	,STRING_AGG(tbo.code, ';') AS operation_on_buoy_code
	,STRING_AGG(tbo.label1, ';') AS operation_on_buoy
	,t.topiaid AS trip_id
	,a.topiaid AS activity_id

FROM ps_common.trip t
	INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
	INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
	INNER JOIN common.person ob ON (t.observer = ob.topiaid)
	INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
	INNER JOIN common.country c ON (v.flagcountry = c.topiaid)
	INNER JOIN ps_observation.route r ON (r.trip = t.topiaid)
	INNER JOIN ps_observation.activity a ON (a.route = r.topiaid)
	INNER JOIN ps_common.vesselactivity va ON (a.vesselactivity = va.topiaid)
	INNER JOIN common.vesseltype vt ON (v.vesseltype = vt.topiaid)
	LEFT OUTER JOIN ps_observation.set s ON (s.activity = a.topiaid)
	LEFT OUTER JOIN ps_common.reasonfornullset rns ON (s.reasonfornullset = rns.topiaid)
	LEFT OUTER JOIN common.fpazone fpa ON (a.currentfpazone = fpa.topiaid)
	LEFT OUTER JOIN ps_observation.floatingobject fo ON (fo.activity=a.topiaid)
	LEFT OUTER JOIN ps_common.objectoperation oo ON (fo.objectoperation=oo.topiaid)
	LEFT OUTER JOIN ps_observation.transmittingbuoy tb ON (tb.floatingobject=fo.topiaid)
	LEFT OUTER JOIN ps_common.transmittingbuoytype tbt ON (tb.transmittingbuoytype=tbt.topiaid)
	LEFT OUTER JOIN ps_common.transmittingbuoyoperation tbo ON (tb.transmittingbuoyoperation=tbo.topiaid)

where
	EXTRACT(year FROM r.date) IN (?time_period)
	AND c.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)

GROUP BY
	o.label1,
	o.code,
	p.label1,
	v.label1,
	v.code,
	c.iso3code,
	v.fleetcountry,
	ob.lastname,
	t.startdate,
	t.enddate,
	r.date,
	a.time,
	va.label1,
	va.code,
	rns.label1,
	a.latitude,
	a.longitude,
	oo.code,
	oo.label1,
	fo.computedwhenarrivingsimplifiedobjecttype,
	fo.computedwhenleavingsimplifiedobjecttype,
	t.topiaid,
	a.topiaid,
	c.code,
	vt.code
;
