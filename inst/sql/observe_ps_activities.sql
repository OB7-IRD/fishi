-------------------------------------------------------------------------------
-- ACTIVITIES - PURSE SEINE - OBSERVE
-------------------------------------------------------------------------------
-- Generic extraction of observed activities from Observe v9
-------------------------------------------------------------------------------
-- Philippe Sabarros <philippe.sabarros@ird.fr>
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2022-06-13 -- v1.0 -- PS -- initial version
-- 2022-10-30 -- v1.1 -- PS -- modified for ANAPO-OBS-PS
-- 2023-11-06 -- v1.2 -- PS -- classes defined
-- 2024-01-11 -- V1.3 -- CL -- update to fishi
-- 2024-06-13 -- V1.4 -- CL -- homogenised for fishi
-------------------------------------------------------------------------------

SELECT
o.label1::text AS ocean
	,p.label1::text AS program
	,v.label1::text AS vessel
	,v.code::integer AS vessel_code
	,c.iso3code::text AS flag
	,v.fleetcountry::text AS fleet_country
	,ob.lastname::text AS observer_name
	,oc.label1::text AS observer_nationality
	,t.startdate::date AS trip_start_date
	,t.enddate::date AS trip_end_date
	,r.date::date AS observation_date
	,a.time AS observation_time
	,va.label1::text AS vessel_activity
	,va.code::text AS vessel_activity_code
	,a.latitude::numeric AS latitude
	,a.longitude::numeric AS longitude
	,oo.code::text AS operation_on_object_code
	,oo.label1::text AS operation_on_object
	,fo.computedwhenarrivingsimplifiedobjecttype::text AS fob_type_when_arriving
	,fo.computedwhenleavingsimplifiedobjecttype::text AS fob_type_when_leaving
	,STRING_AGG(tbo.code, ';')::text AS operation_on_buoy_code
	,STRING_AGG(tbo.label1, ';')::text AS operation_on_buoy
	,(CASE WHEN st.code='0' THEN 'UNK' 
	ELSE (CASE WHEN st.code='1' THEN 'FOB' 
	ELSE (CASE WHEN st.code='2' THEN 'FSC' END) END) END) AS school_type
	,t.topiaid::text AS trip_id
	,a.topiaid::text AS activity_id
	,s.topiaid AS set_id

FROM ps_common.trip t
	INNER JOIN ps_common.program p ON (t.observationsprogram = p.topiaid)
	INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
	INNER JOIN common.person ob ON (t.observer = ob.topiaid)
	INNER JOIN common.country oc ON (ob.country = oc.topiaid)
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
	LEFT OUTER JOIN ps_observation.floatingobject fo ON (fo.activity=a.topiaid)
	LEFT OUTER JOIN ps_common.objectoperation oo ON (fo.objectoperation=oo.topiaid)
	LEFT OUTER JOIN ps_observation.transmittingbuoy tb ON (tb.floatingobject=fo.topiaid)
	LEFT OUTER JOIN ps_common.transmittingbuoytype tbt ON (tb.transmittingbuoytype=tbt.topiaid)
	LEFT OUTER JOIN ps_common.transmittingbuoyoperation tbo ON (tb.transmittingbuoyoperation=tbo.topiaid)

WHERE
p.topiaid::text IN ('fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234', -- DCF (IRD)
					'fr.ird.referential.ps.common.Program#1308048349668#0.7314513252652438', -- DCF (TAAF)
					'fr.ird.referential.ps.common.Program#1363095174385#0.011966550987014823', -- Moratoire ICCAT 2013-2015 (IRD)
					'fr.ird.referential.ps.common.Program#1373642516190#0.998459307142491', -- OCUP-OB
					'fr.ird.referential.ps.common.Program#1686640873342#0.15408837224678917', -- OCUP-OB (hors délai 2022)
					'fr.ird.referential.ps.common.Program#1539949992432#0.4308822110391043') -- OCUP-OE
AND EXTRACT(YEAR FROM r.date) IN (?time_period)
	AND c.iso3code IN (?flag)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)

GROUP BY
o.label1
,o.code
,p.label1
,v.label1
,v.code
,c.iso3code
,v.fleetcountry
,ob.lastname
,oc.label1
,t.startdate
,t.enddate
,r.date
,a.time
,va.label1
,va.code
,rns.label1
,a.latitude
,a.longitude
,oo.code
,oo.label1
,fo.computedwhenarrivingsimplifiedobjecttype
,fo.computedwhenleavingsimplifiedobjecttype
,t.topiaid
,a.topiaid
,st.code
,s.topiaid

ORDER BY
o.label1
,t.startdate
,v.label1
,r.date
;
