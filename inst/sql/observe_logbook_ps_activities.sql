-------------------------------------------------------------------------------
-- LOGBOOK ACTIVITIES - PURSE SEINE - OBSERVE
-------------------------------------------------------------------------------
-- Generic extraction of logbook activities from Observe
-------------------------------------------------------------------------------
-- Philippe Sabarros <philippe.sabarros@ird.fr>
-- Esther Mollier <esther.mollier@ird.fr>
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2023-10-31 -- v1.0 -- EM -- t3 query adapted to observe database
-- 2023-11-02 -- v1.1 -- PS -- object and buoy operation added
-- 2024-06-13 -- V1.2 -- CL -- homogenised for fishi
-------------------------------------------------------------------------------

SELECT
o.label1::text AS ocean
,p.label1::text AS program
,c.iso3code::text AS flag
,v.label1::text AS vessel
,v.code::integer AS vessel_code
,a.latitude::numeric AS latitude
,a.longitude::numeric AS longitude
,r.date::date AS date
,a.number::integer AS number
,va.code::text AS vessel_activity_code
,va.label1::text AS vessel_activity
,st.homeid::text AS school_type
,oo.code::text AS operation_on_object_code
,oo.label1::text AS operation_on_object
,fo.computedwhenarrivingsimplifiedobjecttype::text AS fob_type_when_arriving
,fo.computedwhenleavingsimplifiedobjecttype::text AS fob_type_when_leaving
,STRING_AGG(tbo.code, ';')::text AS operation_on_buoy_code
,STRING_AGG(tbo.label1, ';')::text AS operation_on_buoy
,a.topiaid::text AS activity_id
,t.topiaid::text AS trip_id
,t.startdate::date AS departure_date
,t.enddate::date AS landing_date

FROM
ps_common.trip t
INNER JOIN ps_common.program p ON (t.logbookprogram = p.topiaid)
INNER JOIN common.ocean o ON (t.ocean = o.topiaid)
INNER JOIN common.vessel v ON (t.vessel = v.topiaid)
INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
INNER JOIN common.country c ON (v.flagcountry = c.topiaid)
INNER JOIN ps_logbook.route r ON (r.trip = t.topiaid)
INNER JOIN ps_logbook.activity a ON (a.route = r.topiaid)
INNER JOIN ps_common.vesselactivity va ON (a.vesselactivity = va.topiaid)
LEFT OUTER JOIN ps_common.schooltype st ON (a.schooltype = st.topiaid)
LEFT OUTER JOIN ps_logbook.floatingobject fo ON (fo.activity=a.topiaid)
LEFT OUTER JOIN ps_common.objectoperation oo ON (fo.objectoperation=oo.topiaid)
LEFT OUTER JOIN ps_logbook.transmittingbuoy tb ON (tb.floatingobject=fo.topiaid)
LEFT OUTER JOIN ps_common.transmittingbuoytype tbt ON (tb.transmittingbuoytype=tbt.topiaid)
LEFT OUTER JOIN ps_common.transmittingbuoyoperation tbo ON (tb.transmittingbuoyoperation=tbo.topiaid)

WHERE
	EXTRACT(year FROM r.date) IN (?time_period)
	AND iso3code::text  IN (?flag)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)

GROUP BY
o.label1
,p.label1
,c.iso3code
,v.label1
,v.code
,a.latitude
,a.longitude
,r.date
,a.number
,va.code
,va.label1
,st.homeid
,oo.code
,oo.label1
,fo.computedwhenarrivingsimplifiedobjecttype
,fo.computedwhenleavingsimplifiedobjecttype
,a.topiaid
,t.topiaid
,t.startdate
,t.enddate

ORDER BY
o.label1
,v.label1
,r.date
,a.number

--LIMIT 10
;
