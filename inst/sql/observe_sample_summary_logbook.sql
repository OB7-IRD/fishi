-------------------------------------------------------------------------------------------
-- SAMPLE - TRIP - VESSEL - WELL - OBSERVE
-------------------------------------------------------------------------------------------
-- Generic extraction of trip, vessel and well sampled from Observe (logbook)
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2023 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
-- 2026 -- v2.0 -- JC -- remove the join with ps_logbook.activity and ps_logbook.route tables to avoid dupliactes and add sample_type in results
-------------------------------------------------------------------------------------------
SELECT
	t.topiaid
	,o.label1 AS ocean_label
	,t.startdate AS departure
	,t.enddate AS arrival
	,t.enddate AS landing_date
	,v.label1 AS vessel_label
	,v.code::numeric AS boat_code
	,ct.iso3code AS fleet
	,o.code::numeric AS ocean_code
	,ct.code::numeric AS country_code
	,h2.code AS port_code
	,EXTRACT(YEAR FROM t.enddate)::integer AS landing_year
	,t.landingtotalweight
	,t.localmarkettotalweight
	,t.landingtotalweight + t.localmarkettotalweight AS total_landing
	,s.number AS sample_number
	,st.code::integer AS sample_type_code
	,st.label1 AS sample_type
	,s.well AS vessel_well_number
	,vt.code AS vessel_type_code
	,h1.label1 AS port_departure
	,h2.label1 AS port_arrival

FROM ps_common.trip AS t
	INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
	INNER JOIN common.harbour AS h1 ON t.departureharbour = h1.topiaid
	INNER JOIN common.harbour AS h2 ON t.landingharbour = h2.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct ON v.flagcountry = ct.topiaid
  INNER JOIN ps_logbook.sample s ON t.topiaid = s.trip
	INNER JOIN ps_common.sampletype st ON st.topiaid = s.sampletype

WHERE
	EXTRACT(year FROM t.enddate) IN (?time_period)
	AND EXTRACT(year FROM t.enddate) IN (?landing_year)
	AND ct.code::numeric  IN (?country)
	AND vt.code::numeric IN (?vessel_type)
	AND o.code::numeric IN (?ocean)
;
