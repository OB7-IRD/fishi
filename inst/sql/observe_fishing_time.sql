-------------------------------------------------------------------------------------------
-- FISHING TIME - TIME AT SEA - OBSERVE
-------------------------------------------------------------------------------------------
-- Generic extraction of the time at sea and fishing time from Observe
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2024-05-29 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
SELECT 
	r.date as activity_date
	,r.fishingtime as fishing_time
	,vt.code::numeric AS gear 
	,ct1.iso3code AS country_label
	,v.code as vessel_code
	,o.code::numeric AS ocean_code
    ,vt.code::numeric AS vessel_type_code
    ,ct1.code::numeric AS country_code   
	,r.timeatsea as hrsea
	
FROM ps_common.trip AS t
	INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
	INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
	
WHERE
    EXTRACT(year FROM r.date) IN (?time_period)
    AND ct1.code::numeric IN (?country)
    AND vt.code::numeric IN (?vessel_type)
    AND o.code::numeric IN (?ocean)