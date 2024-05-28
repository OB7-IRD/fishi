SELECT 
	r.date as activity_date
	,r.fishingtime as fishing_time
	,vt.code::numeric AS gear 
	,ct1.iso3code AS country_label
	,v.code as vessel_code
	,o.code::numeric AS ocean_code
    ,vt.code::numeric AS vessel_type_code
    ,ct1.code::numeric AS country_code                                                                                                                                                                                                                           	
--    ,st.homeid AS school_type
--	,a.setcount as total_set
	,r.timeatsea as hrsea
FROM ps_common.trip AS t
	INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
	INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
	INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
	INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
	INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
--	INNER JOIN ps_logbook.activity AS a ON a.route = r.topiaid
--	INNER JOIN ps_common.schooltype AS st 	ON a.schooltype = st.topiaid
WHERE
    EXTRACT(year FROM r.date) IN (?time_period)
    AND ct1.code::numeric IN (?country)
    AND vt.code::numeric IN (?vessel_type)
    AND o.code::numeric IN (?ocean)