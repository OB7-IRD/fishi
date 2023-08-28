SELECT
	ocean.label1 AS ocean
	,flgc.codeiso3 AS flag
	,vessel.label1 AS vessel
	,vessel.code AS vessel_code
	,latitude AS latitude
	,longitude AS longitude
	,route.date AS date
	,number AS number
	,vesselactivity.code AS vessel_activity_code
	,vesselactivity.label1 AS vessel_activity
	,schooltype.homeid AS school_type
	,activity.topiaid AS activity_id
	,trip.topiaid AS trip_id
	,trip.departuredate AS departure_date
	,trip.landingdate AS landing_date

FROM activity
	INNER JOIN ocean ON (activity.ocean=ocean.topiaid)
	INNER JOIN route ON (activity.route=route.topiaid)
	INNER JOIN trip ON (route.trip=trip.topiaid)
	INNER JOIN vessel ON (trip.vessel=vessel.topiaid)
	INNER JOIN vesseltype ON (vessel.vesseltype=vesseltype.topiaid)
	INNER JOIN vesselsimpletype ON (vesseltype.vesselsimpletype=vesselsimpletype.topiaid)					
	INNER JOIN vesselactivity ON (activity.vesselactivity=vesselactivity.topiaid)
	INNER JOIN country flgc ON (vessel.flagcountry=flgc.topiaid)
	LEFT OUTER JOIN schooltype ON (activity.schooltype=schooltype.topiaid)
	LEFT OUTER JOIN fpazone ON (activity.fpazone=fpazone.topiaid)
	LEFT OUTER JOIN country fpac ON (fpazone.country=fpac.topiaid)

WHERE
	EXTRACT(year FROM route.date) IN (?time_period)
	AND flgc.code::numeric  IN (?country)
	AND vesseltype.code::numeric IN (?vessel_type)
	AND ocean.code::numeric IN (?ocean)

--	flgc.codeiso3 IN ('FRA','ITA','MUS','SYC','MYT','BLZ')
--	AND vesselactivity.code IN (0,1,2,14)
--	AND vesseltype.code IN (4,5,6) --4:"Senneur avec appât"; 5:"Senneur sans appât"; 6:"Grand senneur"
--	AND route.date >= '2005-01-01'
;
