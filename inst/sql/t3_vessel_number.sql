SELECT
	v.topiaid::text AS vessel_code
	,o.code::integer AS ocean_code
	,o.label1::text AS ocean_name
	,t.landingdate::date AS landing_date
	,vst.code::integer AS vessel_type_code
	,vst.label1::text AS vessel_type_name
FROM
	public.trip t
	JOIN public.vessel v ON (t.vessel = v.topiaid)
	JOIN public.country c ON (v.flagcountry = c.topiaid)
	JOIN public.vesseltype vt ON (v.vesseltype = vt.topiaid)
	JOIN public.vesselsimpletype vst ON (vt.vesselsimpletype = vst.topiaid)
	JOIN public.harbour h ON (t.landingharbour = h.topiaid)
	JOIN public.ocean o ON (h.ocean = o.topiaid)
WHERE
	EXTRACT (YEAR FROM t.landingdate) IN (?time_period)
	AND o.code IN (?ocean)
	AND c.code IN (?country)
	AND vst.code IN (?vessel_type)
;
