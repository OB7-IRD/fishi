SELECT
	EXTRACT(YEAR FROM act.date)::integer AS year_sample
	,EXTRACT(MONTH FROM act.date)::integer AS month_sample
	,c.codeiso3::text AS fleet_country
	,CASE st.code
		WHEN 1 THEN 'Floating object'
		WHEN 2 THEN 'Free school'
		WHEN 3 THEN 'Unknown'
	END::text AS fishing_mode
	,CASE o.code
		WHEN 1 THEN 'Atlantic Ocean'
		WHEN 2 THEN 'Indian Ocean'
		ELSE 'PROB'
	END::text AS ocean_sample
	,count(*)::integer AS nb_well_sample
FROM
	public.activity act
	JOIN public.schooltype st ON (act.schooltype=st.topiaid)
	JOIN public.ocean o ON (act.ocean=o.topiaid)
--	JOIN public.wellplan wp ON (act.topiaid=wp.activity)
	JOIN public.samplewell sw ON (act.topiaid=sw.activity)
	JOIN public.trip t ON (act.trip=t.topiaid)
	JOIN public.vessel v ON (t.vessel=v.topiaid)
	JOIN public.country c ON (v.fleetcountry=c.topiaid AND v.flagcountry=c.topiaid)
WHERE
	EXTRACT(YEAR FROM act.date) IN ('2018')
GROUP BY
	year_sample
	,month_sample
	,fleet_country
	,fishing_mode
	,ocean_sample
ORDER BY
	month_sample
	,fleet_country
	,fishing_mode
	,ocean_sample
;
