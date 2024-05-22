SELECT
	a.d_act AS activity_date
	,a.v_tpec::numeric AS fishing_time
	,e.c_engin::integer AS gear
	,a.c_bat::text AS vessel_code
	,a.v_tmer::numeric AS hrsea
	,a.c_ocea::integer AS ocean_code
	,a.c_engin::numeric AS vessel_type_code
	,p.c_pays_fao::text AS country_label
	,b.c_pav_b::numeric AS country_code

FROM
	public.activite a
	JOIN public.ocean o ON (a.c_ocea = o.c_ocea)
	INNER JOIN public.bateau b ON (a.c_bat = b.c_bat)
	INNER JOIN public.pavillon p ON (p.c_pav_b = b.c_pav_b)
	INNER JOIN public.type_bateau vt ON (b.c_typ_b = vt.c_typ_b)
	INNER JOIN public.engin e ON (vt.c_engin = e.c_engin)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND o.c_ocea IN (?ocean)
	AND e.c_engin IN (?engin)
	AND p.c_pav_b IN (?country)
;