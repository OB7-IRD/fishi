SELECT
	a.d_act::date AS activity_date
	,b.c_quille AS keel_code
	,b.v_ct_m3::numeric AS catch
	,a.c_ocea::numeric AS ocean_code
	,b.c_pav_b::numeric AS country_code
	,a.c_engin::numeric AS vessel_type_code
FROM
	public.activite a
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
	JOIN public.ocean o ON (a.c_ocea  = o.c_ocea)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
