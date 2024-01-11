SELECT
	a.d_act::date AS activity_date
	,a.cwp11_act 
	,a.v_tpec AS total_hour_fished
	,a.v_dur_cal AS set_duration
	,o.c_ocea::numeric AS ocean_code
	,e.c_engin::numeric AS vessel_type_code
	,b.c_pav_b::numeric AS country_code
FROM
	public.activite a
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
	JOIN public.capture c ON (a.c_bat = c.c_bat AND a.d_act = c.d_act AND a.n_act = c.n_act)
	JOIN public.engin e ON (a.c_engin = e.c_engin)
	JOIN public.ocean o ON (a.c_ocea  = o.c_ocea)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?engin)
	AND a.c_ocea IN (?ocean)
;