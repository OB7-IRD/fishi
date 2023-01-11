SELECT
	o.c_ocea::numeric as ocean_code
	,o.l_ocea::text as ocean_name
	,a.d_act::date as activity_date
	,a.c_bat::numeric as vessel_code
	,e2.c_engin::numeric as vessel_type_code
	,e2.l_engin::text as vessel_type_name
FROM 
	public.activite a 
	JOIN public.capture c on (a.c_bat = c.c_bat and a.d_act = c.d_act and a.n_act = c.n_act)
	JOIN public.espece e on (c.c_esp = e.c_esp)
	JOIN public.ocean o on (a.c_ocea  = o.c_ocea)
	JOIN public.engin e2 on (a.c_engin = e2.c_engin)
	join public.bateau b on (a.c_bat = b.c_bat)
	join public.pavillon p on (b.c_pav_b = p.c_pav_b)
WHERE
	extract (YEAR FROM a.d_act) IN (?time_period)
	AND o.c_ocea IN (?ocean)
	AND p.c_pav_b IN (?country)
	AND e2.c_engin IN (?vessel_type)
;
