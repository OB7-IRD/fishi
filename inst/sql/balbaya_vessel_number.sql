SELECT
	o.c_ocea::numeric as ocean_id
	,o.l_ocea::text as ocean_name
	,a.d_act::date as activity_date
	,a.c_bat::numeric as vessel_id
	,e.c_engin::numeric as vessel_type_id
	,e.l_engin::text as vessel_type_name
	,b.c_pav_b::numeric as country_id 
FROM 
	public.activite a 
	JOIN public.ocean o on (a.c_ocea  = o.c_ocea)
	JOIN public.engin e on (a.c_engin = e.c_engin)
	JOIN public.bateau b on (a.c_bat = b.c_bat)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?engin)
	and b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
