SELECT
	a.d_act::date AS activity_date
	,a.cwp11_act
	,a.v_nb_calees
	,a.v_nb_calee_pos 
	,a.v_tpec
	,a.c_ocea::numeric as ocean_id
	,b.c_pav_b::numeric as country_id 
	,a.c_engin::numeric as vessel_type_id
FROM
	public.activite a
	JOIN public.bateau b on (a.c_bat = b.c_bat)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
