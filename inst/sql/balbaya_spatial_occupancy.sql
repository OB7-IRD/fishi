SELECT
	a.d_act::date AS activity_date
	,a.cwp11_act
	,a.v_nb_calees
	,a.v_nb_calee_pos 
	,a.v_tpec
FROM
	public.activite a
	JOIN public.bateau b on (a.c_bat = b.c_bat)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;