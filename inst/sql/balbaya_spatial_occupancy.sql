SELECT
	a.d_act::date AS activity_date
	,a.cwp11_act
	,a.v_nb_calees AS total_set
	,a.v_nb_calee_pos AS positive_set
	,a.v_tpec AS total_hour_fished
	,a.c_ocea::numeric AS ocean_code
	,b.c_pav_b::numeric AS country_code
	,a.c_engin::numeric AS vessel_type_code
FROM
	public.activite a
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
