SELECT
	a.cwp11_act
	,c.n_act AS activity_id
	,c.d_act::date AS activity_date
	,b.c_bat AS vessel_code
	,c.c_esp AS species_code
	,c.v_poids_capt AS total_catch_weight
	,a.v_nb_calee_pos AS positive_set
	,a.c_tban AS school_code
	,a.c_ocea AS ocean_code	
	,b.c_pav_b::numeric AS country_code
	,tb.c_engin::numeric AS vessel_type_code
FROM
	public.activite a
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
	JOIN public.ocean o ON (a.c_ocea = o.c_ocea)
	JOIN public.capture c ON (a.c_bat = c.c_bat 
								AND a.d_act = c.d_act 
								AND a.n_act = c.n_act)
	JOIN public.pavillon p ON (b.c_pav_b = p.c_pav_b)
	JOIN public.type_bateau tb ON (b.c_typ_b = tb.c_typ_b)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND o.c_ocea IN (?ocean)
	AND tb.c_engin IN (?engin)
	AND b.c_pav_b IN (?country)
;
