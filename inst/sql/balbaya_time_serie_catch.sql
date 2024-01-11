select
		a.d_act::date AS activity_date
		,a.c_tban AS school_code
		,a.v_nb_calee_pos AS positive_set
		,a.v_nb_calees AS total_set
		,a.v_tpec AS total_hour_fished
		,a.v_dur_cal AS set_duration
		,c.v_poids_capt AS total_catch_weight
		,c.c_esp AS species_code
		,a.c_ocea::numeric as ocean_code
		,b.c_pav_b::numeric as country_code
		,a.c_engin::numeric as vessel_type_code
FROM 
		public.activite a
		join public.bateau b on (a.c_bat = b.c_bat)
		JOIN public.capture c on (a.c_bat = c.c_bat and a.d_act = c.d_act and a.n_act = c.n_act)
where
		EXTRACT(year FROM a.d_act) IN (?time_period)
		AND b.c_pav_b  IN (?country)
		AND a.c_engin IN (?engin)
		AND a.c_ocea IN (?ocean)
;
