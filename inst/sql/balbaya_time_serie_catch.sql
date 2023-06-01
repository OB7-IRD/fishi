select
		date_part('year'::text, a.d_act) as year
		,a.c_tban
		,a.v_nb_calee_pos
		,a.v_nb_calees
		,a.v_tpec
		,a.v_dur_cal
		,c.v_poids_capt 
		,c.c_esp 
		,a.c_ocea::numeric as ocean_id
		,b.c_pav_b::numeric as country_id 
		,a.c_engin::numeric as vessel_type_id
FROM 
		public.activite a
		join public.bateau b on (a.c_bat = b.c_bat)
	JOIN public.capture c on (a.c_bat = c.c_bat and a.d_act = c.d_act and a.n_act = c.n_act)
where
		EXTRACT(year FROM a.d_act) IN (?time_period)
		AND b.c_pav_b  IN (?country)
		AND a.c_engin IN (?vessel_type)
		AND a.c_ocea IN (?ocean)
;
