select
		activite.d_act as activity_date
		,activite.v_nb_calees
		,activite.v_nb_calee_pos
		,activite.v_tpec
		,activite.v_dur_cal
		,activite.c_tban
		,activite.c_ocea::numeric as ocean_id
		,bateau.c_pav_b::numeric as country_id 
		,activite.c_engin::numeric as vessel_type_id
FROM 
		public.activite
		join public.bateau on (activite.c_bat = bateau.c_bat)
where
		EXTRACT(year FROM activite.d_act) IN (?time_period)
		AND bateau.c_pav_b  IN (?country)
		AND activite.c_engin  IN (?engin)
		AND activite.c_ocea IN (?ocean)
;
