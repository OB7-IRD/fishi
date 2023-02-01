	select
		activite.d_act as activity_date
		,activite.v_nb_calees
		,activite.v_nb_calee_pos
		,activite.c_tban
		,activite.c_ocea  
		,activite.c_engin 
		,bateau.c_pav_b 
	FROM 
		public.activite
		join public.bateau on (activite.c_bat = bateau.c_bat)
	where
		EXTRACT(year FROM activite.d_act) IN (?time_period)
		AND bateau.c_pav_b  IN (?country)
		AND activite.c_engin  IN (?vessel_type)
		AND activite.c_ocea IN (?ocean)
;
