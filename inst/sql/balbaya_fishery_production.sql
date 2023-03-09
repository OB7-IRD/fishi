	select
			a.d_act as activity_date
			,o.l_ocea AS ocean_name
			,e.c_engin_4l as gear
			,p.l_pav_b as fleet
			,p.l_pav_b as flag
			,e2.c_esp
			,c.v_poids_capt
			,a.rf3
			,tb.l4c_tban
		from
			public.activite a
			join public.bateau b on (a.c_bat = b.c_bat)
			join public.ocean o on (a.c_ocea = o.c_ocea)
			JOIN public.capture c on (a.c_bat = c.c_bat 
										and a.d_act = c.d_act 
										and a.n_act = c.n_act)
			join public.pavillon p on (b.c_pav_b = p.c_pav_b)
			join public.engin e on (a.c_engin = e.c_engin)
			join public.espece e2 on (c.c_esp = e2.c_esp)
			join public.type_banc tb on(a.c_tban = tb.c_tban)
		WHERE
			EXTRACT(year FROM a.d_act) IN (?time_period)
			AND o.c_ocea IN (?ocean)
			AND e.c_engin IN (?vessel_type)
			AND p.c_pav_b IN (?country)
;
