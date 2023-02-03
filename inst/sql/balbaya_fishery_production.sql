	select
			ocean.l_ocea as ocean_name
			,date_part('year'::text, activite.d_act) as year
			,engin.c_engin_4l as gear
			,engin.c_engin as vessel_type_id
			,type_banc.l4c_tban
			,pavillon.l_pav_b as fleet
			,pavillon.l_pav_b as flag
			,espece.c_esp
			,capture.v_poids_capt
			,activite.rf3
			,capture.v_poids_capt * activite.rf3 as "TOTAL_WITH_DSC"
			,espece.c_esp_3l as species_fao_code
			,capture.v_poids_capt * activite.rf3 as captures
		from
			public.activite
			join public.bateau on (activite.c_bat = bateau.c_bat)
			join public.ocean on (activite.c_ocea = ocean.c_ocea)
			JOIN public.capture on (activite.c_bat = capture.c_bat 
										and activite.d_act = capture.d_act 
										and activite.n_act = capture.n_act)
			join public.type_banc on (activite.c_tban = type_banc.c_tban)
			join public.pavillon on (bateau.c_pav_b = pavillon.c_pav_b)
			join public.type_bateau on (bateau.c_typ_b = type_bateau.c_typ_b)
			join public.engin on (type_bateau.c_engin = engin.c_engin)
			join public.espece on (capture.c_esp = espece.c_esp)
			join public.zee on (zee.c_zee = activite.c_zee 
								and zee.c_ocea = activite.c_ocea)
		WHERE
			EXTRACT(year FROM activite.d_act) IN (?time_period)
			AND ocean.c_ocea IN (?ocean)
			AND engin.c_engin IN (?vessel_type)
			AND pavillon.c_pav_b IN (?country)
;
