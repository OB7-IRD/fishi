SELECT 
effort.cwp11_act,
effort.activity_id,
effort.activity_date,
effort.positive_set,
effort.total_set,
--effort.total_hour_fished as total_hour_fished_old,
ROUND(hour_fished_computed/nb_sets_in_the_day,2) AS total_hour_fished,
effort.set_duration,
catches.total_catch_weight,
effort.vessel_code,
catches.species_code,
effort.school_type,
effort.ocean_code,
effort.vessel_type_code,
effort.country_code
	FROM
	(
		SELECT
			a.cwp11_act
			,a.n_act AS activity_id
			,a.d_act::date AS activity_date
			,a.v_nb_calee_pos AS positive_set
			,a.v_nb_calees AS total_set
			,a.v_tpec AS total_hour_fished
			,sum(v_tpec) OVER (PARTITION BY b.c_bat, a.d_act) AS hour_fished_computed
			,sum(a.v_nb_calees) OVER (PARTITION BY b.c_bat, a.d_act) AS nb_sets_in_the_day
			,a.v_dur_cal AS set_duration
			,b.c_bat AS vessel_code
			,CASE
				WHEN a.c_tban::numeric IN (1) THEN 'FOB'
			 	WHEN a.c_tban::numeric IN (2) THEN 'FSC'
				WHEN a.c_tban::numeric IN (3) THEN 'UND'
			 END AS school_type
			,o.c_ocea::numeric AS ocean_code
			,e.c_engin::numeric AS vessel_type_code
			,b.c_pav_b::numeric AS country_code
			FROM
			public.activite a
			JOIN public.bateau b ON (a.c_bat = b.c_bat)
			JOIN public.engin e ON (a.c_engin = e.c_engin)
			JOIN public.ocean o ON (a.c_ocea  = o.c_ocea)
			JOIN public.pavillon p ON (b.c_pav_b = p.c_pav_b)
			JOIN public.type_bateau tb ON (b.c_typ_b = tb.c_typ_b)
			--WHERE b.c_bat = 483
			--AND a.d_act = '2025-01-03'
) AS effort
INNER JOIN
(
		SELECT
		a.cwp11_act
		,c.n_act AS activity_id
		,c.d_act::date AS activity_date
		,a.v_nb_calee_pos AS positive_set
		,c.v_poids_capt AS total_catch_weight
		,b.c_bat AS vessel_code
		,c.c_esp AS species_code
		,CASE
			WHEN a.c_tban::numeric IN (1) THEN 'FOB'
		 	WHEN a.c_tban::numeric IN (2) THEN 'FSC'
			WHEN a.c_tban::numeric IN (3) THEN 'UND'
		 END AS school_type
		,o.c_ocea::numeric AS ocean_code
		,e.c_engin::numeric AS vessel_type_code
		,b.c_pav_b::numeric AS country_code
	FROM
		public.activite a
		JOIN public.bateau b ON (a.c_bat = b.c_bat)
		JOIN public.capture c ON (a.c_bat = c.c_bat 
									AND a.d_act = c.d_act 
									AND a.n_act = c.n_act)
		JOIN public.engin e ON (a.c_engin = e.c_engin)
		JOIN public.ocean o ON (a.c_ocea  = o.c_ocea)
		JOIN public.pavillon p ON (b.c_pav_b = p.c_pav_b)
		JOIN public.type_bateau tb ON (b.c_typ_b = tb.c_typ_b)
		--WHERE b.c_bat = 483
		--AND a.d_act = '2025-01-03'
) AS catches
ON effort.vessel_code  = catches.vessel_code
AND effort.activity_date  = catches.activity_date
AND effort.activity_id  = catches.activity_id
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?engin)
	AND a.c_ocea IN (?ocean)
;
;
