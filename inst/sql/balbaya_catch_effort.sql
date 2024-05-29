-------------------------------------------------------------------------------------------
-- CATCHES - EFFORT - BALBAYA
-------------------------------------------------------------------------------------------
-- Generic extraction of data related to the spatial distribution and effort from Balbaya
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2024-01-17 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
SELECT
	a.cwp11_act
	,c.n_act AS activity_id
	,c.d_act::date AS activity_date
	,a.v_nb_calee_pos AS positive_set
	,a.v_nb_calees AS total_set
	,a.v_tpec AS total_hour_fished
	,a.v_dur_cal AS set_duration
	,c.v_poids_capt AS total_catch_weight
	,b.c_bat AS vessel_code
	,c.c_esp AS species_code
	,a.c_tban AS school_code
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
	
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?engin)
	AND a.c_ocea IN (?ocean)
;
