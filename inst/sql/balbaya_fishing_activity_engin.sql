-------------------------------------------------------------------------------
-- ACTIVITIES - ENGINS - BALBAYA
-------------------------------------------------------------------------------
-- Generic extraction of sets and catches from Balbaya
-------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lereboourg@ird.fr>
-------------------------------------------------------------------------------
-- 2024-01-17 -- v1.0 -- CL -- initial version
-- 2024-06-10 -- v1.1 -- CL -- Update school code (adapt to observe)
-------------------------------------------------------------------------------
SELECT
	activite.d_act AS activity_date
	,activite.v_nb_calees AS total_set
	,activite.v_nb_calee_pos AS positive_set
	,activite.v_tpec AS total_hour_fished
	,CASE
		WHEN activite.c_tban::numeric IN (1) THEN 'FOB'
	 	WHEN activite.c_tban::numeric IN (2) THEN 'FSC'
		WHEN activite.c_tban::numeric IN (3) THEN 'UND'
	 END AS school_code
	,activite.v_dur_cal AS set_duration
	,activite.c_ocea::numeric AS ocean_code
	,bateau.c_pav_b::numeric AS country_code
	,activite.c_engin::numeric AS vessel_type_code
	
FROM 
	public.activite 	
	JOIN public.bateau on (activite.c_bat = bateau.c_bat)
	
WHERE
	EXTRACT(year FROM activite.d_act) IN (?time_period)
	AND bateau.c_pav_b  IN (?country)
	AND activite.c_engin  IN (?engin)
	AND activite.c_ocea IN (?ocean)
;
