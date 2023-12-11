select
	activite.d_act AS activity_date
	,activite.v_nb_calees AS total_set
	,activite.v_nb_calee_pos AS positive_set
	,activite.v_tpec AS total_hour_fished
	,activite.c_tban AS school_code
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
