select
	ocean.c_ocea::numeric as ocean_id
	,activite.c_engin::text as vessel_type_id
	,bateau.c_pav_b::numeric as country_id 
	,pavillon.l_pav_b as fleet
	,pavillon.l_pav_b as flag
	,activite.c_bat
	,bateau.l_bat
	,port.l_port as port
	,activite.d_act as activity_date
	,activite.d_dbq as landing_date
	,activite.v_tmer 
	,activite.v_tpec 
	,activite.v_dur_cal
FROM 
	public.activite
	JOIN public.ocean on (activite.c_ocea  = ocean.c_ocea)
	join public.bateau on (activite.c_bat = bateau.c_bat)
	join public.pavillon on (bateau.c_pav_b = pavillon.c_pav_b)
	join public.port on (activite.c_port = port.c_port)
where
	EXTRACT(year FROM activite.d_act) IN (?time_period)
	AND bateau.c_pav_b  IN (?country)
	AND activite.c_engin IN (?vessel_type)
	AND activite.c_ocea IN (?ocean)
;
