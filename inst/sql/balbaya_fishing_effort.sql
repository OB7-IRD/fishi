SELECT
	pavillon.l_pav_b AS fleet
	,pavillon.l_pav_b AS flag
	,activite.c_bat AS vessel_code
	,bateau.l_bat AS vessel_label
	,port.l_port AS harbour_label
	,activite.d_act AS activity_date
	,activite.d_dbq AS landing_date
	,activite.v_tmer AS total_hour_at_sea
	,activite.v_tpec AS total_hour_fished
	,activite.v_dur_cal AS set_duration
	,ocean.c_ocea::numeric AS ocean_code
	,activite.c_engin::text AS vessel_type_code
	,bateau.c_pav_b::numeric AS country_code 
FROM 
	public.activite
	JOIN public.ocean ON (activite.c_ocea  = ocean.c_ocea)
	JOIN public.bateau ON (activite.c_bat = bateau.c_bat)
	JOIN public.pavillon ON (bateau.c_pav_b = pavillon.c_pav_b)
	JOIN public.port ON (activite.c_port = port.c_port)
WHERE
	EXTRACT(year FROM activite.d_act) IN (?time_period)
	AND bateau.c_pav_b  IN (?country)
	AND bateau.c_typ_b IN (?vessel_type)
	AND activite.c_ocea IN (?ocean)
;
