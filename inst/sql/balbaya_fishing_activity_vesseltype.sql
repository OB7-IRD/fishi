-------------------------------------------------------------------------------
-- ACTIVITIES - VESSELS - BALBAYA
-------------------------------------------------------------------------------
-- Generic extraction of vessels, sets, catches, and effort from Balbaya
-------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2024-01-17 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------
SELECT
	p.l_pav_b AS fleet
	,p.l_pav_b AS flag
	,a.c_bat AS vessel_code
	,b.l_bat AS vessel_label
	,p2.l_port AS harbour_label
	,a.d_act AS activity_date
	,a.d_dbq AS landing_date
	,a.v_tmer AS total_hour_at_sea
	,a.v_tpec AS total_hour_fished
	,a.v_nb_calees AS total_set
	,a.v_nb_calee_pos AS positive_set
	,a.v_dur_cal AS set_duration
	,b.c_quille AS keel_code
	,tb.l4c_tban AS school_code
	,a.cwp11_act
	,b.v_ct_m3::numeric AS catch
	,o.c_ocea::numeric AS ocean_code
	,a.c_engin::text AS vessel_type_code
	,b.c_pav_b::numeric AS country_code 
	
FROM 
	public.activite a
	JOIN public.ocean o ON (a.c_ocea  = o.c_ocea)
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
	JOIN public.pavillon p ON (b.c_pav_b = p.c_pav_b)
	JOIN public.port p2 ON (a.c_port = p2.c_port)
	JOIN public.type_banc tb ON(a.c_tban = tb.c_tban)
	
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
