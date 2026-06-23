-------------------------------------------------------------------------------
-- ACTIVITIES - VESSELS - BALBAYA
-------------------------------------------------------------------------------
-- Generic extraction of vessels, sets, catches, and effort from Balbaya
-------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2024-01-17 -- v1.0 -- CL -- initial version
-- 2024-06-10 -- v1.1 -- CL -- Update school code (adapt to observe)
-- 2026-06-23 -- v1.2 -- VL -- Update due to the spatialization of fishing effort with T3R
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- ACTIVITIES - VESSELS - BALBAYA
-------------------------------------------------------------------------------
-- Generic extraction of vessels, sets, catches, and effort from Balbaya
-------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2024-01-17 -- v1.0 -- CL -- initial version
-- 2024-06-10 -- v1.1 -- CL -- Update school code (adapt to observe)
-- 2026-06-23 -- v1.2 -- VL -- Update due to the spatialization of fishing effort with T3R
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
	,sum(v_tmer) OVER (PARTITION BY b.c_bat, a.d_act) AS total_hour_at_sea_computed
	,a.v_tpec AS total_hour_fished
	,sum(v_tpec) OVER (PARTITION BY b.c_bat, a.d_act) AS total_hour_fished_computed
	,a.v_nb_calees AS total_set
	,a.v_nb_calee_pos AS positive_set
	,a.v_dur_cal AS set_duration
	,b.c_quille AS keel_code
	,CASE
		WHEN tb.c_tban::numeric IN (1) THEN 'FOB'
	 	WHEN tb.c_tban::numeric IN (2) THEN 'FSC'
		WHEN tb.c_tban::numeric IN (3) THEN 'UND'
	 END AS school_type
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
	EXTRACT(year FROM a.d_act) IN (2025)
	AND b.c_pav_b  IN (1)
	AND b.c_typ_b IN (4,5,6)
	AND a.c_ocea IN (1)
	and a.d_act = '2025-02-25%'
	and b.l_bat= 'CAP BOJADOR'
;
