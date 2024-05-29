-------------------------------------------------------------------------------------------
-- CATCHES - SCHOOL TYPES - BALBAYA
-------------------------------------------------------------------------------------------
-- Generic extraction of the catches by school type from Balbaya
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2023 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
SELECT
	a.d_act::date AS activity_date
	,o.l_ocea AS ocean_label
	,e.c_engin_4l AS gear
	,p.l_pav_b AS fleet
	,p.l_pav_b AS flag
	,e2.c_esp AS species_code
	,c.v_poids_capt AS total_catch_weight
	,tb.l4c_tban AS school_code
	,a.c_ocea::numeric AS ocean_code
	,b.c_pav_b::numeric AS country_code
	,a.c_engin::numeric AS vessel_type_code
	
FROM
	public.activite a
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
	JOIN public.ocean o ON (a.c_ocea = o.c_ocea)
	JOIN public.capture c ON (a.c_bat = c.c_bat 
								AND a.d_act = c.d_act 
								AND a.n_act = c.n_act)
	JOIN public.pavillon p ON (b.c_pav_b = p.c_pav_b)
	JOIN public.engin e ON (a.c_engin = e.c_engin)
	JOIN public.espece e2 ON (c.c_esp = e2.c_esp)
	JOIN public.type_banc tb ON(a.c_tban = tb.c_tban)
	
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND o.c_ocea IN (?ocean)
	AND e.c_engin IN (?engin)
	AND p.c_pav_b IN (?country)
;
