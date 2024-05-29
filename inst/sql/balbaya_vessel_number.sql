-------------------------------------------------------------------------------------------
-- VESSELS - BALBAYA
-------------------------------------------------------------------------------------------
-- Generic extraction of the vessels informations from Balbaya
-------------------------------------------------------------------------------------------
-- Julien Lebranchu <julien.lebranchu@ird.fr>
-------------------------------------------------------------------------------------------
-- 2024-02 -- v1.0 -- JL -- initial version
-------------------------------------------------------------------------------------------
SELECT
	o.c_ocea::numeric AS ocean_id
	,o.l_ocea::text AS ocean_name
	,a.d_act::date AS activity_date
	,a.c_bat::numeric AS vessel_id
	,e.c_engin::numeric AS vessel_type_id
	,e.l_engin::text AS vessel_type_name
	,b.c_pav_b::numeric AS country_id 
	
FROM 
	public.activite a 
	JOIN public.ocean o ON (a.c_ocea  = o.c_ocea)
	JOIN public.engin e ON (a.c_engin = e.c_engin)
	JOIN public.bateau b ON (a.c_bat = b.c_bat)
	
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?engin)
	and b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
