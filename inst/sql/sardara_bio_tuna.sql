-------------------------------------------------------------------------------------------
-- SIZE - WEIGHT - SARDARA
-------------------------------------------------------------------------------------------
-- Generic extraction of size and weight of major tuna catches from Sardara
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2023 -- v1.0 -- CL -- initial version
-- 2024-06-10 -- v1.1 -- CL -- Update school code (adapt to observe)
-------------------------------------------------------------------------------------------
SELECT
	cl_taille.v_classe_t AS size_class
	,mensur.v_mensur AS estimated_individual
	,CASE
		WHEN mensur.c_banc IN (1) THEN 'FOB'
	 	WHEN mensur.c_banc IN (2) THEN 'FSC'
	 	WHEN mensur.c_banc IN (3) THEN 'FSC'
	 	WHEN mensur.c_banc IN (9) THEN 'FSC'
	 END AS school_type
	,mensur.c_esp AS species_code
	,temps.an AS activity_date
	,mensur.c_ocean AS ocean_code
	,mensur.c_pav AS country_code
FROM
	public.cl_taille
	join public.mensur ON (mensur.id_classe_t = cl_taille.id_classe_t AND 
						   mensur.v_pas_cl = cl_taille.v_pas_cl)
	join public.temps ON (temps.id_date = mensur.id_date)
WHERE
	mensur.c_pecherie = 1::numeric
	AND mensur.c_type_mens = 1::numeric
	AND mensur.c_g_engin = 2::numeric
	AND temps.an IN (?time_period)
	AND mensur.c_ocean in (?ocean)
	AND mensur.c_pav in (?country)
;
