select
	cl_taille.v_classe_t as size_class
	,mensur.v_mensur
	,mensur.c_banc
	,mensur.c_esp
	,temps.an AS activity_date
	,mensur.c_ocean AS ocean_id
	,mensur.c_pav AS country_id
from
	public.cl_taille
	join public.mensur on (mensur.id_classe_t = cl_taille.id_classe_t and 
						   mensur.v_pas_cl = cl_taille.v_pas_cl)
	join public.temps on (temps.id_date = mensur.id_date)
where
	mensur.c_pecherie = 1::numeric
	and mensur.c_type_mens = 1::numeric
	and mensur.c_g_engin = 2::numeric
	and temps.an IN (?time_period)
	and mensur.c_ocean in (?ocean)
	and mensur.c_pav in (?country)
;
