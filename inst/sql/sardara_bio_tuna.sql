select
	cl_taille.v_classe_t as cl
	,mensur.v_mensur
	,mensur.id_date
	,mensur.c_ocean
	,mensur.c_pav
	,mensur.c_pecherie
	,mensur.c_type_mens
	,mensur.c_banc
	,mensur.c_g_engin
	,mensur.c_esp
	,temps.an 
from
	public.cl_taille
	join public.mensur on (mensur.id_classe_t = cl_taille.id_classe_t and 
						   mensur.v_pas_cl = cl_taille.v_pas_cl)
	join public.temps on (temps.id_date = mensur.id_date)
where
--	mensur.c_ocean = 1::numeric
--	mensur.c_pav = 1::numeric
	mensur.c_pecherie = 1::numeric
	and mensur.c_type_mens = 1::numeric
	and mensur.c_g_engin = 2::numeric
	and temps.an IN (?time_period)
	and mensur.c_ocean in (?ocean)
	and mensur.c_pav in (?country)
;
