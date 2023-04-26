select
	a.d_act::date as activity_date
	,a.cwp11_act 
	,a.v_tpec 
	,a.v_dur_cal
	,o.c_ocea::numeric as ocean_id
	,e.c_engin::numeric as vessel_type_id
	,b.c_pav_b::numeric as country_id
	,c.n_act 
	,c.c_bat 
from
	public.activite a
	join public.bateau b on (a.c_bat = b.c_bat)
	JOIN public.capture c on (a.c_bat = c.c_bat and a.d_act = c.d_act and a.n_act = c.n_act)
	JOIN public.engin e on (a.c_engin = e.c_engin)
	JOIN public.ocean o on (a.c_ocea  = o.c_ocea)
where
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND a.c_engin IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;