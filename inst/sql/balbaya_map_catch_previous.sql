select
	a.cwp11_act
	,c.n_act
	,c.d_act
	,b.c_bat
	,c.c_esp
	,c.v_poids_capt
	,a.v_nb_calee_pos
	,a.c_ocea
	,a.c_tban
	,b.c_pav_b
	,b.c_pav_b::numeric as country_id
	,tb.c_engin::numeric as vessel_type_id
	,b.c_pav_b::NUMERIC AS ocean_id
	
from
	public.activite a
	join public.bateau b on (a.c_bat = b.c_bat)
	join public.ocean o on (a.c_ocea = o.c_ocea)
	JOIN public.capture c on (a.c_bat = c.c_bat 
								and a.d_act = c.d_act 
								and a.n_act = c.n_act)
	join public.pavillon p on (b.c_pav_b = p.c_pav_b)
	join public.type_bateau tb on (b.c_typ_b = tb.c_typ_b)
WHERE
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND o.c_ocea IN (?ocean)
	AND tb.c_engin IN (?engin)
	AND b.c_typ_b IN (?vessel_type)
	AND b.c_pav_b IN (?country)
;
