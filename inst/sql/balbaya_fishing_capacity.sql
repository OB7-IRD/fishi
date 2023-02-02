select
	a.d_act::date as activity_date
	,b.c_quille
	,b.v_ct_m3::numeric as catch
	,b.c_typ_b::numeric as vessel_type_id
	,b.c_pav_b::numeric as country_id
from
	public.activite a
	join public.bateau b on (a.c_bat = b.c_bat)
	JOIN public.ocean o on (a.c_ocea  = o.c_ocea)
where
	EXTRACT(year FROM a.d_act) IN (?time_period)
	AND b.c_pav_b  IN (?country)
	AND b.c_typ_b IN (?vessel_type)
	AND a.c_ocea IN (?ocean)
;
