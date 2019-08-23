SELECT
	f.c_pays_fao::text AS country
	,EXTRACT(YEAR FROM act.d_act)::integer AS year_set
	,EXTRACT(MONTH FROM act.d_act)::integer AS month_set
	,act.c_ocea::integer AS ocean_code
	,act.c_tban::integer AS fishing_mode
	,act.c_engin::integer AS gear
	,sum(act.v_nb_calees) AS nb_set
	,sum(act.v_nb_calee_neg) AS nb_neg_set
	,sum(act.v_nb_calee_pos) AS nb_pos_set
FROM
	public.activite act
	INNER JOIN public.bateau v ON (act.c_bat=v.c_bat)
	INNER JOIN public.pavillon f ON (f.c_pav_b=v.c_pav_b)
	INNER JOIN public.type_bateau vt ON (v.c_typ_b=vt.c_typ_b)
	INNER JOIN public.engin g ON (vt.c_engin=g.c_engin)
	INNER JOIN public.type_banc fm ON (act.c_tban=fm.c_tban)
WHERE
	EXTRACT(YEAR FROM act.d_act) IN year_interpolate
	AND act.c_ocea IN ocean_interpolate
	AND v.c_pav_b IN fleet_interpolate
	AND act.c_tban IN fishing_mode_interpolate
	AND act.c_engin IN gear_interpolate
GROUP BY
	f.c_pays_fao
	,EXTRACT(YEAR FROM act.d_act)
	,EXTRACT(MONTH FROM act.d_act)
	,act.c_ocea
	,act.c_tban
	,act.c_engin
ORDER BY
	f.c_pays_fao
	,EXTRACT(YEAR FROM act.d_act)
	,EXTRACT(MONTH FROM act.d_act)
	,act.c_ocea
	,act.c_tban
	,act.c_engin
;
