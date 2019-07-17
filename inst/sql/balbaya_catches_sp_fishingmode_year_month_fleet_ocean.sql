SELECT
	f.c_pays_fao::text AS country
	,EXTRACT(YEAR FROM c.d_act)::integer AS year_catch
	,EXTRACT(MONTH FROM c.d_act)::integer AS month_catch
	,act.c_ocea::integer AS ocean_code
	,act.c_tban::integer AS fishing_mode
	,sp.c_esp_3l::text AS specie_name
	,sum(c.v_poids_capt)::numeric AS catch
FROM
	public.capture c
	INNER JOIN public.bateau v ON (c.c_bat=v.c_bat)
	INNER JOIN public.pavillon f ON (f.c_pav_b=v.c_pav_b)
	INNER JOIN public.type_bateau vt ON (v.c_typ_b=vt.c_typ_b)
	INNER JOIN public.engin g ON (vt.c_engin=g.c_engin)
	INNER JOIN public.activite act ON (act.c_bat=c.c_bat AND act.d_act=c.d_act AND act.n_act=c.n_act)
	INNER JOIN public.espece sp ON (c.c_esp=sp.c_esp)
	INNER JOIN public.type_banc fm ON (act.c_tban=fm.c_tban)
WHERE
	EXTRACT(YEAR FROM c.d_act) IN year_interpolate
	AND act.c_ocea IN ocean_interpolate
	AND v.c_pav_b IN fleet_interpolate
	AND act.c_tban IN fishing_mode_interpolate
	-- Without discards
	AND c.c_esp != 8
	AND (c.c_esp NOT BETWEEN 800 AND 899)
GROUP BY
	country
	,year_catch
	,month_catch
	,ocean_code
	,fishing_mode
	,specie_name
ORDER BY
	country
	,year_catch
	,month_catch
	,ocean_code
	,fishing_mode
	,specie_name
;
