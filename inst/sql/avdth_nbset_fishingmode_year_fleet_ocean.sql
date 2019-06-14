SELECT
	co.C_ISO3166_A3 AS fleet
	,YEAR(act.D_ACT) AS year_nbset
	,MONTH(act.D_ACT) AS month_nbset
	,o.C_OCEA AS ocean
	,tb.L_TBANC4L AS fishing_mode
	,COUNT(*) AS nb_set
FROM
	((((ACTIVITE act
	INNER JOIN OCEAN o ON act.C_OCEA=o.C_OCEA)
	INNER JOIN BATEAU v ON act.C_BAT=v.C_BAT)
	INNER JOIN PAYS co ON v.C_FLOTTE=co.C_PAYS)
	INNER JOIN TYPE_BANC tb ON act.C_TBANC=tb.C_TBANC)
	INNER JOIN OPERA op ON act.C_OPERA=op.C_OPERA
WHERE
	YEAR(act.D_ACT) = year_interpolate
	AND v.C_FLOTTE IN fleet_interpolate
	AND act.V_NB_OP <> 0
	AND o.C_OCEA = ocean_interpolate
GROUP BY
	YEAR(act.D_ACT)
	,MONTH(act.D_ACT)
	,o.C_OCEA
	,co.C_ISO3166_A3
	,tb.L_TBANC4L
;
