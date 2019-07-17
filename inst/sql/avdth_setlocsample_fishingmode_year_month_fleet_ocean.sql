SELECT
	co.C_ISO3166_A3 AS fleet
	,YEAR(echc.D_ACT) AS year_set
	,MONTH(echc.D_ACT) AS month_set
	,o.C_OCEA AS ocean
	,tb.L_TBANC4L AS fishing_mode
	,echc.Q_ACT AS quadrant
	,echc.V_LAT AS latitude
	,echc.V_LON AS longitude
FROM
	(((((ECH_CALEE echc
	INNER JOIN ACTIVITE act ON (echc.C_BAT=act.C_BAT AND echc.D_DBQ=act.D_DBQ AND echc.D_ACT=act.D_ACT AND echc.N_ACT=act.N_ACT))
	INNER JOIN OCEAN o ON (act.C_OCEA=o.C_OCEA))
	INNER JOIN BATEAU v ON (act.C_BAT=v.C_BAT))
	INNER JOIN PAYS co ON (v.C_FLOTTE=co.C_PAYS))
	INNER JOIN TYPE_BANC tb ON (act.C_TBANC=tb.C_TBANC))
	INNER JOIN OPERA op ON (act.C_OPERA=op.C_OPERA)
WHERE
	YEAR(echc.D_ACT) = year_interpolate
	AND v.C_FLOTTE IN fleet_interpolate
	AND o.C_OCEA = ocean_interpolate
	AND act.C_TBANC = fishing_mode_interpolate
ORDER BY
	co.C_ISO3166_A3
	,YEAR(echc.D_ACT)
	,MONTH(echc.D_ACT)
	,o.C_OCEA
	,tb.L_TBANC4L
;
