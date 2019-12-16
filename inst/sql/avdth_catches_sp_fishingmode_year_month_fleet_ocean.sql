SELECT 
	co.C_ISO3166_A3 AS fleet
	,YEAR(act.D_ACT) AS year_catch
	,MONTH(act.D_ACT) AS month_catch
	,o.C_OCEA AS ocean
	,tb.C_TBANC AS fishing_mode
	,sp.C_ESP_3L AS specie_name
	,sum(c.V_POIDS_CAPT) AS catch
FROM
	((((((ACTIVITE act
	INNER JOIN OCEAN o ON act.C_OCEA=o.C_OCEA)
	INNER JOIN BATEAU v ON act.C_BAT=v.C_BAT)
	INNER JOIN PAYS co ON v.C_FLOTTE=co.C_PAYS)
	INNER JOIN TYPE_BANC tb ON act.C_TBANC=tb.C_TBANC)
	INNER JOIN OPERA op ON act.C_OPERA=op.C_OPERA)
	INNER JOIN CAPT_ELEM c ON (act.C_BAT=c.C_BAT AND act.D_DBQ=c.D_DBQ AND act.D_ACT=c.D_ACT AND act.N_ACT=c.N_ACT))
	INNER JOIN ESPECE sp ON c.C_ESP=sp.C_ESP
WHERE
	YEAR(act.D_ACT) = year_interpolate
	AND v.C_FLOTTE IN fleet_interpolate
	AND act.V_NB_OP <> 0
	AND o.C_OCEA = ocean_interpolate
	AND act.C_TBANC = fishing_mode_interpolate
GROUP BY
	co.C_ISO3166_A3
	,YEAR(act.D_ACT)
	,MONTH(act.D_ACT)
	,o.C_OCEA
	,tb.C_TBANC
	,sp.C_ESP_3L
;
