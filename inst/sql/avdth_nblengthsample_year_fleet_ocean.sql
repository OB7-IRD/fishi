SELECT
	co.C_ISO3166_A3 AS fleet
	,YEAR(echf.D_DBQ) AS year_sample
	,MONTH(echf.D_DBQ) AS month_sample
	,IIF(h.V_LON_P > 20, 2, 1) AS ocean
	,COUNT(*) AS nb_sample
FROM
	(((ECH_FREQT echf
	INNER JOIN BATEAU v ON echf.C_BAT=v.C_BAT)
	INNER JOIN PAYS co ON v.C_FLOTTE=co.C_PAYS)
	INNER JOIN MAREE t ON (echf.C_BAT=t.C_BAT AND echf.D_DBQ=t.D_DBQ))
	INNER JOIN PORT h ON t.C_PORT_DBQ=h.C_PORT
WHERE
	YEAR(echf.D_DBQ) = 2015
	AND v.C_FLOTTE IN (1, 41)
	AND IIF(h.V_LON_P > 20, 2, 1) = 1
GROUP BY
	co.C_ISO3166_A3
	,YEAR(echf.D_DBQ)
	,MONTH(echf.D_DBQ)
	,IIF(h.V_LON_P > 20, 2, 1)
;
