SELECT
	co.C_ISO3166_A3 AS fleet
	,YEAR(w.D_DBQ) AS year_wellsample
	,MONTH(w.D_DBQ) AS month_wellsample
	,IIF(h.V_LON_P > 20, 2, 1) AS ocean
	,COUNT(*) AS totwell
FROM
	CUVE w
	INNER JOIN BATEAU v ON w.C_BAT=v.C_BAT
	INNER JOIN PAYS co ON v.C_FLOTTE=co.C_PAYS
	INNER JOIN MAREE t ON (w.C_BAT=t.C_BAT AND w.D_DBQ=t.D_DBQ)
	INNER JOIN PORT h ON t.C_PORT_DBQ=h.C_PORT
WHERE
	YEAR(w.D_DBQ) = year_interpolate
	AND v.C_FLOTTE IN fleet_interpolate
	AND IIF(h.V_LON_P > 20, 2, 1) = ocean_interpolate
GROUP BY
	fleet
	,year_wellsample
	,month_wellsample
	,ocean
;
