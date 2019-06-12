SELECT 
	*
FROM (SELECT
	co.C_ISO3166_A3 AS fleet
	,YEAR(t.D_DBQ) AS year_nblanding
	,MONTH(t.D_DBQ) AS month_nblanding
	,IIF(h.V_LON_P > 20, 2, 1) AS ocean
	,COUNT(*) AS nb_landing
FROM
	((MAREE t
	INNER JOIN BATEAU v ON t.C_BAT=v.C_BAT)
	INNER JOIN PAYS co ON v.C_FLOTTE=co.C_PAYS)
	INNER JOIN PORT h ON v.C_FLOTTE=h.C_PORT
WHERE
	YEAR(t.D_DBQ) = year_interpolate
	AND v.C_FLOTTE IN fleet_interpolate
GROUP BY
	co.C_ISO3166_A3
	,YEAR(t.D_DBQ)
	,MONTH(t.D_DBQ)
	,IIF(h.V_LON_P > 20, 2, 1))
WHERE
	ocean = ocean_interpolate
;
