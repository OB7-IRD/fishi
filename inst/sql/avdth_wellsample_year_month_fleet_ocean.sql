SELECT
	fleet
	,year_wellsample
	,month_wellsample
	,ocean
	,COUNT(*) AS wellsample
FROM 
	(SELECT
	co.C_ISO3166_A3 AS fleet
	,YEAR(ech.D_DBQ) AS year_wellsample
	,MONTH(ech.D_DBQ) AS month_wellsample
	,IIF(h.V_LON_P > 20, 2, 1) AS ocean
	,ech.C_BAT
	,ech.D_DBQ
	,ech.N_CUVE
	,ech.F_POS_CUVE
	,COUNT(*) AS tmp_wellsample
FROM
	((ECHANTILLON ech
	INNER JOIN BATEAU v ON ech.C_BAT=v.C_BAT)
	INNER JOIN PAYS co ON v.C_FLOTTE=co.C_PAYS)
	INNER JOIN PORT h ON ech.C_PORT_DBQ=h.C_PORT
WHERE
	YEAR(ech.D_DBQ) = year_interpolate
	AND v.C_FLOTTE IN fleet_interpolate
GROUP BY
	co.C_ISO3166_A3
	,YEAR(ech.D_DBQ)
	,MONTH(ech.D_DBQ)
	,IIF(h.V_LON_P > 20, 2, 1)
	,ech.C_BAT
	,ech.D_DBQ
	,ech.N_CUVE
	,ech.F_POS_CUVE)
WHERE
	ocean = ocean_interpolate
GROUP BY
	fleet
	,year_wellsample
	,month_wellsample
	,ocean
;
