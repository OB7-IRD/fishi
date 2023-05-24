SELECT
    CAST(a.D_ACT AS DATE) AS activity_date
    ,o.L_OCEA AS ocean_name
    ,ttb.L_TYPE_TYPE_B AS gear
    ,p.L_PAYS AS fleet
    ,p.L_PAYS AS flag
    ,ce.C_ESP AS c_esp
    ,CAST(a.V_POIDS_CAP AS FLOAT) AS v_poids_capt
    ,tbc.L_TBANC4L AS l4c_tban
    ,a.C_OCEA AS ocean_id
    ,b.C_PAYS AS country_id
    ,ttb.C_TYP_TYPE_B AS vessel_type_id
FROM 
    (((((((ACTIVITE AS a 
    INNER JOIN BATEAU AS b ON a.C_BAT = b.C_BAT)
    INNER JOIN PAYS p ON b.C_PAYS = p.C_PAYS)
    INNER JOIN OCEAN o ON a.C_OCEA = o.C_OCEA)
    INNER JOIN TYPE_BANC tbc on a.C_TBANC = tbc.C_TBANC)
    INNER JOIN CAPT_ELEM ce ON a.C_BAT = ce.C_BAT AND a.D_DBQ = ce.D_DBQ AND a.D_ACT = ce.D_ACT AND a.N_ACT = ce.N_ACT)
    INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
    INNER JOIN TYPE_TYPE_BATEAU ttb ON tb.C_TYP_TYP_B = ttb.C_TYP_TYPE_B)
WHERE
    a.C_OCEA IN (?ocean)
    AND b.C_PAYS IN (?country)
    AND ttb.C_TYP_TYPE_B IN (?vessel_type)
    AND YEAR(a.D_ACT) IN (?time_period)
;
