SELECT 
    CAST(a.D_ACT AS DATE) AS activity_date
    ,b.C_QUILLE AS c_quille
    ,CAST(b.V_CT_M3 AS FLOAT) AS catch
    ,a.C_OCEA AS ocean_id
    ,b.C_PAYS AS country_id 
    ,ttb.C_TYP_TYPE_B AS vessel_type_id

FROM 
    (((ACTIVITE AS a 
    INNER JOIN BATEAU AS b ON a.C_BAT = b.C_BAT)
    INNER JOIN TYPE_BATEAU tb ON b.C_TYP_B = tb.C_TYP_B)
    INNER JOIN TYPE_TYPE_BATEAU ttb ON tb.C_TYP_TYP_B = ttb.C_TYP_TYPE_B)
WHERE
   a.C_OCEA IN (?ocean)
   AND b.C_PAYS IN (?country)
   AND b.C_TYP_b IN (?vessel_type)
   AND ttb.C_TYP_TYPE_B IN (?vessel_type)
   AND YEAR(a.D_ACT) IN (?time_period)
;
