-------IMZILEN TAHA -- 22/08/2023
------- OB7 - IRD/MARBEC

WITH A AS (
    SELECT 
        o.label1 AS ocean,
        o.code AS ocean_code,
        ct1.iso3code AS flag,
        CASE
            WHEN vt.code::numeric IN (1, 2, 3) THEN 'BB'
            WHEN vt.code::numeric IN (4, 5, 6) THEN 'PS'
            WHEN vt.code::numeric IN (7) THEN 'LL'
            WHEN vt.code::numeric IN (10) THEN 'SV'
            ELSE 'OTH'
        END AS vessel_type,
        extract(year FROM r.date)::integer AS fishing_year,
        a.longitude,
        a.latitude,
        v.label1 AS vessel_name,
        oo.code AS object_code_activity,
        oo.label1 AS object_label_activity,
        tbo.code AS buoy_code_activity,
        tbo.label1 AS buoy_label_activity,
        tb.code AS buoy_id
    FROM 
        ps_common.trip AS t 
        INNER JOIN common.ocean AS o ON t.ocean = o.topiaid
        LEFT OUTER JOIN common.harbour AS h ON t.landingharbour = h.topiaid
        INNER JOIN common.country AS ct2 ON h.country = ct2.topiaid
        INNER JOIN common.vessel AS v ON t.vessel = v.topiaid
        INNER JOIN common.vesseltype AS vt ON v.vesseltype = vt.topiaid
        INNER JOIN common.country AS ct1 ON v.flagcountry = ct1.topiaid
        INNER JOIN ps_logbook.route AS r ON r.trip = t.topiaid
        INNER JOIN ps_logbook.activity AS a ON a.route = r.topiaid
        LEFT OUTER JOIN ps_common.schooltype st ON a.schooltype = st.homeid 
        LEFT OUTER JOIN ps_common.vesselactivity AS va ON a.vesselactivity = va.topiaid
        LEFT OUTER JOIN ps_logbook.floatingobject AS fo ON fo.activity = a.topiaid
        LEFT OUTER JOIN ps_common.objectoperation AS oo ON fo.objectoperation = oo.topiaid
        LEFT OUTER JOIN ps_logbook.transmittingbuoy AS tb ON tb.floatingobject = fo.topiaid
        LEFT OUTER JOIN ps_common.transmittingbuoyoperation AS tbo ON tb.transmittingbuoyoperation = tbo.topiaid
    WHERE 
        o.code::numeric IN (?ocean)
        AND ct1.code::numeric  IN (?country)
        AND (oo.code = '1' OR tbo.code = '3')
),
-- extract geom center of 1x1 grid cell from lon/lat of deployment
B AS (
    SELECT 
        fishing_year AS year,
        longitude,
        latitude,
        ST_ASText(ST_SetSRID(ST_MakePoint(longitude, latitude), 4326)) pt_geom, 
        st_snaptogrid(ST_SetSRID(ST_MakePoint(longitude, latitude), 4326), 0.5, 0.5, 1, 1) AS center_pt_geom,
        ocean_code
    FROM 
        A 
    WHERE 
        fishing_year IN (?time_period)
)
-- Aggregate and count the number of deployment in each 1x1 grid cell
SELECT  
    DISTINCT ST_AsText(st_expand(center_pt_geom , 0.5)) AS poly_geom,
    COUNT(center_pt_geom)::INT,
    year,
    ocean_code
FROM 
    B
GROUP BY 
    poly_geom,
    year,
    ocean_code;

