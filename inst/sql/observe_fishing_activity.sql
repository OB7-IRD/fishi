-------------------------------------------------------------------------------------------
-- ACTIVITIES - SETS - OBSERVE
-------------------------------------------------------------------------------------------
-- Generic extraction of the fishing operations from Observe
-------------------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------------------
-- 2024-05-29 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------------------
WITH catch_data AS (
    SELECT
        r.date AS activity_date
        ,st.homeid as school_code
        ,a.setcount AS total_set
        ,CASE
            WHEN sst.code::numeric IN (0) THEN '0'
            WHEN sst.code::numeric IN (1) THEN '1'
            ELSE '0'
        END AS positive_set_info
        ,o.code::numeric AS ocean_code
        ,vt.code::numeric AS vessel_type_code
        ,ct.code::numeric AS country_code
    FROM
        ps_logbook.activity a
    LEFT JOIN ps_logbook.setsuccessstatus sst ON a.setsuccessstatus = sst.topiaid
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN ps_common.schooltype st ON a.schooltype = st.topiaid 
    INNER JOIN common.ocean o ON t.ocean = o.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    INNER JOIN common.country ct ON v.flagcountry = ct.topiaid 
    INNER JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid
    WHERE
       EXTRACT(year FROM r.date) IN (?time_period)
       AND ct.code::numeric IN (?country)
       AND vt.code::numeric IN (?vessel_type)
       AND o.code::numeric IN (?ocean)
)
SELECT
    activity_date,
    school_code,
    total_set,
    ocean_code,
    vessel_type_code,
    country_code,
    CASE
        WHEN positive_set_info = '1' THEN total_set
        ELSE '0'
    END AS positive_set
FROM
    catch_data;
