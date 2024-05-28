WITH catch_data AS (
    SELECT
        r.date AS activity_date
        ,(CASE
			WHEN a.latitude >= 0::double precision AND a.longitude >= 0::double precision THEN 1
			WHEN a.latitude < 0::double precision AND a.longitude >= 0::double precision THEN 2
			WHEN a.latitude < 0::double precision AND a.longitude < 0::double precision THEN 3
			WHEN a.latitude >= 0::double precision AND a.longitude < 0::double precision THEN 4
			ELSE 9
			END || lpad(floor(abs(a.latitude)::double precision)::text, 2, '0'::text)) || lpad(floor(abs(a.longitude)::double precision)::text, 3, '0'::text) 
		AS cwp11_act
        ,CASE
			WHEN st.code::numeric IN (0) THEN 'IND'
	 		WHEN st.code::numeric IN (1) THEN 'BO'
			WHEN st.code::numeric IN (2) THEN 'BL'
	 		END AS school_code
        ,a.setcount AS total_set
        ,r.fishingtime as total_hour_fished
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
    activity_date
    ,cwp11_act
    ,school_code
    ,total_set
    ,total_hour_fished
    ,ocean_code
    ,vessel_type_code
    ,country_code
    ,(CASE
        WHEN positive_set_info = '1' THEN total_set
        ELSE '0'
    end) AS positive_set
FROM
    catch_data

