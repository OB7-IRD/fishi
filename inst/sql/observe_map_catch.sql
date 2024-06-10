-------------------------------------------------------------------------------
-- ACTIVITIES - VESSELS - OBSERVE
-------------------------------------------------------------------------------
-- Generic extraction for map_catch_distribution function from fishi
-------------------------------------------------------------------------------
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2024-06 -- v1.0 -- CL -- initial version
-------------------------------------------------------------------------------
WITH catch_data AS (
    SELECT
       	r.date as activity_date
		,v.code as vessel_code
		,s.code as species_code
		,st.homeid as school_code
		,(CASE
				WHEN a.latitude >= 0::double precision AND a.longitude >= 0::double precision THEN 1
				WHEN a.latitude < 0::double precision AND a.longitude >= 0::double precision THEN 2
				WHEN a.latitude < 0::double precision AND a.longitude < 0::double precision THEN 3
				WHEN a.latitude >= 0::double precision AND a.longitude < 0::double precision THEN 4
				ELSE 9
				END || lpad(floor(abs(a.latitude)::double precision)::text, 2, '0'::text)) || lpad(floor(abs(a.longitude)::double precision)::text, 3, '0'::text) 
			AS cwp11_act
		,a.topiaid as activity_id
		,a.setcount AS total_set
		,CASE
	            WHEN sst.code::numeric IN (0) THEN '0'
	            WHEN sst.code::numeric IN (1) THEN '1'
	            ELSE '0'
	        END AS positive_set_info
	    ,c.weight::numeric as total_catch_weight 
		,o.code::numeric as ocean_code
		,vt.code::numeric as vessel_type_code
		,ct.code::numeric as country_code
	from
		ps_logbook.catch c
		INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid 
		LEFT JOIN ps_logbook.setsuccessstatus sst ON a.setsuccessstatus = sst.topiaid
		INNER JOIN ps_logbook.route r ON a.route = r.topiaid
		INNER JOIN ps_common.trip t ON r.trip = t.topiaid
		INNER JOIN ps_common.schooltype st ON a.schooltype = st.topiaid 
		INNER JOIN common.species s ON c.species = s.topiaid
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
    ,vessel_code
    ,species_code
    ,total_catch_weight
    ,activity_id
    ,cwp11_act
    ,school_code
    ,total_set
    ,ocean_code
    ,vessel_type_code
    ,country_code
    ,(CASE
        WHEN positive_set_info = '1' THEN total_set
        ELSE '0'
    end) AS positive_set
FROM
    catch_data

