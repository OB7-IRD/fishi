SELECT
	c.d_act::date AS activity_date
	,c.v_poids_capt::numeric AS catch
FROM 
	public.capture c 
WHERE
	EXTRACT(year FROM c.d_act) IN (?time_period)
;
