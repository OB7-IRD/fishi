(SELECT
	v.vesselname
	,v.date
	,v.time 
	,v.longitude 
	,v.latitude 
	,v.id
FROM 
	public.archive_from_cnsp v
	INNER JOIN public.turbobat t ON t."NOMBAT"=v.vesselname
WHERE
	EXTRACT(year FROM v.date) IN (?time_period)
	AND t."PAYS"::numeric  IN (?country))
UNION
(SELECT
	v.vesselname 
	,v.date
	,v.time 
	,v.longitude
	,v.latitude
	,v.id
FROM 
	public.clean_nafpositionmessage v
	INNER JOIN public.turbobat t ON t."NOMBAT"=v.vesselname
WHERE
	EXTRACT(year FROM v.date) IN (?time_period)
	AND t."PAYS"::numeric  IN (?country))
;
