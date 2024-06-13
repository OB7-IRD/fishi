-------------------------------------------------------------------------------
-- VMS ACTIVITIES - PURSE SEINE 
-------------------------------------------------------------------------------
-- Generic extraction of vms activities from vms
-------------------------------------------------------------------------------
-- Philippe Sabarros <philippe.sabarros@ird.fr>
-- Esther Mollier <esther.mollier@ird.fr>
-- Clara Lerebourg <clara.lerebourg@ird.fr>
-------------------------------------------------------------------------------
-- 2024-06-13 -- v1.0 -- CL -- adapt for fishi
-------------------------------------------------------------------------------
(SELECT
	v.vesselname AS vessel
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
	AND t."PAYS"::numeric  IN (?country)
	AND v.vesselname IN (?vessel))
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
	AND t."PAYS"::numeric  IN (?country)
	AND v.vesselname IN (?vessel))
ORDER BY vesselname, date, time
;
