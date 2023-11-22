-------------------------------------------------------------------------------
-- VMS
-------------------------------------------------------------------------------
-- Extraction of VMS data from vms db on ot2.ird.fr
-------------------------------------------------------------------------------
-- Philippe Sabarros <philippe.sabarros@ird.fr>
-------------------------------------------------------------------------------
-- 2017-10-24 -- v1.0 -- PS -- [...]
-- 2018-10-09 -- v1.1 -- PS -- PS vessels
-- 2019-11-21 -- v2.0 -- PS -- union with historical data
-- 2019-11-25 -- v2.1 -- PS -- selected list of fields
-- 2019-11-26 -- v2.2 -- PS -- minor modifications
-- 2021-05-06 -- v2.3 -- PS -- clean_nafpositionmessage
-------------------------------------------------------------------------------

(
SELECT
v.vesselname, v.date, v.time, v.longitude, v.latitude, v.id
FROM public.archive_from_cnsp v
INNER JOIN public.turbobat t ON t."NOMBAT"=v.vesselname
WHERE v.vesselname LIKE '<VESSEL>'
AND v.date BETWEEN '<STARTDATE>' AND '<ENDDATE>'
)
UNION
(
SELECT
v.vesselname, v.date, v.time, v.longitude, v.latitude, v.id
FROM public.clean_nafpositionmessage v
INNER JOIN public.turbobat t ON t."NOMBAT"=v.vesselname
WHERE v.vesselname LIKE '<VESSEL>'
AND v.date BETWEEN '<STARTDATE>' AND '<ENDDATE>'
)
ORDER BY vesselname, date, time
--LIMIT 100
;
