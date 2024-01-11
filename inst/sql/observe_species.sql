SELECT
	sp.faocode as FAO
	,sp.scientificlabel as scientificname
	,s.label1 as speciesgroup 
FROM common.species sp 
	INNER JOIN common.speciesgroup s ON (s.topiaid = sp.speciesgroup)
;
