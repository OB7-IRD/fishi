select
	t0.annee as "Year",
	t0.v0 as "TOTAL",
	t1.v1 as "#sets",
	t2.v2 as "Catch >0",
	t3.v3 as "Effort > 1 d",
	t4.v4 as "Effort > 5 d"
from
	(
--T0 start
	select
		date_part('year'::text, activite.d_act) as annee,
		count(distinct activite.cwp11_act) as v0
	from
		activite,
		cat_bateau,
		bateau
	where
		activite.c_bat = bateau.c_bat
		and cat_bateau.c_cat_b = bateau.c_cat_b
		and activite.c_ocea = 1::numeric
		and (bateau.c_typ_b = any (array[4::numeric,
		5::numeric,
		6::numeric]))
		and (bateau.c_pav_b = any (array[1::numeric,
		41::numeric]))
	group by
		date_part('year'::text, activite.d_act)) t0
-- T0 end
left join (
	select
		date_part('year'::text, activite.d_act) as annee,
		count(distinct activite.cwp11_act) as v1
	from
		activite,
		cat_bateau,
		bateau
	where
		activite.c_bat = bateau.c_bat
		and cat_bateau.c_cat_b = bateau.c_cat_b
		and activite.c_ocea = 1::numeric
		and (bateau.c_typ_b = any (array[4::numeric,
		5::numeric,
		6::numeric]))
			and (bateau.c_pav_b = any (array[1::numeric,
			41::numeric]))
				and activite.v_nb_calees > 0::numeric
			group by
				date_part('year'::text, activite.d_act)) t1 on
	t1.annee = t0.annee
left join (
	select
		date_part('year'::text, activite.d_act) as annee,
		count(distinct activite.cwp11_act) as v2
	from
		activite,
		cat_bateau,
		bateau
	where
		activite.c_bat = bateau.c_bat
		and cat_bateau.c_cat_b = bateau.c_cat_b
		and activite.c_ocea = 1::numeric
		and (bateau.c_typ_b = any (array[4::numeric,
		5::numeric,
		6::numeric]))
			and (bateau.c_pav_b = any (array[1::numeric,
			41::numeric]))
				and activite.v_nb_calee_pos > 0::numeric
			group by
				date_part('year'::text, activite.d_act)) t2 on
	t2.annee = t0.annee
left join (
	select
		r1.annee,
		count(distinct r1.cwp11_act) as v3,
		sum(r1.t_pec) as sum
	from
		(
		select
			date_part('year'::text, activite.d_act) as annee,
			activite.cwp11_act,
			sum(activite.v_tpec) as t_pec
		from
			activite,
			cat_bateau,
			bateau
		where
			activite.c_bat = bateau.c_bat
			and cat_bateau.c_cat_b = bateau.c_cat_b
			and activite.c_ocea = 1::numeric
			and (bateau.c_typ_b = any (array[4::numeric,
			5::numeric,
			6::numeric]))
				and (bateau.c_pav_b = any (array[1::numeric,
				41::numeric]))
			group by
				date_part('year'::text, activite.d_act),
				activite.cwp11_act
			having
				round(sum(activite.v_tpec) / 12::numeric, 0) > 1::numeric
			order by
				sum(activite.v_tpec)) r1
	group by
		r1.annee) t3 on
	t3.annee = t0.annee
left join (
	select
		r1.annee,
		count(distinct r1.cwp11_act) as v4,
		sum(r1.t_pec) as sum
	from
		(
		select
			date_part('year'::text, activite.d_act) as annee,
			activite.cwp11_act,
			sum(activite.v_tpec) as t_pec
		from
			activite,
			cat_bateau,
			bateau
		where
			activite.c_bat = bateau.c_bat
			and cat_bateau.c_cat_b = bateau.c_cat_b
			and activite.c_ocea = 1::numeric
			and (bateau.c_typ_b = any (array[4::numeric,
			5::numeric,
			6::numeric]))
				and (bateau.c_pav_b = any (array[1::numeric,
				41::numeric]))
			group by
				date_part('year'::text, activite.d_act),
				activite.cwp11_act
			having
				round(sum(activite.v_tpec) / 12::numeric, 0) > 5::numeric
			order by
				sum(activite.v_tpec)) r1
	group by
		r1.annee) t4 on
	t4.annee = t0.annee;