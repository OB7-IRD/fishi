select
	a.cwp11_act,
	round(sum(
        case
            when c.c_esp = 1::numeric then c.poids
            else 0.00000001
        end), 2) as yft,
	round(sum(
        case
            when c.c_esp = 2::numeric then c.poids
            else 0.00000001
        end), 2) as skj,
	round(sum(
        case
            when c.c_esp = 3::numeric then c.poids
            else 0.00000001
        end), 2) as bet,
	round(sum(
        case
            when c.c_esp = any (array[1::numeric, 2::numeric, 3::numeric]) then c.poids
            else 0.00000001
        end), 2) as total
from
	activite a
join (
	select
		capture.n_act,
		capture.d_act,
		bateau.c_bat,
		capture.c_esp,
		sum(capture.v_poids_capt) as poids
	from
		capture
	join bateau on
		capture.c_bat = bateau.c_bat
	join type_bateau on
		bateau.c_typ_b = type_bateau.c_typ_b
	join engin on
		engin.c_engin = type_bateau.c_engin
	where
		date_part('year'::text, capture.d_act) = ((
		select
			max(date_part('year'::text, a2.d_act)) - 1::double precision as max
		from
			activite a2))
		and bateau.c_pav_b = 1::numeric
		and type_bateau.c_engin = 1::numeric
	group by
		capture.n_act,
		capture.d_act,
		bateau.c_bat,
		capture.c_esp) c on
	a.c_bat = c.c_bat
	and a.n_act = c.n_act
	and a.d_act = c.d_act
where
	a.v_nb_calee_pos > 0::numeric
	and a.c_ocea = 1::numeric
	and a.c_tban = 1::numeric
group by
	a.cwp11_act;