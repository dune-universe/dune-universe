
(**----------------------------------------------------------

            UTILITAIRES

------------------------------------------------------------

----------------------------------------------------------*)

(**********************************************************)


(** iter_sep f s [x1;x2; ...;xn] est équivalent à :

	(f x1); s; (f x2); s; ... ; s; (f xn)
*)
let rec iter_sep f s l = (
   match l with
      [] -> ()
   |  [x] -> (f x)
   |  x::t -> (f x); (s x); iter_sep f s t
)

let _paranoid = ref false
let paranoid () = !_paranoid
let set_paranoid () = (_paranoid := true)

let stat_tab = Hashtbl.create 10

let _prof = ref false
let set_prof () = (_prof := true)

let time_C s = if !_prof then (
		let t = Sys.time () in
		let (n, tot, _lc) = try (
			Hashtbl.find stat_tab s
		) with Not_found -> (0,0.0,0.0)
	in
	Hashtbl.replace stat_tab s (n, tot, t)	
)

let time_R s = if !_prof then (
	let t = Sys.time () in
	let (n, tot, lc) = try (
		Hashtbl.find stat_tab s
	) with Not_found -> (0,0.0,0.0)
	in
	Hashtbl.replace stat_tab s (n+1, tot +. (t -. lc), 0.0)	
)

let time_P () = if !_prof then (
	let pr s (n,tot,_lc) acc = 
		(Printf.sprintf "%-18s %3d  %10.6f\n" s n tot)::acc
	in
	let lines = List.fast_sort compare (Hashtbl.fold pr stat_tab []) in
	List.iter (fun s -> prerr_string s)  lines
)
