(*----------------------------------------------------------------------
	module : Verbose
	date :
------------------------------------------------------------------------
-- New: 2010/11, debug flag system

	description :

	Affichage verbeux avec appel "printf-like" :

Verbose.put "format" args...

N.B. ça a l'air de marcher, seul problème c'est peut-être
     l'efficacité, vu que la string '"format" args...' est
     évaluée même si on n'est pas en mode verbose ...
     Autrement dit : c'est l'affichage qui est 'bloqué'
     en non-verbose, pas l'évaluation du message ... 

N.B. VERSION GORE : le kprintf n'est appelé que si besoin,
     sinon c'est une branche "gore" qui est appelée :
     - evalue le nb d'args attendu ****ATTENTION = nbre de % !!!
     - "dépile" ces nb args sans rien en faire

----------------------------------------------------------------------*)

type flag = bool ref

let _no_flag = ref false

let _flag_tab : (string, flag) Hashtbl.t = Hashtbl.create 10
(* warning: last in first ! *)
let _flag_list : string list ref = ref []

let get_flag s = (
	try (
		Hashtbl.find _flag_tab s
	) with Not_found -> (
		let res = ref false in
		Hashtbl.add _flag_tab s res;
		_flag_list := s::!_flag_list;
		res
	) 
)
let set_flag f = (f := true)
let flag_list () = !_flag_list

(* type msg = string Lazy.t *)

let _level = ref 0

let on () = ( _level := 1 )
let off () = ( _level := 0 )
let set (l:int) = ( _level := l )

let level () = !_level

(**** VERSION PAS TROP GORE *****)
let printf ?(level=2) ?(flag=_no_flag) s = (
	Printf.kprintf (fun t -> 
     if (!flag || (!_level >= level)) then
       (prerr_string t; flush stderr) else ()) s 
) 
let put = printf

let print_string ?(level=1) ?(flag=_no_flag) s =
  if (!flag || (!_level >= level)) then 
    (prerr_string s; flush stderr)

let exe ?(level=2) ?(flag=_no_flag) f = (
  if (!flag || (!_level >= level)) then 
    f () else ()
)

(**** VERSION GORE *****)
(*
let count_args s = (
	let max = (String.length s) -1  in
	let res = ref 0 in
	for i = 0 to max do
		if ((String.unsafe_get s i) = '%') 
		then (incr res)
		else ()
	done;
	!res
) 

let put s = (
	if (!_level > 0) then (
		(* let toto = string_of_format s in *)
		(* print_string toto ; flush stdout; *)
		Printf.kprintf (fun t -> prerr_string t) s 
	) else (
		let toto = string_of_format s in
		(* print_string toto ; flush stdout; *)
		let rec f n x = (
		Printf.printf "appel de f %d\n" n; flush stdout;
			if (n > 0) then (
				Obj.magic (f (n-1))
			) else (Obj.magic ())
		) in
		let nbargs = count_args toto in 
		Obj.magic ( f nbargs)
	)
)
*)

(* put "%d %s %d\n" 42 "toto" 43; flush stderr;; *)
