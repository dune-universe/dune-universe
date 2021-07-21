
(**----------------------------------------------------------

                 COMPILATION/EXPANSION

------------------------------------------------------------

Représentation des expressions de traces

N.B. un identificateur unique est associé
à chaque expression de boucle complexe, ce qui les rend
de fait toutes différentes

----------------------------------------------------------*)

(**********************************************************)

open Utils

(** compteur des expressions de boucle *)
let loop_cpt_number = ref 0;;
let reset () = (
   loop_cpt_number := 0
)
let new_loop_cpt () = (
   let res = !loop_cpt_number in
   incr loop_cpt_number;
   res
)

let nb_loops () = !loop_cpt_number

(* type loop_weighter = int -> (int, int) *)

(* exist-scope = list of ident + init_value option *)
type escope = (CoIdent.t * CoAlgExp.t option) list 

let add_escope ctx (i,v) = (i,v)::ctx
let new_escope () = []

type src_info = CoIdent.scope_stack

type 't t =
|	TE_eps
|	TE_ref of CoIdent.t
|  TE_constraint of CoAlgExp.t * src_info
|  TE_fby of 't t * 't t
|  TE_prio of 't t list
|  TE_para of 't t list
|  TE_choice of ('t t * CoAlgExp.t option) list
|  TE_dyn_choice of int * ((int * 't t) list)
|  TE_noeps of 't t
|  TE_loop of 't t
|  TE_omega of 't t
|  TE_loopi of int * CoAlgExp.t * CoAlgExp.t * 't t * src_info
|  TE_loopa of int * CoAlgExp.t * CoAlgExp.t option * 't t * src_info
(* internal loop with inline weigth computer + compteur *)
|  TE_dyn_loop of (int -> int * int) * int * 't t
|  TE_assert of CoAlgExp.t * 't t * src_info
|  TE_strong_assert of CoAlgExp.t * 't t * src_info
|  TE_exist of escope * 't t
|  TE_raise of string 
|	TE_try of 't t * 't t option
|  TE_catch of string * 't t * 't t option
(* internal run *)
|  TE_erun of string * escope * CoAlgExp.t list * 't t
|	TE_dyn_erun      of string * Reactive.prg      * CoIdent.t list * CoAlgExp.t list * 't t 
|	TE_dyn_erun_ldbg of string * 't Reactive.prg_ldbg * CoIdent.t list * CoAlgExp.t list * 't t 
|  TE_run of string * CoAlgExp.t * escope * CoAlgExp.t list * 't t * src_info
|	TE_dyn_run of string * Reactive.prg * CoAlgExp.t * escope * CoAlgExp.t list * 't t * src_info
|	TE_dyn_run_ldbg of string * 't Reactive.prg_ldbg * CoAlgExp.t * escope * CoAlgExp.t list * 't t * src_info

(** Batterie de créateurs *)

(** WARNING: var scopes are built in reverse order *)
let of_erun runid scop args e = TE_erun (runid, List.rev scop, args, e)

let of_run runid andexp scop args e si = TE_run (runid, andexp, List.rev scop, args, e, si)

let of_constraint (ea : CoAlgExp.t) (si:src_info) = ( TE_constraint (ea,si) )
let of_ref (s : CoIdent.t) = ( TE_ref s )
let of_loop (e : 't t) = (TE_loop e)
let of_omega (e : 't t) = (TE_omega e)
let _of_noeps (e : 't t) = (TE_noeps e)
let of_fby (e1 : 't t) (e2 : 't t) = (TE_fby (e1,e2))
let of_choice lst = TE_choice lst
let of_prio lst = TE_prio lst
let of_para lst = TE_para lst

let of_loope nb e si = (TE_loopi (new_loop_cpt (), nb,nb,e,si))
let of_loopi min max e si = (TE_loopi (new_loop_cpt (), min,max,e,si))
let of_loopa moy ect e si = (TE_loopa (new_loop_cpt (), moy,ect,e,si))

let of_assert c e si = (TE_assert (c,e,si))
let of_strong_assert c e si = (TE_strong_assert (c,e,si))

let of_exist ctx e = (TE_exist (ctx,e))

let of_raise s = (TE_raise s)

let of_catch i e eo = (TE_catch (i,e,eo))
let of_try e eo = (TE_try (e,eo))

(** Pretty print *)

let rec _dump (pr: string -> unit) te = (
	match te with
	|	TE_eps -> pr "eps"
	|	TE_ref s -> pr (CoIdent.to_string s)
	|	TE_constraint (ae,_) -> (
			pr "{" ; pr (CoAlgExp.lus_dumps ae) ; pr "}" ;
		)
	|	TE_fby (te1, te2) -> (
		pr " {";
		_dump pr te1; pr " fby "; _dump pr te2;
		pr "}";
	)
	|	TE_choice wtel -> (
		let dump_we = function
		| (e, None) -> _dump pr e
		| (e, Some w) -> (
				_dump pr e; pr " weight "; pr (CoAlgExp.lus_dumps w)
		) in
		pr " {";
		iter_sep dump_we (fun _ -> (pr " | ")) wtel;
		pr "}";
	)
	|	TE_dyn_choice (_, wtel) -> (
		let dump_we = function
		| (1, e) -> _dump pr e
		| (w, e) -> (
				_dump pr e; pr " weight "; pr (string_of_int w)
		) in
		pr " {";
		iter_sep dump_we (fun _ -> (pr " | ")) wtel;
		pr "}";
	)
	|	TE_prio el -> (
		pr " {";
		iter_sep (_dump pr) (fun _ -> (pr " |> ")) el;
		pr "}";
	)
	|	TE_para el -> (
		pr "{";
		iter_sep (_dump pr) (fun _ -> (pr " &> ")) el;
		pr "}";
	)
	|	TE_loop te -> (
		pr "loop ";
		match te with
		|	TE_choice _ -> _dump pr te
		|	_ -> pr "{"; _dump pr te; pr "}"
	)
	|	TE_omega te -> (
		pr "loop omega ";
		match te with
		|	TE_choice _ -> _dump pr te
		|	_ -> pr "{"; _dump pr te; pr "}"
	)
	|	TE_noeps te -> (
		pr "noeps ";
		match te with
		|	TE_choice _ -> _dump pr te
		|	_ -> pr "{"; _dump pr te; pr "}"
	)
	|	TE_loopi (_cpt,min,max,e,_) -> (
      pr "loop [";
      pr (CoAlgExp.lus_dumps min);
      pr ", ";
      pr (CoAlgExp.lus_dumps max);
      pr "] ";
      match e with
      |	TE_choice _ -> _dump pr e
      | _ -> ( pr " {"; _dump pr e; pr "}" )
	)
	|	TE_loopa (_cpt,av, ecopt,e,_) -> (
      pr "loop ~";
      pr (CoAlgExp.lus_dumps av);
		let _ = match ecopt with
		| Some ec -> pr ":"; pr (CoAlgExp.lus_dumps ec)
		| None -> ()
		in
      match e with
      | TE_choice _ -> _dump pr e
		| _ -> ( pr " {"; _dump pr e; pr "}" )
	)
	|	TE_dyn_loop (_f,_n,e) -> (
      pr "loop (*internal*) ";
      match e with
      | TE_choice _ -> _dump pr e
		| _ -> ( pr " {"; _dump pr e; pr "}" )
	)
	|	TE_assert (a, e,_) -> (
      pr "assert ";
      pr (CoAlgExp.lus_dumps a) ;
      pr " in ";
      _dump pr e ;
	)
	|	TE_strong_assert (a, e,_) -> (
      pr "strong assert ";
      pr (CoAlgExp.lus_dumps a) ;
      pr " in ";
      _dump pr e ;
	)
	|	TE_exist (ctx,e) -> (
      pr "exist ";
		let pp (i,vo) = (
			pr (CoIdent.to_string i);
			let _ = match vo with
				| Some v ->	(
      			pr " init ";
					pr (CoAlgExp.lus_dumps v) ;
				)
				| None -> ()
			in
			pr ";"
		) in
		List.iter pp ctx;
      pr " in ";
      _dump pr e ;
	)
	|	TE_erun (rid, vars, args, e) -> (
      pr "erun ";
		let pp (i,vo) = (
			pr (CoIdent.to_string i);
			let _ = match vo with
				| Some v ->	(
      			pr " init ";
					pr (CoAlgExp.lus_dumps v) ;
				)
				| None -> ()
			in
			pr ";"
		) in
		List.iter pp vars;
		pr " = "; pr rid ;
		pr "(";
		let al = List.map CoAlgExp.lus_dumps args in
		pr (String.concat "," al);
		pr ")";
      pr " in ";
      _dump pr e ;
	)
	|	TE_run (rid, andexp, vars, args, e, _) -> (
      pr "run ";
		let pp (i,vo) = (
			pr (CoIdent.to_string i);
			let _ = match vo with
				| Some v ->	(
      			pr " init ";
					pr (CoAlgExp.lus_dumps v) ;
				)
				| None -> ()
			in
			pr ";"
		) in
		List.iter pp vars;
		pr " = "; pr rid ;
		pr "(";
		let al = List.map CoAlgExp.lus_dumps args in
		pr (String.concat "," al);
		pr ") then (";
      	pr (CoAlgExp.lus_dumps andexp) ;
      pr ")";
      pr " in ";
      _dump pr e ;
	)
	|	TE_dyn_erun_ldbg (rid, _, vars, args, e) 
	|	TE_dyn_erun (rid, _, vars, args, e) -> (
      pr "erun ";
		let pp i = (
			pr (CoIdent.to_string i);
			pr ";"
		) in
		List.iter pp vars;
		pr " = "; pr rid ;
		pr "(";
		let al = List.map CoAlgExp.lus_dumps args in
		pr (String.concat "," al);
		pr ")";
      pr " in ";
      _dump pr e ;
	)
	|	TE_dyn_run_ldbg (rid, _, _, vars, args, e,_) 
	|	TE_dyn_run (rid, _, _, vars, args, e,_) -> (
      pr "erun ";
		let pp (i,vo) = (
			pr (CoIdent.to_string i);
			let _ = match vo with
				| Some v ->	(
      			pr " init ";
					pr (CoAlgExp.lus_dumps v) ;
				)
				| None -> ()
			in
			pr ";"
		) in
		List.iter pp vars;
		pr " = "; pr rid ;
		pr "(";
		let al = List.map CoAlgExp.lus_dumps args in
		pr (String.concat "," al);
		pr ")";
      pr " in ";
      _dump pr e ;
	)
	|	TE_raise s -> (
		pr "raise "; pr s
	)

	|	TE_catch (i,e,eco) -> (
		pr "{ catch "; pr i; pr " in";
      let _ = match e with
      |	TE_choice _ -> _dump pr e
		| _ -> ( pr " {"; _dump pr e; pr "}")
		in
		pr "}";
		match eco with
		| Some ee -> (
			pr " do";
      	match ee with
      	| TE_choice _ -> _dump pr ee
			| _ -> ( pr " {"; _dump pr ee; pr "}")
		)
		| _ -> ()
	)
	|	TE_try (e,eco) -> (
		pr "try " ;
      let _ = match e with
      | TE_choice _ -> _dump pr e
		| _ -> ( pr " {"; _dump pr e; pr "}")
		in
		match eco with Some ee -> (
			pr " do";
      	match ee with
      	|	TE_choice _ -> _dump pr ee
			| _ -> ( pr " {"; _dump pr ee; pr "}")
		) | _ -> ()
	)
)

let dump te = _dump (fun s -> print_string s) te 

let dumps te = (
	let zebuff = Buffer.create 512 in
	_dump (fun s -> Buffer.add_string zebuff s) te;
	let res = Buffer.contents zebuff in
	Buffer.reset zebuff;
	res
)
