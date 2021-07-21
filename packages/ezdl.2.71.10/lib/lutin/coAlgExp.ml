
(**----------------------------------------------------------

                 COMPILATION/EXPANSION

------------------------------------------------------------

Représentation des expressions algébriques.

----------------------------------------------------------*)

(**********************************************************)

type node =
   AE_true
|  AE_false
|  AE_const of string
(*
|  AE_iconst of Syntaxe.ident
|  AE_rconst of Syntaxe.ident
*)
|  AE_iconst of string
|  AE_rconst of string
|  AE_ival of int
|  AE_rval of float
|  AE_support of CoIdent.t
|  AE_pre of CoIdent.t
|  AE_alias of CoIdent.t
|  AE_call of CoIdent.t * t list
|  AE_external_call of
		CoIdent.t *
		CkIdentInfo.extern_info *
		CkTypeEff.profile *
		t list
and t = {
   ae_type : CkTypeEff.t;
   ae_ctrl : bool;
   ae_val : node ;
}

let rec sizeof e = (
	match e.ae_val with
	| AE_call (_, ops) | AE_external_call (_,_,_,ops) -> (
		List.fold_left (fun acc e -> acc + (sizeof e)) 1 ops
	)
	| _ -> 1
) 

let is_controlable ae = (ae.ae_ctrl)

let get_type ae = (ae.ae_type)

(** Batterie de "constructeurs" *)

let of_true =
	{ ae_type = CkTypeEff.boolean; ae_ctrl = false; ae_val = AE_true }

let of_false =
	{ ae_type = CkTypeEff.boolean; ae_ctrl = false; ae_val = AE_false }

let of_const s t =
	{ ae_type = t; ae_ctrl = false; ae_val = AE_const s}

let of_huge_weight =
	{ ae_type = CkTypeEff.weight; ae_ctrl = false; ae_val = AE_const LutPredef.kw_huge}

let of_iconst i =
	{ ae_type = CkTypeEff.integer; ae_ctrl = false; ae_val = AE_iconst i}

let of_ival i =
  { ae_type = CkTypeEff.integer; ae_ctrl = false; ae_val = AE_ival i}

let of_rconst r =
	{ ae_type = CkTypeEff.real; ae_ctrl = false; ae_val = AE_rconst r }

let of_rval r =
	{ ae_type = CkTypeEff.real; ae_ctrl = false; ae_val = AE_rval r }

let of_bval b =
	{ ae_type = CkTypeEff.integer; ae_ctrl = false; ae_val = (if b then AE_true else AE_false)}

let of_support i t c =
	{ ae_type = t; ae_ctrl = c; ae_val = AE_support i }

let of_pre i t =
	{ ae_type = t; ae_ctrl = false; ae_val = AE_pre i }

let of_alias i t c =
	{ ae_type = t; ae_ctrl = c; ae_val = AE_alias i }

let of_call i t args = (
   let f b ae = (b || ae.ae_ctrl) in
   let c = List.fold_left f false args in
   { ae_type = t; ae_ctrl = c; ae_val = AE_call (i, args)}
)

let of_external_call i ei prof t args = (
	{ ae_type = t; ae_ctrl = false; ae_val = AE_external_call (i, ei, prof, args)}
) 

let of_and e1 e2 = (
	of_call (CoIdent.from_string LutPredef.kw_and) CkTypeEff.boolean [e1;e2]
)

let rec of_big_and : t list -> t = function
	| [] -> of_true
	| [e] -> e
	| e::el' -> of_and e (of_big_and el')

let of_eq e1 e2 = (
	of_call (CoIdent.from_string LutPredef.kw_eq) CkTypeEff.boolean [e1;e2]
)

let of_not e1 = (
	of_call (CoIdent.from_string LutPredef.kw_not) CkTypeEff.boolean [e1]
)

let of_ite te cond e1 e2 = (
	of_call (CoIdent.from_string LutPredef.kw_ite) te [cond;e1;e2]
)

(** Affichage *)


let rec _dump
	(infx: bool)         (* infixed style = lustre-like *)
	(pr: string -> unit) (* print proc *)
	ae                   (* the exp *)
= (
(*
	printf "alg_exp { type = %s, ctrl = %B, val = "
		(CkTypeEff.to_string ae.ae_type) ae.ae_ctrl ;
*)
	match ae.ae_val with
   	AE_true    -> pr "true"
	|  AE_false   -> pr "false"
	|  AE_const s -> pr s 
(*
	|  AE_iconst id -> pr id.it
	|  AE_rconst id -> pr id.it
*)
	|  AE_iconst i  -> pr i
	|  AE_rconst i  -> pr i
	|  AE_ival i  -> pr (string_of_int i)
	|  AE_rval r  -> pr (string_of_float r)
	|  AE_support s -> pr (CoIdent.to_string s)
	|  AE_pre s     -> pr "pre "; pr (CoIdent.to_string s)
	|  AE_alias s   -> pr (CoIdent.to_string s)
	|  AE_external_call (s, _, _,args)
	|  AE_call (s, args) -> (
		let print_prefixed nme args = (
			pr nme ; pr " (";
			Utils.iter_sep 
				(_dump infx pr)
				(function _ -> pr ", ")
				args ;
			pr ")"
		) in
		let print_infixed sepl args = (
			pr "(" ; pr (List.hd sepl);
			let pa_n_sep a sep = (
				_dump infx pr a;
				pr sep
			) in
			List.iter2 pa_n_sep args (List.tl sepl) ;
			pr ")" ;
		) in
		let nme = (CoIdent.to_string s) in 
		if (infx) then (
			match (LutPredef.as_infixed_syntax nme) with
			Some seplist -> (
				print_infixed seplist args
			) |
			None -> print_prefixed nme args
		) else print_prefixed nme args
	)
)

let dump ae = _dump false (fun s -> print_string s) ae

let dumpf os ae = _dump false (fun s -> output_string os s) ae

let lus_dumpf os ae = _dump true (fun s -> output_string os s) ae

let lus_dumps ae = (
	let zebuff = Buffer.create 512 in
	_dump true (fun s -> Buffer.add_string zebuff s) ae;
	let res = Buffer.contents zebuff in
	Buffer.reset zebuff;
	res
)

(* *)
let (_to_expr : t -> Expr.t) = 
  fun ae ->
	match ae.ae_val with
   	AE_true    -> Expr.True
	|  AE_false   -> Expr.False
	|  AE_const _str -> assert false
	|  AE_iconst i  -> Expr.Ival (Num.num_of_string i)
	|  AE_rconst r  -> Expr.Fval (float_of_string r)
	|  AE_ival i    -> Expr.Ival (Num.num_of_int i)
	|  AE_rval r    -> Expr.Fval r
	|  AE_support s -> Expr.Var s
	|  AE_alias s   -> Expr.Var (CoIdent.to_string s)
	|  AE_pre _s     -> assert false (* pr "pre "; pr (CoIdent.to_string s) *)
	|  AE_external_call (_s, _, _,_args) -> assert false
	|  AE_call (_s, _args) -> assert false

