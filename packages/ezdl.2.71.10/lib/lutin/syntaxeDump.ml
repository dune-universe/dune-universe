(** SYNTAXE ABSTRAITE : dump 
*)

open Utils
open Lexeme
open Syntaxe

let os = ref Format.std_formatter

let doflush () = (Format.pp_print_flush !os ())

let set_std () = (os := Format.std_formatter)
let set_err () = (
	doflush () ;
	os := Format.err_formatter
)


let rec
(*----------------------------*)
(* Dump d'un pack             *)
(*----------------------------*)
dump_pack p = (
	doflush ();
	let dump_item i = (
		match i with
		LetDef l -> (
			dump_let (pack_get_let p l.it);
			Format.fprintf !os "@\n"
		) |
		ExternDef l -> (
			dump_extern (pack_get_let p l.it);
			Format.fprintf !os "@\n"
		) |
		NodeDef n -> (
			(*dump_node n*)
			dump_node (pack_get_node p n.it);
			Format.fprintf !os "@\n"
		) |
		ExceptDef x -> (
			Format.fprintf !os "exception %s@\n" x.it
		)
	) in
	List.iter dump_item p.pck_deflist
) and
(*----------------------------*)
(* Dump d'un extern           *)
(*----------------------------*)
dump_extern (li : Syntaxe.let_info) = (
	Format.fprintf !os "extern %s" li.lti_ident.it;
	(match li.lti_inputs with
		None -> (
			Format.fprintf !os " = ";
		) |
		Some palist -> (
			Format.fprintf !os "(";
			dump_typed_params palist ;
			Format.fprintf !os ") = ";
		)
	);
	(match li.lti_type with
		Some t -> (
			Format.fprintf !os " : ";
			dump_type t
		)
		| None -> ()
	);
	Format.fprintf !os "\n"
) and
(*----------------------------*)
(* Dump d'un let top-level    *)
(*----------------------------*)
dump_let (li : Syntaxe.let_info) = (
	Format.fprintf !os "let %s" li.lti_ident.it;
	(match li.lti_inputs with
		None -> () |
		Some palist -> (
			Format.fprintf !os "(";
			dump_typed_params palist ;
			Format.fprintf !os ") ";
		)
	);
	(match li.lti_type with
		Some t -> (
			Format.fprintf !os " : ";
			dump_type t
		)
		| None -> ()
	);
	Format.fprintf !os " = ";
	match li.lti_def with
	Some def -> (
		Format.fprintf !os "@\n@[<hv 3>   ";
		dump_exp def;
		Format.fprintf !os "@]@\n"
	) | None -> (
		Format.fprintf !os "\n"
	) 
) and
(*--------------------------------*)
(* Dump d'une liste d'e params    *)
(*--------------------------------*)
dump_typed_params til = (
	match til with
		[] -> ()
	|	(s,t) :: [] -> (
		Format.fprintf !os "%s: " s.it ;
		dump_param_type t
		
	)
	|  (s,t) :: x  -> (
		Format.fprintf !os "%s: " s.it ;
		dump_param_type t ;
		Format.fprintf !os "; ";
		dump_typed_params x
	)
) and
(*--------------------------------*)
(* Dump d'une liste d'ident typés *)
(*--------------------------------*)
dump_typed_idents til = (
	match til with
		[] -> ()
	|	(s,t) :: [] -> (
		Format.fprintf !os "%s: " s.it ;
		dump_type t
		
	)
	|  (s,t) :: x  -> (
		Format.fprintf !os "%s: " s.it ;
		dump_type t ;
		Format.fprintf !os "; ";
		dump_typed_idents x
	)
) and
(* ICI : vopt ignoree, a corriger *)
dump_typed_valued_idents til = (
	let dump_one_ident (s,t,vopt,range_opt) = (
		Format.fprintf !os "%s: " s.it ;
		dump_type t ;
		(match vopt with
		|	None -> ()
		|	Some e -> (
			Format.fprintf !os " = ";
			dump_exp e
                        )
                );
                (match range_opt with
                   | None -> ()
                   | Some (low,high) -> 
		       Format.fprintf !os " [";
		       dump_exp low;
                       Format.fprintf !os "; ";
		       dump_exp high;
                       Format.fprintf !os "] "
		)
	) in
	match til with
		[] -> ()
	|	[h] -> dump_one_ident h
	|  h::t -> (
		dump_one_ident h;
		Format.fprintf !os "; ";
		dump_typed_valued_idents t
	)
) and
dump_run_idents idl = (
	let dump_one_ident (s,topt,vopt) = (
		Format.fprintf !os "%s: " s.it ;
		(match topt with
		| Some t -> dump_type t
		| None -> ()
		);
		(match vopt with
		|	None -> ()
		|	Some e -> (
				Format.fprintf !os " = ";
				dump_exp e
			)
		)
	) in
	match idl with
		[] -> ()
	|	[h] -> dump_one_ident h
	|  h::t -> (
		dump_one_ident h;
		Format.fprintf !os "; ";
		dump_run_idents t
	)
) and
(*------------------------*)
(* Dump d'une exp         *) 
(*------------------------*)
(* pour le type: on omet le "ref" sauf si c'est
   explicitement demandé
*)
dump_type t = (
	match t with
   TEXP_predef Bool  -> Format.fprintf !os "bool"
|  TEXP_predef Int   -> Format.fprintf !os "int"
|  TEXP_predef Real  -> Format.fprintf !os "real"
|  TEXP_trace -> Format.fprintf !os "trace"
|	TEXP_ref  x -> dump_type (TEXP_predef x)
) and
dump_param_type t = (
	match t with
	TEXP_ref x -> (
		dump_type (TEXP_predef x) ; Format.fprintf !os " ref"
	) |
	x -> dump_type x
) and
(*------------------------*)
(* Dump d'un noeud        *)
(*------------------------*)
dump_node n = (
	Format.fprintf !os "node %s(@\n" n.ndi_ident.it;
	Format.fprintf !os "@[<hv 3>   ";
	dump_typed_valued_idents n.ndi_inputs ;
	Format.fprintf !os "@]";
	Format.fprintf !os "@\n) returns (@\n";
	Format.fprintf !os "@[<hv 3>   ";
	dump_typed_valued_idents n.ndi_outputs ;
	Format.fprintf !os "@]";
	Format.fprintf !os "@\n) =";
	Format.fprintf !os "@\n@[<hv 3>   ";
	dump_exp n.ndi_def ;
	Format.fprintf !os "@]@\n"
) and
(*------------------------*)
(* Dump d'une exp list    *) 
(*------------------------*)
dump_exp_list el = (
	match el with
		[] -> ()
	|	e :: [] -> (
		dump_exp e
	)
	|  e :: tle  -> (
		dump_exp e ;
		Format.fprintf !os ", ";
		dump_exp_list tle
	)
) and
(*------------------------*)
(* Dump d'une exp         *) 
(*------------------------*)
dump_exp e = (
let _ = 
 match e.it with
     TRUE_n -> Format.fprintf !os "true"
	| FALSE_n -> Format.fprintf !os "false"
	| ICONST_n id -> Format.fprintf !os "%s" id.it
	| RCONST_n id -> Format.fprintf !os "%s" id.it
	| IDENT_n id -> Format.fprintf !os "%s" id.it
	| PRE_n id -> Format.fprintf !os "pre %s" id.it
	| FBY_n (e1,e2) -> (
		Format.fprintf !os " {";
		dump_exp e1;
		Format.fprintf !os " fby ";
		dump_exp e2;
		Format.fprintf !os "}"
	)
	| CHOICE_n wcl -> (
		let dump_we = (
		function
			(e, None) -> (
				dump_exp e
			) |
			(e, Some w) -> (
				dump_exp e;
				Format.fprintf !os " weight ";
				dump_exp w.it
			)
		) in
		Format.fprintf !os " {@\n@[<h 3>   ";
		iter_sep dump_we (fun _ -> (Format.fprintf !os "@]@\n|@[<h 3>@\n")) wcl;
		Format.fprintf !os "@]@\n}"
	)
	| PRIO_n el -> (
		Format.fprintf !os " {@\n@[<h 3>   ";
		iter_sep dump_exp (fun _ -> (Format.fprintf !os "@]@\n|>@[<h 3>@\n")) el;
		Format.fprintf !os "@]@\n}"
	)
	| PARA_n el -> (
		Format.fprintf !os "merge {@\n@[<h 3>   ";
		iter_sep dump_brace_exp (fun _ -> (Format.fprintf !os "@]@\n&&@[<h 3>@\n")) el;
		Format.fprintf !os "@]@\n}"
	)
	| LOOP_n (f,e) -> (
		let _ = match f with
		| Strong -> Format.fprintf !os "strong loop ";
		| Weak -> Format.fprintf !os "loop ";
		in
		dump_brace_exp e
	)
(*
	| LOOPE_n (nb,e) -> (
		Format.fprintf !os "loop ";
		dump_exp nb;
		dump_brace_exp e
	)
*)
	| LOOPI_n (min,max,e) -> (
		Format.fprintf !os "loop [";
		dump_exp min;
		Format.fprintf !os ", ";
		dump_exp max;
		Format.fprintf !os "] ";
		dump_brace_exp e
	)
	| LOOPA_n (av,ecopt,e) -> (
		Format.fprintf !os "loop ~";
		dump_exp av;
		(
			match ecopt with
			Some ec -> (
				Format.fprintf !os ":";
				dump_exp ec
			) | None -> ()
		);
		dump_brace_exp e
	)
	| ASSERT_n (f, a, e) -> (
		let _ = match f with
		| Strong -> Format.fprintf !os "strong assert ";
		| Weak -> Format.fprintf !os "assert ";
		in
		dump_exp a ;
		Format.fprintf !os " in ";
		dump_brace_exp e ;
	)
	| EXIST_n (til, e) -> (
		Format.fprintf !os "exist ";
		dump_typed_valued_idents	til ;
		Format.fprintf !os " in @\n";
		dump_brace_exp e ;
	)
	| ERUN_n (vol,ce, e) -> (
		Format.fprintf !os "erun ";
		dump_run_idents vol ;
		Format.fprintf !os " = ";
		dump_exp ce ;
		Format.fprintf !os " in @\n";
		dump_brace_exp e ;
	)
	| RUN_n (idl,ce, eopt) -> (
		Format.fprintf !os "run ";
		(* dump_run_idents vol ; *)
		Format.fprintf !os "(%s)" (String.concat ", " (List.map (fun x -> x.it) idl));
		Format.fprintf !os " = ";
		dump_exp ce ;
		match eopt with
		| Some e -> 
			Format.fprintf !os " in @\n";
			dump_brace_exp e ;
		| None -> ()
	)
	| EXCEPT_n (idlst,e) -> (
		Format.fprintf !os "exception ";
		let rec pil il = match il with
		   [] -> ()
		|	[id] -> Format.fprintf !os "%s" id.it
		|	id::tail -> (
			Format.fprintf !os "%s, " id.it;
			pil tail
		) in
		pil idlst;
		Format.fprintf !os " in@\n";
		dump_brace_exp e ;
	)
	|	TRY_n (e, eco) -> (
		Format.fprintf !os " {try@\n@[<h 3>   ";
		dump_brace_exp e ;
		(match eco with
		  None -> ()
		 | Some ec -> (
			Format.fprintf !os " do ";
			dump_brace_exp ec
		)
		);
		Format.fprintf !os "@]@\n}"
	)
	|	CATCH_n (id, e, eco) -> (
		Format.fprintf !os "catch %s in " id.it;
		dump_brace_exp e;
		match eco with
		None -> ()
		| Some ec -> (
			Format.fprintf !os " do ";
			dump_brace_exp ec
		)
	)
	|	TRAP_n (id, e, eco) -> (
		Format.fprintf !os "trap %s in " id.it;
		dump_brace_exp e;
		match eco with
		None -> ()
		| Some ec -> (
			Format.fprintf !os " do ";
			dump_brace_exp ec
		)
	)
	| CALL_n (id,ops) -> (
		Format.fprintf !os "%s (" id.it ;
		dump_exp_list ops ;
		Format.fprintf !os ")"
	) |
	LET_n (mi, e) -> (
		dump_let mi ;
		Format.fprintf !os " in@\n" ;
		dump_brace_exp e
	) |
	RAISE_n id -> (
		Format.fprintf !os "raise %s" id.it
	)
in doflush ()
) and 
dump_brace_exp e = (
	match e.it with
	  CHOICE_n _
	| PRIO_n _ -> (
		dump_exp e;
	) |
	PARA_n _ -> (
		dump_exp e;
	) |
	_ -> (
		Format.fprintf !os " {";
		dump_exp e;
		Format.fprintf !os "}"
	)
)
