

(*----------------------------------------------------------
                 TYPE/BINDING CHECK 
------------------------------------------------------------

Information attachées idents au cours du type/binding check 

Le def_ident est :
- l'instance de la déclaration de l'ident
  dans le fichier source s'il existe

N.B. On distingue :
   - les macros, appelées avec (...) même si ... est vide
   - les idents simples, qui sont de simples alias 
----------------------------------------------------------*)

open Lexeme

type extern_info = {
   ed_lib_name: string;
   ed_lib_desc: Ezdl.t;
   ed_sym: Ezdl.cfunc;
}

type t = {
   ii_name : string;
   ii_def_ident : Syntaxe.ident option;
   ii_nature : nature ;
	(* result type :
		- single for macros
		- tuple for nodes
	*)
   ii_type : CkTypeEff.t list ;
	ii_hideable : bool;
} and nature =
   Formal_param
|  Support_var
|  Const_ident
|  Def_ident of Syntaxe.let_info
|  Macro_ident of (Syntaxe.let_info option * CkTypeEff.profile)
|  Node_ident of (Syntaxe.node_info option * CkTypeEff.profile)
|  External_func of (Syntaxe.let_info option * extern_info option * CkTypeEff.profile)

(* info *)
let get_nature x = x.ii_nature

let get_type x = match x.ii_type with [t] -> t | _ -> assert false

let is_hideable (x:t) = x.ii_hideable

let def_ident ii = (
	match ii.ii_def_ident with
	Some id -> id
	| None -> assert false 
)

let is_predef ii = (
	match ii.ii_def_ident with
	Some _id -> false
	| None -> true 
)

(* extern = macro sans def *)
let is_extern ii = (
	match ii.ii_nature with
	| External_func _ -> true
	| _ -> false
)

(* création *)

let of_local_cst (id : Syntaxe.ident) (te : CkTypeEff.t) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Const_ident;
		ii_type = [te];
		ii_hideable = true;
	}
)

let of_global_cst (id : Syntaxe.ident) (te : CkTypeEff.t) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Const_ident;
		ii_type = [te];
		ii_hideable = false;
	}
)

let of_predef_cst (nme : string) (te : CkTypeEff.t) = (
	{
		ii_name = nme;
		ii_def_ident = None;
		ii_nature = Const_ident ;
		ii_type = [te] ;
		ii_hideable = false;
	}
)

let of_support (id : Syntaxe.ident) (te : CkTypeEff.t) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Support_var;
		ii_type = [te] ;
		ii_hideable = true;
	}
)


let of_param (id : Syntaxe.ident) (te : CkTypeEff.t) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Formal_param;
		ii_type = [te] ;
		ii_hideable = true;
	}
)

let of_alias (id : Syntaxe.ident) (te : CkTypeEff.t) 
     (def : Syntaxe.let_info) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Def_ident def;
		ii_type = [te] ;
		ii_hideable = true;
	}
)

let of_macro (id : Syntaxe.ident) (prof : CkTypeEff.profile) 
     (def : Syntaxe.let_info) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Macro_ident (Some def, prof) ;
		(* Single out type only *)
		ii_type =  CkTypeEff.res_of_prof prof ;
		ii_hideable = true;
	}
)

let of_node (id : Syntaxe.ident) (prof : CkTypeEff.profile) 
     (def : Syntaxe.node_info) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = Node_ident (Some def, prof) ;
		(* Single out type only *)
		ii_type =  CkTypeEff.res_of_prof prof ;
		ii_hideable = true;
	}
)


(* la seule difference est qu'on ne peut pas masquer *)
let of_extern (id : Syntaxe.ident) (prof : CkTypeEff.profile)
     (def : Syntaxe.let_info)
     (einfo : extern_info option) = (
	{
		ii_name = id.it;
		ii_def_ident = Some id;
		ii_nature = External_func (Some def, einfo, prof) ;
		ii_type =  CkTypeEff.res_of_prof prof ;
		ii_hideable = false;
	}
)

let of_predef_op (nme : string) (prof : CkTypeEff.profile) = (
	{
		ii_name = nme;
		ii_def_ident = None;
		ii_nature = Macro_ident (None, prof) ;
		ii_type =  CkTypeEff.res_of_prof prof ;
		ii_hideable = false;
	}
)

let to_string i = (
	(* prints more accurate info: type or profile *)
	let (nat, typing_info) =
		let t = CkTypeEff.list_to_string i.ii_type in
		match i.ii_nature with
		| Formal_param -> ("Formal_param", t)
   	| Support_var -> ("Support_var", t)
   	| Const_ident -> ("Const_ident", t)
   	| Def_ident (_ ) -> ("Def_ident", t)
   	| Macro_ident (_ , p) -> ("Macro_ident", CkTypeEff.prof_to_string p)
   	| Node_ident (_ , p) -> ("Node_ident", CkTypeEff.prof_to_string p)
   	| External_func (_,_,p ) -> ("External_func", CkTypeEff.prof_to_string p)
	in
	nat^
	", decl: "^
	(match i.ii_def_ident with
		None -> "predef"
		| Some x -> (LutErrors.lexeme_details x.src)
	)^
	", typing: "^
	typing_info
	^
	", hideable: "^
	(if (i.ii_hideable) then "yes" else "no")
)
