(* Time-stamp: <modified the 26/02/2015 (at 13:46) by Erwan Jahier> *)

(** Static evaluation of constants. *)

(** Evaluation statique des expressions "réputées" constantes : defs
	 de constantes, tailles de tableaux, indices et step des arrays. *)

(*
	
PARAMETRES :
	Pour avoir qq chose de générique, les fonctions
	sont paramétrées par un "IdSolver.t", qui contient deux fonctions :
	(voir Lic. 

	type IdSolver.t = {
		id2const : Lv6Id.idref -> Lxm.t -> const_eff
		id2type : Lv6Id.idref -> Lxm.t -> const_eff
	}
	(N.B. on passe le lexeme pour déventuels messages d'erreurs)

FONCTION PRINCIPALE : 
	Elle lève "Compile_error lxm msg" en cas d'erreur.

	eval_const
		(env : IdSolver.t) 
		(vexp : val_exp)
	-> const_eff list

	(N.B. dans le cas général, une val_exp peut dénoter un tuple, on
	rend donc bien une liste de constante)

FONCTIONS DERIVEES : (permet de préciser les messages d'erreur)
	Elles lèvent "EvalArray_error msg" en cas d'erreur,
	se qui permet de récupérer l'erreur. 

	eval_array_size
		(env : IdSolver.t) 
		(vexp : val_exp)
	-> int

	(N.B. ne renvoie que des taille correctes : > 0)

	eval_array_index
		(env : IdSolver.t) 
		(vexp : val_exp)
		(sz : int)
	-> int 

	(N.B. on doit préciser la taille sz du tableau) 

	eval_array_slice
		(env : IdSolver.t) 
		(sl : slice_info srcflagged)
		(sz : int)
		(lxm : Lxm.t)
	-> slice_eff

	N.B. slice_info_eff = {first: int; last: int; step: int; width: int}
	N.B. on doit passer le lexeme de l'opération à cause d'éventuels
	warnings
*)


exception EvalArray_error of string

val f : IdSolver.t -> AstCore.val_exp -> Lic.const list

(**
   Rôle : calcule une taille de tableau 
   
   Entrées: 

   Sorties :
	int (strictement positif)

   Lic.ts de bord :
	EvalArray_error "bad array size, type int expected but get <t>" si t pas int
	EvalArray_error "bad array size <n>" si n <= 0
*)
val eval_array_size : IdSolver.t -> AstCore.val_exp -> int

(**
   Rôle :

   Entrées :
	id_solver, val_exp, taille du tableau

   Sorties :
	int (entre 0 et taille du tableau -1)

   Lic.ts de bord :
	EvalArray_error msg si pas bon
*)
val eval_array_index : IdSolver.t -> AstCore.val_exp -> Lxm.t -> int

(**
   Rôle :

   Entrées :
	id_solver, slice_info, size du tableau,
	lxm (source de l'opération slice pour warning)
   Sorties :
	slice_info_eff, i.e.
	(fisrt,last,step,width) tels que step <> 0 et
	- si step > 0 alors 0<=first<=last et first<=sz
	- si step < 0 alors 0<=last<=first et first<=sz
	- 1<=width<=sz 
   Lic.ts de bord :
	EvalArray_error msg si pas bon
*)
val eval_array_slice : 
  IdSolver.t -> AstCore.slice_info -> Lxm.t -> Lic.slice_info
