(* 

A) Tout ce qu'il faut pour intégrer facilement Lutin à  Lurette
--------------------------------------------------------------

- création/initialisation à partir d'un Expand.t
  (ou directement d'un fichier Lutin)
  résultat : le control_state initial

- (1) génération de contraintes satisfiables, avec :
    une valeur des ins * une valeur des pres * un control state
    -> un comportement REALISABLE (goto, vanish, raise programmé)
       + une fonction pour obtenir d'autres comportements satisfiables alternatifs 
	      (test épais du contrôle)
	 -> plus/pas de comportements

N.B. c'est un peu une liste "lazzy" des comportements possibles

- (2) pour les comportements REALISABLES de type "Goto",
  (i.e. un couple contrainte réalisable * control_state suivant) :
  génération d'une (liste) de solutions concrètes :
  (valeur des sorties, valeur des locales)
N.B. pourrait être lazzy ? ce qui n'est pas le cas pour lucky, où
on demande explicitement "n" solutions.

- pour rappeler (1), un utilitaire qui "merge" (en élagant) :
  valeur des ins * valeur des outs * valeur des locales
  -> valeur des pres 

B) Pour faire un Lutin "réactif" simple
---------------------------------------

C'est juste une version simplifiée où on fournit juste
un step qui fait une réaction complète :
valeur des ins * prg -> valeur des outs * prg suivant

N.B. dans un premier temps, c'est un step simple qu'on
  utilisera pour les appels interne "exist outs = node(ins)",
  On verra plus tard pour la version Lurette récursive !

*)

type t

val make: MainArg.t -> string list -> string -> t

(* Misc info *)
val in_var_list: t -> Exp.var list 
val out_var_list: t -> Exp.var list 
val loc_var_list: t -> Exp.var list 

type control_state

type data_state =  {
  ins : Value.OfIdent.t;
  outs: Value.OfIdent.t;
  mems: Value.OfIdent.t; 
}

type guard
val guard_to_string : guard -> string

type behavior =
| Goto of guard * control_state 
| Raise of string
| Vanish

(* lazzy-like list *)
type behavior_gen =
|  NoMoreBehavior of int (* event number *)
|  SomeBehavior of behavior * (unit -> behavior_gen)


val get_init_state: t -> control_state
val clear: t -> t

val get_init_pres: t -> Value.OfIdent.t

val get_behavior_gen : t -> Var.env_in -> Var.env -> control_state ->
  (unit -> t * behavior_gen)

val find_some_sols : t -> Thickness.formula_draw_nb -> Thickness.numeric -> guard ->
  t * guard * (Var.env_out * Var.env_loc) list

val find_one_sol : t -> guard -> t * guard * (Var.env_out * Var.env_loc)

val make_pre : Var.env_in -> Var.env_out -> Var.env_loc -> Var.env 

(* 
   May raise Deadlock  (or RdbgEvent.Error ("deadlock",event))
*)
type ctx = RdbgEvent.t
type e = RdbgEvent.t
val step: t -> control_state -> data_state -> t * control_state * data_state
val step_rdbg: ctx -> string -> t -> control_state -> data_state ->
  (ctx -> t -> control_state -> data_state -> e) -> e


(***** Interface for building simple step main loop (e.g. for run statements *)
type internal_state 

val get_init_internal_state : t -> internal_state
(* can/must be abstract ? *)
(* = { data: Var.env; ctrl: control_state }  *)
exception Stop
exception Exception of string

(* debug *)
val dump : t -> unit
val string_of_control_state : control_state -> string
