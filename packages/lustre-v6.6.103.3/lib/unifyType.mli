(* Time-stamp: <modified the 13/12/2012 (at 14:24) by Erwan Jahier> *)

(** Define type-checking utilities *)

(** This unify function is quite specific. It can only unify 2 lists
    of types with at most one type variable (Any or Overload).

    Moreover, it deals with the concept of overloaded
    variable. Currently, an overloaded variable is polymorphic var
    that can only be an int or a real.
    
    [f] has 3 kinds of results:
    - the 2 lists are equal
    - the 2 lists are unifiable, via a substitution of one Any type
    - the 2 lists are not unifiable with one substitution
*)

type t = 
  | Equal
  | Unif of Lic.type_
  | Ko of string (* a msg explaining why the unification failed *)

val f : Lic.type_ list -> Lic.type_ list -> t

(**/**)

val unit_test : unit -> unit

val profile_is_compatible: Lic.node_key -> Lxm.t -> Lic.type_ list * Lic.type_ list ->
  Lic.type_ list * Lic.type_ list -> Lic.type_ option

(** nouvelle version assymétrique :
'ismatched expected_type_list given_type_list' renvoie :
   - la liste des matches nécessaires dans 'expected_type_list' pour
     le rendre "egal" à 'given_type_list'
     (n.b. vide si completement egal !)
   - raise Match_failed si pas possible
*)
exception Match_failed of string
val is_matched : Lic.type_ list -> Lic.type_ list -> Lic.type_matches
