open Base

module Env : sig
  (* env is a map from strings (fields) to bit strings *)
  type t = (string, Bitstring.t) Hashtbl.t [@@deriving sexp]
  include Comparator.S with type t := t
end

(** [of_str_lst lst] is [env] where [x] maps to [Bistring.of_binary v] in [env] 
    if [(x,v)] is in [lst] *)
val of_str_lst : (string * string) list -> Env.t

type env_set = Set.M(Env).t

(** [to_str_lst set] is the list of elements of the form [(x1,s1);...;(xn,sn)] 
    for some [(x1,b1);...;(xn,bn)] in set such that
    si=[Bitstring.to_string bi] *)
val to_str_lst : env_set -> (string * string) list list

(** [eval ~env exp] is the set of mappings from field values to bit strings 
    produced by [exp] evaluated on [env]. Note that fields unspecified by [env]
    are assumed to be 0. *)
val eval : env:Env.t -> Ast.exp -> env_set