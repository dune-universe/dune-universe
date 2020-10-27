open Common_types

type error =
  | Unbound_variable of string
  | Mutliple_declaration of id
  | Mutliple_declaration_field of id
  | General of (Format.formatter -> unit)
  | Unbound_field of (Format.formatter -> unit)

exception Error of error * Loc.t

type vtyp =
  | Texp
  | Tarr
  | Tobj
  | Tint
  | Tstr

module type Object = sig
  type k
  type v
  module M : Map.S with type key = k
  val v : v M.t
  val typ : vtyp option
end

type value =
  | Vint of int
  | Varray of value array
  | Vobj of value t_object
  | Vlp_exp of Expr.t
  | Vstring of string

(* hack to avoid recursive modules *)
and 'a t_object = (module Object with type v = 'a and type k = value)

module Value : sig
  type t = value
  val compare : t -> t -> int
  val print_value : Format.formatter -> t -> unit
end

module Object : sig
  type 'a t = 'a t_object
  val mem : 'a t -> Value.t -> bool
  val compare : Value.t t -> Value.t t -> int
  val add : Value.t -> 'a -> 'a t -> Loc.t -> 'a t
  val empty : 'a t
  val find : Value.t -> 'a t -> Loc.t -> 'a
  val bindings : 'a t -> (Value.t * 'a) list
  val map : ('a -> 'b) -> 'a t -> 'b t
  val merge : (Value.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
end

val print_val : value -> Format.formatter -> unit

val vtyp : value -> vtyp

module Ident :
  sig
    type t = string
    val compare : String.t -> String.t -> int
    val create : 'a -> 'a
  end

module Env :
  sig
    module IdMap : Map.S with type key = Map.Make(Ident).key
                          and type 'a t = 'a Map.Make(Ident).t

    type t = { values : value IdMap.t; types : Common_types.typ IdMap.t; }
    val empty : t
    val find_val : t -> IdMap.key -> Loc.t -> value
    val find_typ : t -> IdMap.key -> Loc.t -> Common_types.typ
    val add_table : 'a IdMap.t -> IdMap.key -> 'a -> Loc.t -> 'a IdMap.t
    val add : t -> IdMap.key -> value -> Common_types.typ -> Loc.t -> t
    val add_typ : t -> IdMap.key -> Common_types.typ -> Loc.t -> t
    val bind_var : t -> IdMap.key -> value -> Loc.t -> t
    val int_typ : t -> IdMap.key list
    val bool_typ : t -> IdMap.key list
    val get_val : t -> IdMap.key -> value option
    val replace_var : t -> IdMap.key -> value -> t
  end

type bool_result =
  | Constraints of Lp.constr list
  | Bool of bool

type btyp =
  | Tconstr
  | Tbool

val btyp : bool_result -> btyp

val report_error : Format.formatter -> error -> unit
