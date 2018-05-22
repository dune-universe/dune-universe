
open Parsetree

type t

val make : name:string ->
           matcher:((core_type * core_type) -> (core_type * core_type) list option) ->
           builder:(expression list -> expression) ->
           unit -> t
(** Builds a rule with a name (of type [string]), an optional priority
   (of type [int]; lower integers represent higher priorities), a
   matcher function and a builder function. *)

val name_ : t -> string
val match_ : t -> (core_type * core_type) -> (core_type * core_type) list option
val build_ : t -> expression list -> expression
