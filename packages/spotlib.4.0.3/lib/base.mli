(** Basic functionns. They are accessible via [open Spotlib.Spot] *)

(** {6 Common types } *)

type ('a, 'b) poly_result = [ `Ok of 'a | `Error of 'b ]
(** Result/Either monad using polymorphic variant.  Obsolete.  Use the primitive type [result] instead. *)

(** {6 Function compositions and other operators } *)

val ( ** ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
(** Functional composition. Haskell's (.) 
    Use [power] if you want to use the original [( ** )].
*)

val ( *< ) : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)
(** Same as [( ** )]. *)

val ( *> ) : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c)
(** Functional composition in swapped order. 
    Same as Haskell's [(>>>)]
*)

external power : float -> float -> float = "caml_power_float" "pow" "float"
(** Replacement of [( ** )] *)

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"
(** Haskell's [($)]. *)

external (&~) : (f:'a -> 'b) -> 'a -> 'b = "%apply"
(** Haskell's [($)], with label 'f'. *)

external id : 'a -> 'a = "%identity"

val (|-) : 'a -> ('a -> 'b) -> 'a
(** "tee". [v |-- f] is [v] but [f v] is run before [v] is returned *)

val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
(** [flip] of Haskell *)

val (~~) : ('a -> 'b) -> f:'a -> 'b
(** Super [flip] of Haskell. The first argument is labeled and becomes commutative. *)

val flipf : ('a -> 'b) -> f:'a -> 'b
(** Super [flip] of Haskell. The first argument is labeled and becomes commutative. *)

val flip2 : ('a -> 'b -> 'c -> 'd) -> 'b -> 'c -> 'a -> 'd
(** [flip f x2 x3 x1 = f x1 x2 x3] *)
  
(** {6 Imperative operations } *)

val with_ref : 'a -> ('a ref -> 'b) -> ('a * 'b)
val with_ref_ : 'a -> ('a ref -> unit) -> 'a
(** [with_ref] and [with_ref_] runs a function with a reference
    with the specified initial value.
*)

(** {6 I/O } *)

val with_oc : out_channel -> (out_channel -> 'a) -> 'a
val with_ic : in_channel -> (in_channel -> 'a) -> 'a

(** {6 Misc things } *)

val sprintf : ('a, unit, string) format -> 'a
(** [Printf.sprintf] is available without tying [Printf.] *)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** [Printf.ksprintf] is available without tying [Printf.] *)

val (!%)    : ('a, unit, string) format -> 'a
(** [Printf.sprintf], prefix style. Bought from ITPL. *)

val (!!%)    : ('a, Format.formatter, unit) format -> 'a
(** [Format.eprintf], prefix style. *)

val memoize_gen : (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a, 'b) Hashtbl.t
(** Generic memozation by hash with fixed point. 
    You can call memoized self inside:

    memoize_gen (fun self -> .... self v ....) f
 *)

val memoize : ('c -> 'd) -> 'c -> 'd
(** Memozation by hash *)

val memoize_rec : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
(** Memozation by hash with fixed point. You can call memoized self inside:

    memoize_rec (fun self -> .... self v ....) f
 *)

val time : ('a -> 'b) -> 'a -> 'b * float
(** Simple profiling. Returns seconds. 
    If [f a] raises an exception [e], then [time f a] raises [e] too.
*)

val (+=) : int ref -> int -> unit
val (-=) : int ref -> int -> unit

val find_by_iter : (('a -> unit) -> 'collection -> unit) -> ('a -> bool) -> 'collection -> 'a option
(** find the first element where [predicate] holds by iteration [find_by_iter iterator predicate colleciton] *)

val find_in_tree : ('a -> 'a list) -> ('a -> 'res option) -> 'a -> 'res option
(** [find_in_tree get_subs p a] visits [a] and finds the first sub node 
    in which [p] holds. [get_subs] returns the sub nodes of a node. *)

val loop : ('a -> [< `Continue of 'a | `Break of 'b]) -> 'a -> 'b
(** Keep repeating a function until a final result is producted. *)

(** {6 Comparison } *)

val compare_on : ('a -> 'b) -> 'a -> 'a -> int
(** [compare_on f a b] compares [a] and [b] not by themselves 
    but by [f a] and [f b]. *)

val rev_compare_on : ('a -> 'b) -> 'a -> 'a -> int
(** [rev_compare_on f a b = - (compare_on f a b)] *)

val add_if_not_mem : 'a -> 'a list ref -> [> `AlreadyIn | `NewlyAdded ]
(** [add_if_not_mem] adds [a] to [asref] if [not (List.mem a !asref)] then return [`NewlyAdded].
    Otherwise it returns [`AlreadIn] and [asref] is not modified.
*)

(** String <-> bytes conversions *)

val (!<$) : string -> bytes

val (!>$) : bytes -> string
