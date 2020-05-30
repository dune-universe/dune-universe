type ('a,'error) t = [`Ok of 'a | `Error of 'error]

(** Monad.T2 with type ('a,'error) t := [`Ok of 'a | `Error of 'error], but
    Infix contains extra operators *)
include Monad.S2  with type ('a,'error) t := ('a,'error) t
include Monad.EX2 with type ('a,'error) t := ('a,'error) t
module Infix : sig
  include Monad.Infix2 with type ('a,'error) t := ('a,'error) t

  val (>>=!) : ('a, 'e) t -> ('e -> ('a, 'e2) t) -> ('a, 'e2) t  
  (** bindE *)

  val (>>|!) : ('a, 'e) t -> ('e -> 'e2) -> ('a, 'e2) t  
  (** mapE *)
end
include module type of struct include Infix end

val fail : 'a -> [> `Error of 'a ]

val catch : (fail:('error -> 'exn) -> 'a) -> [> ('a, 'error) t]
(** [catch f] runs [f ~fail]. 

    If [f ~fail] returns a value [v] then [catch f] returns [`Ok v].

    If [f ~fail] calls [fail e], then the execution of the [f ~fail] immediately
    exists and [catch f] returns [`Error e]

    Any exception raised in [f ~fail] is not caught.
*)

val catch_exn : (unit -> 'a) -> [> ('a, exn) t ]
(** [catch_exn f] runs [f ()]. 

    If [f ()] returns a value [v] then [catch f] returns [`Ok v].

    If an exception [exn] raised in [f ()], [catch f] returns [`Error exn].
*)

val from_Ok : [< ('a, 'error) t] -> 'a
(** Haskell's fromJust *)

val result : ('a -> 'b) -> ('c -> 'b) -> [< ('a, 'c) t] -> 'b
(** Haskell's either *)

val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t

val at_Error : ('err -> 'a) -> [< ('a, 'err) t] -> 'a
(** [at_Error = result id] *)

val to_option : [< ('a, 'err) t] -> 'a option
(** Conversion to option type *)
  
module Stdlib : sig
  val ok : 'a -> [> `Ok of 'a ]
  val ng : 'a -> [> `Error of 'a ]
  (** No Good *)

  val from_Ok : [< ('a, 'error) t] -> 'a
  (** Raises [Spotlib.Spot.Result.IsError] when not [Ok _] *)

  val result : ('a -> 'b) -> ('c -> 'b) -> [< ('a, 'c) t] -> 'b
  (** Haskell's either *)

  val at_Error : ('err -> 'a) -> [< ('a, 'err) t] -> 'a
  (** [at_Error = result id] *)
end

