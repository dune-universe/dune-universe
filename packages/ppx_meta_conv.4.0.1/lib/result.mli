(** {6 Result monad } *)

type ('a, 'error) t = ('a, 'error) result

(** monadic ops *)

val bind : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
val (>>=) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t

val fmap : ('a -> 'b) -> ('a, 'error) t -> ('b, 'error) t
val (>>|) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
(** Same as [fmap] but as a binop *)

val map : ('a -> ('b, 'error) t) -> 'a list -> ('b list, 'error) t
val mapi : (int -> 'a -> ('b, 'error) t) -> 'a list -> ('b list, 'error) t

val fail : 'error -> ('a, 'error) t

val catch : (fail:('error -> 'exn) -> 'a) -> ('a, 'error) t
val catch_exn : (unit -> 'a) -> ('a, exn) t

val result : ('a -> 'b) -> ('error -> 'b) -> ('a, 'error) t -> 'b
(** same as Haskell's [either] *)  

module Open : sig
  val (>>=) : ('a, 'error) t -> ('a -> ('b, 'error) t) -> ('b, 'error) t
  val (>>|) : ('a, 'error) t -> ('a -> 'b) -> ('b, 'error) t
end
