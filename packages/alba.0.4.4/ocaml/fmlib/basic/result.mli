(** Result Monad *)

open Module_types


type ('a, 'e) t = ('a, 'e) result

val (>>=): ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t


val map: ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t


val map_error: ('e1 -> 'e2) -> ('a, 'e1) t -> ('a, 'e2) t


val throw: 'e -> ('a, 'e) t


val catch: ('a, 'e) t -> ('e -> ('a, 'e) t) -> ('a, 'e) t



(** Result Monad, satisfying the complete monadic interface as described in
{!module-type:Module_types.MONAD}.
*)
module Make (Error: ANY):
sig
    type error = Error.t

    type 'a t = ('a, error) result

    val return: 'a -> 'a t

    val (>>=): 'a t -> ('a -> 'b t) -> 'b t

    val (>=>): ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t

    val map: ('a -> 'b) -> 'a t -> 'b t

    val join: 'a t t -> 'a t

    val (<*>):  ('a -> 'b) t -> 'a t -> 'b t

    val throw: error -> 'a t

    val catch: 'a t -> (error -> 'a t) -> 'a t

    val continue: 'a t -> ('a -> 'r) -> (error -> 'r) -> 'r
end
