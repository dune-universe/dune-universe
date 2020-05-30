include module type of struct include Monad_intf end

module Make1(M : S1) : T1 with type 'a t := 'a M.t
(** Build a full Monad interface of [T1] from the minimum specification of [S1].
    Note that it only builds functions.  The type is not exported.
*)

module Make(M : S1) : T1 with type 'a t := 'a M.t
(** Synonym of [Make1] *)

module Make2(M : S2) : T2 with type ('a, 'z) t := ('a, 'z) M.t
(** Build a full Monad interface of data type with 2 parameters, [T2], 
    from the minimum specification of [S2].
    Note that it only builds functions.  The type is not exported.
*)
