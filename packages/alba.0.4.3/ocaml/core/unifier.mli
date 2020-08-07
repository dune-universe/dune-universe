module type HOLES =
sig
    include Gamma_algo.GAMMA

    val context: t -> Gamma.t

    val expand: Term.t -> t -> Term.t

    val is_hole: int -> t -> bool

    val value: int -> t -> Term.t option

    val fill_hole0: int -> Term.t -> bool -> t -> t

    val fold_entries:
        (int -> int -> string -> Term.typ -> bool -> Term.t option -> 'a -> 'a)
        -> t
        -> 'a
        -> 'a
end




module Make (H: HOLES):
sig
    val unify: Term.typ -> Term.typ -> bool -> H.t -> H.t option
    (**
         [unify act req is_super gh]

         Unify the type [act] with the type [req] using [gh] as the context with
         holes.

         [is_super] indicates if the typ [req] can be regarded as a supertype of
         [act].
    *)
end
