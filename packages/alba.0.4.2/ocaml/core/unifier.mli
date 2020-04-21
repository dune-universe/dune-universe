module type HOLES =
sig
    include Gamma_algo.GAMMA

    val context: t -> Gamma.t

    val expand: Term.t -> t -> Term.t

    val is_hole: int -> t -> bool

    val value: int -> t -> Term.t option

    val fill_hole0: int -> Term.t -> bool -> t -> t
end




module Make (H: HOLES):
sig
    type t

    val make: H.t -> t

    val context: t -> H.t

    val push: string -> Term.typ -> t -> t

    val unify0: Term.typ -> Term.typ -> bool -> t -> t option

    val unify: Term.typ -> Term.typ -> bool -> H.t -> H.t option
    (**
         [unify act req is_super gh]

         Unify the type [act] with the type [req] using [gh] as the context with
         holes.

         [is_super] indicates if the typ [req] can be regarded as a supertype of
         [act].
    *)
end
