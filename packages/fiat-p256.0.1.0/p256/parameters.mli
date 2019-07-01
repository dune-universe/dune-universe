(** Curve parameters.
    They are stored as [Hex.t] because [Fe.t] and [Cstruct.t] are
    mutable. *)

val a : Hex.t

val b : Hex.t

val g : Hex.t
(** The base point, in uncompressed form. *)

val p : Hex.t
(** The prime number corresponding to [Fe]. *)

val n : Hex.t
(** The group order. *)
