
(** val coq_Q2R : Q.t -> float **)

let coq_Q2R x =
  ( *. ) (Float.of_int ((fun q -> Z.to_int (Q.num q)) x))
    (((/.) 1.0) (Float.of_int ((fun q -> Z.to_int (Q.den q)) x)))
