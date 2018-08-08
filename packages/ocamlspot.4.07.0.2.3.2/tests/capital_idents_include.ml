include Capital_idents

let _ = (E (* ? exception E *) : exn)

module E2 = E (* ? module E *)

module N : E (* ? modtype E *) = M (* ? module M *)

type u = t (* ? type t *)
