type (* t => *) t (* <= t *) = ..

type t (* ? t *) += M of int

module M : sig
  type (* M.t => *) t (* <= M.t *) = ..
end

type (* M.t => *) M.t (* <= M.t *) += N of t (* ? t *)
