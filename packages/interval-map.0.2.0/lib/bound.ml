module type S = sig
  module C : Comparable.S

  type t =
    | Included of C.t
    | Excluded of C.t
    | Unbounded

  val compare_lower : t -> t -> int

  val compare_upper : t -> t -> int

  val min_lower : t -> t -> t

  val max_upper : t -> t -> t
end

module Make (C : Comparable.S) : S with module C = C = struct
  module C = C

  type t =
    | Included of C.t
    | Excluded of C.t
    | Unbounded

  let compare_lower a b =
    match a, b with
    | Included low_a, Included low_b ->
      C.compare low_a low_b
    | Included low_a, Excluded low_b when C.compare low_a low_b <= 0 ->
      -1
    | Included _, Excluded _ ->
      1
    | Excluded low_a, Included low_b when C.compare low_a low_b < 0 ->
      -1
    | Excluded _, Included _ ->
      1
    | Excluded low_a, Excluded low_b ->
      C.compare low_a low_b
    | Unbounded, Unbounded ->
      0
    | Unbounded, _ ->
      -1
    | _, Unbounded ->
      1

  let compare_upper a b =
    match a, b with
    | Included high_a, Included high_b ->
      C.compare high_a high_b
    | Included high_a, Excluded high_b when C.compare high_a high_b < 0 ->
      -1
    | Included _, Excluded _ ->
      1
    | Excluded high_a, Included high_b when C.compare high_a high_b <= 0 ->
      -1
    | Excluded _, Included _ ->
      1
    | Excluded high_a, Excluded high_b ->
      C.compare high_a high_b
    | Unbounded, Unbounded ->
      0
    | Unbounded, _ ->
      1
    | _, Unbounded ->
      -1

  let min_lower a b =
    if compare_lower a b < 0 then
      a
    else
      b

  let max_upper a b =
    if compare_upper a b < 0 then
      b
    else
      a
end
