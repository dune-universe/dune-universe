open Base

type t = Range.t list

let rec check_invariant : t -> bool = function
  | [] | _ :: [] -> true
  | u :: (v :: _ as l) -> u.hi < v.lo && check_invariant l

let empty = []

let singleton r = [ r ]

let rec add xs r =
  match xs with
  | [] -> [ r ]
  | h :: t ->
    match Range.relative_position r ~wrt:h with
    | `Before -> r :: xs
    | `Before_with_intersection ->
      Range.make ~lo:r.lo ~hi:h.hi :: xs
    | `Included | `Equal -> xs
    | `Contains -> add t r
    | `After_with_intersection ->
      add t (Range.make ~lo:h.lo ~hi:r.hi)
    | `After -> h :: add t r

let%test "Interval_union_add_1" =
  List.fold
    [ 2, 4 ; 3, 5 ; 1, 8 ; 45, 47 ; 45, 45 ]
    ~init:empty
    ~f:(fun acc (lo, hi) -> add acc (Range.make ~lo ~hi))
  |> check_invariant


let rec diff_range xs r =
  match xs with
  | [] -> []
  | h :: t ->
    match Range.relative_position r ~wrt:h with
    | `Before -> xs
    | `Before_with_intersection ->
      Range.make ~lo:(r.hi + 1)  ~hi:h.hi :: t
    | `Equal -> t
    | `Contains -> diff_range t r
    | `Included ->
      if r.lo = h.lo then Range.make ~lo:(r.hi + 1) ~hi:h.hi :: t
      else if r.hi = h.hi then Range.make ~lo:h.lo ~hi:(r.lo - 1) :: t
      else Range.make ~lo:h.lo ~hi:(r.lo - 1) :: Range.make ~lo:(r.hi + 1) ~hi:h.hi :: t
    | `After_with_intersection ->
      Range.make ~lo:h.lo ~hi:(r.lo - 1) :: diff_range t r
    | `After -> h :: diff_range t r

