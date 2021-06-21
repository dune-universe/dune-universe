type t = Bits of int [@@unboxed]

let[@inline] compare (Bits x) (Bits y) =
  Int.compare x y

let get k (Bits n) =
  let two_to_k = Int.shift_left Int.one (k - 1) in
  if Int.equal (Int.logand two_to_k n) 0 then
    false
  else
    true

let get_lane ~lane:l ~k bv =
  get (k + 1 - l) bv

let get_right_of_lane ~lane:l ~k ~m bv =
    get_lane ~lane:(l + m) ~k bv

let get_left_of_lane ~lane:l ~k ~m bv =
    get_lane ~lane:(l - m) ~k bv

let rec pos_fold ~f ~init n =
  match n with
  | 0 -> init
  | n -> pos_fold ~f ~init:(f n init) (n - 1)

let snoc_one (Bits n) =
  let n' = Int.logor Int.one (Int.shift_left n 1) in
  Bits n'

let snoc_zero (Bits n) =
  Bits (Int.shift_left n 1)

let zero = (Bits Int.zero)
