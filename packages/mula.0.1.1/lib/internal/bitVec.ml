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

let snoc_ones (Bits n) ~m =
  if m >= Sys.int_size then
    Bits (Int.minus_one)
  else
    let one_m_zeros = Int.shift_left Int.one m in
    let m_ones = one_m_zeros - 1 in
    let n' = Int.logor m_ones (Int.shift_left n m) in
    Bits n'

let ones ~m =
  snoc_ones (Bits Int.zero) ~m

let snoc_zero (Bits n) =
  Bits (Int.shift_left n 1)

let snoc_zeros (Bits n) ~m =
  if m >= Sys.int_size then
    Bits (Int.zero)
  else
    let n' = Int.shift_left n m in
    Bits n'

let zero = (Bits Int.zero)

let pp_bv ppf (Bits n)=
  Format.fprintf ppf "%o" n
