type t = Zero | One

let zero = Zero

let one = One

let add (x : t) (y : t) =
  match (x, y) with
  | (Zero, Zero) -> Zero
  | (Zero, One) | (One, Zero) -> One
  | (One, One) -> Zero

let neg (x : t) = x

let sub x y = add x (neg y)

let mul (x : t) (y : t) =
  match (x, y) with (Zero, _) | (_, Zero) -> Zero | (One, One) -> One

let div (x : t) (y : t) =
  match y with Zero -> Stdlib.failwith "Z2: division by zero" | _ -> x

let hash (x : t) = match x with Zero -> 0 | One -> 1

let pp fmtr (x : t) =
  match x with
  | Zero -> Format.pp_print_string fmtr "zero"
  | One -> Format.pp_print_string fmtr "one"

let equal (x : t) (y : t) =
  match (x, y) with (Zero, Zero) -> true | (One, One) -> true | _ -> false

let compare (x : t) (y : t) =
  match (x, y) with
  | (Zero, Zero) | (One, One) -> 0
  | (Zero, One) -> -1
  | (One, Zero) -> 1

let of_int (i : int) = if i mod 2 = 0 then Zero else One
