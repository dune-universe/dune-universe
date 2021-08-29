type t = float

let zero = neg_infinity

let one = 0.0

let mul = ( +. )

let div = ( -. )

let of_float = log

let to_float = exp

let min (x : t) (y : t) = if x <. y then x else y

let compare = Float.compare

let equal = Float.equal

let hash = Hashtbl.hash

let pp fmtr l = Format.fprintf fmtr "%f" (to_float l)
