type t =
  | Lident of string
  | Ldot of t * string
  | Lapply of t * t

let rec to_string = function
  | Lident s -> s
  | Ldot (t, s) -> to_string t ^ "." ^ s
  | Lapply (t1, Lident "()") -> to_string t1 ^ "()"
  | Lapply (t1, t2) -> to_string t1 ^ "(" ^ to_string t2 ^ ")"

let rec concat l1 = function
  | Lident s -> Ldot (l1, s)
  | Ldot (t, s) -> Ldot (concat l1 t, s)
  | Lapply (t1, t2) -> Lapply (concat l1 t1, t2)
