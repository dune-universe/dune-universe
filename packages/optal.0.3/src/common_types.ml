
type direction =
  | Minimize
  | Maximize

type typ =
  | Bool
  | Int
  | Float
  | Arr of typ

type id = string

type cmp =
  | Neq
  | Eq
  | Leq
  | Geq
  | Lt
  | Gt
