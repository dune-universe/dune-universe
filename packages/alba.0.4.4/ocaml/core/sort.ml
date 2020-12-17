type t =
  | Proposition
  | Any of int


let compare (s1: t) (s2: t): int =
    match s1, s2 with
    | Proposition, Proposition ->
        0
    | Proposition, _ ->
        -1
    | Any i, Any j ->
        Stdlib.compare i j
    | Any _, Proposition ->
        +1


let is_sub (s1: t) (s2: t): bool =
    compare s1 s2 <= 0


let is_super (s1: t) (s2: t): bool =
    is_sub s2 s1


let type_of (s: t): t =
    match s with
    | Proposition ->
        Any 0
    | Any i ->
        Any (i + 1)


let pi_sort (arg: t) (res: t): t =
    match arg, res with
    | _, Proposition ->
        Proposition
    | Proposition, Any j ->
        Any j
    | Any i, Any j ->
        Any (max i j)
