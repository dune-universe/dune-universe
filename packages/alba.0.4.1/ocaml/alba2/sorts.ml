
type t =
  | Proposition
  | Any
  | Box


let equal (s1:t) (s2:t): bool =
  s1 = s2



let type_of (s:t): t option =
  match s with
  | Proposition ->
     Some Any
  | Any ->
     Some Box
  | Box ->
     None




let product (s1:t) (s2:t): t =
  match s1, s2 with
  | Proposition, _ ->
     s2
  | _, Proposition ->
     Proposition
  | Any, Any ->
     Any
  | _, _ ->
     Box



let sub (s1:t) (s2:t): bool =
  (* Proposition < Any < Box *)
  match s1, s2 with
  | Proposition, _ | Any, Any | Any,Box | Box, Box ->
     true
  | _, _ ->
     false
