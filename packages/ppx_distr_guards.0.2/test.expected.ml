type t =
  | A of int 
  | B of int 
let f =
  function | (A x, _) when x <> 0 -> 1 | (_, A x) when x <> 0 -> 1 | _ -> 2
let g x =
  match x with
  | (A x, _) when x <> 0 -> 1
  | (_, A x) when x <> 0 -> 1
  | (B x, _) when x <> 0 -> 1
  | _ -> 2
let h =
  function
  | (A x, _) when x <> 0 -> 1
  | (_, A x) when x <> 0 -> 1
  | (B x, _) when x <> 0 -> 1
  | x ->
      (match x with
       | (A x, _) when x <> 0 -> 1
       | (_, A x) when x <> 0 -> 1
       | (B x, _) when x <> 0 -> 1
       | _ -> 2)
