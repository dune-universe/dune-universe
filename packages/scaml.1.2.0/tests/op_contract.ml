(* self must be defined out *)
open SCaml
open Contract
let [@entry] main x y =
  [],
  assert ( 
    match (contract (address self) : unit contract option) with 
    | None -> false 
    | Some c -> address c = address self
  )
