(*

  Sub-shell call $`...` which calls `command'. The command variable must be bound manually.

*)

open Spotlib.Spot
module Qx = Ppx_orakuda.Qx

let () = {qx|ls .|qx} |> fun (_, xs) -> prerr_string & String.concat "" xs

