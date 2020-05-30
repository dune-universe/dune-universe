(* some shell commands via shell *)
(* new version which can work in MinGW *)

open Unix
open Command

let com name tokens = exec (name :: tokens) |> print |> void |> wait |> snd

let cp   = com  "/bin/cp"
let mv   = com  "/bin/mv"
let rm   = com  "/bin/rm"
let cat  = com  "/bin/cat"
  
let file path = 
  match 
    exec ["/usr/bin/file"; path] |> stdout |> wait
  with
  | [], WEXITED 0 -> Ok None
  | lines, WEXITED 0 -> Ok (Some (Xlist.last lines))
  | _, st -> Error st
  
let grep args ~init ~f = exec ("/bin/grep" :: args) |> Command.fold f init |> wait
  
let grep_ xs = grep xs ~init:() ~f:(fun _st _ -> ()) |> snd
  
let cmp p1 p2 =
  match exec ["/bin/cmp"; p1; p2] |> void |> wait |> snd with
  | WEXITED 0 -> `Same
  | WEXITED 1 -> `Different
  | WEXITED 2 -> `Error
  | _ -> `Error (* something extremely wrong happened *)

