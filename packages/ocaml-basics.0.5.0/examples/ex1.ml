open Basics
open Result.Infix

let _ = Ok 42 >>= fun x ->
  Printf.printf "%d\n" x
  |> Result.return
