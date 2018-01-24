(*
  Regexp case match

  string |! $/regexp/ as x -> ... | $/regexp/ as x -> ... | _ -> ...

*)

open Ppx_orakuda.Regexp_pcre.Infix
open Ppx_orakuda.Regexp_pcre.Literal (* to use {m|...|m} *)

let r s = 
  case s 
  |> ({m|hello|m} ==> fun _ -> "world")
  |> ({m|bye|m} ==> fun _ -> "universe")
  |> default (fun () -> "default")

let _ = 
  assert (r "hello world" = "world");
  assert (r "bye universe" = "universe");
  assert (r "42" = "default")
;;

(* CR jfuruse: hmmm. Precedence seems glitchy *)

let _ = 
  let res = 
    case "world bye universe" 
    |> ( {m|hello|m} ==> fun _ -> "world")
    |> ( {m|bye|m} ==> fun x -> x#_left ^ x#_right)
    |> default (fun () -> "default")
  in
  assert ("world  universe" = res)
;;

let () = 
  let res = case "world bye universe" 
    |> ( {m|([^ ]+) ([^ ]+) ([^ ]+)|m} ==> fun x -> x#_1 )
    |> ( {m|([^ ]+) ([^ ]+) ([^ ]+)|m} ==> fun x -> match x#_1opt with Some v -> v | None -> "" )
    |> ( {m|(.*)|m} ==> fun x -> x#_1 )
    |> default (fun () -> "default")
  in
  assert ("world" = res)
;;

let _ =
  case "hello"
  |> ( {m|^S_|\/S_|m} ==> fun _ -> true )
  |> default (fun () -> false)
