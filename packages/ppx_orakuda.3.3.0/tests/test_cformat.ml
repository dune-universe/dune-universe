(*
  Pre-compiled printf format $%"....." and its use by Cformat

  Cformat.sprintf $%"..." is actually equivalent with $"...".
*)

open Ppx_orakuda

let () = 
  assert (Cformat.sprintf {fmt|hello world|fmt} = "hello world");
  assert (Cformat.sprintf {fmt|hello%dworld|fmt} 2 = "hello2world");
  let x = 1 and y = 2 in 
  assert (Cformat.sprintf {fmt|%d %${x}d %d %${y}d %d|fmt} 3 4 5 = "3 1 4 2 5");
  assert (Cformat.sprintf {fmt|%1.F|fmt} 123.456 = "123.")

let _f = fun x -> {fmt|%*.*d|fmt} x

(* special char '"' escaping *)

let () = 
  prerr_endline "special char escaping test";
  assert (Cformat.sprintf {fmt|hello"%d"world|fmt} 2 = "hello\"2\"world");
  assert (Cformat.sprintf {fmt|hello"%d"world|fmt} 2 = "hello\"2\"world");
  assert (Cformat.sprintf {fmt|h"ello"%d"world|fmt} 2 = "h\"ello\"2\"world")
