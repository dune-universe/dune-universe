(*
  Regexp substitution $s/.../.../
*)

open Ppx_orakuda.Regexp_pcre.Literal

let s = {s|l+/$&$0|s} "hello world";;
let () = assert (s = "hellllo world");;

let s = {s|l+/$&$0/g|s} "hello world";;
let () = assert (s = "hellllo worlld");;
