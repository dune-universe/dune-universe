open Orakuda.Std
open Orakuda.Regexp.Infix

(* str_item *)
let x = {m||m}
let x = {m|x|m}
let rex = {m|[0-9]|m}
let rex = {m|([0-9]+)(?P<x>[a-z]+)|([A-Z]+)|m}
let rex = {m|hello/i|m}

let s = {s|l+/$&$0|s} "hello world";;
