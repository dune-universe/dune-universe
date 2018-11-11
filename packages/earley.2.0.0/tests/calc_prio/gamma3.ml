open Earley_core.Earley
(*open Common*)


let parser gamma3 =
  _:'a' | gamma3 gamma3 -> () | gamma3 gamma3 gamma3 -> ()

let n = int_of_string Sys.argv.(1)

let input = String.make n 'a'

let _ = Earley_core.Earley.debug_lvl :=0; Earley_core.Earley.warn_merge := false

let _ = parse_string gamma3 no_blank input
