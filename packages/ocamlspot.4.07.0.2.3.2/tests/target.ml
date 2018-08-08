(* Target *)

exception (* Target.E => *) E (* <= Target.E *)
let int = 1

external external_value : int -> int = "external_value_impl"
