open! Import

val wrap
  :  ?cfg:(string * Wave_format.t) list
  -> ('i, 'o) Hardcaml.Cyclesim.t
  -> ('i, 'o) Hardcaml.Cyclesim.t * Wave.t array
