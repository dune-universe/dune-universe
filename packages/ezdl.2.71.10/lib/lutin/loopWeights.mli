
(*--------------------------------------------------------------
--------------------------------------------------------------*)

(* usage: (goon, stop) = interval min max x *)
val interval : int -> int -> int -> (int * int)

(* usage: (goon, stop) = average av ec x *)
val average : int -> int -> int -> (int * int)

