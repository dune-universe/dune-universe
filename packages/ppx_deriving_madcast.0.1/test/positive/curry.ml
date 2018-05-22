
let f (x, y, z) = (z + 1, y ^ "2", x -. 3.)

let g = [%madcast: ((float * string * int) -> (int * string * float)) ->
                   (float -> int -> string -> (string * string * string))]

let () = assert (g f 1. 2 "3" = ("4", "22", "-2."))

let h = [%madcast: (float -> int -> string -> (string * string * string)) ->
                   ((float * int * int) -> (float * int * float))]

let () = assert (h (g f) (1., 2, 3) = (4., 22, -2.))
