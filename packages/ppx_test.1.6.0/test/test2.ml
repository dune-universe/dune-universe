module PTest = Ppx_test.Test

(* [%%TEST let name = e]  is equivalent with [let %TEST name = e],
   but it is more suitable to list more than one tests.
*)

[%%TEST

let equal = 1 = 1
let equal_ = assert (1 = 1)
]

let () = PTest.collect ()
