module type S = sig end ;;
 
module F (X : S) = struct
  let v (* <= Checking the type of this fails *) = 0 ;;
  let () = 0 ;;
end ;;
 
(*
$ OCAMLRUNPARAM=b ocamlspot --debug spot_test.ml:l4c6
anonargs = [spot_test.ml:l4c6]
ocamlspot spot_test.ml:l4c6
cwd: /home/xxx/test/ocaml/debug_ocamlspot
load /home/xxx/test/ocaml/debug_ocamlspot/spot_test.cmt
cmt loading from /home/xxx/test/ocaml/debug_ocamlspot/spot_test.cmt
Warning: this file is made from compilation with errors
Aiee File "spot.ml", line 1377, characters 17-23: Assertion failed
Uncaught exception: File "spot.ml", line 1377, characters 17-23: Assertion failed
Raised at file "spot.ml", line 1402, characters 14-15
Called from file "utils.ml", line 131, characters 8-11
Re-raised at file "utils.ml", line 133, characters 10-13
Called from file "spot.ml", line 1417, characters 26-41
Called from file "spotfile.ml", line 72, characters 36-63
Called from file "spotfile.ml", line 82, characters 19-37
Re-raised at file "spotfile.ml", line 70, characters 4-200
Called from file "utils.ml", line 304, characters 16-19
Called from file "ocamlspot.ml", line 82, characters 15-49
Called from file "ocamlspot.ml", line 250, characters 15-24
Called from file "ocamlspot.ml", line 268, characters 8-23
 
BYE!
*)

