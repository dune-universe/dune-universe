(* ocamlc -ppx 'ppx_type_of' test.ml 

or ppx_type_of -debug test.ml 
*)

module Ppx_type_of = struct
  let type_of : 'a -> string = fun _ -> raise (Failure "use ppx_type_of")
end

open Ppx_type_of

let () =
  assert (type_of 1 = "int");
  assert (type_of [1] = "int list");
  assert (type_of None = "'a option");
  assert (type_of (fun x -> x + 1) = "int -> int");
  prerr_endline "test done"
    
