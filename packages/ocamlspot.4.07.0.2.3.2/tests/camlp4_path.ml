(* See ocamlc_path.ml *)
module X = struct
  let (* x => *) x (* <= x *) = 1

  type t = (* Foo => *) Foo  (* <= Foo *)
end

let _ = X.(* ? x *)x (* ? x *)


let _ = function
  | X.(* ? Foo *)Foo (* ? Foo *) -> ()
