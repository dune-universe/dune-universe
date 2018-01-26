(* ocamlc -ppx 'src/ppx_curried_constr' -c -i tests/test.ml *)

let x : int option = Some 1
let x = (Some) 1
let x = Some @@ 1
(* let x = (Some..) 1    Error: Unary constructor cannot be curried. *)
let x = (None)
(* let x = (None..)    Error: Nullary constructor cannot be curried. *)

type t = Foo of int * float
let x : t = (Foo) (1,1.0)

let x : t = !Foo 1 1.0
let x : int -> float -> t = !Foo
let x : float -> t = !Foo 1
let x : (int * float) -> t = Foo
let x : (int * float) -> t = fun x -> (Foo) x

(* (::)(x,xs) has a special parsing rule. We can handle it but requires parser.mly modificaiton *)
let cons0 = (::)(1,[])
(*
let cons1 = ((::)) (1,[])
let cons2 = !(::) 1 []
*)

let z = !`Foo @@ (1,2,3)

let () = prerr_endline "test done"
  
