
module%gen rec Foo : sig
  type t = string [@@deriving show]
end = Foo

let foo : Foo.t = "foo"

let () = print_endline (Foo.show foo)


let () =
  let empty_program = Flow_ast.Program.{
    statements = [];
    comments = None;
    all_comments = [];
  } in
  let nop = fun _ _ -> () in 
  print_endline (Flow_ast.Program.show nop nop (None, empty_program))