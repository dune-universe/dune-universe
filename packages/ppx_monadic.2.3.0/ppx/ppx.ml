open Ast_405

open Ppxx.Utils (* must come after Ast_helper *)
open Ast_mapper

include Ppxx.Ppx.Make(struct
    let name = "ppx_monadic"
    let options = []
    let make_mapper _ _ =
      Comprehension.extend & Pattern_guard.extend & Do_.extend default_mapper
  end)

let () = register ()
