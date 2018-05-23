open Ast_405

open Ppxx.Utils (* must come after Ast_helper *)
open Ast_mapper

include Ppxx.Ppx.Make(struct
    let name = "ppx_integer"
    let options = []
    let make_mapper _ _ = 
      Integer.reset ();
      Integer.extend default_mapper
  end)

let () = register ()
