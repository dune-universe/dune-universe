(* containers for generated code, with i/o functions *)

type 'a t = {
    add           : 'a -> unit;
    add_from_file : string -> unit;
    write         : string -> unit;
  }

val make_signature : unit -> Ast_404.Parsetree.signature_item t

val make_structure : unit -> Ast_404.Parsetree.structure_item t
