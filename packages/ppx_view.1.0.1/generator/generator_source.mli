(* source file parsing/printing *)

val parse_signature
   : string
  -> Migrate_parsetree.Ast_404.Parsetree.signature

val parse_structure
   : string
  -> Migrate_parsetree.Ast_404.Parsetree.structure

val print_signature
   : Format.formatter
  -> Migrate_parsetree.Ast_404.Parsetree.signature
  -> unit

val print_structure
   : Format.formatter
  -> Migrate_parsetree.Ast_404.Parsetree.structure
  -> unit
