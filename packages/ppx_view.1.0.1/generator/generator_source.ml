let parse read write path f =
  let ast = read Format.std_formatter ~tool_name:"ppx_view_generator" path in
  let temp_file = Filename.temp_file "ppx_view" "ast" in
  write temp_file ast;
  let chan = open_in temp_file in
  let ast = Migrate_parsetree_ast_io.from_channel chan in
  close_in chan;
  match ast with
  | Ok (_filename, ast) -> f ast
  | Error err ->
    let msg =
      match err with
      | Migrate_parsetree_ast_io.Not_a_binary_ast msg ->
        Printf.sprintf "not a binary AST (%s)" msg
      | Migrate_parsetree_ast_io.Unknown_version msg ->
        Printf.sprintf "unknown version (%s)" msg
    in
    failwith msg

let parse_signature path =
  parse Pparse.parse_interface (Pparse.write_ast Pparse.Signature) path
    (function
      | Migrate_parsetree_ast_io.Intf ((module V), ast) ->
        (Migrate_parsetree_versions.migrate
           (module V)
           (module Migrate_parsetree.OCaml_404)).copy_signature ast
      | Migrate_parsetree_ast_io.Impl _ ->
        failwith "unexpected structure")

let parse_structure path =
  parse Pparse.parse_implementation (Pparse.write_ast Pparse.Structure) path
    (function
      | Migrate_parsetree_ast_io.Impl ((module V), ast) ->
        (Migrate_parsetree_versions.migrate
           (module V)
           (module Migrate_parsetree.OCaml_404)).copy_structure ast
      | Migrate_parsetree_ast_io.Intf _ ->
        failwith "unexpected signature")

let migrate_from_404 =
  Migrate_parsetree_versions.migrate
    (module Migrate_parsetree.OCaml_404)
    (module Migrate_parsetree.OCaml_current)

let print f fmt conv x =
  f fmt (conv x)

let print_signature fmt sign =
  print Pprintast.signature fmt migrate_from_404.copy_signature sign

let print_structure fmt struc =
  print Pprintast.structure fmt migrate_from_404.copy_structure struc
