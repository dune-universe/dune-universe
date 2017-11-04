let of_405_impl =
  (Migrate_parsetree_versions.migrate
    (module Migrate_parsetree.OCaml_405)
    (module Migrate_parsetree.OCaml_current)).copy_structure

let to_405_impl =
  (Migrate_parsetree_versions.migrate
    (module Migrate_parsetree.OCaml_current)
    (module Migrate_parsetree.OCaml_405)).copy_structure

let of_405_intf =
  (Migrate_parsetree_versions.migrate
    (module Migrate_parsetree.OCaml_405)
    (module Migrate_parsetree.OCaml_current)).copy_signature

let to_405_intf =
  (Migrate_parsetree_versions.migrate
    (module Migrate_parsetree.OCaml_current)
    (module Migrate_parsetree.OCaml_405)).copy_signature

let () =
  let js_mapper = Ppx_js.mapper in
  let impl x =
    of_405_impl (js_mapper.structure js_mapper (to_405_impl x))
  in
  let intf x =
    of_405_intf (js_mapper.signature js_mapper (to_405_intf x))
  in
  Ppx_driver.register_transformation_using_ocaml_current_ast "js_of_ocaml"
    ~impl
    ~intf
