open Ppx_type_conv.Std
open Ppx_hash_expander_lib.Std
open Ppx_core

let type_extension name f =
  Context_free.Rule.extension
    (Extension.declare name
       Core_type Ast_pattern.(ptyp __)
       (fun ~loc ~path:_ ty -> f ~loc ty))
;;

let () =
  let name = "hash_fold" in
  Type_conv.ignore
    (Type_conv.add name
       ~extension:(fun ~loc:_ ~path:_ ty -> Expand.hash_fold_core_type ty));
  Ppx_driver.register_transformation name
    ~rules:[ type_extension name Expand.hash_fold_type ]
;;

let () =
  let name = "hash" in
  Type_conv.ignore
    (Type_conv.add name
       ~str_type_decl:(Type_conv.Generator.make_noarg Expand.str_type_decl
                         ~attributes:Expand.str_attributes)
       ~sig_type_decl:(Type_conv.Generator.make_noarg Expand.sig_type_decl)
       ~extension:(fun ~loc:_ ~path:_ ty -> Expand.hash_core_type ty));
  Ppx_driver.register_transformation name
    ~rules:[ type_extension name Expand.hash_type ]
;;
