(*
This is an answer to Bramford Horton's question:
https://discuss.ocaml.org/t/producing-and-using-typed-asts-for-all-source-files-in-a-project/3713/3

> Traversing the entire tree recursively in search of all function
> applications (i.e. Texp_apply) is quite laborious. Is there a
> library that can simplify this traversal for me (e.g. Given a
> Typedtree.structure, return all expressions of type Texp_apply)?
*)

open Types

[%%rewrite
  type path = Path.t [@@rewrite]
  type ident = Ident.t [@@rewrite] 
  type env = Env.t [@@rewrite]
  type primite_description = Primitive.description [@@rewrite]
  type concr = Types.Concr.t [@@rewrite]
  type ident_meths = ident Types.Meths.t [@@rewrite]
  type types_class_type_declaration = Types.class_type_declaration [@@rewrite]
  type types_class_declaration = Types.class_declaration [@@rewrite]
  type types_class_type = Types.class_type [@@rewrite]
  type types_class_signature = Types.class_signature [@@rewrite]
  type types_extension_constructor = Types.extension_constructor [@@rewrite]
  type types_module_type = Types.module_type [@@rewrite]
  type types_signature = Types.signature [@@rewrite]
  type types_type_declaration = Types.type_declaration [@@rewrite]
  type types_value_description = Types.value_description [@@rewrite]
  type parsetree_pattern = Parsetree.pattern [@@rewrite]

  [%%recursive
#if OCAML_VERSION >= (4, 07, 0)
    module%import Stdlib = struct
      module%import Lexing = struct
        type position = _ [@@rewrite]
      end
    end
#else
    module%import Lexing = struct
      type position = _ [@@rewrite]
    end
#endif
    module%import Location = struct
      type location = _ [@@from: t] [@@rewrite]

      type 'a loc = _ [@@rewrite]
    end
    module%import Longident = struct
      type longident = _ [@@from: t] [@@rewrite]
    end
    module%import Asttypes = struct
      type 'a loc = _ [@@rewrite] [@@remove]
      [%%types] [@@rewrite] [@@opaque]
    end
    module%import Typedtree = struct
      [%%types]
    end
    module%import Cmt_format = struct
      type binary_annots = _ and co
    end] [@@deriving traverse_fold]
]

type texp_apply = expression * (arg_label * expression option) list

let collector =
  let any _ acc = acc in object
    inherit [texp_apply list] Ppxlib_traverse_builtins.fold
    inherit [texp_apply list] fold as super

    method! expression_desc e acc =
      let acc = super#expression_desc e acc in
      match e with
      | Texp_apply (a, b) -> (a, b) :: acc
      | _ -> acc

    method types_value_description = any
    method types_type_declaration = any
    method types_signature = any
    method types_module_type = any
    method types_extension_constructor = any
    method types_class_type_declaration = any
    method types_class_type = any
    method types_class_signature = any
    method types_class_declaration = any
    method type_expr = any
    method row_desc = any
    method ref : 'a . ('a, 'acc) Ppxlib_traverse_builtins.T.fold
      -> ('a ref, 'acc) Ppxlib_traverse_builtins.T.fold = fun f x acc ->
      f !x acc
    method record_representation = any
    method primite_description = any
    method path = any
    method parsetree_pattern = any
    method nativeint = any
    method label_description = any
    method int64 = any
    method int32 = any
    method ident_meths = any
    method ident = any
    method env = any
    method constructor_description = any
    method concr = any
    method module_presence = any
  end

let collect_texp_apply_from_structure (structure : Typedtree.structure)
    : texp_apply list =
  collector#structure structure []

let collect_texp_apply_from_binary_annots
    (binary_annots : Cmt_format.binary_annots) : texp_apply list =
  collector#binary_annots binary_annots []
