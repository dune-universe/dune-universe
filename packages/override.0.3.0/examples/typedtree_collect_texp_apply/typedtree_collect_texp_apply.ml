(*
This is an answer to Bramford Horton's question:
https://discuss.ocaml.org/t/producing-and-using-typed-asts-for-all-source-files-in-a-project/3713/3

> Traversing the entire tree recursively in search of all function
> applications (i.e. Texp_apply) is quite laborious. Is there a
> library that can simplify this traversal for me (e.g. Given a
> Typedtree.structure, return all expressions of type Texp_apply)?
*)

[%%metapackage metapp]

module%override Env = struct
  type t = Env.t [@opaque] [@@deriving refl]
end

module%override Ident = struct
  type t = Ident.t [@opaque] [@@deriving refl]
end

module%override Path = struct
  type t = _ [@@deriving refl]
end

module%override Longident = struct
  type t = _ [@@deriving refl]
end

module%override Location = struct
  type t = Location.t [@opaque] [@@deriving refl]

  type 'a loc = _ [@@deriving refl]
end

module%override Asttypes = struct
  [%%types] [@@deriving refl]
end

module%override Parsetree = struct
  [%%types] [@@deriving refl]
end

module%override Primitive = struct
  type description = Primitive.description [@opaque] [@@deriving refl]
end

[%%meta if Sys.ocaml_version >= "4.10.0" then
  [%stri module%override Type_immediacy = struct
    [%%types] [@@deriving refl]
  end]
else
  Metapp.Stri.of_list []]

module%override Types = struct
  module%override Variance = struct
    type t = Types.Variance.t [@opaque] [@@deriving refl]

    [%%types] [@@deriving refl]
  end

  module%override Meths = struct
    type 'a t = 'a Types.Meths.t [@opaque] [@@deriving refl]
  end

  module%override Vars = struct
    type 'a t = 'a Types.Vars.t [@opaque] [@@deriving refl]
  end

  module%override Concr = struct
    type t = Types.Concr.t [@opaque] [@@deriving refl]
  end

  type unboxed_status = Types.unboxed_status [@opaque] [@@deriving refl]

  [%%types] [@@deriving refl]
end

module%override Typedtree = struct
  [%%types] [@@deriving refl]
end

module%override Cmt_format = struct
  type binary_part = _ [@@deriving refl]

  type binary_annots = _ [@@deriving refl]
end

type texp_apply =
    Typedtree.expression *
      (Asttypes.arg_label * Typedtree.expression option) list

module Visitor = struct
  type accu = texp_apply list

  module Applicative = Traverse.Applicative.Fold (struct type t = accu end)

  let hook : type a . a Refl.refl -> (a -> accu -> accu) -> a -> accu -> accu =
  fun refl super x accu ->
    match (refl, x) with
    | (Typedtree.Refl_expression_desc, Texp_apply (a, b)) -> (a, b) :: accu
    | _ -> super x accu
end

module Visit = Refl.Visit.Make (Visitor)

let collect_texp_apply_from_structure (structure : Typedtree.structure)
    : texp_apply list =
  Visit.visit [%refl: Typedtree.structure] [] structure []

let collect_texp_apply_from_binary_annots
    (binary_annots : Cmt_format.binary_annots) : texp_apply list =
  Visit.visit [%refl: Cmt_format.binary_annots] [] binary_annots []
