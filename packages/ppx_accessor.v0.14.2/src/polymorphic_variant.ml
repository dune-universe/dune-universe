open! Base
open! Import
open Common

module Constructor = struct
  type t =
    { name : Name.t
    ; loc : Location.t
    ; fields : Tuple.t
    }

  let of_rtag lloc possibly_empty cts =
    let { txt = name; loc } = lloc in
    let name = Name.of_string name in
    let fields =
      match possibly_empty, cts with
      | true, [] -> Tuple.empty
      | false, [ _ ] -> Tuple.singleton
      | false, _ | true, _ :: _ -> unsupported ~loc "intersection type"
    in
    { name; loc; fields }
  ;;

  let wildcard_pattern { name; loc; fields } =
    let fields = Tuple.Polymorphic_variant.wildcard_pattern fields ~loc in
    ppat_variant ~loc (Name.to_constructor_string name) fields
  ;;

  let to_str { name; loc; fields } ~wildcard =
    Tuple.Polymorphic_variant.to_str fields ~loc ~name ~wildcard
  ;;
end

module Inherit = struct
  type t =
    { type_constructor : longident_loc
    ; loc : Location.t
    }

  let of_core_type ct =
    let loc = ct.ptyp_loc in
    match ct.ptyp_desc with
    | Ptyp_constr (type_constructor, []) -> { type_constructor; loc }
    | _ -> unsupported ~loc "non-simple type constructor in polymorphic variant"
  ;;

  let wildcard_pattern { type_constructor; loc } = ppat_type ~loc type_constructor

  let to_construct_expr { type_constructor; loc } =
    [%expr fun ([%p ppat_type ~loc type_constructor] as bt) -> bt]
  ;;

  let to_match_expr { type_constructor; loc } ~wildcard =
    [%expr
      function
      | [%p ppat_type ~loc type_constructor] as a -> First a
      | [%p wildcard] as bt -> Second bt]
  ;;

  let to_str ({ type_constructor; loc } as t) ~wildcard =
    let name =
      Longident.flatten_exn type_constructor.txt
      |> String.concat ~sep:"_"
      |> Name.of_string
      |> Name.to_lowercase_string
    in
    Polymorphize.binding
      ~loc
      ~name
      ~expr:
        [%expr
          Accessor.variant
            ~match_:[%e to_match_expr t ~wildcard]
            ~construct:[%e to_construct_expr t]]
  ;;
end

module Row = struct
  type t =
    | Constructor of Constructor.t
    | Inherit of Inherit.t

  let of_row_field rf =
    match rf.prf_desc with
    | Rtag (lloc, possibly_empty, cts) ->
      Constructor (Constructor.of_rtag lloc possibly_empty cts)
    | Rinherit ct -> Inherit (Inherit.of_core_type ct)
  ;;

  let wildcard_pattern = function
    | Constructor constructor -> Constructor.wildcard_pattern constructor
    | Inherit inherit_ -> Inherit.wildcard_pattern inherit_
  ;;

  let to_str t ~wildcard =
    match t with
    | Constructor constructor -> Constructor.to_str constructor ~wildcard
    | Inherit inherit_ ->
      (match wildcard with
       | None ->
         Location.raise_errorf
           ~loc:inherit_.loc
           "Bug in ppx_accessor: unexpectedly lonely inherited polymorphic variant"
       | Some wildcard -> Inherit.to_str inherit_ ~wildcard)
  ;;
end

type t = Row.t list

let of_row_fields = List.map ~f:Row.of_row_field

let of_core_type_desc t ~loc =
  match t with
  | Ptyp_variant (rfs, Closed, None) -> of_row_fields rfs
  | Ptyp_variant _ -> unsupported ~loc "non-simple polymorphic variant"
  | _ -> unsupported ~loc "manifest type that is not a polymorphic variant"
;;

let of_core_type ct = of_core_type_desc ct.ptyp_desc ~loc:ct.ptyp_loc

let wildcard_patterns t ~loc =
  List.reduce (List.map t ~f:Row.wildcard_pattern) ~f:(ppat_or ~loc)
;;

let to_strs t ~loc =
  Common.map_with_context t ~f:(fun constructor ~context:other_constructors ->
    let wildcard = wildcard_patterns other_constructors ~loc in
    Row.to_str constructor ~wildcard)
;;
