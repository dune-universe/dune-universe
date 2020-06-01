open! Base
open! Import
open Common

type t =
  { name : string
  ; loc : Location.t
  }

let of_label_declaration ld =
  let { txt = name; loc } = ld.pld_name in
  { name; loc }
;;

let record_get_expr { name; loc } ~type_name =
  let record_pat, record_expr = gen_symbol type_name ~loc in
  [%expr
    fun [%p record_pat] -> [%e pexp_field ~loc record_expr (Loc.make (Lident name) ~loc)]]
;;

let record_set_expr { name; loc } ~type_name =
  let record_pat, record_expr = gen_symbol type_name ~loc in
  let value_pat, value_expr = gen_symbol name ~loc in
  [%expr
    fun [%p record_pat] [%p value_pat] ->
      [%e
        pexp_record ~loc [ Loc.make (Lident name) ~loc, value_expr ] (Some record_expr)]]
;;

let record_construct_expr { name; loc } =
  let value_pat, value_expr = gen_symbol name ~loc in
  [%expr
    fun [%p value_pat] ->
      [%e pexp_record ~loc [ Loc.make (Lident name) ~loc, value_expr ] None]]
;;

let to_str ({ name; loc } as t) ~type_name =
  Polymorphize.binding
    ~loc
    ~name
    ~expr:
      [%expr
        Accessor.field
          ~get:[%e record_get_expr t ~type_name]
          ~set:[%e record_set_expr t ~type_name]]
;;

let to_str_singleton ({ name; loc } as t) ~type_name =
  Polymorphize.binding
    ~loc
    ~name
    ~expr:
      [%expr
        Accessor.isomorphism
          ~get:[%e record_get_expr t ~type_name]
          ~construct:[%e record_construct_expr t]]
;;

module Inline = struct
  let pat_and_expr { name; loc } ~type_name ~constructor_name =
    let record_pat, record_expr = gen_symbol type_name ~loc in
    let pat =
      ppat_construct
        ~loc
        (Loc.make ~loc (Lident (Name.to_constructor_string constructor_name)))
        (Some record_pat)
    in
    let expr = pexp_field ~loc record_expr (Loc.make (Lident name) ~loc) in
    pat, expr
  ;;

  let to_get_expr ({ name = _; loc } as t) ~type_name ~constructor_name =
    let pat, expr = pat_and_expr t ~type_name ~constructor_name in
    [%expr fun [%p pat] -> [%e expr]]
  ;;

  let to_match_expr ({ name = _; loc } as t) ~type_name ~constructor_name ~wildcard =
    let pat, expr = pat_and_expr t ~type_name ~constructor_name in
    [%expr
      function
      | [%p pat] -> First [%e expr]
      | [%p wildcard] as bt -> Second bt]
  ;;

  let set_pat_and_expr { name; loc } ~constructor_name ~value_expr =
    let constructor_string = Name.to_constructor_string constructor_name in
    let record_pat, record_expr =
      gen_symbol (Name.to_lowercase_string constructor_name) ~loc
    in
    let pat =
      Some record_pat |> ppat_construct ~loc (Loc.make ~loc (Lident constructor_string))
    in
    let expr =
      Some record_expr
      |> pexp_record ~loc [ Loc.make (Lident name) ~loc, value_expr ]
      |> Option.some
      |> pexp_construct ~loc (Loc.make (Lident constructor_string) ~loc)
    in
    pat, expr
  ;;

  let to_set_expr ({ name; loc } as t) ~type_name ~constructor_name =
    let variant_pat, variant_expr = gen_symbol type_name ~loc in
    let value_pat, value_expr = gen_symbol name ~loc in
    let pat, expr = set_pat_and_expr t ~constructor_name ~value_expr in
    [%expr
      fun [%p variant_pat] [%p value_pat] ->
        let [%p pat] = [%e variant_expr] in
        [%e expr]]
  ;;

  let to_optional_set_expr ({ name; loc } as t) ~type_name ~constructor_name ~wildcard =
    let variant_pat, variant_expr = gen_symbol type_name ~loc in
    let value_pat, value_expr = gen_symbol name ~loc in
    let pat, expr = set_pat_and_expr t ~constructor_name ~value_expr in
    [%expr
      fun [%p variant_pat] [%p value_pat] ->
        match [%e variant_expr] with
        | [%p pat] -> [%e expr]
        | [%p wildcard] as bt -> bt]
  ;;

  let to_construct_expr { name; loc } ~constructor_name =
    let value_pat, value_expr = gen_symbol name ~loc in
    let constructor_string = Name.to_constructor_string constructor_name in
    [%expr
      fun [%p value_pat] ->
        [%e
          pexp_construct
            ~loc
            (Loc.make (Lident constructor_string) ~loc)
            (Some (pexp_record ~loc [ Loc.make (Lident name) ~loc, value_expr ] None))]]
  ;;

  let to_isomorphism_str ({ name; loc } as t) ~type_name ~constructor_name =
    Polymorphize.binding
      ~loc
      ~name
      ~expr:
        [%expr
          Accessor.isomorphism
            ~get:[%e to_get_expr t ~type_name ~constructor_name]
            ~construct:[%e to_construct_expr t ~constructor_name]]
  ;;

  let to_field_str ({ name; loc } as t) ~type_name ~constructor_name =
    Polymorphize.binding
      ~loc
      ~name
      ~expr:
        [%expr
          Accessor.field
            ~get:[%e to_get_expr t ~type_name ~constructor_name]
            ~set:[%e to_set_expr t ~type_name ~constructor_name]]
  ;;

  let to_variant_str ({ name; loc } as t) ~type_name ~constructor_name ~wildcard =
    Polymorphize.binding
      ~loc
      ~name
      ~expr:
        [%expr
          Accessor.variant
            ~match_:[%e to_match_expr t ~type_name ~constructor_name ~wildcard]
            ~construct:[%e to_construct_expr t ~constructor_name]]
  ;;

  let to_optional_str ({ name; loc } as t) ~type_name ~constructor_name ~wildcard =
    Polymorphize.binding
      ~loc
      ~name
      ~expr:
        [%expr
          Accessor.optional
            ~match_:[%e to_match_expr t ~type_name ~constructor_name ~wildcard]
            ~set:[%e to_optional_set_expr t ~type_name ~constructor_name ~wildcard]]
  ;;

  let to_str t ~type_name ~constructor_name ~wildcard =
    match wildcard with
    | None -> to_field_str t ~type_name ~constructor_name
    | Some wildcard -> to_optional_str t ~type_name ~constructor_name ~wildcard
  ;;

  let to_str_singleton t ~type_name ~constructor_name ~wildcard =
    match wildcard with
    | None -> to_isomorphism_str t ~type_name ~constructor_name
    | Some wildcard -> to_variant_str t ~type_name ~constructor_name ~wildcard
  ;;
end
