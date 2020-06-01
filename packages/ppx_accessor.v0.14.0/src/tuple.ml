open! Base
open! Import
open Common

type t = { fields : int }

let empty = { fields = 0 }
let singleton = { fields = 1 }
let of_core_types cts = { fields = List.length cts }

let gen_pat_expr_opt { fields } ~loc ~name =
  let pats, exprs =
    List.unzip (List.init fields ~f:(fun (_ : int) -> gen_symbol name ~loc))
  in
  ppat_tuple_opt ~loc pats, pexp_tuple_opt ~loc exprs
;;

module Make (M : sig
    val pat : loc:location -> string -> pattern option -> pattern
    val exp : loc:location -> string -> expression option -> expression
  end) =
struct
  let wildcard_pattern { fields } ~loc =
    match fields with
    | 0 -> None
    | _ -> Some (ppat_any ~loc)
  ;;

  let pat_and_expr tuple ~loc ~name =
    let pat, expr = gen_pat_expr_opt tuple ~loc ~name:(Name.to_lowercase_string name) in
    let pat = M.pat ~loc (Name.to_constructor_string name) pat in
    let expr =
      match expr with
      | None -> eunit ~loc
      | Some expr -> expr
    in
    pat, expr
  ;;

  let to_get_expr tuple ~loc ~name =
    let pat, expr = pat_and_expr tuple ~loc ~name in
    [%expr fun [%p pat] -> [%e expr]]
  ;;

  let to_match_expr tuple ~loc ~name ~wildcard =
    let pat, expr = pat_and_expr tuple ~loc ~name in
    [%expr
      function
      | [%p pat] -> First [%e expr]
      | [%p wildcard] as bt -> Second bt]
  ;;

  let to_construct_expr tuple ~loc ~name =
    let pat, expr = gen_pat_expr_opt tuple ~loc ~name:(Name.to_lowercase_string name) in
    let pat =
      match pat with
      | None -> punit ~loc
      | Some pat -> pat
    in
    [%expr fun [%p pat] -> [%e M.exp ~loc (Name.to_constructor_string name) expr]]
  ;;

  let to_isomorphism_str tuple ~loc ~name =
    Polymorphize.binding
      ~loc
      ~name:(Name.to_lowercase_string name)
      ~expr:
        [%expr
          Accessor.isomorphism
            ~get:[%e to_get_expr tuple ~loc ~name]
            ~construct:[%e to_construct_expr tuple ~loc ~name]]
  ;;

  let to_variant_str tuple ~loc ~name ~wildcard =
    Polymorphize.binding
      ~loc
      ~name:(Name.to_lowercase_string name)
      ~expr:
        [%expr
          Accessor.variant
            ~match_:[%e to_match_expr tuple ~loc ~name ~wildcard]
            ~construct:[%e to_construct_expr tuple ~loc ~name]]
  ;;

  let to_str tuple ~loc ~name ~wildcard =
    match wildcard with
    | None -> to_isomorphism_str tuple ~loc ~name
    | Some wildcard -> to_variant_str tuple ~loc ~name ~wildcard
  ;;
end

module Inline = Make (struct
    let pat ~loc name pat = ppat_construct ~loc (Loc.make ~loc (Lident name)) pat
    let exp ~loc name expr = pexp_construct ~loc (Loc.make ~loc (Lident name)) expr
  end)

module Polymorphic_variant = Make (struct
    let pat = ppat_variant
    let exp = pexp_variant
  end)
