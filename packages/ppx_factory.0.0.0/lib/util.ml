open Ppxlib

let affix_from_type_name ~kind type_name =
  match type_name, kind with
  | "t", _ -> ""
  | _, `Suffix -> "_" ^ type_name
  | _, `Prefix -> type_name ^ "_"

let constr_from_type_param ~loc (core_type, _variance) =
  {core_type with ptyp_loc = loc; ptyp_attributes = []}

let core_type_from_type_decl ~loc {ptype_name; ptype_params; _} =
  let constr = List.map (constr_from_type_param ~loc) ptype_params in
  let type_lident = {txt = Lident ptype_name.txt; loc} in
  Ast_builder.Default.ptyp_constr ~loc type_lident constr

module Expr = struct
  let var ~loc var_name = Ast_builder.Default.pexp_ident ~loc {txt = Lident var_name; loc}
  let constructor ~loc ~constructor_name expr =
    Ast_builder.Default.pexp_construct ~loc {txt = Lident constructor_name; loc} expr
end

module List_ = struct
  exception Exit

  (* Thanks c-cube's containers *)
  let all_ok l =
    let err = ref None in
    try Ok (List.map (function Ok x -> x | Error e -> err := Some e; raise Exit) l)
    with Exit ->
    match !err with
    | Some e -> Error e
    | None -> assert false

  let rec find_ok ~f = function
    | [] -> Error `Empty
    | [last] ->
      ( match f last with
        | Ok _ as ok -> ok
        | Error err -> Error (`Last err)
      )
    | hd::tl ->
      ( match f hd with
        | Ok _ as ok -> ok
        | Error _ -> find_ok ~f tl
      )
end

module Result_ = struct
  let (>|=) res f =
    match res with
    | Ok x -> Ok (f x)
    | Error _ as err -> err
end
