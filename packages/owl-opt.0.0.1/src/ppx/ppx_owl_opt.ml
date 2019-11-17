(* adapted from deriving-slowly: https://github.com/rgrinberg/deriving-slowly *)
open Base
open Ppxlib

let str_gen ~loc ~path:_ (_rec, t) =
  let (module Ast) = Ast_builder.make loc in
  let t = List.hd_exn t in
  let fields =
    match t.ptype_kind with
    | Ptype_record fields -> fields
    | _ -> Location.raise_errorf ~loc "ppx_owl_optimise only works on records"
  in
  let lident_of_field field =
    Ast_builder.Default.Located.lident ~loc:field.pld_name.loc field.pld_name.txt
  in
  let map =
    let map_record_expr =
      Ast.pexp_record
        (List.map fields ~f:(fun field ->
             let z = Ast.pexp_ident (lident_of_field field) in
             let pattern = [%expr f [%e z]] in
             let field_id = lident_of_field field in
             field_id, pattern))
        None
    in
    let map_record_pat =
      let fields =
        List.map fields ~f:(fun field ->
            let pattern = Ast.pvar field.pld_name.txt in
            let field_id = lident_of_field field in
            field_id, pattern)
      in
      Ast.ppat_record fields Closed
    in
    let f_name = "map" in
    let pat = Ast.pvar f_name in
    let expr =
      Ast.pexp_fun Nolabel None map_record_pat map_record_expr
      |> Ast.pexp_fun (Labelled "f") None (Ast.pvar "f")
    in
    [ Ast.value_binding ~pat ~expr ] |> Ast.pstr_value Nonrecursive
  in
  let map2 =
    let map2_record_expr =
      let er1 = [%expr r1] in
      let er2 = [%expr r2] in
      let fields =
        List.(
          map fields ~f:(fun field ->
              let z = lident_of_field field in
              let r1 = Ast.pexp_field er1 z in
              let r2 = Ast.pexp_field er2 z in
              let pattern = [%expr f [%e r1] [%e r2]] in
              let field_id = lident_of_field field in
              field_id, pattern))
      in
      Ast.pexp_record fields None
    in
    let f_name = "map2" in
    let pat = Ast.pvar f_name in
    let expr =
      Ast.pexp_fun Nolabel None (Ast.pvar "r2") map2_record_expr
      |> Ast.pexp_fun Nolabel None (Ast.pvar "r1")
      |> Ast.pexp_fun (Labelled "f") None (Ast.pvar "f")
    in
    [ Ast.value_binding ~pat ~expr ] |> Ast.pstr_value Nonrecursive
  in
  let iter =
    let iter_record_expr =
      let rec f a = function
        | [] -> a
        | hd :: tl ->
          let z = Ast.pexp_ident (lident_of_field hd) in
          let a =
            [%expr
              f [%e z];
              [%e a]]
          in
          f a tl
      in
      match fields with
      | [] -> failwith "record not empty"
      | hd :: tl ->
        let z = Ast.pexp_ident (lident_of_field hd) in
        let a = [%expr f [%e z]] in
        f a tl
    in
    let iter_record_pat =
      let fields =
        List.map fields ~f:(fun field ->
            let pattern = Ast.pvar field.pld_name.txt in
            let field_id = lident_of_field field in
            field_id, pattern)
      in
      Ast.ppat_record fields Closed
    in
    let f_name = "iter" in
    let pat = Ast.pvar f_name in
    let expr =
      Ast.pexp_fun Nolabel None iter_record_pat iter_record_expr
      |> Ast.pexp_fun (Labelled "f") None (Ast.pvar "f")
    in
    [ Ast.value_binding ~pat ~expr ] |> Ast.pstr_value Nonrecursive
  in
  let iter2 =
    let iter2_record_expr =
      let er1 = [%expr r1] in
      let er2 = [%expr r2] in
      let rec f a = function
        | [] -> a
        | hd :: tl ->
          let z = lident_of_field hd in
          let r1 = Ast.pexp_field er1 z in
          let r2 = Ast.pexp_field er2 z in
          let a =
            [%expr
              f [%e r1] [%e r2];
              [%e a]]
          in
          f a tl
      in
      match fields with
      | [] -> failwith "record not empty"
      | hd :: tl ->
        let z = lident_of_field hd in
        let r1 = Ast.pexp_field er1 z in
        let r2 = Ast.pexp_field er2 z in
        let a = [%expr f [%e r1] [%e r2]] in
        f a tl
    in
    let f_name = "iter2" in
    let pat = Ast.pvar f_name in
    let expr =
      Ast.pexp_fun Nolabel None (Ast.pvar "r2") iter2_record_expr
      |> Ast.pexp_fun Nolabel None (Ast.pvar "r1")
      |> Ast.pexp_fun (Labelled "f") None (Ast.pvar "f")
    in
    [ Ast.value_binding ~pat ~expr ] |> Ast.pstr_value Nonrecursive
  in
  [ map; map2; iter; iter2 ]


let str_type_decl = Deriving.Generator.make_noarg str_gen
let name = "prms"
let () = Deriving.add name ~str_type_decl |> Deriving.ignore
