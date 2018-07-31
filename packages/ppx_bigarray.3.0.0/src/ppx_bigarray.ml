(* ppx_bigarray --- A PPX extension for providing big array literals in OCaml

   Copyright (C) 2015 Akinori ABE
   This software is distributed under MIT License
   See LICENSE.txt for details. *)

open Format
open Compat

let ( >> ) f g x = g (f x)

let rec split chr offset str =
  try
    let i = String.index_from str offset chr in
    (String.sub str offset (i - offset)) :: (split chr (i + 1) str)
  with _ ->
    [String.sub str offset (String.length str - offset)]

let bigarray_kind_of_string ~loc = function
  | "float32" -> NestedMatrix.Float32
  | "float64" -> NestedMatrix.Float64
  | "int8_signed" | "sint8" -> NestedMatrix.Int8_signed
  | "int8_unsigned" | "uint8" -> NestedMatrix.Int8_unsigned
  | "int16_signed" | "sint16" -> NestedMatrix.Int16_signed
  | "int16_unsigned" | "uint16" -> NestedMatrix.Int16_unsigned
  | "int32" -> NestedMatrix.Int32
  | "int64" -> NestedMatrix.Int64
  | "int" -> NestedMatrix.Int
  | "nativeint" -> NestedMatrix.Nativeint
  | "complex32" -> NestedMatrix.Complex32
  | "complex64" -> NestedMatrix.Complex64
  | "char" -> NestedMatrix.Char
  | s -> NestedMatrix.Dynamic (ExtAst.Exp.ident ~loc s)

let bigarray_layout_of_string ~loc = function
  | "c" | "c_layout" -> NestedMatrix.C_layout
  | "fortran" | "fortran_layout" -> NestedMatrix.Fortran_layout
  | s -> NestedMatrix.Dynamic_layout (ExtAst.Exp.ident ~loc s)

let check_nested_matrix size mat =
  let msg_head =
    sprintf "This %d-dimensional big array literal expects size %s"
      (List.length size) (NestedMatrix.string_of_size size) in
  let gather_warnings acc = function
    | (n, NestedMatrix.Leaf (loc, _)) ->
      Error.exnf ~loc "Syntax Error: @[%s@\n\
                       This expression should be a list or an array@ \
                       of length %d@ syntactically@]" msg_head n ()
    | (-1, NestedMatrix.Node (loc, _)) ->
      Error.exnf ~loc "Syntax Error: @[%s@\n\
                       This expression should be an element of a big array@\n\
                       but a list or an array is given@]" msg_head ()
    | (n, NestedMatrix.Node (loc, children)) ->
      let msg = sprintf "@[%s@\nThis row should have %d element(s),@ \
                         but %d element(s) are given@]"
          msg_head n (List.length children) in
      (Ast_mapper.attribute_of_warning loc msg) :: acc
  in
  let errors = NestedMatrix.check_rect size mat in
  List.fold_left gather_warnings [] errors

let get_padding loc attrs =
  match List.partition (fun (x, _) -> x.Location.txt = "bigarray.padding") attrs with
  | ([], attrs') -> (None, attrs')
  | ([(_, payload)], attrs') ->
    begin
      match payload with
      | PStr [{ pstr_desc = Pstr_eval (expr, _); _ }] -> (Some expr, attrs')
      | _ ->
        Error.exnf ~loc
          "Syntax Error: @[This should be [@@bigarray.padding expression]@]" ()
    end
  | _ ->
    Error.exnf ~loc "Error: @[Duplicated bigarray.padding attributes@]" ()

let make_bigarray_literal loc attrs ba_type kind layout = function
  | PStr [{ pstr_desc = Pstr_eval (expr, _); _ }] -> (* [%ext expression] *)
    let level = match ba_type with
      | NestedMatrix.Array1 -> Some 1
      | NestedMatrix.Array2 -> Some 2
      | NestedMatrix.Array3 -> Some 3
      | NestedMatrix.Genarray -> None in
    let (padding, attrs') = get_padding loc expr.pexp_attributes in
    let mat = NestedMatrix.of_expression ?level expr in
    let size = NestedMatrix.size mat in
    let (mat', warnings) =
      match padding with
      | None -> (mat, check_nested_matrix size mat)
      | Some ep -> (* Padding inhibits warnings *)
        ignore (check_nested_matrix size mat);
        (NestedMatrix.pad size mat ep.pexp_loc ep, [])
    in
    NestedMatrix.to_expression ~loc ~attrs:(warnings @ attrs @ attrs')
      ba_type kind layout size mat'
  | _ -> Error.exnf ~loc
           "Syntax Error: @[This expression should be a list or an array@ \
            syntactically@]" ()

let map_ext_bigarray loc attrs ba_type kind layout payload =
  make_bigarray_literal loc attrs ba_type
    (bigarray_kind_of_string ~loc kind)
    (bigarray_layout_of_string ~loc layout) payload

let map_ext_bigarray_alias loc attrs ba_type name payload =
  let alias = ExtAst.Exp.ident ~loc ("ppx_bigarray__" ^ name) in
  let kind = NestedMatrix.Dynamic
      (ExtAst.Exp.field ~loc alias "Ppx_bigarray_runtime.kind") in
  let layout = NestedMatrix.Dynamic_layout
      (ExtAst.Exp.field ~loc alias "Ppx_bigarray_runtime.layout") in
  make_bigarray_literal loc attrs ba_type kind layout payload

let bigarray_mapper _ _ =
  let super = Ast_mapper.default_mapper in
  let expr self e = match e with
    | { pexp_desc = Pexp_extension ({ Location.txt; _ }, payload); _ } ->
      begin
        let loc = e.pexp_loc in
        let attrs = self.Ast_mapper.attributes self e.pexp_attributes in
        try
          match split '.' 0 txt with
          | ["bigarray1"; kind; layout] ->
            map_ext_bigarray loc attrs NestedMatrix.Array1 kind layout payload
          | ["bigarray2"; kind; layout] ->
            map_ext_bigarray loc attrs NestedMatrix.Array2 kind layout payload
          | ["bigarray3"; kind; layout] ->
            map_ext_bigarray loc attrs NestedMatrix.Array3 kind layout payload
          | ["bigarray"; kind; layout] ->
            map_ext_bigarray loc attrs NestedMatrix.Genarray kind layout payload
          | ["bigarray1"; alias] ->
            map_ext_bigarray_alias loc attrs NestedMatrix.Array1 alias payload
          | ["bigarray2"; alias] ->
            map_ext_bigarray_alias loc attrs NestedMatrix.Array2 alias payload
          | ["bigarray3"; alias] ->
            map_ext_bigarray_alias loc attrs NestedMatrix.Array3 alias payload
          | ["bigarray"; alias] ->
            map_ext_bigarray_alias loc attrs NestedMatrix.Genarray alias payload
          | _ -> super.Ast_mapper.expr self e
        with
        | Location.Error error ->
          Ast_helper.Exp.extension ~loc ~attrs (Ast_mapper.extension_of_error error)
      end
    | _ -> super.Ast_mapper.expr self e
  in
  { super with Ast_mapper.expr; }

let () =
  Migrate_parsetree.Driver.register
    ~name:"bigarray" ~args:[]
    ocaml_version bigarray_mapper
