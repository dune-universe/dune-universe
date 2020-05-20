open Migrate_parsetree.Ast_406

let make_expr ~loc desc =
  Parsetree.{ pexp_desc = desc; pexp_loc = loc; pexp_attributes = [] }

let make_indent ~loc s =
  let desc = Parsetree.Pexp_ident { txt = Longident.parse s; loc } in
  make_expr ~loc desc

let make_const_int ~loc i =
  let desc = Parsetree.Pexp_constant (Pconst_integer (string_of_int i, None)) in
  make_expr ~loc desc

let make_const_string ~loc s =
  let desc = Parsetree.Pexp_constant (Pconst_string (s, None)) in
  make_expr ~loc desc

let make_tuple ~loc exprs =
  let desc = Parsetree.Pexp_tuple exprs in
  make_expr ~loc desc

let make_apply ~loc callee args =
  let desc = Parsetree.(Pexp_apply (callee, args)) in
  make_expr ~loc desc

let gen_log_expr severity ~loc expr =
  let apply args =
    let line = loc.Location.loc_start.pos_lnum in
    let path = loc.Location.loc_start.pos_fname in
    let callee =
      let s =
        match severity with
        | Loga.Severity.Emergency -> "Loga.Logger.emergency"
        | Loga.Severity.Alert -> "Loga.Logger.alert"
        | Loga.Severity.Critical -> "Loga.Logger.critical"
        | Loga.Severity.Error -> "Loga.Logger.error"
        | Loga.Severity.Warning -> "Loga.Logger.warning"
        | Loga.Severity.Notice -> "Loga.Logger.notice"
        | Loga.Severity.Info -> "Loga.Logger.info"
        | Loga.Severity.Debug -> "Loga.Logger.debug"
      in
      make_indent ~loc s
    in
    let logger = make_indent ~loc "Loga.logger" in
    let path = make_const_string ~loc path in
    let line = make_const_int ~loc line in
    let location = make_tuple ~loc [ path; line ] in
    let applied =
      make_apply ~loc callee
        ((Asttypes.Nolabel, logger) :: (Asttypes.Nolabel, location) :: args)
    in
    Parsetree.{ expr with pexp_desc = applied.pexp_desc }
  in
  match expr with
  | Parsetree.{ pexp_desc = Pexp_constant _; _ } ->
      (* Loga.* "" *)
      let args = [ (Asttypes.Nolabel, expr) ] in
      apply args
  | Parsetree.{ pexp_desc = Pexp_apply (recv, args_with_labels); _ } ->
      (* Loga.* "" ... *)
      let args = (Asttypes.Nolabel, recv) :: args_with_labels in
      apply args
  | _ -> Location.raise_errorf ~loc "Expr constant/apply is expected"

let gen_log_pstr severity ~loc payload =
  match payload with
  | Parsetree.PStr
      [ Parsetree.{ pstr_desc = Pstr_eval (sexpr, _attrs); pstr_loc; _ } ] ->
      gen_log_expr ~loc:pstr_loc severity sexpr
  | _ -> Location.raise_errorf ~loc "Structure is expected"

let expr mapper expr =
  match expr with
  | Parsetree.{ pexp_desc = Pexp_extension ({ txt; loc }, payload); _ } ->
      let generator =
        match txt with
        | "loga.emergency" -> Some (gen_log_pstr Loga.Severity.Emergency)
        | "loga.alert" -> Some (gen_log_pstr Loga.Severity.Alert)
        | "loga.critical" -> Some (gen_log_pstr Loga.Severity.Critical)
        | "loga.error" -> Some (gen_log_pstr Loga.Severity.Error)
        | "loga.warning" -> Some (gen_log_pstr Loga.Severity.Warning)
        | "loga.notice" -> Some (gen_log_pstr Loga.Severity.Notice)
        | "loga.info" -> Some (gen_log_pstr Loga.Severity.Info)
        | "loga.debug" -> Some (gen_log_pstr Loga.Severity.Debug)
        | _ -> None
      in
      let ast =
        match generator with
        | Some gen -> gen ~loc payload
        | None -> Ast_mapper.default_mapper.expr mapper expr
      in
      ast
  | _ -> Ast_mapper.default_mapper.expr mapper expr

let () =
  Migrate_parsetree.Driver.register ~name:"loga"
    (module Migrate_parsetree.OCaml_406)
    (fun _ _ -> { Ast_mapper.default_mapper with expr })
