(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* expansion(?) of expr/pattern by type *)

open Asttypes
open Types
open Format
open Utils

module EnvSummary = struct
  open Env

  let rec format ppf = function
    | Env_empty -> fprintf ppf "END"
    | Env_value (sum, id, _vdesc) -> 
        fprintf ppf "Value %s@ " (Ident.name id);
        format ppf sum
    | Env_type (sum, id, _tdesc) ->
        fprintf ppf "Type %s@ " (Ident.name id);
        format ppf sum
    | Env_extension (sum, id, _) ->
        fprintf ppf "Exc %s@ " (Ident.name id);
        format ppf sum
    | Env_module (sum, id, _) ->
        fprintf ppf "Module %s@ " (Ident.name id);
        format ppf sum
    | Env_modtype (sum, id, _) -> 
        fprintf ppf "Module type %s@ " (Ident.name id);
        format ppf sum
    | Env_class (sum, id, _) -> 
        fprintf ppf "Class %s@ " (Ident.name id);
        format ppf sum
    | Env_cltype (sum, id, _) -> 
        fprintf ppf "Class type %s@ " (Ident.name id);
        format ppf sum
    | Env_open (sum, _, p) ->
        fprintf ppf "open %s@ " (Path.name p);
        format ppf sum
    | Env_functor_arg (sum, id) ->
        fprintf ppf "Functor arg %s@ " (Ident.name id);
        format ppf sum
    | Env_constraints (sum, (tdpm : type_declaration PathMap.t)) ->
        fprintf ppf "Constraints @[";
        PathMap.iter (fun k _ -> fprintf ppf "%s@," (Path.name k)) tdpm;
        fprintf ppf "@]";
        format ppf sum
    | Env_copy_types (sum, ts) ->
        fprintf ppf "Copy_types %s" (String.concat " " ts);
        format ppf sum

  let format ppf sum = fprintf ppf "@[<v>%a@]" format sum
end

type t =
  | Function of (arg_label * type_expr) list * type_expr
  | Tuple of type_expr list
  | Variant of Path.t option * Types.constructor_declaration list
  | Record of Path.t option * (Ident.t * type_expr) list
  | Polyvar of (label * row_field) list
  | Abstract
  | Open

let constructor_with_path name = function
  | None -> Ident.name name
  | Some p -> Path.name p ^ "." ^ Ident.name name

let format_as_expr ppf = function
  | Function (label_typ_list, _) ->
      fprintf ppf "(fun @[%a@] -> assert false)"
        (Format.list " " (fun ppf (l, _typ) -> match l with
        | Nolabel -> fprintf ppf "_"
        | Labelled l -> fprintf ppf "~%s" l
        | Optional l -> fprintf ppf "?%s" l))
        label_typ_list
  | Tuple typs ->
      fprintf ppf "(@[%a@])" (Format.list ", " (fun ppf _ -> fprintf ppf "assert false")) typs
  | Variant (pathopt, args) ->
      fprintf ppf "(assert false (* @[%a@] *))" 
        (Format.list "@ | " (fun ppf -> 
          function
            | { cd_id=name; cd_args= Cstr_tuple [_; _] } when Ident.name name = "::" -> fprintf ppf "(_ :: _)"
            | { cd_id=name; cd_args= Cstr_tuple [] } -> fprintf ppf "%s" (constructor_with_path name pathopt)
            | { cd_id=name; cd_args= Cstr_tuple args } -> 
                fprintf ppf "%s (@[%a@])"
                  (constructor_with_path name pathopt)
                  (Format.list ", " (fun ppf _ -> fprintf ppf "assert false" )) args
            | { cd_id=_name; cd_args= Cstr_record _lds } -> 
                (* CR jfuruse: not yet *)
                assert false
         ))
        args
  | Record (None, label_typ_list) -> 
      fprintf ppf "{ @[%a@] }"
        (Format.list "; " (fun ppf (l, _ty) ->
          fprintf ppf "%a = assert false" Ident.print l)) 
        label_typ_list
  | Record (Some path, label_typ_list) -> 
      fprintf ppf "{ @[%s.%a@] }"
        (Path.name path)
        (Format.list "; " (fun ppf (l, _ty) ->
          fprintf ppf "%a = assert false" Ident.print l)) 
        label_typ_list
  | Polyvar l_field_list ->
      fprintf ppf "(assert false (* @[%a@] | ... *))" 
        (Format.list "@ | " (fun ppf (name, row_field) -> 
          match row_field with
          | Rabsent | Rpresent None -> fprintf ppf "`%s" name
          | Rpresent (Some _) -> fprintf ppf "`%s (assert false)" name
          (* CR jfuruse: not sure... *)
          | Reither (true, [], _, _) -> fprintf ppf "`%s" name
          | Reither (true, _, _, _) -> fprintf ppf "`%s (assert false)" name
          | Reither (false, _, true, { contents = None }) -> fprintf ppf "`%s" name
          | Reither (false, _, true, { contents = Some _ }) -> fprintf ppf "`%s (assert false)" name
          | Reither (false, _, false, _) -> fprintf ppf "`%s (* ??? *)" name))
        l_field_list
  | Abstract -> fprintf ppf "(assert false (* abstract *))"
  | Open -> fprintf ppf "(assert false (* open *))"

let format_as_pattern ppf = function
  | Function (_label_typ_list, _) -> fprintf ppf "_ (* function *)"
  | Tuple typs ->
      fprintf ppf "(@[%a@])" (Format.list ", " (fun ppf _ -> fprintf ppf "_")) typs
  | Variant (pathopt, args) ->
      fprintf ppf "( @[%a@] )" 
        (Format.list "@ | " (fun ppf -> 
          function
            | { cd_id=name; cd_args= Cstr_tuple [_; _] } when Ident.name name = "::" -> fprintf ppf "(_ :: _)"
            | { cd_id=name; cd_args= Cstr_tuple [] } -> fprintf ppf "%s" (constructor_with_path name pathopt)
            | { cd_id=name; cd_args= Cstr_tuple [_arg] } -> fprintf ppf "%s _" (constructor_with_path name pathopt)
            | { cd_id=name; cd_args= Cstr_tuple args } -> 
                fprintf ppf "%s (@[%a@])"
                  (constructor_with_path name pathopt)
                  (Format.list ", " (fun ppf _ -> fprintf ppf "_" )) args
            | { cd_id=_name; cd_args= Cstr_record _ } -> assert false (* CR jfuruse: not yet *)
         ))
        args
  | Record (None, label_typ_list) -> 
      fprintf ppf "{ @[%a@] }"
        (Format.list "; " (fun ppf (l, _ty) ->
          fprintf ppf "%a = _" Ident.print l)) 
        label_typ_list
  | Record (Some path, label_typ_list) -> 
      fprintf ppf "{ @[%s.%a@] }"
        (Path.name path)
        (Format.list "; " (fun ppf (l, _ty) ->
          fprintf ppf "%a = _" Ident.print l)) 
        label_typ_list
  | Polyvar l_field_list ->
      fprintf ppf "(@[%a@] | ... )" 
        (Format.list "@ | " (fun ppf (name, row_field) -> 
          match row_field with
          | Rabsent | Rpresent None -> fprintf ppf "`%s" name
          | Rpresent (Some _) -> fprintf ppf "`%s _" name
          (* CR jfuruse: not sure... *)
          | Reither (true, [], _, _) -> fprintf ppf "`%s" name
          | Reither (true, _, _, _) -> fprintf ppf "`%s _" name
          | Reither (false, _, true, { contents = None }) -> fprintf ppf "`%s" name
          | Reither (false, _, true, { contents = Some _ }) -> fprintf ppf "`%s _" name
          | Reither (false, _, false, _) -> fprintf ppf "`%s (* ??? *)" name))
        l_field_list
  | Abstract -> fprintf ppf "_ (* abstract *)"
  | Open -> fprintf ppf "_ (* open *)"

(** get_path:  Foo.t => Foo *)
let get_path = function
  | Path.Pident _ -> None
  | Path.Pdot (t, _, _) -> Some t
  | Path.Papply _ -> None (* Strange! *)

let rec expand env typ = match (Ctype.repr typ).desc with
  | Tarrow (label, typ_arg, typ_body, _) -> expand_arrow [label, typ_arg] typ_body
  | Ttuple typs -> Tuple typs
  | Tconstr (path, typs_param, _) ->
      begin try 
        Ctype.init_def (Btype.generic_level - 1); (* CR jfuruse: recover the level? *)
        let tdesc = Ctype.instance_declaration (Env.find_type path env) in
        assert (List.length typs_param = tdesc.type_arity);
        (* Should success *)
        List.iter2 (Ctype.unify_var env) tdesc.type_params typs_param;
        match tdesc.type_kind with
        | Type_variant label_args -> Variant (get_path path, label_args)
        | Type_record (fields, _) -> 
            Record (get_path path, List.map (fun { ld_id= name; ld_type= ty } -> (name, ty)) fields)
        | Type_open -> Open
        | Type_abstract ->
            match tdesc.type_manifest with
            | None -> Abstract
            | Some typ -> expand env typ
      with
      | Not_found -> 
          eprintf "ENV @[%a@]@." EnvSummary.format (Env.summary env);
          eprintf "NOT FOUND %s@." (Path.name path);
          Abstract (* pity *)
      end
  | Tvariant row_desc -> Polyvar row_desc.row_fields
  | Tpoly (typ, _) -> expand env typ (* CR jfuruse: ? *)
  | Tvar _ | Tnil | Tobject (_, _) | Tfield (_, _, _, _) | Tpackage _ -> Abstract
  | Tlink _ -> assert false
  | Tsubst _ -> assert false
  | Tunivar _ -> assert false

and expand_arrow st typ =
  match (Ctype.repr typ).desc with
  | Tarrow (label, typ_arg, typ_body, _) -> 
      expand_arrow ((label, typ_arg) :: st) typ_body
  | _ -> Function (List.rev st, typ)

let expand load_path env ty =
  let load_path_back = !Config.load_path in
  Config.load_path := load_path;
  Utils.protect ~f:(fun () -> expand env ty) () ~finally:(fun _ ->
    Config.load_path := load_path_back)

