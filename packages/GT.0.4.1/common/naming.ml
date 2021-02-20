open Base
open Printf

let meth_of_constr = sprintf "c_%s"

let self_arg_name = "fself"
let self_typ_param_name = "self"
let gcata_name_for_typ name = Printf.sprintf "gcata_%s" name
let class_name_for_typ name = Printf.sprintf "%s_t" name
let trait_class_name_for_typ ~trait name =
  class_name_for_typ (if String.equal trait ""
                      then name
                      else Printf.sprintf "%s_%s" trait name)

let fix_name ~plugin_name = sprintf "%s_fix"
(* 1st structure is planned to contain transformation function *)
let typ1_for_class_arg ~plugin = sprintf "%s_t_%s_1" plugin
let trf_field ~plugin = sprintf "%s_%s_trf" plugin

(* Should contain object for transforming mutally declared type *)
(* let typ2_for_class_arg ~plugin_name = sprintf "%s_t_%s_2" plugin_name *)
let mut_ofield ~plugin = sprintf "%s_o%s_func" plugin

(* Largest. Containt not fully initialized stib class *)
let typ3_for_class_arg ~plugin_name = sprintf "%s_t_%s_3" plugin_name
let mut_oclass_field ~plugin = sprintf "%s_%s_func" plugin

let extra_param_name = "extra"
let self_arg_name = "fself"
let all_trfs_together = "all_trfs_together"
let make_extra_param = sprintf "%s_%s" extra_param_name

open Ppxlib

let meth_name_for_record tdecl = sprintf "do_%s" tdecl.ptype_name.txt

let fix_result_record trait tdecls =
  assert (List.length tdecls > 0);
  let name = (List.hd_exn tdecls).ptype_name.txt in
  String.concat ~sep:"_" [trait; "fix"; name]

let trf_function trait s = Printf.sprintf "%s_%s" trait s
let make_stub_class_name ~plugin tname =
  sprintf "%s_%s_t_stub" plugin tname
let stub_class_name ~plugin tdecl =
  make_stub_class_name ~plugin tdecl.ptype_name.txt

let init_trf_function trait s = trf_function trait s ^ "_0"

let make_fix_name tdecls =
  (* Let's use only first type for fix function definition *)
  assert (List.length tdecls > 0);
  let name = (List.hd_exn tdecls).ptype_name.txt in
  String.concat ~sep:"_" ["fix"; name]

let name_fix_generated_object ~plugin tdecl =
  sprintf "%s_o_%s" plugin tdecl.ptype_name.txt
let prereq_name ~plugin tail = sprintf "%s_%s_prereq" plugin tail
let mut_arg_composite = (* "mut_trfs_here" *) "call"
let mut_arg_name ~plugin = sprintf "for_%s_%s" plugin
(* let mut_class_stubname ~plugin tdecl =
 *   sprintf "%s_%s_stub" plugin_name tdecl.ptype_name.txt *)

let fix_result tdecl =
  sprintf "fix_result_%s" tdecl.ptype_name.txt

let cname_index typname = String.capitalize typname
let mutuals_pack = "_mutuals_pack"
let hack_index_name tdecls s =
  assert (List.length tdecls > 0);
  sprintf "%s_%s" s (List.hd_exn tdecls).ptype_name.txt

let fix_func_name ?for_ trait =
  match for_ with
  | None -> sprintf "%s_fix" trait
  | Some s -> sprintf "%s_%s_fix" trait s

let fix_func_name_tdecls trait tdecls =
  assert (List.length tdecls > 0);
  fix_func_name ~for_:(List.hd_exn tdecls).ptype_name.txt trait


let for_ trait s = sprintf "%s_%s" trait s

let meth_name_for_constructor attrs default_name =
  let good_attr =
    let open Deriving.Args in
    attribute ~name:(string "name") ~payload:(single_expr_payload (estring __))
  in
  let cond attr =
    (* Stdlib.Sys.command "notify-send 'hecking an attribute of constructor' " |> ignore; *)
    Deriving.Args.parse good_attr Ppxlib.Location.none attr (fun s ->
      (* let _ = Stdlib.Sys.command (Printf.sprintf "notify-send 'found %s'" s) in *)
      Some s
    )
  in

  List.find_map attrs ~f:cond
  |> Option.value ~default:default_name
  |> meth_of_constr
