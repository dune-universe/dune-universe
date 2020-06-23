(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2020  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Spotlib.Spot
open Tools

open Michelson
open Tezos_micheline.Micheline

open Untyped
open Parsetree
open Ast_builder

open Result.Infix

let rec revert tyenv ty = 
  let loc = Location.none in
  let rec fn tyenv ty = 
    let ty = Ctype.expand_head tyenv ty in
    match ty.desc with
    | Tvar _ when ty.level = Btype.generic_level -> Error "type variable"
    | Tvar _ ->
        (* Non generalized type variable.  We are brave enough to unify it with Unit *)
        Ctype.unify tyenv ty Predef.type_unit; (* must succeed *)
        fn tyenv ty
    | Tarrow (Nolabel, _f, _t, _) -> 
        Ok (fun _ -> Error "Lambda cannot be reverted")
    | Ttuple [t1; t2] -> 
        fn tyenv t1 >>= fun r1 ->
        fn tyenv t2 >>| fun r2 -> 
        (function 
          | Prim (_, "Pair", [t1; t2], _) -> 
              r1 t1 >>= fun m1 ->
              r2 t2 >>= fun m2 ->
              Ok [%expr ([%e m1], [%e m2])]
          | _ -> Error "Expected a Pair")
    | Ttuple tys -> 
        Result.mapM (fn tyenv) tys >>| fun rs ->
        let rtree = Binplace.place rs in
        (fun m ->
           let rec scan m rtree = match m, rtree with
             | Prim (_, "Pair", [t1; t2], _), Binplace.Branch (rt1, rt2) ->
                 scan t1 rt1 >>= fun m1 ->
                 scan t2 rt2 >>= fun m2 ->
                 Ok (Binplace.Branch (m1, m2))
             | _, Branch _ -> Error "Pair expected"
             | t, Leaf r -> r t >>| fun m -> Binplace.Leaf m
           in
           scan m rtree >>= fun mtree ->
           let mlist = 
             let rec flatten = function
               | Binplace.Leaf m -> [m]
               | Branch (mt1, mt2) -> flatten mt1 @ flatten mt2
             in
             flatten mtree
           in
           begin match Ast_builder.pexp_tuple_opt mlist with
             | Some e -> Ok e
             | None -> Error "failed to build IML tuple"
           end)
    | Tconstr (p, [], _) when p = Predef.path_bool -> 
        Ok (function
            | Prim (_, "True", [], _) -> Ok (ebool true)
            | Prim (_, "False", [], _) -> Ok (ebool false)
            | _ -> Error "bool expected")
    | Tconstr (p, [ty], _) when p = Predef.path_list -> 
        Ok (function
            | Seq (_, ts) ->
                fn tyenv ty >>= fun r ->
                Result.mapM r ts >>| elist
            | _ -> Error "sequence expected")
    | Tconstr (p, [ty], _) when p = Predef.path_option -> 
        Ok (function
            | Prim (_, "Some", [t], _) ->
                fn tyenv ty >>= fun r -> r t
            | Prim (_, "None", [], _) -> Ok [%expr None]
            | _ -> Error "option expected")
    | Tconstr (p, [], _) when p = Predef.path_unit ->
        Ok (function
            | Prim (_, "Unit", [], _) -> Ok [%expr ()]
            | _ -> Error "Unit expected")
    | Tconstr (p, [], _) when p = Predef.path_string -> 
        Ok (function
            | String (_, s) -> Ok (estring s)
            | _ -> Error "String expected")
    | Tconstr (p, tys, _) ->
        Result.mapM (fn tyenv) tys >>= fun rs ->
        begin match Path.is_scaml p, rs with
          | Some "sum", [r1; r2] -> 
              Ok (function
                  | Prim (_, "Left", [t], _) -> r1 t >>| fun x -> [%expr Left [%e x] ]
                  | Prim (_, "Right", [t], _) -> r2 t >>| fun x -> [%expr Right [%e x] ]
                  | _ -> Error "Left or Right expected")
          | Some "int", [] -> 
              Ok (function
                  | Int (_, n) -> Ok ([%expr Int [%e eint (Z.to_int n)] ]) (* XXX overflow *)
                  | _ -> Error "Int expected")
          | Some "nat", [] -> 
              Ok (function
                  | Int (_, n) -> Ok ([%expr Nat [%e eint (Z.to_int n)] ]) (* XXX overflow *)
                  | _ -> Error "Int expected")
          | Some "tz",  [] -> 
              Ok (function
                  | Int (_, n) -> 
                      let d,r = Z.div_rem n (Z.of_int 1_000_000) in
                      let s = Printf.sprintf "%d.%06d" (Z.to_int d) (Z.to_int r) (* XXX overflow *) in
                      Ok ([%expr Tz [%e efloat s ] ])
                  | _ -> Error "Int expected")
          | Some "set"         , [r]       -> 
              Ok (function
                  | Seq (_, ts) -> 
                      Result.mapM r ts >>= fun ms ->
                      Ok ([%expr Set [%e elist ms ] ])
                  | _ -> Error "Set expected")
          | Some "map"         , [r1; r2] -> 
              Ok (function
                  | Seq (_, kvs) ->
                      Result.mapM (function
                          | Prim (_, "Elt", [k; v], _) ->
                              r1 k >>= fun mk ->
                              r2 v >>| fun mv ->
                              [%expr ( [%e mk], [%e mv] )]
                          | _ -> Error "Elt expected") kvs >>| fun mkvs ->
                      [%expr Map [%e elist mkvs] ]
                  | _ -> Error "Map expected")
          | Some "big_map"     , [_;_] -> Error "big_map is not reversible"
          | Some "operation"   , [] -> Error "operation is not reversible"
          | Some "contract"    , [_] -> Error "contract is not reversible"
          | Some "timestamp"   , [] ->
              Ok (function
                  | String (_, s) -> Ok [%expr Timestamp [%e estring s] ]
                  | Int (_, z) ->
                      let s = 
                        (* Dupe.  must have a function *)
                        match Ptime.of_float_s @@ Z.to_float z with
                          | None -> assert false
                          | Some t -> Ptime.to_rfc3339 ~space:false ~frac_s:0 t
                      in
                      Ok [%expr Timestamp [%e estring s] ]
                  | _ -> Error "Int expected")
          | Some "address"     , [] ->
              Ok (function
                  | String (_, s) -> Ok [%expr Address [%e estring s ] ]
                  | _ -> Error "String expected")
          | Some "key"     , [] ->
              Ok (function
                  | String (_, s) -> Ok [%expr Key [%e estring s ] ]
                  | _ -> Error "String expected")
          | Some "signature"     , [] ->
              Ok (function
                  | String (_, s) -> Ok [%expr Signature [%e estring s ] ]
                  | _ -> Error "String expected")
          | Some "key_hash"     , [] ->
              Ok (function
                  | String (_, s) -> Ok [%expr Key_hash [%e estring s ] ]
                  | _ -> Error "String expected")
          | Some "bytes"       , []         ->
              Ok (function
                  | Bytes (_, s) -> 
                      let `Hex h = Hex.of_string (Tezos_stdlib.MBytes.to_string s) in
                      let h = "0x" ^ h in
                      Ok [%expr Bytes [%e estring h] ]
                  | _ -> Error "Bytes expected")
          | Some "chain_id"    , []         ->
              Ok (function
                  | String (_, s) -> Ok [%expr Chain_id [%e estring s ] ]
                  | _ -> Error "String expected")
          | Some _, _ -> Error "Unsupported data type p" (* (Unsupported_data_type p) *)

          | None, _ -> 
              match Env.find_type_descrs p tyenv with
              | [], [] -> Error "unsupported data type p" (* (Unsupported_data_type p) (* abstract XXX *) *)
              | [], labels -> revert_record_type tyenv ty p labels
              | constrs, [] -> revert_variant_type tyenv ty p constrs
              | _ -> assert false (* impossible *)
              | exception _ -> Error "unsupported data type p" (* (Unsupported_data_type p) *)
        end
    | Tpoly (ty, []) -> fn tyenv ty
    | _ -> Error "unsupported type ty" (* (Unsupported_type ty) *)
  in
  fn tyenv ty

and revert_record_type tyenv ty _p labels =
  (* record *)
  let ltys = List.map (fun label ->
      let _, ty_arg, ty_res = 
        Ctype.instance_label false (* XXX I do not know what it is *)
          label
      in
      Ctype.unify tyenv ty ty_res; (* XXX should succeed *)
      (label.lbl_name, ty_arg)) labels
  in
  Result.mapM (fun (l,ty) -> revert tyenv ty >>| fun m -> (l,m) ) ltys >>= fun lrs ->
  let lrtree = Binplace.place lrs in
  Ok (fun m ->
      let rec scan m lrtree = match m, lrtree with
        | Prim (_, "Pair", [t1; t2], _), Binplace.Branch (lrt1, lrt2) ->
            scan t1 lrt1 >>= fun lm1 ->
            scan t2 lrt2 >>= fun lm2 ->
            Ok (Binplace.Branch (lm1, lm2))
        | _, Branch _ -> Error "Pair expected"
        | t, Leaf (l,r) -> r t >>| fun m -> Binplace.Leaf (l,m)
      in
      scan m lrtree >>= fun lmtree ->
      let lmlist = 
        let rec flatten = function
          | Binplace.Leaf (lm) -> [lm]
          | Branch (lmt1, lmt2) -> flatten lmt1 @ flatten lmt2
        in
        flatten lmtree
      in
      Ok { pexp_desc= Pexp_record (List.map (fun (l,m) -> ({Location.txt=Longident.Lident l; loc=Location.none}, m)) lmlist, None)
         ; pexp_loc= Location.none
         ; pexp_loc_stack= []
         ; pexp_attributes= []
         })

and revert_variant_type tyenv ty _p constrs =
  let consts, non_consts = Variant.variant_type tyenv ty constrs in

  let revert_consts = match consts with
      | None -> []
      | Some names -> 
          [ function
            | Int (_, n) ->
                let n = Z.to_int n in
                begin match List.nth names n with
                  | exception _ -> Error "strange tag"
                  | l -> 
                      Ok { pexp_desc= Pexp_construct ({Location.txt= Longident.Lident l; loc= Location.none}, None)
                         ; pexp_loc = Location.none
                         ; pexp_loc_stack = []
                         ; pexp_attributes= []
                         }
                end    
            | _ -> Error "Int expected"
          ]
  in
  let revert_non_consts =
    Result.mapM (fun (l, tys) ->
        (match tys with 
         | [] -> assert false
         | [ty] -> revert tyenv ty
         | tys -> revert tyenv (Ctype.newty (Ttuple tys))) >>| fun f ->
        fun m -> 
          f m >>| fun x ->
          { pexp_desc= Pexp_construct ({Location.txt= Longident.Lident l; loc= Location.none}, Some x)
          ; pexp_loc = Location.none
          ; pexp_loc_stack= []
          ; pexp_attributes= []
          }) non_consts
  in
  revert_non_consts >>= fun revert_non_consts ->
  let nctree = Binplace.place (revert_consts @ revert_non_consts) in
  Ok (fun m ->
    let rec scan m nctree = match m, nctree with
      | Prim (_, "Left", [t], _), Binplace.Branch (nc1, _nc2) ->
          scan t nc1
      | Prim (_, "Right", [t], _), Binplace.Branch (_nc1, nc2) ->
          scan t nc2
      | _, Binplace.Branch _ -> Error "Left or Right expected"
      | t, Leaf r -> r t
    in
    scan m nctree)

(* revert mode *)
let get_type str = 
  let attrs = List.concat @@ List.map snd @@ Attribute.get_scaml_toplevel_attributes str in
  Flags.update (fun t -> List.fold_left (fun t ({Location.txt; loc}, v) -> 
      Result.at_Error (errorf_flags ~loc "%s") & Flags.eval t (txt, v))
      t attrs);

  let structure_item _str_final_env { Typedtree.str_desc; str_loc= loc } =
    match str_desc with
    | Tstr_value (Recursive, _) -> unsupported ~loc "recursive definitions"
    | Tstr_primitive _          -> unsupported ~loc "primitive declaration"
    | Tstr_typext _             -> unsupported ~loc "type extension"
    | Tstr_exception _          -> unsupported ~loc "exception declaration"
    | Tstr_module _ 
    | Tstr_recmodule _          -> unsupported ~loc "module declaration"
    | Tstr_class _              -> unsupported ~loc "class declaration"
    | Tstr_class_type _         -> unsupported ~loc "class type declaration"
    | Tstr_include _            -> unsupported ~loc "include"
    | Tstr_modtype _            -> unsupported ~loc "module type declaration"
    | Tstr_eval _               -> unsupported ~loc "evaluation"
    | Tstr_value _              -> unsupported ~loc "value definition"
    | Tstr_open _open_description -> []
    | Tstr_type (_, tds) -> 
        List.map (fun td -> match td.Typedtree.typ_params with
            | _::_ -> errorf_type_expr ~loc:td.typ_loc "Revert mode does not support parameterized type declarations"
            | [] ->
                let id = td.typ_id in
                let ty = Btype.newgenty (Tconstr (Path.Pident td.typ_id, [], ref Types.Mnil)) in
                `Type (id, ty)) tds
    | Tstr_attribute _ -> []
  in
  let structure { Typedtree.str_items= sitems ; str_final_env } =
    List.concat_map (structure_item str_final_env) sitems
  in
  structure str

open Tezos_error_monad
open Tezos_micheline

let get_michelson_value s =
  match Micheline_parser.tokenize s with
  | _, (_::_ as es) ->
      Format.eprintf "Michelson value parse error: %a@."
        Error_monad.pp_print_error es; assert false
  | tokens, [] ->
      match Micheline_parser.parse_expression ~check:false tokens with
      | _, (_::_ as es) ->
          Format.eprintf "Michelson value parse error: %a@."
            Error_monad.pp_print_error es; assert false
      | node, [] -> node

let do_revert str v = 
  match get_type str with
  | [] -> errorf_type_expr ~loc:Location.none "Needs one type definition"
  | _::_::_ -> errorf_type_expr ~loc:Location.none "Cannot have more than one type definitions"
  | [`Type (_id, ty)] ->
      match revert str.str_final_env ty with
      | Error e -> failwith e
      | Ok reverter ->
          let m = get_michelson_value v in
          let m = Micheline_printer.printable (fun s -> s) (Micheline.strip_locations m) in
          Format.eprintf "input: %a@." Mline.pp m;
          reverter m
