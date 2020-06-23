(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* InterMediate Language, or Intermediate ML *)
open Spotlib.Spot
open Asttypes
open Untyped
open Typedtree
open Tools
open Result.Infix

let if_debug = Flags.if_debug

module M = Michelson
open M.Type
module C = M.Constant

open IML

let modname = ref None (* XXX HACK *)

let repr_desc ty = (Ctype.repr ty).desc

let create_ident n = Ident.create_local & "__" ^ n

let contract_self_id = create_ident "self"

let noloc = Location.none

module Cnstr = struct
  type t =
    | Unit | Left | Right | Some | None | Cons | Nil | Bool of bool | Pair
    | Constant of C.t
    (* XXX [Constant of C.t] cannot contain values of Unit, Left, Right...
       listed in the first line.  This is confusing. *)

  let to_string = function
    | Pair -> "(,)"
    | Left -> "Left"
    | Right -> "Right"
    | Some -> "Some"
    | None -> "None"
    | Cons -> "(::)"
    | Nil -> "[]"
    | Unit -> "()"
    | Bool true -> "true"
    | Bool false -> "false"
    | Constant c -> Format.sprintf "%a" C.pp c
end

module P = struct
  type desc =
    | Var of Ident.t
    | Constr of Cnstr.t * t list
    | Wild
    | Alias of t * Ident.t * Location.t (* location of ident *)
    | Or of t * t

  and t = (desc, unit) with_loc_and_type

  let rec pp ppf pat =
    let open Format in
    match pat.desc with
    | Var i -> fprintf ppf "%s" (Ident.unique_name i)
    | Constr (c, []) -> fprintf ppf "%s" (Cnstr.to_string c)
    | Constr (c, ps) -> fprintf ppf "@[%s (%a)@]" (Cnstr.to_string c) (Format.list ",@ " pp) ps
    | Wild -> string ppf "_"
    | Alias (p, id, _) -> fprintf ppf "(@[%a as %s@])" pp p (Ident.unique_name id)
    | Or (p1,p2) -> fprintf ppf "(@[%a@ | %a@])" pp p1 pp p2

  let rec vars p =
    let open IdTys in
    match p.desc with
    | Var id -> singleton (id, p.typ)
    | Constr (_, []) -> empty
    | Constr (_, p::ps) -> List.fold_left union (vars p) (List.map vars ps)
    | Wild -> empty
    | Alias (p, id, _) -> add (id, p.typ) & vars p
    | Or (p1, p2) -> union (vars p1) (vars p2)
end

type lenv =
  { local_variables : Ident.t list (* Vars defined in the current innermost function scope *)
  ; non_local_variables : Ident.t list (* Vars defined out of the current innermost function scope *)
  ; fun_loc : Location.t (* Location of the current innermost function scope *)
  ; fun_level : int (* Level of the current innermost funciton scope.
                       Less than 0 means it is at one of entry point function abstractions.*)
  }

module Lenv = struct
  let add_locals vs lenv = { lenv with local_variables = vs @ lenv.local_variables }
  let into_fun ~loc lenv =
    { local_variables= []; non_local_variables= lenv.local_variables @ lenv.non_local_variables; fun_loc=loc; fun_level= lenv.fun_level + 1 }
  let _pp ppf lenv =
    Format.fprintf ppf "local= %s@."
      (String.concat ", " (List.map (fun id -> Ident.unique_name id) lenv.local_variables));
    Format.fprintf ppf "non_local= %s@."
      (String.concat ", " (List.map (fun id -> Ident.unique_name id) lenv.non_local_variables));
end


let mke ~loc typ desc = { typ; desc; loc; attrs= [] }

let mkfst ~loc e =
  let ty = match e.typ.desc with
    | TyPair (_, ty, _, _) -> ty
    | _ -> assert false
  in
  mke ~loc ty (Prim ("fst", tyLambda (e.typ, ty), [e]))

let mksnd ~loc e =
  let ty = match e.typ.desc with
    | TyPair (_, _, _, ty) -> ty
    | _ -> assert false
  in
  mke ~loc ty (Prim ("snd", tyLambda (e.typ, ty), [e]))

let mkleft ~loc ty e = mke ~loc (tyOr (None, e.typ, None, ty)) (Left e)
let mkright ~loc ty e = mke ~loc (tyOr (None, ty, None, e.typ)) (Right e)

let mkeq ~loc e1 e2 =
  mke ~loc tyBool (Prim ("=", tyLambda (e1.typ, tyLambda (e2.typ, tyBool)), [e1; e2]))

let mkpair ~loc e1 e2 = mke ~loc (tyPair (None, e1.typ, None, e2.typ)) (Pair (e1, e2))

let mkint ~loc n = mke ~loc tyInt (Const (M.Constant.Int (Z.of_int n)))

let mkfun ~loc pvar e = mke ~loc (tyLambda (pvar.typ, e.typ)) & Fun (pvar, e)
let mkcons ~loc h t = mke ~loc t.typ (Cons (h, t))
let mksome ~loc t = mke ~loc (tyOption (None, t.typ)) (IML_Some t)
let mkunit ~loc () = mke ~loc tyUnit Unit
let mkassert ~loc t = mke ~loc tyUnit & Assert t
let mkassertfalse ~loc ty = mke ~loc ty & AssertFalse
let mklet ~loc p t1 t2 = mke ~loc t2.typ & Let (p, t1, t2)
let mkvar ~loc (id, typ) = mke ~loc typ & Var id

let mkp ~loc typ desc =  { loc; desc; typ; attrs= () }
let mkppair ~loc p1 p2 = mkp ~loc (tyPair (None, p1.typ, None, p2.typ)) (P.Constr (Cnstr.Pair, [p1; p2]))
let mkpint ~loc n = mkp ~loc tyInt (P.Constr (Cnstr.Constant (Michelson.Constant.Int (Z.of_int n)), []))
let mkpleft ~loc ty p = mkp ~loc (tyOr (None, p.typ, None, ty)) & P.Constr (Cnstr.Left, [p])
let mkpright ~loc ty p = mkp ~loc (tyOr (None, ty, None, p.typ)) & P.Constr (Cnstr.Right, [p])

type type_expr_error =
  | Type_variable of Types.type_expr
  | Unsupported_type of Types.type_expr
  | Unsupported_data_type of Path.t
  | Invalid_michelson of M.Type.t * string
  | Exception_out_of_raise
  | GADT of Path.t

let pp_type_expr_error ppf = function
  | Type_variable ty ->
      Format.fprintf ppf "Type variable %a is not supported in SCaml." Printtyp.type_expr ty
  | Unsupported_type ty ->
      Format.fprintf ppf "Type %a is not supported in SCaml." Printtyp.type_expr ty
  | Unsupported_data_type p ->
      Format.fprintf ppf "Data type %s is not supported in SCaml." (Path.name p)
  | Invalid_michelson (mty, s) ->
      Format.fprintf ppf "Michelson type %a is invalid: %s" M.Type.pp mty s
  | Exception_out_of_raise ->
      Format.fprintf ppf "Values of type exn are only allowed to raise."
  | GADT p ->
      Format.fprintf ppf "Data type %s is GADT, not supported in SCaml." (Path.name p)


let encode_by branch xs =
  Binplace.fold
      ~leaf:(fun x -> x)
      ~branch
  & Binplace.place xs

let unify tyenv ty1 ty2 =
  let tr = Printexc.get_backtrace () in
  try Ctype.unify tyenv ty1 ty2 with e ->
    prerr_endline tr; raise e

let rec type_expr tyenv ty =
  let rec fn tyenv ty =
    let ty = Ctype.expand_head tyenv ty in
    match ty.desc with
    | Tvar _ when ty.level = Btype.generic_level -> Error (Type_variable ty)
    | Tvar _ ->
        (* Non generalized type variable.  We are brave enough to unify it with Unit *)
        unify tyenv ty Predef.type_unit; (* must succeed *)
        fn tyenv ty
    | Tarrow (Nolabel, f, t, _) ->
        fn tyenv f >>= fun f ->
        fn tyenv t >>= fun t ->
        Ok (tyLambda (f, t))
    | Ttuple [t1; t2] ->
        fn tyenv t1 >>= fun t1 ->
        fn tyenv t2 >>= fun t2 -> Ok (tyPair (None, t1, None, t2))
    | Ttuple tys ->
        Result.mapM (fn tyenv) tys
        >>| encode_by (fun ty1 ty2 -> tyPair (None, ty1, None, ty2))
    | Tconstr (p, [], _) when p = Predef.path_bool -> Ok (tyBool)
    | Tconstr (p, [t], _) when p = Predef.path_list ->
        fn tyenv t >>= fun t -> Ok (tyList t)
    | Tconstr (p, [t], _) when p = Predef.path_option ->
        fn tyenv t >>= fun t -> Ok (tyOption (None, t))
    | Tconstr (p, [], _) when p = Predef.path_unit -> Ok (tyUnit)
    | Tconstr (p, [], _) when p = Predef.path_string -> Ok (tyString)
    (* | Tconstr (p, [], _) when p = Predef.path_bytes -> Ok (tyBytes) *)
    | Tconstr (p, tys, _) ->
        let rec f res = function
          | [] -> Ok (List.rev res)
          | ty::tys ->
              fn tyenv ty >>= fun ty ->
              f (ty::res) tys
        in
        f [] tys >>= fun tys ->
        begin match Path.is_scaml p, tys with
          | Some "sum", [t1; t2] -> 
              Ok (type_annotate (fun _ -> Some "sum") & tyOr (None, t1, None, t2))
          | Some "int", [] -> Ok (tyInt)
          | Some "nat", [] -> Ok (tyNat)
          | Some "tz",  [] -> Ok (tyMutez)
          | Some "set"         , [ty]       -> Ok (tySet ty)
          | Some "map"         , [ty1; ty2] -> Ok (tyMap (ty1, ty2))
          | Some "big_map"     , [ty1; ty2] -> Ok (tyBigMap (ty1, ty2))
          | Some "operation"   , []         -> Ok (tyOperation)
          | Some "contract"    , [ty]       -> Ok (tyContract ty)
          | Some "timestamp"   , []         -> Ok (tyTimestamp)
          | Some "address"     , []         -> Ok (tyAddress)
          | Some "key"         , []         -> Ok (tyKey)
          | Some "signature"   , []         -> Ok (tySignature)
          | Some "key_hash"    , []         -> Ok (tyKeyHash)
          | Some "bytes"       , []         -> Ok (tyBytes)
          | Some "chain_id"    , []         -> Ok (tyChainID)
          | Some _, _ -> Error (Unsupported_data_type p)
          | None, _ ->
              match Env.find_type_descrs p tyenv with
              | [], [] when p = Predef.path_exn -> Error Exception_out_of_raise
              | [], [] -> Error (Unsupported_data_type p) (* abstract XXX *)
              | [], labels -> record_type tyenv ty p labels
              | constrs, [] -> variant_type tyenv ty p constrs >>| fun (_, _, ty) -> ty
              | _ -> assert false (* impossible *)
              | exception _ -> Error (Unsupported_data_type p)
        end
    | Tpoly (ty, []) -> fn tyenv ty
    | _ -> Error (Unsupported_type ty)
  in
  fn tyenv ty >>= fun mty ->
  (* double check type invariants in Michelson *)
  match M.Type.validate mty with
  | Ok () -> Ok mty
  | Error (mty', mes) -> Error (Invalid_michelson (mty', mes))

and record_type tyenv ty p labels =
  (* record *)
  let ltys = List.map (fun label ->
      let _, ty_arg, ty_res =
        Ctype.instance_label false (* XXX I do not know what it is *)
          label
      in
      unify tyenv ty ty_res; (* XXX should succeed *)
      (label.lbl_name, ty_arg)) labels
  in
  Result.mapM (fun (l,ty) ->
      type_expr tyenv ty >>| fun ty -> Some l, ty) ltys
  >>| encode_by (fun (f1,ty1) (f2,ty2) -> None,tyPair (f1,ty1, f2,ty2))
  >>| fun (_,ty) -> M.Type.type_annotate (fun _ -> Some (Path.name p)) (* XXX p=P(Q) fails *) ty

and variant_type tyenv ty p constrs =
  if List.exists (fun c -> c.Types.cstr_generalized) constrs then
    Error (GADT p)
  else
  let consts, non_consts = Variant.variant_type tyenv ty constrs in
  let non_consts =
    Result.mapM (fun (n,tys) ->
        Result.mapM (type_expr tyenv) tys >>| fun tys -> (n,tys)) non_consts
    >>| fun ctys_list ->
    List.map (fun (n,tys) ->
        (n, encode_by (fun ty1 ty2 -> tyPair (None,ty1, None,ty2)) tys))
      ctys_list
  in
  non_consts >>| fun non_consts ->
  ( consts,
    non_consts,
    M.Type.type_annotate (fun _ -> Some (Path.name p)) (* XXX p=P(Q) fails *)
    & snd & encode_by (fun (f1,ty1) (f2,ty2) -> (None, tyOr (f1, ty1, f2, ty2)))
    & List.map (fun (f,ty) -> Some f, ty)
      ((match consts with
          | None -> []
          | Some names -> [String.concat "_" names, tyInt])
       @ non_consts) 
  )

(* Literals *)

let parse_timestamp s = match Ptime.of_rfc3339 s with
  | Ok (t, _, _) ->
      let t' = Ptime.truncate ~frac_s:0 t in
      if not (Ptime.equal t t') then Error "Subsecond is not allowed in timestamps"
      else
        let posix = Ptime.to_float_s t in
        if posix < 0. then Error "Timestamp before Epoch is not allowed"
        else Ok (C.Timestamp (Z.of_float posix))
  | Error (`RFC3339 (_, e)) ->
      Error (Format.sprintf "%a" Ptime.pp_rfc3339_error e)

let parse_bytes s =
  try
    ignore & Hex.to_string (`Hex s); Ok (C.Bytes s)
  with
  | _ -> Error "Bytes must take hex representation of bytes"

let constructions_by_string =
  [ ("signature" , ("signature", "Signature", tySignature,
                    fun x -> Ok (C.String x)));
    ("key_hash"  , ("key_hash", "Key_hash", tyKeyHash,
                    fun x -> Ok (C.String x)));
    ("key"       , ("key", "Key", tyKey,
                    fun x -> Ok (C.String x)));
    ("address"   , ("address", "Address", tyAddress,
                    fun x -> Ok (C.String x)));
    ("timestamp" , ("timestamp", "Timestamp", tyTimestamp,
                    parse_timestamp));
    ("bytes"     , ("bytes", "Bytes", tyBytes,
                    parse_bytes));
    ("chain_id"  , ("chain_id", "Chain_id", tyChainID,
                    fun x -> Ok (C.String x)))
  ]

let pattern_simple { pat_desc; pat_loc=loc; pat_type= mltyp; pat_env= tyenv } =
  let typ =
    Result.at_Error (fun e ->
        errorf_type_expr ~loc "This pattern has type %a.  %a"
          Printtyp.type_expr mltyp pp_type_expr_error e)
    & type_expr tyenv mltyp
  in
  let mk loc id typ = { loc; desc=id; typ; attrs= () } in
  let mk_dummy loc typ = mk loc Ident.dummy typ in
  match pat_desc with
  | Tpat_var (id, {loc}) -> [mk loc id typ]
  | Tpat_alias ({ pat_desc = Tpat_any; pat_loc=_ }, id, _) ->
      (* We transform (_ as x) to x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      [mk loc id typ]
  | Tpat_any         -> [mk_dummy loc typ]
  | Tpat_construct ({loc}, _, []) when typ.desc = TyUnit -> [mk_dummy loc typ]
  | Tpat_construct _ -> unsupported ~loc "variant pattern"
  | Tpat_alias _     -> unsupported ~loc "alias pattern"
  | Tpat_constant _  -> unsupported ~loc "constant pattern"
  | Tpat_tuple _     -> unsupported ~loc "tuple pattern"
  | Tpat_variant _   -> unsupported ~loc "polymorphic variant pattern"
  | Tpat_record _    -> unsupported ~loc "record pattern"
  | Tpat_array _     -> unsupported ~loc "array pattern"
  | Tpat_or _        -> unsupported ~loc "or pattern"
  | Tpat_lazy _      -> unsupported ~loc "lazy pattern"
  | Tpat_exception _ -> unsupported ~loc "exception pattern"

let rec pattern { pat_desc; pat_loc=loc; pat_type= mltyp; pat_env= tyenv } =
  let gloc = Location.ghost loc in
  let typ =
    Result.at_Error (fun e -> errorf_type_expr ~loc
                        "This pattern has type %a.  %a"
                        Printtyp.type_expr mltyp pp_type_expr_error e)
    & type_expr tyenv mltyp
  in
  let mk desc = { loc; desc; typ; attrs= () } in
  match pat_desc with
  | Tpat_array _     -> unsupported ~loc "array pattern"
  | Tpat_lazy _      -> unsupported ~loc "lazy pattern"
  | Tpat_variant _   -> unsupported ~loc "polymorphic variant pattern"
  | Tpat_exception _ -> unsupported ~loc "exception pattern"

  | Tpat_var (id, _) -> mk (P.Var id)

  | Tpat_alias ({pat_desc = Tpat_any; pat_loc=_}, id, _) ->
      (* We transform (_ as x) in x if _ and x have the same location.
         The compiler transforms (x:t) into (_ as x : t).
         This avoids transforming a warning 27 into a 26.
       *)
      mk (P.Var id)

  | Tpat_any         -> mk P.Wild

  | Tpat_alias (p, id, {loc}) -> mk (P.Alias (pattern p, id, loc))

  | Tpat_tuple [p1; p2] -> mk (P.Constr (Cnstr.Pair, [pattern p1; pattern p2]))

  | Tpat_tuple ps -> encode_by (mkppair ~loc:gloc) & List.map pattern ps

  | Tpat_constant (Const_string (s, None)) ->
      mk (P.Constr (Cnstr.Constant (C.String s), []))

  | Tpat_constant (Const_string (s, Some _)) ->
      (* quoted string *)
      mk (P.Constr (Cnstr.Constant (C.String s), []))

  | Tpat_constant _ ->
      (* Since the numbers are variant wrapped, i.e. Int 32 *)
      unsupported ~loc "constant pattern of type %s"
        (Format.sprintf "%a" Printtyp.type_scheme mltyp)

  | Tpat_or (p1, p2, None)   -> mk & P.Or (pattern p1, pattern p2)

  | Tpat_or (_, _, _)        -> unsupported ~loc "or pattern with row"

  | Tpat_construct (_, cdesc, ps) ->

      (* XXX should check the module path *)
      begin match cdesc.cstr_name, typ.desc, ps with
        | "()", TyUnit, [] -> mk (P.Constr (Cnstr.Unit, []))
        | "Left", TyOr _, [p] -> mk (P.Constr (Cnstr.Left, [pattern p]))
        | "Right", TyOr _, [p] -> mk (P.Constr (Cnstr.Right, [pattern p]))
        | "Some", TyOption _, [p] -> mk (P.Constr (Cnstr.Some, [pattern p]))
        | "None", TyOption _, [] -> mk (P.Constr (Cnstr.None, []))
        | "::", TyList _, [p1; p2] -> mk (P.Constr (Cnstr.Cons, [pattern p1; pattern p2]))
        | "[]", TyList _, [] -> mk (P.Constr (Cnstr.Nil, []))
        | "true", TyBool, [] -> mk (P.Constr (Cnstr.Bool true, []))
        | "false", TyBool, [] -> mk (P.Constr (Cnstr.Bool false, []))
        | "Int", TyInt, [{pat_desc= Tpat_constant (Const_int n)}] ->
            mk & P.Constr (Cnstr.Constant (C.Int (Z.of_int n)), [])
        | "Int", TyInt, [_] ->
            errorf_constant ~loc "Int can only take an integer constant"
        | "Nat", TyNat, [{pat_desc= Tpat_constant (Const_int n)}] ->
            if n < 0 then
              errorf_constant ~loc "Nat can only take a positive integer constant";
            mk & P.Constr (Cnstr.Constant (C.Int (Z.of_int n)), [])
        | "Nat", TyNat, [_] ->
            errorf_constant ~loc "Nat can only take an integer constant"
        | _, TyMutez, [_] -> errorf_constant ~loc "tz constant cannot be used as a pattern"
        | "Key_hash", TyKeyHash, [{pat_desc= Tpat_constant (Const_string (s, _))}] ->
            mk & P.Constr (Cnstr.Constant (C.String s), [])
        | "Key_hash", TyKeyHash, [_] -> unsupported ~loc "Key_hash can only take a string constant"

        | "Address", TyAddress, [{pat_desc= Tpat_constant (Const_string (s, _))}] ->
            mk & P.Constr (Cnstr.Constant (C.String s), [])
        | "Address", TyAddress, [_] ->
            errorf_constant ~loc "Address can only take a string constant"

        | "Timestamp", TyTimestamp, [{pat_desc= Tpat_constant (Const_string (s, _))}] ->
            begin match parse_timestamp s with
              | Error _e -> errorf_constant ~loc "strange arguments for Timestamp"
              | Ok t -> mk & P.Constr (Cnstr.Constant t, [])
            end
        | "Timestamp", TyTimestamp, [_] -> unsupported ~loc "Timestamp can only take a string constant"

        | "Bytes", TyBytes, [{pat_desc= Tpat_constant (Const_string (s, _))}] ->
            begin match parse_bytes s with
              | Error _e -> errorf_constant ~loc "strange arguments for Bytes"
              | Ok t -> mk & P.Constr (Cnstr.Constant t, [])
            end
        | "Bytes", TyBytes, [_] ->
            errorf_constant ~loc "Bytes can only take a string constant"

        | _, _, _ ->

            begin match repr_desc mltyp with
              | Tconstr (p, _, _) ->
                  begin match Env.find_type_descrs p tyenv with
                    | [], [] when p = Predef.path_exn ->
                        errorf_type_expr ~loc
                          "%a" pp_type_expr_error Exception_out_of_raise
                    | [], [] ->
                        errorf_type_expr ~loc
                          "Abstract data type %s is not supported in SCaml"
                          (Path.name p)
                    | [], _::_ -> assert false (* record cannot come here *)
                    | _::_, _::_ -> assert false
                    | constrs, [] ->
                        let consts_ty, non_consts_ty, entire_ty =
                          from_Ok & variant_type tyenv mltyp p constrs
                        in
                        let rec find_constr i = function
                          | [] -> assert false
                          | n::_ when n = cdesc.cstr_name -> i
                          | _::names -> find_constr (i+1) names
                        in
                        let rec embed ty sides arg = match ty.M.Type.desc, sides with
                          | _, [] -> arg
                          | TyOr (_,ty1, _,ty2), Binplace.Left::sides ->
                              mkpleft ~loc:gloc ty2 (embed ty1 sides arg)
                          | TyOr (_,ty1, _,ty2), Right::sides ->
                              mkpright ~loc:gloc ty1 (embed ty2 sides arg)
                          | _ -> assert false
                        in
                        match cdesc.cstr_arity, consts_ty, non_consts_ty with
                        | _, None, [] -> assert false
                        | 0, None, _ -> assert false
                        | 0, Some names, [] ->
                            mkpint ~loc:gloc & find_constr 0 names
                        | 0, Some names, xs ->
                            let sides = Binplace.path 0 (List.length xs + 1) in
                            embed entire_ty sides & mkpint ~loc:gloc & find_constr 0 names
                        | _, _, [] -> assert false
                        | _, _, cs ->
                            let names = List.map fst cs in
                            let i = find_constr 0 names in
                            let sides =
                              let shift = if consts_ty = None then 0 else 1 in
                              Binplace.path (i + shift) (List.length names + shift)
                            in
                            embed entire_ty sides
                            & encode_by (mkppair ~loc:gloc) & List.map pattern ps
                  end
            | _ ->  unsupported ~loc "pattern %s" cdesc.cstr_name
            end
      end

  | Tpat_record (pfields, _) ->
      (* fields are sorted, but omitted labels are not in it *)
      match repr_desc mltyp with
      | Tconstr (p, _, _) ->
          begin match Env.find_type_descrs p tyenv with
            | [], labels ->
                let labels =
                  List.map (fun label ->
                      let _, arg, res =
                        Ctype.instance_label false (* XXX ? *) label
                      in
                      unify tyenv res mltyp;
                      label.lbl_name, arg
                    ) labels
                in
                let rec f labels pfields = match labels, pfields with
                  | (n, _)::labels, (_, plabel, p)::pfields when n = plabel.Types.lbl_name ->
                      pattern p :: f labels pfields
                  | (n, typ)::labels, _ ->
                      let typ =
                        Result.at_Error (fun e ->
                            errorf_type_expr ~loc "This pattern has a field %s with type %a.  %a"
                              n
                              Printtyp.type_expr typ
                              pp_type_expr_error e)
                        & type_expr tyenv typ
                      in
                      { loc; desc= P.Wild; typ; attrs= () } :: f labels pfields
                  | [], [] -> []
                  | [], _ -> assert false
                in
                encode_by (mkppair ~loc:gloc) & f labels pfields

            | _, _ -> assert false
          end
      | _ -> assert false

let attr_has_entry_point =
  let open Parsetree in
  List.find_map_opt (function
      | { attr_name= { txt = "entry"; loc }; attr_payload= payload; _ } ->
          begin match Attribute.parse_options_in_payload "entry" ~loc payload with
            | _::_::_ -> errorf_entry ~loc "@entry cannot specify more than one options"
            | [] -> Some (loc, None)
            | [{txt=Longident.Lident "name"}, `Constant (Pconst_string (s, _))] ->
                Some (loc, Some s)
            | [{txt=Longident.Lident "name"}, _] -> errorf_entry ~loc "@entry can take only a string literal"
            | [_, _] -> errorf_entry ~loc "@entry can take at most one name=<name> binding"
          end
      | _ -> None)

module Pmatch = struct
  module Type = Michelson.Type

  (* rows <-> columns *)
  let transpose : 'p list list -> 'p list list = fun rows ->
    if rows = [] then []
    else
      let ncolumns = List.length & List.hd rows in
      List.init ncolumns (fun i ->
          List.map (fun row ->
              assert (List.length row = ncolumns);
              List.nth row i) rows)

  type id_ty = Ident.t * Type.t

  type case =
    { pats : P.t list
    ; guard : int option
    ; action : int
    ; bindings : (Ident.t * Location.t * id_ty) list
    }

  type matrix = case list

  type tree =
    | Fail
    | Leaf of (Ident.t * Location.t * id_ty) list * int
    | Switch of id_ty * (Cnstr.t * id_ty list * tree) list * tree option (* default *)
    | Guard of (Ident.t * Location.t * id_ty) list (* binder *)
               * int (* guard *)
               * int (* case *)
               * tree (* otherwise *)

  let rec pp_tree ppf =
    let f fmt = Format.fprintf ppf fmt in
    function
    | Fail -> f "Fail"
    | Leaf (binders, n) ->
        f "Leaf %a %d"
          (Format.list ",@," (fun ppf (v,_,(v',_)) ->
               Format.fprintf ppf "%s=%s"
                 (Ident.unique_name v)
                 (Ident.unique_name v'))) binders
          n
    | Switch (v, xs, None) ->
        f "@[<2>Switch %s@ [ @[%a@] ]@]"
          (Ident.unique_name & fst v)
          (Format.list ";@ " (fun ppf ->
               let f fmt = Format.fprintf ppf fmt in
               let pvs _ppf vs =
                 f "%s" & String.concat "," & List.map (fun (x,_) -> Ident.unique_name x) vs
               in
               fun (c, vs, t) ->
                 f "%s %a (%a)" (Cnstr.to_string c) pvs vs pp_tree t
             )) xs
    | Switch (v, xs, Some d) ->
        f "@[<2>Switch %s@ [ @[%a@] default %a]@]"
          (Ident.unique_name & fst v)
          (Format.list ";@ " (fun ppf ->
               let f fmt = Format.fprintf ppf fmt in
               let pvs _ppf vs =
                 f "%s" & String.concat "," & List.map (fun (x,_) -> Ident.unique_name x) vs
               in
               fun (c, vs, t) ->
                 f "%s %a (%a)" (Cnstr.to_string c) pvs vs pp_tree t
             )) xs
          pp_tree d
    | Guard (binders, guard, case, otherwise) ->
        f "@[<2>Guard (%a) guard%d case%d [%a]@]"
          (Format.list ",@," (fun ppf (v,_,(v',_)) ->
               Format.fprintf ppf "%s=%s"
                 (Ident.unique_name v)
                 (Ident.unique_name v'))) binders
          guard
          case
          pp_tree otherwise

  (* specialize on Left and Right *)
  let rec specialize o c (matrix : matrix) : matrix =
    List.fold_right (fun ({ pats; bindings } as case) st ->
        match pats with
        | [] -> assert false
        | pat::pats ->
            let rec f pat =
              let loc = pat.loc in
              let gloc = Location.ghost loc in
              match c, pat.desc with
              | _, P.Alias (pat, i, loc) ->
                  let cases = f pat in
                  List.map (fun case -> { case with bindings = (i,loc,o) :: case.bindings }) cases
              | c, P.Or (p1, p2) ->
                  specialize o c [{ case with pats= p1::pats }]
                  @ specialize o c [{ case with pats= p2::pats }]
              | c, P.Constr (c', ps) ->
                  if c = c' then [{ case with pats= ps @ pats }] else []

              (* For wild, we need to build another wild with arg type.
                 XXX Currently we must code for each.  Ugh.
              *)
              | Cnstr.Pair, P.Wild ->
                  let ty1, ty2 = match pat.typ.desc with
                    | TyPair (_,ty1, _,ty2) -> ty1, ty2
                    | _ -> assert false
                  in
                  [{ case with pats= mkp ~loc:gloc ty1 P.Wild :: mkp ~loc:gloc ty2 P.Wild :: pats }]

              | Left, P.Wild ->
                  let typl = match pat.typ.desc with
                    | TyOr (_,typl, _,_typr) -> typl
                    | _ -> assert false
                  in
                  [{ case with pats= mkp ~loc:gloc typl P.Wild :: pats }]

              | Right, P.Wild ->
                  let typr = match pat.typ.desc with
                    | TyOr (_,_typl, _,typr) -> typr
                    | _ -> assert false
                  in
                  [{ case with pats= mkp ~loc:gloc typr P.Wild :: pats }]

              | Some, P.Wild ->
                  let typ = match pat.typ.desc with
                    | TyOption (_,typ) -> typ
                    | _ -> assert false
                  in
                  [{ case with pats= mkp ~loc:gloc typ P.Wild :: pats }]

              | Cons, P.Wild ->
                  let typ = match pat.typ.desc with
                    | TyList typ -> typ
                    | _ -> assert false
                  in
                  [{ case with pats= mkp ~loc:gloc typ P.Wild :: mkp ~loc:gloc pat.typ P.Wild :: pats }]

              | (None | Nil | Unit | Bool _ | Constant _), P.Wild ->
                  [{ case with pats }]

              | _ , P.Var v -> [{ case with pats; bindings= (v,loc,o) :: bindings }]
            in
            let cases = f pat in
            cases @ st
      ) matrix []

  let pp_matrix ppf matrix =
    let open Format in
    fprintf ppf "matrix:@.";
    List.iter (function
        | { pats; guard= None; action= i } ->
            fprintf ppf "| %a -> %d@."
              (list ", " P.pp) pats
              i
        | { pats; guard= Some g; action= i } ->
            fprintf ppf "| %a when %d -> %d@."
              (list ", " P.pp) pats
              g
              i
      ) matrix

  let pp_osmatrix ppf (os, matrix) =
    let open Format in
    fprintf ppf "match %a with@ " (list ", " (fun ppf (id,_) ->
        fprintf ppf "%s" & Ident.unique_name id)) os;
    List.iter (function
        | { pats; guard= None; action= i } ->
            fprintf ppf "| %a -> %d@ "
               (list ", " P.pp) pats
               i
        | { pats; guard= Some g; action= i } ->
            fprintf ppf "| %a when %d -> %d@ "
              (list ", " P.pp) pats
              g
              i
      ) matrix


  let specialize o c matrix =
    if_debug (fun () -> Format.eprintf "specializing... %a@." pp_matrix matrix);
    let matrix = specialize o c matrix in
    if_debug (fun () -> Format.eprintf "specialized... %a@." pp_matrix matrix);
    matrix

  let rec default o (matrix : matrix) : matrix =
    List.fold_right (fun ({ pats } as case) st ->
        match pats with
        | [] -> assert false
        | pat::pats ->
            let rec f pat = match pat.desc with
              | P.Constr (_, _) -> st
              | P.Wild -> { case with pats } :: st
              | P.Var v -> { case with pats ; bindings= (v,pat.loc,o) :: case.bindings } :: st
              | P.Alias (pat, _id, _loc) -> f pat
              | P.Or (p1, p2) ->
                  default o [{ case with pats= p1::pats }]
                  @ default o [{ case with pats= p2::pats}]
                  @ st
            in
            f pat
      ) matrix []

  let swap i os (matrix : matrix) : _ * matrix =
    let rec f rev_st i xs = match i, xs with
      | 0, x::xs -> x::List.rev rev_st@xs
      | _, [] -> assert false
      | i, x::xs -> f (x::rev_st) (i-1) xs
    in
    f [] i os,
    List.map (fun ({ pats } as case) -> { case with pats= f [] i pats }) matrix

  let rec cc os matrix =
    if_debug (fun () -> Format.eprintf "@[<v2>Pmatch.cc:@ @[%a@]@]@." pp_osmatrix (os, matrix));
    match matrix with
    | [] -> Fail
    | { pats=ps; guard= g; action= a; bindings }::_ ->
        if List.for_all (fun p ->
            let rec f p = match p.desc with
              | P.Alias (p, _, _) -> f p
              | P.Wild | P.Var _ -> true
              | P.Constr _ -> false
              | P.Or _ -> false
            in
            f p ) ps
        then
          let bindings = List.fold_right2 (fun v p st ->
              match p.desc with
              | P.Wild -> st
              | P.Var v' -> (v',p.loc,v)::st
              | P.Alias _ | P.Constr _ | P.Or _ -> assert false) os ps bindings
          in
          match g with
          | None -> Leaf (bindings, a)
          | Some g ->
              if_debug (fun () -> prerr_endline "guard");
              Guard (bindings, g, a, cc os & List.tl matrix)
        else
          (* find column i where at least one pattern which is not a wildcard *)
          let columns = transpose & List.map (fun case -> case.pats) matrix in
          let icolumns = List.mapi (fun i c -> (i,c)) columns in
          let i, column =
            match
              List.find_all (fun (_i,c) ->
                  List.exists (fun p ->
                      let rec f p = match p.desc with
                        | P.Alias (p, _, _) -> f p
                        | P.Wild | P.Var _ -> false
                        | P.Constr _ -> true
                        | P.Or (p1, p2) -> f p1 || f p2
                      in
                      f p
                    ) c) icolumns
            with
            | [] -> assert false
            | (i,c)::_ -> i,c (* blindly select the first *)
          in
          (* algo 3 (a) *)
          let algo os column =
            let constructors =
              List.sort_uniq compare
              & List.fold_left (fun st p ->
                  let rec f p = match p.desc with
                    | P.Alias (p, _, _) -> f p
                    | P.Constr (c, _) -> [c]
                    | P.Wild | P.Var _ -> []
                    | P.Or (p1, p2) -> f p1 @ f p2
                  in
                  f p @ st
                ) [] column
            in
            (* Mending constructors for sum, option, list and bools.
               Since non-exhaustive matches are already rejected, they are always listed fully.
            *)
            let constructors =
              (* Things must be sorted! *)
              match constructors with
              | [] -> assert false
              | [Cnstr.Left] | [Right] -> [Cnstr.Left; Right]
              | [Some] | [None] -> [Some; None]
              | [Cons] | [Nil] -> [Cons; Nil]
              | [(Bool _)] -> [(Bool false) ; (Bool true)]
              | _ -> constructors
            in
            (* if it is a signature, no default case is required *)
            let is_signature =
              (* Things must be sorted! *)
              match constructors with
              | [Left; Right]
              | [Some; None]
              | [Cons; Nil]
              | [Pair]
              | [Unit]
              | [(Bool false) ; (Bool true)] -> true
              | _ ->
                  (* Others including integers.
                     XXX If it is an integer for nullary constructors,
                     no default case is required *)
                  false
            in
            (* XXX weak. this depends on the above code *)

            let ivty = List.hd os in
            Switch (ivty,
                    (let _, vty = ivty in
                    List.map (fun c ->
                        let os = List.tl os in
                        let vs =
                          match c with
                          | Cnstr.Left ->
                              let ty = match vty.desc with
                                | TyOr (_,ty, _,_) -> ty
                                | _ -> assert false
                              in
                              [ create_ident "l", ty ]
                          | Right ->
                              let ty = match vty.desc with
                                | TyOr (_,_, _,ty) -> ty
                                | _ -> assert false
                              in
                              [ create_ident "r", ty ]
                          | Pair ->
                              let ty1,ty2 = match vty.desc with
                                | TyPair (_,ty1, _,ty2) -> ty1, ty2
                                | _ -> assert false
                              in
                              [ create_ident "l", ty1 ;
                                create_ident "r", ty2 ]
                          | Cons ->
                              let ty = match vty.desc with
                                | TyList ty -> ty
                                | _ -> assert false
                              in
                              [ create_ident "hd", ty
                              ; create_ident "tl", vty
                              ]
                          | Some ->
                              let ty = match vty.desc with
                                | TyOption (_,ty) -> ty
                                | _ -> assert false
                              in
                              [ create_ident "x", ty ]

                          | Nil | None | Bool _ | Constant _ (* int/nat/tz *)
                          | Unit -> []

                        in
                        c,
                        vs,
                        (if_debug (fun () -> Format.eprintf "specialize on %s@." (Cnstr.to_string c));
                        cc (vs @ os) (specialize ivty c matrix))
                       ) constructors),

                    if is_signature then None
                    else Some (if_debug (fun () -> Format.eprintf "default@."); cc (List.tl os) (default ivty matrix))
                   )
            in
            if i = 0 then algo os column
            else begin
              let o', matrix' = swap i os matrix in
              cc o' matrix' (* xxx inefficient *)
            end

  let build aty acts guards t =
    let warn_unused_default = function
      | None -> ()
      | Some _ -> Format.eprintf "Warning: meaningless default case in a pattern match found.  This is likely a compiler bug.@."
    in
    let rec f = function
      | Fail ->
          (* Nullary constructor is converted to integer.
             We need the default case for them. *)
          mkassertfalse ~loc:noloc aty
      | Leaf (binders, i) ->
          List.fold_right (fun (v,loc,(v',ty)) st ->
              mklet ~loc (mkp ~loc ty v) (mkvar ~loc (v',ty)) st)
            binders (List.nth acts i)
      | Guard (binders, guard, case, otherwise) ->
          let guarde = List.nth guards guard in
          List.fold_right (fun (v,loc,(v',ty)) st ->
              mklet ~loc (mkp ~loc ty v) (mkvar ~loc (v',ty)) st)
            binders
          & mke ~loc:guarde.loc aty & IfThenElse (guarde,
                                                  List.nth acts case,
                                                  Some (f otherwise))
      | Switch (_, [], _) -> assert false
      | Switch (v, [Pair, [v1,ty1; v2,ty2], t], d) ->
          warn_unused_default d;
          let t = f t in
          (* let v1 = fst v in let v2 = snd v in <t> *)
          mklet ~loc:noloc (mkp ~loc:noloc ty1 v1) (mkfst ~loc:noloc & mkvar ~loc:noloc v)
          & mklet ~loc:noloc (mkp ~loc:noloc ty2 v2) (mksnd ~loc:noloc & mkvar ~loc:noloc v) t
      | Switch (_, [Unit, [], t], d) ->
          warn_unused_default d;
          f t
      | Switch (v, ( [ Left,  [vl,tyl], tl
                     ; Right, [vr,tyr], tr ]
                   | [ Right, [vr,tyr], tr
                     ; Left,  [vl,tyl], tl ] ), d) ->
          warn_unused_default d;
          let tl = f tl in
          let tr = f tr in
          mke ~loc:noloc aty & Switch_or (mkvar ~loc:noloc v,
                                          mkp ~loc:noloc tyl vl, tl,
                                          mkp ~loc:noloc tyr vr, tr)

      | Switch (v, ( [(Bool true), [], tt ;
                      (Bool false), [], tf]
                   | [(Bool false), [], tf ;
                      (Bool true), [], tt] ), d) ->
          warn_unused_default d;
          let tt = f tt in
          let tf = f tf in
          mke ~loc:noloc aty & IfThenElse (mkvar ~loc:noloc v, tt, Some tf)

      | Switch (v, ( [Some, [vs,tys], ts;
                      None, [], tn]
                   | [None, [], tn;
                      Some, [vs,tys], ts]), d) ->
          warn_unused_default d;
          let ts = f ts in
          let tn = f tn in
          mke ~loc:noloc aty & Switch_none (mkvar ~loc:noloc v, tn, mkp ~loc:noloc tys vs, ts)

      | Switch (v, ( [Cons, [v1,ty1; v2,ty2], tc;
                      Nil, [], tn]
                   | [Nil, [], tn;
                      Cons, [v1,ty1; v2,ty2], tc]), d) ->
          warn_unused_default d;
          let tc = f tc in
          let tn = f tn in
          mke ~loc:noloc aty & Switch_cons (mkvar ~loc:noloc v,
                                            mkp ~loc:noloc ty1 v1,
                                            mkp ~loc:noloc ty2 v2, tc, tn)

      | Switch (_v, _cases, None) -> assert false

      | Switch (v, cases, Some d) ->
          (* all cases must be about constants with infinite members *)
          List.iter (function
              | (Cnstr.Constant (C.Int _ | String _ | Bytes _ | Set _ | Map _ | Timestamp _), _, _) -> ()
              | ((Cnstr.Unit|Left|Right|Some|None|Cons|Nil|Pair|Bool _), _, _) ->
                  assert false (* must be handled in the former cases *)
              | (Cnstr.Constant (C.Unit | Bool _ | Option _ | List _ | Pair _ | Left _ | Right _), _, _) ->
                  assert false (* invalid *)
              | (Cnstr.Constant (C.Code _), _, _) -> assert false
            ) cases;
          List.fold_right (fun case telse ->
              match case with
              | (Cnstr.Constant c, [], t) ->
                  (* XXX comparable check *)
                  let t = f t in
                  mke ~loc:noloc aty
                  & IfThenElse (mkeq ~loc:noloc (mkvar ~loc:noloc v)
                                  (mke ~loc:noloc (snd v) & Const c),
                                t, Some telse)
              | _ -> assert false) cases  & f d
    in
    f t

  let compile ~loc e (cases : (P.t * t option * t) list) =
    let gloc = Location.ghost loc in

    (* actions as functions *)
    let acts =
      List.mapi (fun i (pat, _g, action) ->
          let gloc = Location.ghost action.loc in
          let vars = IdTys.elements & P.vars pat in

          let unpackables =
            let fvs = List.fold_left (fun fvs idty ->
                IdTys.remove idty fvs)
                (freevars action)
                (if vars = [] then [] else List.tl (List.rev vars)) (* the last one cannot be free inside the body *)
            in
            IdTys.filter (fun (_id,ty) -> not & Michelson.Type.is_packable ~legacy:false ty) fvs
          in
          let _must_expand = not & IdTys.is_empty unpackables in
          (* It's inefficient for the storage, but we do not want to get troubled
             by unpackables around the LAMBDAs introduced by pmatch.
          *)
          let must_expand = true in
          let case =
            if must_expand then
              create_ident (Printf.sprintf "case_must_expand%d" i)
            else
              create_ident (Printf.sprintf "case%d" i)
          in

          match vars with
          | [] ->
              (* if [vars = []], we need a [fun () ->].
                 Think about the case of [| _ -> assert false].
              *)
              let pvar = mkp ~loc:gloc Type.tyUnit & create_ident "unit" in
              let f = mkfun ~loc:gloc pvar action in
              let e = mke ~loc:gloc action.typ (App (mkvar ~loc:gloc (case, f.typ), [mkunit ~loc:gloc ()])) in
              if must_expand then
                (case, None, IML.subst [(case, f)] e)
              else
                (case, Some f, e)

          | _ ->
              (* match ... with
                 | ..x.. when g[x] -> e[x]

                 let case xnew = e[xnew] in
                 match ... with
                 | ..x.. when g[x] -> case x

                 We have to rename the pattern variables x in e[x]
                 to void name crashes which confuse [count_variables].

                 XXX This is very inefficient!
              *)
              let vars' = List.map (fun (v,ty) ->
                  (create_ident & Ident.name v, ty)) vars in
              (* We use alpha_conv, not subst, to keep the original code
                 locations *)
              let s = List.map2 (fun (v,_) (v',_) -> v, v') vars vars' in
              let action = alpha_conv s action in

              (* f = fun v'1 v'2 .. v'n -> action

                 XXX Curried.  This makes expansion happens more often
                 to avoid unstorable free vars
              *)
              let f = List.fold_right (fun (v',ty) st ->
                  mkfun ~loc:gloc (mkp ~loc:gloc ty v') st) vars' action
              in
              (* e = f v1 v2 .. vn *)
              let e =
                mke ~loc:gloc action.typ (App (mkvar ~loc:gloc (case, f.typ), List.map (mkvar ~loc:gloc) vars))
              in

              if must_expand then
                (case, None, IML.subst [(case, f)] e)
              else
                (case, Some f, e)
            ) cases
          in

          let cases, guards =
            let cases, guards, _ =
              List.fold_left (fun (cases, guards, i) case ->
                  match case with
                  | p, None, e -> (p, None, e)::cases, guards, i
                  | p, Some g, e -> (p, Some i, e)::cases, g::guards, i+1)
                ([], [], 0) cases
            in
            List.rev cases, List.rev guards
          in

          let v = create_ident "v" in

          let typ = (match List.hd cases with (_,_,e) -> e).typ in

          (* let casei = fun ... in let v = e in ... *)
          let make x =
            (* let v = <e> in <x> *)
            let match_ = mklet ~loc:gloc (mkp ~loc:gloc e.typ v) e x in
            (* let casei = <f> in .. *)
            List.fold_right (fun (v, fopt, _e) st ->
                match fopt with
                | None -> st
                | Some f ->
                    mklet ~loc:f.loc (mkp ~loc:f.loc f.typ v) f st) acts match_
          in

          let matrix : matrix =
            List.mapi (fun i (pat, g, _) ->
                { pats=[pat]; guard= g; action= i; bindings= [] }) cases
          in

          (* XXX if match target is a tuple literal, no need to form a real tuple *)
          let res = cc [(v,e.typ)] matrix in
          if_debug (fun () -> Format.eprintf "pmatch debug: %a@." pp_tree res);
          let e = build typ (List.map (fun (_,_,e) -> e) acts) guards res in
          if_debug (fun () -> Format.eprintf "pmatch debug: %a@." pp e);
          make e
end

let rec list_elems e = match e.desc with
  | Cons (e,es) -> e :: list_elems es
  | Nil -> []
  | _ -> errorf_constant ~loc:e.loc "List is expected"

let rec construct lenv ~loc tyenv exp_type ({Types.cstr_name} as cdesc) args =
  let gloc = Location.ghost loc in
  let typ =
    Result.at_Error (fun e ->
        errorf_type_expr ~loc "This has type %a.  %a" Printtyp.type_expr exp_type pp_type_expr_error e)
    & type_expr tyenv exp_type
  in
  let make typ desc = { loc; typ; desc; attrs= [] } in
  match (Ctype.expand_head tyenv exp_type).Types.desc, typ.desc with
  (* bool *)
  | Tconstr (p, [], _), _ when p = Predef.path_bool ->
      make tyBool (match cstr_name with
          | "true" -> Const (C.Bool true)
          | "false" -> Const (C.Bool false)
          | s -> internal_error ~loc "strange bool constructor %s" s)

  (* list *)
  | Tconstr (p, [_], _), TyList ty when p = Predef.path_list ->
      begin match cstr_name with
        | "[]" -> make (tyList ty) Nil
        | "::" ->
            begin match args with
              | [e1; e2] ->
                  let e1 = expression lenv e1 in
                  let e2 = expression lenv e2 in
                  mkcons ~loc e1 e2
              | _ -> internal_error ~loc "strange cons"
            end
        | s -> internal_error ~loc "strange list constructor %s" s
      end

  (* option *)
  | Tconstr (p, [_], _), TyOption (_,ty) when p = Predef.path_option ->
      begin match cstr_name with
        | "None" -> make (tyOption (None, ty)) IML_None
        | "Some" ->
            begin match args with
              | [e1] ->
                  let e1 = expression lenv e1 in
                  mksome ~loc e1
              | _ -> internal_error ~loc "strange cons"
            end
        | s -> internal_error ~loc "strange list constructor %s" s
      end

  (* sum *)
  | Tconstr (p, [_; _], _), TyOr (_,tyl, _,tyr) when (match Path.is_scaml p with Some "sum" -> true | _ -> false) ->
      let arg = match args with [arg] -> arg | _ -> internal_error ~loc "strange sum arguments" in
      begin match cstr_name with
      | "Left" ->
          let e = expression lenv arg in
          (* e.typ = ty1 *)
          mkleft ~loc tyr e
      | "Right" ->
          let e = expression lenv arg in
          (* e.typ = ty2 *)
          mkright ~loc tyl e
      | s -> internal_error ~loc "strange sum constructor %s" s
      end

  (* unit *)
  | Tconstr (p, _, _), TyUnit when p = Predef.path_unit ->
      make tyUnit Unit

  (* int *)
  | Tconstr (p, [], _), TyInt when Path.is_scaml_dot "int" p ->
      make tyInt begin
        let arg = match args with
            | [arg] -> arg
            | _ -> internal_error ~loc "strange Int arguments"
          in
          match arg.exp_desc with
            | Texp_constant (Const_int n) -> Const (Int (Z.of_int n))
            | _ -> errorf_constant ~loc "Int can only take an integer constant"
        end

  (* nat *)
  | Tconstr (p, [], _), TyNat when Path.is_scaml_dot "nat" p ->
      make tyNat begin
        let arg = match args with
          | [arg] -> arg
          | _ -> internal_error ~loc "strange Nat arguments"
        in
        match arg.exp_desc with
          | Texp_constant (Const_int n) ->
              if n < 0 then
                errorf_constant ~loc "Nat can only take a positive integer constant";
              Const (Int (Z.of_int n))
          | _ -> errorf_constant ~loc "Nat can only take an integer constant"
      end

  (* tz *)
  | Tconstr (p, [], _), TyMutez when Path.is_scaml_dot "tz" p ->
      make tyMutez begin
        let arg = match args with
          | [arg] -> arg
          | _ -> internal_error ~loc "strange Tz arguments"
        in
        match arg.exp_desc with
          | Texp_constant (Const_float f) ->
              begin try
                let pos = String.index f '.' in
                let dec = String.sub f 0 pos in
                let sub = String.sub f (pos+1) (String.length f - pos - 1) in
                let all_dec s =
                  for i = 0 to String.length s - 1 do
                    match String.unsafe_get s i with
                    | '0'..'9' -> ()
                    | _ -> errorf_constant ~loc "%s: Tz can only take simple decimal floats" f
                  done
                in
                all_dec dec;
                all_dec sub;
                let sub =
                  if String.length sub > 6 then
                    errorf_constant ~loc "%s: the smallest expressive franction of tz is micro" f;

                  sub ^ String.init (6 - String.length sub) (fun _ -> '0')
                in
                Const (Int (Z.of_string (dec ^ sub)))
              with
              | _ -> errorf_constant ~loc "%s: Tz can only take simple decimal floats" f
            end
          | _ -> errorf_constant ~loc "Nat can only take an integer constant"
      end

  (* set *)
  | Tconstr (p, [_], _), TySet _ when (match Path.is_scaml p with Some "set" -> true | _ -> false) ->
      (* XXX comparable type check *)
      if cstr_name <> "Set" then internal_error ~loc "strange set constructor";
      begin match args with
        | [arg] ->
            let es = list_elems & expression lenv arg in
            mke ~loc typ (Set es)
        | _ -> internal_error ~loc "strange set arguments"
      end

  (* map *)
  | Tconstr (p, [_; _], _), TyMap _ when (match Path.is_scaml p with Some "map" -> true | _ -> false) ->
      (* XXX comparable type check *)
      if cstr_name <> "Map" then internal_error ~loc "strange map constructor";
      begin match args with
        | [arg] ->
            let es = list_elems & expression lenv arg in
(*
                let rec check_uniq = function
                  | [] | [_] -> ()
                  | (c1,_)::(c2,_)::_ when c1 = c2 -> (* XXX OCaml's compare *)
                      errorf ~loc "Map literal contains duplicated key %a" C.pp c1
                  | _::xs -> check_uniq xs
                in
                check_uniq xs;
*)
            let kvs = List.map (fun e -> match e.desc with
                | Pair (k,v) -> (k,v)
                | _ ->
                    errorf_constant ~loc:e.loc "Map binding must be a pair expression")
                es
            in
            { loc; typ; desc= Map kvs; attrs= [] }
        | _ -> internal_error ~loc "strange map arguments"
      end

  (* big_map
     We cannot write big_map constants in Michelson but we can write them
     at storage initializations: tezos-client run script .. on storage '{ Elt 1 1 }' ..
  *)
  | Tconstr (p, [_; _], _), TyBigMap _ when (match Path.is_scaml p with Some "big_map" -> true | _ -> false) ->
      (* XXX comparable type check *)
      if cstr_name <> "BigMap" then internal_error ~loc "strange big_map constructor";
      begin match args with
        | [arg] ->
            let es = list_elems & expression lenv arg in
(*
                let rec check_uniq = function
                  | [] | [_] -> ()
                  | (c1,_)::(c2,_)::_ when c1 = c2 -> (* XXX OCaml's compare *)
                      errorf ~loc "Map literal contains duplicated key %a" C.pp c1
                  | _::xs -> check_uniq xs
                in
                check_uniq xs;
*)
            let kvs = List.map (fun e -> match e.desc with
                | Pair (k,v) -> (k,v)
                | _ ->
                    errorf_constant ~loc:e.loc "Map binding must be a pair expression")
                es
            in
            { loc; typ; desc= BigMap kvs; attrs= [] }
        | _ -> internal_error ~loc "strange map arguments"
      end

  (* C "string" style constants *)
  | Tconstr (p, [], _), _ when Path.is_scaml p <> None ->
      begin match Path.is_scaml p with
        | None -> assert false
        | Some n ->
            match List.assoc_opt n constructions_by_string with
            | None ->
                unsupported ~loc "constants of data type %s" (Path.name p)
            | Some (tyname, cname, typ, parse) ->
                if cstr_name <> cname then internal_error ~loc "strange constructor for %s" tyname;
                match args with
                | [] | _::_::_ ->
                    internal_error ~loc "strange arguments for %s" cname
                | [arg] ->
                    let e = expression lenv arg in
                    match e.desc with
                    | Const (String s) ->
                        begin match parse s with
                          | Ok v -> { e with typ; desc= Const v }
                          | Error s -> errorf_constant ~loc "Parse error of %s string: %s" tyname s
                        end
                    | _ -> errorf_constant ~loc "%s only takes a string literal" cname
      end

  (* others *)
  | Tconstr (p, [], _), _ when Path.is_scaml p = None ->
      let rec embed ty sides arg = match ty.M.Type.desc, sides with
        | _, [] -> arg
        | TyOr (_,ty1, _,ty2), Binplace.Left::sides ->
            mkleft ~loc:gloc ty2 (embed ty1 sides arg)
        | TyOr (_,ty1, _,ty2), Right::sides ->
            mkright ~loc:gloc ty1 (embed ty2 sides arg)
        | _ -> assert false
      in
      begin match Env.find_type_descrs p tyenv with
        | [], [] when p = Predef.path_exn ->
            errorf_type_expr ~loc
              "%a" pp_type_expr_error Exception_out_of_raise
        | [], [] ->
            errorf_type_expr ~loc "Abstract data type %s is not supported in SCaml" (Path.name p)
        | [], _::_ -> assert false (* record cannot come here *)
        | _::_, _::_ -> assert false

        | constrs, [] ->
            (* XXX mty is already calculated at the beginning *)
            let consts_ty, non_consts_ty, entire_ty =
              from_Ok & variant_type tyenv exp_type p constrs
            in
            let rec find_constr i = function
              | [] -> assert false
              | n::_ when n = cstr_name -> i
              | _::names -> find_constr (i+1) names
            in
            match cdesc.cstr_arity, consts_ty, non_consts_ty with
            | _, None, [] -> assert false
            | 0, None, _ -> assert false
            | 0, Some names, [] ->
                mkint ~loc:gloc & find_constr 0 names
            | 0, Some names, xs ->
                let sides = Binplace.path 0 (List.length xs + 1) in
                embed entire_ty sides & mkint ~loc:gloc & find_constr 0 names
            | _, _, [] -> assert false
            | _, _, cs ->
                let names = List.map fst cs in
                let i = find_constr 0 names in
                let sides =
                  let shift = if consts_ty = None then 0 else 1 in
                  Binplace.path (i + shift) (List.length names + shift)
                in
                embed entire_ty sides
                & encode_by (mkpair ~loc:gloc) & List.map (expression lenv) args
      end

  | _ -> prerr_endline ("Constructor compilation failure: " ^ cstr_name); assert false (* XXX *)

and expression (lenv:lenv) { exp_desc; exp_loc=loc; exp_type= mltyp; exp_env= tyenv; exp_extra=_; exp_attributes } =
  let gloc = Location.ghost loc in
  (* wildly ignores extra *)
  (* if exp_extra <> [] then unsupported ~loc "expression extra"; *)
  let exp_attributes = Migrate_parsetree__.Migrate_parsetree_409_408_migrate.copy_attributes exp_attributes in
  begin match attr_has_entry_point exp_attributes with
    | None -> ()
    | Some (loc, _) ->
        errorf_entry ~loc "entry declaration is only allowed for the toplevel definitions";
  end;
  let typ = Result.at_Error (fun e ->
      errorf_type_expr ~loc "This expression has type@ %a.@ %a" Printtyp.type_expr mltyp pp_type_expr_error e)
      & type_expr tyenv mltyp
  in
  let mk desc = { loc; typ; desc; attrs= [] } in
  let e = match exp_desc with
    | Texp_ident (Path.Pident id, {loc=vloc}, _vd) ->
        if not (List.mem id (lenv.local_variables @ lenv.non_local_variables)) then
          internal_error ~loc:vloc "Hey %s is not tracked!  env=%s"
            (Ident.unique_name id)
            (String.concat ", " (List.map (fun id -> Ident.unique_name id) lenv.local_variables));
        if not (List.mem id lenv.local_variables)
           && not (Michelson.Type.is_packable ~legacy:false typ)
           && lenv.fun_level > 0 then
          errorf_freevar ~loc:lenv.fun_loc "Function body cannot have a free variable occurrence `%s` with unpackable type."
            (Ident.name id);
        (* if List.mem id lenv.local_variables
             && not (Michelson.Type.storable typ) then
            Format.eprintf "wow it is properly abstracted: ...@.";
        *)
        mk & Var id
    | Texp_ident (p, {loc}, _vd) ->
        begin match Path.is_scaml p with
          | Some "Contract.self" ->
              if lenv.fun_level > 0 then
                errorf_self ~loc:lenv.fun_loc "Contract.self cannot freely occur in a function body except the entrypoints."
              else
                mk & Var contract_self_id

          | Some "Contract.create_raw" ->
              (* SCaml.Contract.create_raw must be always applied with a string literal.
                 If we see it here, it is not.
              *)
              errorf_contract ~loc "Contract.create_raw must be immediately applied with a string literal"
          | Some n -> mk & primitive ~loc typ n []
          | None ->
              match Path.is_stdlib p with
              | None ->
                  (* XXX If we see Dune__exe, it is highly likely that 
                     scaml.ppx is used without (wrapped_executables false)
                     in dune-project.  Should be warned.
                  *)
                  (* XXX Var should take Path.t... here we use a workaround *)
                  mk & Var (Ident.create_persistent (Path.name p))
              | Some _ ->
                  errorf_stdlib ~loc "This value is defined in Stdlib module which is not supported by SCaml."
        end
    | Texp_constant (Const_string (s, _)) ->
        mk & Const (String s)
    | Texp_constant _ -> unsupported ~loc "constant"
    | Texp_tuple [e1; e2] ->
        let e1 = expression lenv e1 in
        let e2 = expression lenv e2 in
        (* tyPair (e1.typ, e2.typ) = typ *)
        mk & Pair (e1, e2)
    | Texp_tuple es ->
        Binplace.fold
          ~leaf:(fun c -> c)
          ~branch:(fun c1 c2 -> mk & Pair (c1, c2))
          & Binplace.place
          & List.map (expression lenv) es

    | Texp_construct ({loc}, c, args) ->
        construct lenv ~loc tyenv mltyp c args

    | Texp_assert e ->
        begin match e.exp_desc with
        | Texp_construct (_, {cstr_name="false"}, []) ->
            (* assert false has type 'a *)
            mk AssertFalse
        | _ -> mkassert ~loc & expression lenv e
        end

    | Texp_let (Recursive, _, _) -> unsupported ~loc "recursion"
    | Texp_let (Nonrecursive, vbs, e) ->
        let lenv' = Lenv.add_locals (Typedtree.let_bound_idents vbs) lenv in
        if not Flags.(!flags.iml_pattern_match) then begin
          (* simple let x = e in e' *)
          let rev_vbs =
            List.fold_left (fun rev_vbs vb ->
                let _, v, e = value_binding lenv vb in
                (v, e) :: rev_vbs) [] vbs
          in
          let e = expression lenv' e in
          List.fold_left (fun e (v,def) ->
              { loc; (* XXX inaccurate *)
                typ= e.typ;
                desc= Let (v, def, e);
                attrs= [] } ) e rev_vbs
        end else begin
          (* let p = e and p' = e' in e''
             =>
             let xnew = e in match xnew with p ->
             let xnew' = e' in match xnew' with p' -> e''
          *)
          List.fold_right (fun vb e' ->
              let { vb_pat; vb_expr } = vb in
              let vb_expr = expression lenv vb_expr in
              let typ = vb_expr.typ in
              let i = create_ident "x" in
              let x = { desc= i; typ; loc= Location.none; attrs= () } in
              let ex = { desc= Var i; typ; loc= Location.none; attrs= [] } in
              { desc= Let (x, vb_expr,
                           Pmatch.compile ~loc ex [(pattern vb_pat, None, e')])
              ; typ= e'.typ
              ; loc
              ; attrs= []
              }
            ) vbs & expression lenv' e
        end

    | Texp_apply (_, []) -> assert false

    | Texp_apply (f, args) ->
        let get_args = List.map (function
            | (Nolabel, Some (e: Typedtree.expression)) -> expression lenv e
            | _ -> unsupported ~loc "labeled arguments")
        in
        let name = match f with
          | { exp_desc= Texp_ident (p, _, _) } -> Path.is_scaml p
          | _ -> None
        in
        begin match name with
        | None ->
          let name = match f with
            | { exp_desc= Texp_ident (p, _, _) } -> Path.is_stdlib p
            | _ -> None
          in
          begin match name with
            | Some "@@" ->
                (* e1 @@ e2 => e1 e2 *)
                begin match get_args args with
                  | [e] -> e
                  | e1 :: es -> mk & App (e1, es)
                  | [] -> assert false
                end
            | _ -> mk & App (expression lenv f, get_args args)
          end

        | Some "Obj.pack'" ->
            let fty = Result.at_Error (fun e ->
                errorf_type_expr ~loc:f.exp_loc "This primitive has type %a.  %a"
                  Printtyp.type_expr f.exp_type
                  pp_type_expr_error e)
                & type_expr f.exp_env
                & match (Ctype.repr f.exp_type).Types.desc with
                  | Tarrow (_, _, ty, _) -> ty
                  | _ -> assert false
            in
            (* fty = fty' *)
            mk & primitive ~loc:f.exp_loc fty "Obj.pack" & get_args (List.tl args)

        | Some "Obj.unpack'" ->
            let fty = Result.at_Error (fun e ->
                errorf_type_expr ~loc:f.exp_loc "This primitive has type %a.  %a"
                  Printtyp.type_expr f.exp_type
                  pp_type_expr_error e)
                & type_expr f.exp_env
                & match (Ctype.repr f.exp_type).Types.desc with
                  | Tarrow (_, _, ty, _) -> ty
                  | _ -> assert false
            in
            (* fty = fty' *)
            mk & primitive ~loc:f.exp_loc fty "Obj.unpack" & get_args (List.tl args)

        | Some "raise" -> translate_raise ~loc lenv typ args
        | Some n when  String.is_prefix "Contract.create" n ->
            mk & contract_create ~loc n & get_args args
        | Some n ->
            let fty = Result.at_Error (fun e ->
                errorf_type_expr ~loc:f.exp_loc "This primitive has type %a.  %a"
                  Printtyp.type_expr f.exp_type
                  pp_type_expr_error e)
                & type_expr f.exp_env f.exp_type
            in
            mk & primitive ~loc:f.exp_loc fty n & get_args args
        end

    | Texp_function { arg_label= (Labelled _ | Optional _) } ->
        unsupported ~loc "labeled arguments"

    | Texp_function { arg_label= Nolabel; param=_; cases; partial } ->
        if partial = Partial then errorf_pattern_match ~loc "Pattern match is not exhaustive";
        (* name the same name of the original if possible *)
        let i = create_ident & match cases with
          | [ { c_lhs = { pat_desc= (Tpat_var (id, _) |
                                     Tpat_alias ({ pat_desc= Tpat_any }, id, _)) } } ] ->
              Ident.name id
          | _ -> "arg"
        in
        let targ, _tret = match typ.desc with
          | TyLambda (targ, tret) -> targ, tret
          | _ -> assert false
        in
        let var = { desc= i; typ= targ; loc= Location.none; attrs= () } in
        let lenv = Lenv.add_locals [i] & Lenv.into_fun ~loc lenv in
        let evar = { desc= Var i; typ= targ; loc= Location.none; attrs= [] } in
        if not Flags.(!flags.iml_pattern_match) then begin
          mkfun ~loc var & switch lenv ~loc evar cases
        end else begin
          (* function case1 | .. | casen
             =>
             fun xnew -> match xnew with case1 | .. | casen
          *)
          let compile_case case =
            let lenv = Lenv.add_locals (Typedtree.pat_bound_idents case.c_lhs) lenv in
            let guard = Option.fmap (expression lenv) case.c_guard in
            (pattern case.c_lhs, guard, expression lenv case.c_rhs)
          in
          let t = Pmatch.compile ~loc evar & List.map compile_case cases in
          mkfun ~loc var t
        end

    | Texp_ifthenelse (cond, then_, Some else_) ->
        let econd = expression lenv cond in
        let ethen = expression lenv then_ in
        let eelse = expression lenv else_ in
        (* ignore (unify ethen.typ eelse.typ);
           ignore (unify typ ethen.typ); *)
        mk & IfThenElse (econd, ethen, Some eelse)

    | Texp_ifthenelse (cond, then_, None) ->
        let econd = expression lenv cond in
        let ethen = expression lenv then_ in
        if ethen.typ.desc <> TyUnit then internal_error ~loc:ethen.loc "else None has non unit type";
        mk & IfThenElse (econd, ethen, None)

    | Texp_match (_ , _, Partial) ->
        unsupported ~loc "non exhaustive pattern match"

    | Texp_match (e , cases, Total) ->
        let e = expression lenv e in
        if not Flags.(!flags.iml_pattern_match) then begin
          switch lenv ~loc e cases
        end else begin
          let compile_case case =
            let lenv = Lenv.add_locals (Typedtree.pat_bound_idents case.c_lhs) lenv in
            let guard = Option.fmap (expression lenv) case.c_guard in
            (pattern case.c_lhs, guard, expression lenv case.c_rhs)
          in
          Pmatch.compile ~loc e (List.map compile_case cases)
        end
    | Texp_try _ -> unsupported ~loc "try-with"
    | Texp_variant _ -> unsupported ~loc "polymorphic variant"

    | Texp_record { fields; extended_expression= None; _ } ->
        (* I believe fields are already sorted *)
        let es =
          List.map (fun (_ldesc, ldef) -> match ldef with
              | Overridden (_, e) -> expression lenv e
              | Kept _ -> assert false) & Array.to_list fields
        in
        Binplace.fold
          ~leaf:(fun c -> c)
          ~branch:(fun c1 c2 -> mkpair ~loc:gloc c1 c2)
        & Binplace.place es

    | Texp_record { fields; extended_expression= Some e; } ->
        (* optimal code, I hope *)
        (* I believe fields are already sorted *)
        let es =
          List.map (fun (_ldesc, ldef) -> match ldef with
              | Overridden (_, e) -> Some (expression lenv e)
              | Kept _ -> None) & Array.to_list fields
        in
        let tree = Binplace.place es in
        let rec simplify = function
          | Binplace.Leaf x as t -> t, x = None
          | Branch (t1, t2) ->
              let t1, b1 = simplify t1 in
              let t2, b2 = simplify t2 in
              if b1 && b2 then Leaf None, true
              else Branch (t1, t2), false
        in
        let rec f e = function
          | Binplace.Leaf None -> e (* no override *)
          | Leaf (Some e) -> e
          | Branch (t1, t2) ->
              let e1 = f (mkfst ~loc:gloc e) t1 in
              let e2 = f (mksnd ~loc:gloc e) t2 in
              mkpair ~loc:gloc e1 e2
        in
        f (expression lenv e) & fst & simplify tree

    | Texp_field (e, _, label) ->
        let pos = label.lbl_pos in
        let nfields = Array.length label.lbl_all in
        if_debug (fun () -> Format.eprintf "field %d %s of %d @." pos label.lbl_name nfields);
        let e = expression lenv e in
        let rec f e = function
          | [] -> e
          | Binplace.Left  :: dirs -> f (mkfst ~loc:gloc e) dirs
          | Binplace.Right :: dirs -> f (mksnd ~loc:gloc e) dirs
        in
        f e & Binplace.path pos nfields
    | Texp_open (_,e) -> expression lenv e
    | Texp_sequence (e1, e2) -> mk & Seq ( expression lenv e1, expression lenv e2 )
    | Texp_setfield _ -> unsupported ~loc "record field set"
    | Texp_array _ -> unsupported ~loc "array"
    | Texp_while _ -> unsupported ~loc "while-do-done"
    | Texp_for _ -> unsupported ~loc "for-do-done"
    | Texp_send _ -> unsupported ~loc "method call"
    | Texp_new _ -> unsupported ~loc "new"
    | Texp_instvar _ -> unsupported ~loc "class instance variable"
    | Texp_setinstvar _ -> unsupported ~loc "class instance variable set"
    | Texp_override _ -> unsupported ~loc "override"
    | Texp_letmodule _ -> unsupported ~loc "let-module"
    | Texp_letexception _ -> unsupported ~loc "let-exception"
    | Texp_lazy _ -> unsupported ~loc "lazy"
    | Texp_object _ -> unsupported ~loc "object"
    | Texp_pack _ -> unsupported ~loc "first class module"
    | Texp_extension_constructor _ -> unsupported ~loc "open variant"
    | Texp_unreachable -> unsupported ~loc "this type of expression"
    | Texp_letop _ -> unsupported ~loc "let op"
  in
  { e with typ }

(* XXX loc is likely incorrect *)
and primitive ~loc fty n args =
  let apply_left x left = match left with
    | [] -> x
    | _ ->
        let typ =
          let rec f ty = function
            | [] -> ty
            | _arg::args ->
                match ty.M.Type.desc with
                | TyLambda (_,ty2) -> f ty2 args
                | _ -> assert false
          in
          f fty args
        in
        App ({ loc; (* XXX inaccurate *)
               typ;
               desc= x;
               attrs= [] }, left)
  in
  match n with
  | "Contract.self" -> assert false (* must be handled already *)
  | "Contract.contract'" ->
      begin match args with
        | [] | [_] ->
            errorf_contract ~loc "Contract.contract' must be fully applied"
        | address :: ({ desc= Const (M.Constant.String _) } as entry) :: left ->
            apply_left (Prim (n, fty, [address; entry])) left
        | _ :: { loc } :: _ ->
            errorf_contract ~loc "contract entry name must be a constant"
      end
  | _ ->
      match List.assoc_opt n Primitives.primitives with
      | None -> errorf_primitive ~loc "Unknown primitive SCaml.%s" n
      | Some (_pure, arity, _conv) ->
          if arity > List.length args then
            let tys, ret = M.Type.args fty in
            let tys =
              List.take (arity - List.length args)
              & List.drop (List.length args) tys
            in
            let xtys = List.map (fun ty -> create_ident "x", ty) tys in
            let e =
              List.fold_right (fun (x,ty) e -> mkfun ~loc (mkp ~loc ty x) e)
                xtys
              & mkprim ~loc ret n fty (args @ List.map (fun (x,ty) -> mkvar ~loc (x,ty)) xtys)
            in
            e.desc
          else
            let args, left = List.split_at arity args in
            apply_left (Prim (n, fty, args)) left

and switch lenv ~loc:loc0 e cases =
  let ty = e.typ in
  let compile_case case = match case.c_guard with
    | Some e -> unsupported ~loc:e.exp_loc "guard"
    | None ->
        match case.c_lhs.pat_desc with
        | Tpat_construct (_, { cstr_name }, xs) -> cstr_name, xs, expression lenv case.c_rhs
        | _ -> unsupported ~loc:case.c_lhs.pat_loc "non variant pattern"
  in
  let cases =
    List.sort (fun (n1,_,_) (n2,_,_) -> compare n1 n2) (List.map compile_case cases)
  in
  let mk desc = { desc; loc=loc0; typ= (let _, _, e = List.hd cases in e.typ); attrs= [] } in
  match ty.desc, cases with
  | TyOr (_,_ty1, _,_ty2), [("Left",[l],le); ("Right",[r],re)] ->
      let get_var p = match pattern_simple p with [v] -> v | _ -> assert false in
      let lv = get_var l in
      let rv = get_var r in
      mk & Switch_or (e,
                      lv, le,
                      rv, re)
  | TyOr _, _ -> internal_error ~loc:loc0 "sum pattern match"
  | TyList _ty1, [("::",[l1;l2],le); ("[]",[],re)] ->
      let get_var p = match pattern_simple p with [v] -> v | _ -> assert false in
      let lv1 = get_var l1 in
      let lv2 = get_var l2 in
      mk & Switch_cons (e,
                        lv1, lv2, le,
                        re)
  | TyOption (_,_ty1), [("None",[],le); ("Some",[r],re)] ->
      let get_var p = match pattern_simple p with [v] -> v | _ -> assert false in
      let rv = get_var r in
      mk & Switch_none (e,
                        le,
                        rv, re)
  | _, _ -> unsupported ~loc:loc0 "pattern match other than SCaml.sum, list, and option"

and value_binding lenv { vb_pat; vb_expr; vb_attributes=_; vb_loc=_loc } =
  (* currently we only handle very simple sole variable pattern *)
  match pattern_simple vb_pat with
  | [v] ->
      let e = expression lenv vb_expr in
      (* ignore & unify v.typ e.typ; *)
      vb_pat.pat_type, v, e
  | _ -> assert false

(* The condition of the entry point is a bit too strict.
   Currently: the last sitem must be an entry point.
   Better: the last value binding must be an entry point.,
*)
and structure_item lenv { str_desc; str_loc=loc } =
  match str_desc with
  | Tstr_eval _       -> unsupported ~loc "toplevel evaluation"
  | Tstr_primitive _  -> unsupported ~loc "primitive declaration"
  | Tstr_typext _     -> unsupported ~loc "type extension"
  | Tstr_recmodule _  -> unsupported ~loc "recursive module declaration"
  | Tstr_class _      -> unsupported ~loc "class declaration"
  | Tstr_class_type _ -> unsupported ~loc "class type declaration"
  | Tstr_modtype _    -> unsupported ~loc "module type declaration"
  | Tstr_include _    -> unsupported ~loc "include"

  | Tstr_value (Recursive, _vbs) -> unsupported ~loc "recursive definitions"

  | Tstr_value (Nonrecursive, vbs) ->
      let rev_vbs =
        List.fold_left (fun rev_vbs vb ->
            let _pat_typ,v,b = value_binding lenv vb in
            (v,b)::rev_vbs) [] vbs
      in
      let lenv = Lenv.add_locals (Typedtree.let_bound_idents vbs) lenv in
      lenv, List.map (fun (pv,def) -> (pv,def,true (* defined here *))) (List.rev rev_vbs)

  | Tstr_open _open_description -> lenv, []

  | Tstr_exception _ -> lenv, []
  | Tstr_type _ -> lenv, []

  | Tstr_module mb ->
      let mname = Ident.name mb.mb_id in
      let rec module_expr me = match me.mod_desc with
        | Tmod_structure str -> structure lenv str
        | Tmod_constraint (me, _, _, _) -> module_expr me
        | _ -> unsupported ~loc "module declaration other than simple structure"
      in
      let lenv, vbs = module_expr mb.mb_expr in
      (* Make local defs accessible from the outside *)
      (* XXX Dirty boolean hack to prevent x in
         module X = struct
           module Y = struct
             let x = 1
           end
         end

         from being copied as X.x_0.  It should be copied only to Y.x_0 and X.Y.x_0.

      *)
      let vbs' = List.filter_map (fun (pv,t,defined_here) ->
          if defined_here then
            (* XXX Check shaodowing *)
            Some ({ pv with IML.desc= Ident.create_persistent (mname ^ "." ^ Ident.name pv.IML.desc) },
                  { t with IML.desc= IML.Var pv.desc },
                  true)
          else None) vbs
      in
      Lenv.add_locals (List.map (fun (pv,_,_) -> pv.IML.desc) vbs') lenv,
      List.map (fun (pv,def,_) -> (pv,def,false)) vbs
      @ vbs'

  | Tstr_attribute _ ->
      (* simply ignores it for now *)
      lenv, []

and structure lenv { str_items= sitems } =
  let lenv, rev_vbss =
    List.fold_left (fun (lenv, rev_vbss) sitem ->
        let lenv, vbs = structure_item lenv sitem in
        lenv, vbs :: rev_vbss) (lenv, []) sitems
  in
  let vbs = List.flatten & List.rev rev_vbss in
  (lenv, vbs)

and contract_create ~loc n args = match n with
  | "Contract.create_raw" | "Contract.create_from_tz_code" ->
      begin match args with
      | [] | [_] | [_;_] | [_;_;_] ->
          errorf_contract ~loc "%s must be fully applied" n
      | [e0; e1; e2; e3] ->
          let s = match e0.desc with
            | Const (C.String s) -> s
            | _ ->
                errorf_contract ~loc:e0.loc
                  "The first argument of %s must be a string literal of Michelson code" n
          in
          Contract_create (Tz_code s, e0.loc, e1, e2, e3)
      | _ -> assert false (* too many args must be rejeced by OCaml type system *)
      end
  | "Contract.create_from_tz_file" ->
      begin match args with
      | [] | [_] | [_;_] | [_;_;_] ->
          errorf_contract ~loc "%s must be fully applied" n
      | [e0; e1; e2; e3] ->
          let s = match e0.desc with
            | Const (C.String s) -> s
            | _ ->
                errorf_contract ~loc:e0.loc
                  "The first argument of %s must be a string literal of Michelson file path" n
          in
          Contract_create (Tz_file s, e0.loc, e1, e2, e3)
      | _ -> assert false (* too many args must be rejeced by OCaml type system *)
      end
  | _ -> internal_error ~loc "Unknown Contract.create* function: %s" n

and translate_raise lenv ~loc typ args = match args with
  | [Nolabel, Some arg] ->
      begin match arg.exp_desc with
        | Texp_construct (_, cdesc, args) ->
            (* raise C(a1, .., an)  = >  failwith ("C", a1, .., an) *)
            begin match cdesc.cstr_tag with
              | Cstr_extension (p, _) ->
                  let name =
                    let rec make_name = function
                      | Path.Pident id ->
                          if Ident.persistent id || Ident.is_predef id then Ident.name id
                          else begin
                            match !modname with
                            | None -> assert false
                            | Some n -> n ^ "." ^ Ident.name id
                          end
                      | Pdot (p, s) -> make_name p ^ "." ^ s
                      | Papply _ -> assert false
                    in
                    make_name p
                  in
                  let args = List.map (expression lenv) args in
                  let arg =
                    Binplace.fold
                       ~leaf:(fun c -> c)
                       ~branch:(fun c1 c2 -> mkpair ~loc:arg.exp_loc c1 c2)
                       & Binplace.place
                       & mke ~loc:cdesc.cstr_loc tyString (Const (C.String name)) :: args
                  in
                  mke ~loc typ
                  & Prim ("raise", tyLambda (arg.typ, typ), [arg])
              | _ -> assert false
            end
        | _ -> errorf_constant ~loc "raise takes only an exception constant"
      end
  | _ -> assert false

(* parameter and storage types *)

let toplevel_value_bindings str =
  let rec structure_item st { str_desc; _ } =
    match str_desc with
      | Tstr_value (Nonrecursive, vbs) ->
          List.rev_append vbs st
      | Tstr_module mb ->
          let rec module_expr me = match me.mod_desc with
            | Tmod_structure str -> structure str
            | Tmod_constraint (me, _, _, _) -> module_expr me
            | _ -> unsupported ~loc:mb.mb_loc "module declaration other than simple structure"
          in
          let vbs = module_expr mb.mb_expr in
          st @ vbs
        | _ -> st
  and structure { str_items= sitems } =
    List.rev & List.fold_left (fun st sitem ->
        structure_item st sitem) [] sitems
  in
  structure str

let get_explicit_entries vbs =
  List.filter_map (fun vb ->
      match attr_has_entry_point & Migrate_parsetree__.Migrate_parsetree_409_408_migrate.copy_attributes vb.vb_attributes with
      | None -> None
      | Some (_, name) -> Some (vb, name)) vbs

let get_entries compile_only vbs =
  match get_explicit_entries vbs with
  | [] ->
      if compile_only then []
      else begin
        (* XXX This is obsolete *)
        match List.last vbs with
        | None -> []
        | Some vb -> [vb, None]
      end
  | ents -> ents

let type_check_entry templ vb =
  let unify ty ty' =
    let open Ctype in
    let env = vb.vb_pat.pat_env in
    let loc = vb.vb_pat.pat_loc in
    try unify env ty ty' with
    | Unify trace ->
        wrap_ocaml_exn
          (Typecore.Error(loc, env, Pattern_type_clash(trace, None)))
          310
          ~loc:vb.vb_loc
          "Entry point typing error"
    | Tags(l1,l2) ->
        wrap_ocaml_exn
          (Typetexp.Error(loc, env, Typetexp.Variant_tags (l1, l2)))
          310
          ~loc:vb.vb_loc
          "Entry point typing error"
    | e ->
        internal_error ~loc:vb.vb_loc
          "unify raised something unfamiliar: %s" (Printexc.to_string e)
  in
  unify
    vb.vb_pat.pat_type (* the actual type of the pattern *)
    templ              (* must have this type *)

(*
   Even if the parameter and storage types of the entries are generalized,
   they are canceled by unifing with non-generalized type variables.

   XXX This was ... unintentionally introduced, but useful feature.
   XXX This must be documented.
*)
let type_check_entries tyenv vbns =
  let ty_storage = Ctype.newvar () in
  let ty_entry () =
    let ty_parameter = Ctype.newvar () in
    let path =
      try
        Env.lookup_type (*~loc: *)
          (Longident.(Ldot (Lident "SCaml", "entry"))) tyenv
      with
      | Not_found -> internal_error ~loc:Location.none "Type SCaml.entry is not defined.  Something wrong in your SCaml installation."
    in
    ty_parameter,
    Ctype.newconstr path [ty_parameter; ty_storage]
  in
  List.map (fun (vb,name) ->
      let ty_parameter, templ = ty_entry () in
      type_check_entry templ vb;
      (ty_parameter, vb, name)
    ) vbns,
  ty_storage


let global_parameter_ocaml_type tyenv node =
  let path =
    Env.lookup_type (*~loc: *)
      (Longident.(Ldot (Lident "SCaml", "sum"))) tyenv
  in
  let rec f = function
    | Binplace.Leaf (param_ty, _, _) -> param_ty
    | Branch (n1, n2) ->
        let ty1 = f n1 in
        let ty2 = f n2 in
        Ctype.newconstr path [ty1; ty2]
  in
  f node


let check_self ty_self str =
  let selfs = ref [] in
  let record_self e = selfs := e :: !selfs in
  let iter = { Tast_iterator.default_iterator with
               expr = (fun _it e -> match e.exp_desc with
                   | Texp_ident (p, {loc=_}, _vd) ->
                       begin match Path.is_scaml p with
                         | Some "Contract.self" -> record_self e
                         | _ -> ()
                       end
                   | _ -> ()) }
  in
  iter.structure iter str;

  (* This tries to instantiate Contract.self's type to ty_parameter contract,
     but this does not fully work since we have polymorphism:
       let self = Contract.self

     Only a good way is to add a type constraint around Contract.self
     as (Contract.self : ty_parameter contract) and then retype it,
     which makes the compilation path more complex...
  *)

  List.iter (fun self ->
    let unify ty ty' =
      let open Ctype in
      let tyenv = self.exp_env in
      let loc = self.exp_loc in
      try unify tyenv ty ty' with
      | Unify trace ->
          wrap_ocaml_exn
            (Typecore.Error(loc, tyenv, Expr_type_clash(trace, None, None)))
            510
            ~loc
            "Contract.self typing error"
      | Tags(l1,l2) ->
          wrap_ocaml_exn
            (Typetexp.Error(loc, tyenv, Typetexp.Variant_tags (l1, l2)))
            510
            ~loc
            "Contract.self typing error"
      | e ->
          internal_error ~loc
            "unify raised something unfamiliar: %s" (Printexc.to_string e)
    in
    match (Ctype.expand_head self.exp_env self.exp_type).Types.desc with
    | Tconstr (p, [t], _) when Path.is_scaml p =  Some "contract" ->
        if t.level = Btype.generic_level then
          errorf_self ~loc:self.exp_loc "Contract.self cannot have a generic type, but it has type %a.  Please use a type constraint to instantiate its type." Printtyp.type_scheme self.exp_type;
        unify self.exp_type ty_self
    | _ -> assert false
    ) !selfs


let add_self self_typ t =
  (* let __contract_id = SELF in t *)
  (* This variable must not be inlined *)
  Attr.add (Attr.Annot "not_expand")
  & mklet ~loc:noloc
    { desc= contract_self_id; typ= self_typ; loc= Location.none; attrs= () }
    (mkprim ~loc:noloc self_typ "Contract.self" self_typ [])
    t

let compile_global_entry ty_storage ty_return node =

  let id_storage = Ident.create_local "storage" in
  let pat_storage = mkp ~loc:noloc ty_storage id_storage in
  let e_storage = mkvar ~loc:noloc (id_storage, ty_storage) in

  let rec f param_id node = match node with
    | Binplace.Leaf (_,vb,_name) ->
        (* XXX top entry has poor pattern expressivity *)
        (* id, var: name of the entrypoint *)
        let gloc = Location.ghost vb.vb_loc in
        let id, var = match pattern_simple vb.vb_pat with
          | [p] -> p.desc, mkvar ~loc:p.loc (p.desc, p.typ)
          | _ -> assert false (* XXX error *)
        in
        let param_type = match var.typ with
          | { desc= TyLambda (t1, _); _ } -> t1
          | _ -> assert false
        in
        if not (M.Type.is_parameterable param_type) then begin
          errorf_entry_typing ~loc:var.loc "The entry point has a contract parameter of %a which is not parameterable.  It cannot contain operation.@."
            M.Type.pp param_type
        end;
        let param_var = mkvar ~loc:noloc (param_id, param_type) in
        (* <var> <param_var> <e_storage> *)
        Attr.add (Attr.Comment ("entry " ^ Ident.unique_name id))
        & mke ~loc:gloc ty_return (App (var, [param_var; e_storage])),
        param_type

    | Branch (n1, n2) ->
        let id_l = Ident.create_local "l" in
        let id_r = Ident.create_local "r" in
        let e_l, param_typ_l = f id_l n1 in
        let e_r, param_typ_r = f id_r n2 in
        let pat_l = mkp ~loc:noloc param_typ_l id_l in
        let pat_r = mkp ~loc:noloc param_typ_r id_r in
        let param_typ = tyOr (None,param_typ_l, None,param_typ_r) in
        let param_var = mkvar ~loc:noloc (param_id, param_typ) in
        mke ~loc:noloc ty_return (Switch_or (param_var, pat_l, e_l, pat_r, e_r)),
        param_typ
  in
  let param_id = Ident.create_local "global_param" in
  let e, param_typ = f param_id node in

  (* This is the last defence.  We should check it earlier where the locations
     are known. *)
  if not (M.Type.is_parameterable param_typ) then begin
    errorf_entry_typing ~loc:noloc "Contract's parameter type %a is not parameterable.  It cannot contain operation.@."
      M.Type.pp param_typ
  end;

  (* inserting the definition of self *)
  let e = add_self (tyContract param_typ) e in

  let param_pat = mkp ~loc:noloc param_typ param_id in

  (* fun param_storage ->
       let param = fst param_storage in
       let storage = snd param_storage in
       e
  *)
  let ty_param_storage = tyPair (Some "parameter",param_typ, Some "storage",ty_storage) in
  let id_param_storage = Ident.create_local "param_storage" in
  let pat_param_storage = mkp ~loc:noloc ty_param_storage id_param_storage in
  let e_param_storage = mkvar ~loc:noloc (id_param_storage, ty_param_storage) in
  mkfun ~loc:noloc pat_param_storage
  & mklet ~loc:noloc param_pat (mkfst ~loc:noloc e_param_storage)
  & mklet ~loc:noloc pat_storage (mksnd ~loc:noloc e_param_storage)
  & e

let with_flags_in_code str f =
  let attrs = List.concat @@ List.map snd @@ Attribute.get_scaml_toplevel_attributes str in
  Flags.with_flags
    (fun t -> List.fold_left (fun t ({txt; loc}, v) ->
         Result.at_Error (errorf_flags ~loc "%s") & Flags.eval t (txt, v))
         t attrs) f

let compile_structure sourcefile str =
  let _loc_file = Location.in_file sourcefile in
  let loc = { local_variables= []; non_local_variables= []; fun_loc= Location.none; fun_level= -2} in
  snd & structure loc str

let compile_entry_points compile_only sourcefile str =
  let vbs = toplevel_value_bindings str in
  match get_entries compile_only vbs with
  | [] ->
    if compile_only then None
    else
      errorf_entry ~loc:(Location.in_file sourcefile)
        "SCaml requires at least one value definition for an entry point"
  | entry_vbns ->
      let _entry_ids =
        List.map (fun (vb, _) ->
            match vb.vb_pat.pat_desc with
            | Tpat_var (id, _) -> id
            | Tpat_alias ({ pat_desc = Tpat_any; pat_loc=_ }, id, _) ->
                (* We transform (_ as x) to x if _ and x have the same location.
                   The compiler transforms (x:t) into (_ as x : t).
                   This avoids transforming a warning 27 into a 26.
                 *)
                id
            | Tpat_any -> errorf_entry ~loc:vb.vb_pat.pat_loc "Entrypoint must have a name"
            | _ -> errorf_entry ~loc:vb.vb_pat.pat_loc "Entrypoint must have a simple variable"
          ) entry_vbns
      in
      let tyenv = str.str_final_env in
      let entry_pvbns, ty_storage = type_check_entries str.str_final_env entry_vbns in
      let entry_tree = Binplace.place entry_pvbns in
      let ocaml_ty_param = global_parameter_ocaml_type tyenv entry_tree in

      (* self type *)
      let () =
        let path =
          Env.lookup_type (*~loc: *)
            (Longident.(Ldot (Lident "SCaml", "contract"))) tyenv
        in
        let self_type = Ctype.newconstr path [ocaml_ty_param] in
        check_self self_type str
      in

      (* convert to Michelson types *)

      let ty_operations =
        let ty_operations =
          let path =
            Env.lookup_type (*~loc: *)
              (Longident.(Ldot (Lident "SCaml", "operations"))) str.str_final_env
          in
          Ctype.newconstr path []
        in
        Result.at_Error (fun e ->
            errorf_type_expr ~loc:(Location.in_file sourcefile)
              "SCaml.operations failed to be converted to Michelson type: %a"
              pp_type_expr_error e)
          & type_expr tyenv ty_operations
      in

      let ty_storage =
        let res =
          Result.at_Error (fun e ->
              errorf_type_expr ~loc:(Location.in_file sourcefile) "Contract has storage type %a.  %a"
                Printtyp.type_expr ty_storage
                pp_type_expr_error e)
            & type_expr tyenv ty_storage
        in
        if not & M.Type.is_storable res then
          errorf_entry_typing ~loc:(Location.in_file sourcefile) "Contract has non storable type %a for the storage.@."
            (* XXX show the original type if the expansion is different from it *)
            Printtyp.type_expr (Ctype.full_expand tyenv ty_storage);
        res
      in

      let ty_return = tyPair (Some "operations", ty_operations, Some "storage", ty_storage) in

      let global_entry = compile_global_entry ty_storage ty_return entry_tree in

      let ty_param =
        Result.at_Error (fun e ->
            errorf_type_expr ~loc:(Location.in_file sourcefile) "Contract has parameter type %a.  %a"
              Printtyp.type_expr ocaml_ty_param
              pp_type_expr_error e)
          & type_expr tyenv ocaml_ty_param
      in
      let ty_param = 
        let get_field = function
          | Binplace.Leaf (_,vb,name) ->
              (* XXX dup *)
              let id = match pattern_simple vb.vb_pat with
                | [p] -> p.desc
                | _ -> assert false
              in
              let fix_name s = match name with
                | Some n -> n
                | None -> s
              in
              Some (fix_name (Ident.name id))
          | _ -> None
        in
        let rec add_fields (ty, b) = match ty.M.Type.desc, b with
          | TyOr (_,ty1, _,ty2), Binplace.Branch (n1,n2) ->
              tyOr (get_field n1, add_fields (ty1,n1), get_field n2, add_fields (ty2,n2))
          | _, _ -> ty
        in
        add_fields (ty_param, entry_tree)
      in
      Some (ty_param, ty_storage, global_entry)

let implementation compile_only sourcefile outputprefix str =
    modname := Some (String.capitalize_ascii (Filename.basename outputprefix));
    let entry_points = compile_entry_points compile_only sourcefile str in
    let vbs = compile_structure sourcefile str in
    modname := None;
    entry_points, vbs

let link (ty_param, ty_storage, global_entry) vbs =
  ty_param,
  ty_storage,
  List.fold_right (fun (v,b) x ->
      (* Format.eprintf "subst %s: @[%a@]@." (Ident.unique_name v.desc) IML.pp b; *)
      IML.subst [v.desc, b] x) vbs global_entry

(* Used only for iml dumping *)
let connect vbs =
  let es = List.map (fun ({desc=id; typ}, _) -> mkvar ~loc:Location.none (id, typ)) vbs in
  if es = [] then mkunit ~loc:Location.none ()
  else
    let t =
      Binplace.fold
        ~leaf:(fun c -> c)
        ~branch:(fun c1 c2 -> mkpair ~loc:Location.none c1 c2)
        & Binplace.place es
    in
    let rec f = function
      | [] -> t
      | (p,t)::vbs -> mklet ~loc:Location.none p t (f vbs)
    in
    f vbs

(* convert mode *)
let convert str =
  let attrs = List.concat @@ List.map snd @@ Attribute.get_scaml_toplevel_attributes str in
  Flags.update (fun t -> List.fold_left (fun t ({txt; loc}, v) ->
      Result.at_Error (errorf_flags ~loc "%s") & Flags.eval t (txt, v))
      t attrs);

  let structure_item lenv str_final_env { str_desc; str_loc= loc } =
    match str_desc with
    | Tstr_value (Recursive, _) -> unsupported ~loc "recursive definitions"
    | Tstr_primitive _          -> unsupported ~loc "primitive declaration"
    | Tstr_typext _             -> unsupported ~loc "type extension"
    | Tstr_module _
    | Tstr_recmodule _          -> unsupported ~loc "module declaration"
    | Tstr_class _              -> unsupported ~loc "class declaration"
    | Tstr_class_type _         -> unsupported ~loc "class type declaration"
    | Tstr_include _            -> unsupported ~loc "include"

    | Tstr_modtype _            -> unsupported ~loc "module type declaration"

    | Tstr_eval (e, _) -> [ `Value (None, expression lenv e) ]
    | Tstr_value (Nonrecursive, vbs) ->
        List.map (fun { vb_pat; vb_expr; vb_attributes=_; vb_loc=_loc } ->
            let ido, e = match vb_pat.pat_desc with
              | Tpat_var (id, _) -> (Some id, vb_expr)
              | Tpat_alias ({ pat_desc = Tpat_any; pat_loc=_ }, id, _) -> (Some id, vb_expr)
              | Tpat_any -> (None, vb_expr)
              | _ ->
                  errorf_pattern_match ~loc:vb_pat.pat_loc "Conversion mode does not support complex patterns"
            in
            `Value (ido, expression lenv e)
          ) vbs
    | Tstr_open _open_description -> []
    | Tstr_exception _ -> [] (* XXX *)
    | Tstr_type (_, tds) ->
        List.map (fun td -> match td.typ_params with
            | _::_ -> errorf_type_expr ~loc:td.typ_loc "Conversion mode does not support parameterized type declarations"
            | [] ->
                let id = td.typ_id in
                let ty = Btype.newgenty (Tconstr (Path.Pident td.typ_id, [], ref Types.Mnil)) in
                match type_expr str_final_env ty with
                | Ok x -> `Type (id, x)
                | Error e ->
                    errorf_type_expr ~loc:td.typ_loc "Type %a.  %a" Printtyp.type_expr ty pp_type_expr_error e) tds
    | Tstr_attribute _ -> []
  in
  let structure { str_items= sitems ; str_final_env } =
    List.concat_map (structure_item { local_variables= []; non_local_variables= []; fun_loc= Location.none; fun_level= -1 } str_final_env) sitems
  in
  structure str

let reject_SCaml_attribute_in_complex_structure str =
  let open Typedtree in
  let open Tast_iterator in
  let bad loc = errorf_attribute ~loc "SCaml attributes cannot appear in let module, functors, functor applications and packed modules" in

  let module Reject = struct
    let structure iter str = 
      match Attribute.get_scaml_toplevel_attributes str with
      | [] -> default_iterator.structure iter str
      | (loc,_)::_ -> bad loc
    let iter = { default_iterator with structure }
  end in
  
  let module Check = struct
    let expr iter expr = match expr.exp_desc with
      | Texp_letmodule (_, _, _, me, e) ->
          Reject.iter.module_expr Reject.iter me;
          iter.expr iter e
      | Texp_pack me ->
          Reject.iter.module_expr Reject.iter me
      | _ -> default_iterator.expr iter expr
    let module_expr iter me = match me.mod_desc with
      | Tmod_functor (_, _, _, me) ->
          Reject.iter.module_expr Reject.iter me
      | Tmod_apply (me1, me2, _) ->
          Reject.iter.module_expr Reject.iter me1;
          Reject.iter.module_expr Reject.iter me2
      | _ -> default_iterator.module_expr iter me
    let iter = { default_iterator with expr; module_expr }
  end in
  Check.iter.structure Check.iter str

let filter_by_SCaml_attribute str =
  let open Tast_mapper in
  let open Typedtree in
  let structure mapper str =
    match Attribute.get_scaml_toplevel_attributes str with
    | _::_ -> str
    | [] ->
        let str_items =
          let do_mb mb = match mb.mb_expr.mod_desc with 
            | Tmod_structure str ->
                Some { mb with mb_expr= 
                                 { mb.mb_expr with mod_desc= Tmod_structure (mapper.structure mapper str) } }
            | _ -> None
          in
          List.filter_map (fun sitem -> match sitem.str_desc with
              | Tstr_module mb ->
                  Option.fmap (fun mb -> { sitem with str_desc= Tstr_module mb }) @@ do_mb mb
              | Tstr_recmodule mbs ->
                  begin match List.filter_map do_mb mbs with
                  | [] -> None
                  | mbs -> Some { sitem with str_desc= Tstr_recmodule mbs }
                  end
              | _ -> None) str.str_items
        in
        { str with str_items }
  in
  let mapper = { default with structure } in
  mapper.structure mapper str
