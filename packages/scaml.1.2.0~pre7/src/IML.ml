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
open Tools
open Untyped

module M = Michelson
module Type = M.Type

type ('desc, 'attrs) with_loc_and_type =
  { desc  : 'desc
  ; loc   : Location.t
  ; typ   : Type.t
  ; attrs : 'attrs
  }

module IdTys = Set.Make(struct type t = Ident.t * Type.t let compare (id1,_) (id2,_) = compare id1 id2 end)

module PatVar = struct
  type t = (Ident.t, unit) with_loc_and_type
  
  let pp ppf var = Format.fprintf ppf "%s" (Ident.unique_name var.desc)
end

module Attr = struct
  type t = 
    | Comment of string
    | Annot of string
  
  type ts = t list
  
  let add a t = { t with attrs= a :: t.attrs }
  let adds attrs t = { t with attrs= attrs @ t.attrs }
end

type contract_source =
  | Tz_code of string
  | Tz_file of string

type t = (desc, Attr.ts) with_loc_and_type

and desc =
  | Const of M.Constant.t
  | Nil
  | Cons of t * t
  | IML_None
  | IML_Some of t
  | Left of t
  | Right of t
  | Unit
  | Var of Ident.t
  | Pair of t * t
  | Assert of t
  | AssertFalse
  | Fun of PatVar.t * t
  | IfThenElse of t * t * t option
  | App of t * t list
  | Prim of string * M.Type.t * t list
  | Let of PatVar.t * t * t
  | Switch_or of t * PatVar.t * t * PatVar.t * t
  | Switch_cons of t * PatVar.t * PatVar.t * t * t
  | Switch_none of t * t * PatVar.t * t
  | Contract_create of contract_source * Location.t * t * t * t
  | Seq of t * t
  | Set of t list
  | Map of (t * t) list
  | BigMap of (t * t) list

module P = struct
  (* forge OCaml untyped AST from IML.t just for printing *)
  open Ast_builder
  open Parsetree

  let loc = Location.none

  let rec type_ (ty : M.Type.t) = 
    let open M.Type in
    match ty.desc with
    | TyString -> [%type: string]
    | TyNat    -> [%type: nat]
    | TyInt    -> [%type: int]
    | TyBytes  -> [%type: byte]
    | TyBool   -> [%type: bool]
    | TyUnit   -> [%type: unit]
    | TyList t -> [%type: [%t type_ t] list]
    | TyPair (_,t1, _,t2) -> [%type: [%t type_ t1] * [%t type_ t2]]
    | TyOption (_,t) -> [%type: [%t type_ t] option]
    | TyOr (_,t1, _,t2) -> [%type: ([%t type_ t1], [%t type_ t2]) sum]
    | TySet t -> [%type: [%t type_ t] set]
    | TyMap (k,v) -> [%type: ([%t type_ k], [%t type_ v]) map]
    | TyBigMap (k,v) -> [%type: ([%t type_ k], [%t type_ v]) big_map]
    | TyMutez     -> [%type: tz]
    | TyKeyHash   -> [%type: key_hash]
    | TyTimestamp -> [%type: timestamp]
    | TyAddress   -> [%type: address]
    | TyChainID   -> [%type: chain_id]
  
    | TyKey -> [%type: key]
    | TySignature -> [%type: signature]
    | TyOperation -> [%type: operation]
    | TyContract t -> [%type: [%t type_ t] contract]
    | TyLambda (t1, t2) -> [%type: [%t type_ t1] -> [%t type_ t2]]

  let _type = type_

  let rec constant = 
    let open M.Constant in
    function
    | Unit -> [%expr ()]
    | Bool true -> [%expr true]
    | Bool false -> [%expr false] 
    | Int z ->
        { pexp_desc= Pexp_constant (Pconst_integer (Z.to_string z, None))
        ; pexp_loc= Location.none
        ; pexp_loc_stack= []
        ; pexp_attributes= [] 
        }
    | String s -> estring s
    | Bytes b ->
        let `Hex h = Hex.of_string b in
        [%expr Bytes [%e estring h]]
    | Option None -> [%expr None]
    | Option (Some e) -> [%expr Some [%e constant e]]
    | List ts -> elist & List.map constant ts
    | Set ts -> [%expr Set [%e elist & List.map constant ts]]
    | Map kvs -> 
        [%expr Map [%e elist & List.map (fun (k,v) -> 
            from_Some & pexp_tuple_opt [ constant k ; constant v ]) kvs]]
    | Pair (t1, t2) ->
        from_Some & pexp_tuple_opt [ constant t1 ; constant t2 ]
    | Left t -> [%expr Left [%e constant t]]
    | Right t -> [%expr Right [%e constant t]]
    | Timestamp z -> 
        begin match Ptime.of_float_s (Z.to_float z) with
        | Some t -> 
            [%expr Timestamp [%e estring (Ptime.to_rfc3339 t)]]
        | None ->
            [%expr Timestamp [%e eint (Z.to_int z)]]
        end
    | Code ops -> 
        let s = Format.sprintf "@[<2>{ %a }@]" (Format.list ";@ " M.Opcode.pp) ops in
        { pexp_desc= Pexp_constant (Pconst_string (s, Some "michelson"))
        ; pexp_loc= Location.none
        ; pexp_loc_stack= []
        ; pexp_attributes= [] 
        }

  let rec iml { desc; typ=_ } = match desc with
    | Const c -> constant c
    | Nil -> [%expr []] (* type? *)
    | Cons (t1, t2) -> 
        let t1 = iml t1 in
        let t2 = iml t2 in
        [%expr [%e t1] :: [%e t2]]
    | IML_None -> [%expr None]
    | IML_Some t -> [%expr Some [%e iml t]]
    | Left t -> [%expr Left [%e iml t]]
    | Right t -> [%expr Right [%e iml t]]
    | Unit -> [%expr ()]
    | Var id -> evar (Ident.unique_name id)
    | Pair (t1, t2) ->
        from_Some & pexp_tuple_opt [ iml t1 ; iml t2 ]
    | Assert t ->[%expr assert [%e iml t]]
    | AssertFalse ->[%expr assert false]
    | Fun (pv, t) -> 
        let pv = pvar & Ident.unique_name pv.desc in
        [%expr fun [%p pv] -> [%e iml t] ]
    | IfThenElse (t1, t2, None) ->
        [%expr if [%e iml t1] then [%e iml t2] ]
    | IfThenElse (t1, t2, Some t3) ->
        [%expr if [%e iml t1] then [%e iml t2] else [%e iml t3] ]
    | App (t, ts) -> eapply (iml t) (List.map iml ts)
    | Prim (s, _, ts) -> eapply (evar s) (List.map iml ts)
    | Let (pv, t1, t2) ->
        let ty = type_ pv.typ in
        let pv = pvar & Ident.unique_name pv.desc in
        [%expr let [%p pv] : [%t ty] = [%e iml t1] in [%e iml t2]]
    | Switch_or (t, pv1, t1, pv2, t2) ->
        let pv1 = pvar & Ident.unique_name pv1.desc in
        let pv2 = pvar & Ident.unique_name pv2.desc in
        [%expr match [%e iml t] with
          | Left [%p pv1] -> [%e iml t1]
          | Right [%p pv2] -> [%e iml t2] ]
    | Switch_cons (t, pv1, pv2, t1, t2) ->
        let pv1 = pvar & Ident.unique_name pv1.desc in
        let pv2 = pvar & Ident.unique_name pv2.desc in
        [%expr match [%e iml t] with
          | [%p pv1] :: [%p pv2] -> [%e iml t1]
          | [] -> [%e iml t2] ]
    | Switch_none (t, t1, pv2, t2) ->
        let pv2 = pvar & Ident.unique_name pv2.desc in
        [%expr match [%e iml t] with
          | None -> [%e iml t1]
          | Some [%p pv2] -> [%e iml t2] ]
    | Contract_create (Tz_code code, _, t1, t2, t3) ->
        let code = 
          { pexp_desc= Pexp_constant (Pconst_string (code, Some ""))
          ; pexp_loc= Location.none
          ; pexp_loc_stack= []
          ; pexp_attributes= [] 
          }
        in
        [%expr Contract.create_from_tz_code [%e code] 
            [%e iml t1]
            [%e iml t2]
            [%e iml t3]]
    | Contract_create (Tz_file file, _, t1, t2,t3) ->
        [%expr Contract.create_from_tz_file [%e estring file] 
            [%e iml t1]
            [%e iml t2]
            [%e iml t3]]
    | Seq (t1, t2) -> [%expr [%e iml t1]; [%e iml t2]]
    | Set ts -> [%expr Set [%e elist & List.map iml ts]]
    | Map kvs -> 
        [%expr Map [%e elist & List.map (fun (k,v) -> 
            from_Some & pexp_tuple_opt [ iml k ; iml v ]) kvs]]
    | BigMap kvs -> 
        [%expr BigMap [%e elist & List.map (fun (k,v) -> 
            from_Some & pexp_tuple_opt [ iml k ; iml v ]) kvs]]
        
  let pp ppf t = Pprintast.expression ppf (iml t)
end

let pp = P.pp

let save path t = 
  let oc = open_out path in
  let ppf = Format.of_out_channel oc in
  Format.fprintf ppf "%a@." P.pp t;
  close_out oc

let rec freevars t = 
  let open IdTys in
  let psingleton p = singleton (p.desc, p.typ) in
  let (+) = union in
  match t.desc with
  | Const _ | Nil | IML_None | Unit -> empty
  | Contract_create (_, _,  t1, t2, t3) -> 
      freevars t1 + freevars t2 + freevars t3
  | Cons (t1,t2) | Pair (t1,t2) | Seq (t1,t2) -> union (freevars t1) (freevars t2)
  | Left t | Right t | IML_Some t | Assert t -> freevars t
  | AssertFalse -> empty
  | Var id -> singleton (id, t.typ)
  | IfThenElse (t1,t2,Some t3) -> union (freevars t1) (union (freevars t2) (freevars t3))
  | IfThenElse (t1,t2,None) -> union (freevars t1) (freevars t2)
  | App (t,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty (t::ts)
  | Prim (_,_,ts) ->
      List.fold_left (fun acc t -> union acc (freevars t)) empty ts
  | Fun (pat,t) -> 
      diff (freevars t) (psingleton pat)
  | Let (pat, t1, t2) ->
      diff (union (freevars t1) (freevars t2)) (psingleton pat)
  | Switch_or (t, p1, t1, p2, t2) ->
      union (freevars t)
        (union 
           (diff (freevars t1) (psingleton p1))
           (diff (freevars t2) (psingleton p2)))
  | Switch_cons (t, p1, p2, t1, t2) ->
      union (freevars t)
        (union 
           (diff (diff (freevars t1) (psingleton p1)) (psingleton p2))
           (freevars t2))
  | Switch_none (t, t1, p2, t2) ->
      union (freevars t)
        (union 
           (freevars t1)
           (diff (freevars t2) (psingleton p2)))
  | Set ts -> unions (List.map freevars ts)
  | Map tts -> unions (List.map (fun (t1,t2) -> union (freevars t1) (freevars t2)) tts)
  | BigMap tts -> unions (List.map (fun (t1,t2) -> union (freevars t1) (freevars t2)) tts)

(* t2[t_i/id_i] 
   XXX very inefficient.  should be removed somehow.
*)
let subst id_t_list t2 =
  let rec f t = 
    let mk desc = { t with desc } in
    match t.desc with
    | Var id ->
        begin match List.assoc_opt id id_t_list with
          | None -> t
          | Some t' -> Attr.adds t.attrs t'
        end
    | Const _ | Nil | IML_None | Unit | AssertFalse -> t

    | Contract_create (s, l, t1, t2, t3) -> mk & Contract_create (s, l, f t1, f t2, f t3)

    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (pat, t) -> mk & Fun (pat, f t)
    | Let (p, t1, t2) -> mk & Let (p, f t1, f t2)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | App (t, ts) -> mk & App (f t, List.map f ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Seq (t1, t2) -> mk & Seq (f t1, f t2)
    | Set ts  -> mk & Set (List.map f ts)
    | Map tts -> mk & Map (List.map (fun (k,v) -> f k, f v) tts)
    | BigMap tts -> mk & BigMap (List.map (fun (k,v) -> f k, f v) tts)
  in
  f t2

(* t2[id'_i/id_i] 
   Same as subst, but variables are renamed to variables.
   It keeps the original locations.
   XXX very inefficient.  should be removed somehow.
*)
let alpha_conv id_t_list t2 =
  let rec f t = 
    let mk desc = { t with desc } in
    match t.desc with
    | Var id ->
        begin match List.assoc_opt id id_t_list with
          | None -> t
          | Some id' -> { t with desc= Var id' }
        end
    | Const _ | Nil | IML_None | Unit | AssertFalse -> t
    | Contract_create (cs, l, t1, t2, t3) ->
        mk & Contract_create (cs, l, f t1, f t2, f t3)
    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (pat, t) -> mk & Fun (pat, f t)
    | Let (p, t1, t2) -> mk & Let (p, f t1, f t2)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | App (t, ts) -> mk & App (f t, List.map f ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Seq (t1, t2) -> mk & Seq (f t1, f t2)
    | Set ts  -> mk & Set (List.map f ts)
    | Map tts -> mk & Map (List.map (fun (k,v) -> f k, f v) tts)
    | BigMap tts -> mk & BigMap (List.map (fun (k,v) -> f k, f v) tts)
  in
  f t2

(* Variables with contract, big_map and operations cannot appear freely 
   inside [fun]'s body.
*)
let check_unstorable t = 
  let module E = struct
    exception Found of Ident.t * Michelson.Type.t
  end in
  match t.desc with
  | Fun _ ->
      begin try
        IdTys.iter (fun (id, ty) ->
         if not & Michelson.Type.is_packable ~legacy:false ty then
           raise (E.Found (id, ty)))
        & freevars t;
        Ok ()
      with
      | E.Found (id, ty) -> Error (id, ty)
      end
  | _ -> Ok ()

open Michelson.Type

let mke ~loc typ desc = { typ; desc; loc; attrs= [] }
let mkvar ~loc (id, typ) = mke ~loc typ & Var id
let mklet ~loc p t1 t2 = mke ~loc t2.typ & Let (p, t1, t2)
let mkunit ~loc () = mke ~loc tyUnit Unit
let mkfun ~loc pvar e = mke ~loc (tyLambda (pvar.typ, e.typ)) & Fun (pvar, e)
let mkpair ~loc e1 e2 = mke ~loc (tyPair (None,e1.typ, None,e2.typ)) (Pair (e1, e2))

let mkprim ~loc ty name ty' args =
  match List.assoc_opt name Primitives.primitives with
  | None -> errorf_primitive ~loc "Unknown primitive SCaml.%s" name
  | Some _ -> mke ~loc ty (Prim (name, ty', args))

let mkfst ~loc e =
  let ty = match e.typ.desc with
    | TyPair (_,ty, _,_) -> ty
    | _ -> assert false
  in
  mkprim ~loc ty "fst" (tyLambda (e.typ, ty)) [e]

let mksnd ~loc e =
  let ty = match e.typ.desc with
    | TyPair (_,_, _,ty) -> ty
    | _ -> assert false
  in
  mkprim ~loc ty "snd" (tyLambda (e.typ, ty)) [e]

let mkleft ~loc ty e = mke ~loc (tyOr (None, e.typ, None, ty)) (Left e)
let mkright ~loc ty e = mke ~loc (tyOr (None, ty, None, e.typ)) (Right e)

let mkint ~loc n = mke ~loc tyInt (Const (M.Constant.Int (Z.of_int n)))
let mkcons ~loc h t = mke ~loc t.typ (Cons (h, t))
let mksome ~loc t = mke ~loc (tyOption (None, t.typ)) (IML_Some t)
let mkassert ~loc t = mke ~loc tyUnit & Assert t
let mkassertfalse ~loc ty = mke ~loc ty & AssertFalse

let mkeq ~loc e1 e2 =
  mkprim ~loc tyBool "=" (tyLambda (e1.typ, tyLambda (e2.typ, tyBool))) [e1; e2]
