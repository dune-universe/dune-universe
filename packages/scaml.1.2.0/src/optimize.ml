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

open IML

let rec knorm (t : IML.t) : (IML.PatVar.t * IML.t) list * IML.t =
  let t0 = t in
  let mk defs desc =
    defs, { t0 with desc }
  in
  match t0.desc with
  | Cons (t1,t2) ->
      let defs1, v1 = k t1 in
      let defs2, v2 = k t2 in
      mk (defs1 @ defs2) & Cons (v1, v2)
  | IML_Some t ->
      let defs1, v1 = k t in
      mk defs1 & IML_Some v1
  | Left t ->
      let defs1, v1 = k t in
      mk defs1 & Left v1
  | Right t ->
      let defs1, v1 = k t in
      mk defs1 & Right v1
  | Pair (t1,t2) ->
      let defs1, v1 = k t1 in
      let defs2, v2 = k t2 in
      mk (defs1 @ defs2) & Pair (v1,v2)
  | Assert t ->
      let defs1, v1 = k t in
      mk defs1 & Assert v1
  | Fun _ ->
      (* Keep contiguous abstractions w/o introducing variables *)
      let rec get_absts t = match t.desc with
        | Fun (pv, t) ->
            let pvs, t = get_absts t in
            (pv::pvs), t
        | _ -> [], t
      in
      let pvs, t = get_absts t in
      let add_absts pvs t =
        List.fold_right (mkfun ~loc:Location.none) pvs t
      in
      [], add_absts pvs (knormalize t)
  | IfThenElse (t1, t2, None) ->
      let defs1, v1 = k t1 in
      mk defs1 & IfThenElse (v1, knormalize t2, None)
  | IfThenElse (t1, t2, Some t3) ->
      let defs1, v1 = k t1 in
      mk defs1 & IfThenElse (v1, knormalize t2, Some (knormalize t3))
  | App (t, ts) ->
      let defs, v = k t in
      let defss, vs = List.split (List.map k ts) in
      mk (List.concat (defs :: defss)) & App (v, vs)
  | Prim (n, f, ts) ->
      let defss, vs = List.split (List.map k ts) in
      mk (List.concat defss) & Prim (n, f, vs)
  | Let (pv, t1, t2) ->
      let defs1, t1 = knorm t1 in
      mk defs1 & Let (pv, t1, knormalize t2)
  | Switch_or (t1, pv2, t2, pv3, t3) ->
      let defs1, v1 = k t1 in
      mk defs1 & Switch_or (v1, pv2, knormalize t2, pv3, knormalize t3)
  | Switch_cons (t1, pv1, pv2, t2, t3) ->
      let defs1, v1 = k t1 in
      mk defs1 & Switch_cons (v1, pv1, pv2, knormalize t2, knormalize t3)
  | Switch_none (t1, t2, pv, t3) ->
      let defs1, v1 = k t1 in
      mk defs1 & Switch_none (v1, knormalize t2, pv, knormalize t3)
  | Contract_create (s, loc, t1, t2, t3) ->
      let defs1, v1 = k t1 in
      let defs2, v2 = k t2 in
      let defs3, v3 = k t3 in
      mk (defs1 @ defs2 @ defs3) & Contract_create (s, loc, v1, v2, v3)
  | Seq (t1, t2) ->
      let defs1, _v1 = k t1 in
      defs1, knormalize t2
  | _ -> [], t0

and k t =
  match t.desc with
  | Var _ -> [], t
  | _ ->
      let defs, t = knorm t in
      let i = Ident.create_local "k" in
      let def = { typ= t.typ; loc= t.loc; attrs= (); desc= i }, t in
      defs @ [def], mkvar ~loc:t.loc (i, t.typ)

and build defs v =
  List.fold_right (fun (pv,t) e -> mklet ~loc:Location.none pv t e) defs v

and knormalize t =
  let defs, v = knorm t in
  build defs v

(* let x = y in e  =>  e[y/x] *)
let beta modified exp =
  let rec f env t0 =
    let g = f env in
    let mk desc = { t0 with desc } in
    match t0.desc with
    | Let (pv, ({ desc= Var _} as t1), t2) ->
        modified := true;
        let t1 = g t1 in
        f ((pv.desc,t1)::env) t2
    | Let (pv, t1, t2) -> mk & Let (pv, g t1, g t2)
    | Var id ->
        let rec seek id t =
          match List.assoc_opt id env with
          | Some ({ desc= Var id } as t) ->
              modified := true;
              seek id t
          | Some _ -> t (* we do not map to non vars *)
          | _ -> t
        in
        seek id t0
    | Const _ | Nil | IML_None | AssertFalse -> t0
    | Contract_create (s, l, t1, t2, t3) ->
        mk & Contract_create (s, l, g t1, g t2, g t3)
    | App (t, ts) -> mk & App (g t, List.map g ts)
    | IML_Some t -> mk & IML_Some (g t)
    | Left t -> mk & Left (g t)
    | Right t -> mk & Right (g t)
    | Assert t -> mk & Assert (g t)
    | Fun (c, t) -> mk & Fun (c, g t)
    | Cons (t1, t2) -> mk & Cons (g t1, g t2)
    | Pair (t1, t2) -> mk & Pair (g t1, g t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (g t1, g t2, Some (g t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (g t1, g t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (g t1, p1, g t2, p2, g t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (g t1, p1, p2, g t2, g t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (g t1, g t2, p, g t3)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map g ts)
    | Seq (t1, t2) -> mk & Seq (g t1, g t2)
    | Set ts -> mk & Set (List.map g ts)
    | Map kvs -> mk & Map (List.map (fun (k,v) -> g k, g v) kvs)
    | BigMap kvs -> mk & BigMap (List.map (fun (k,v) -> g k, g v) kvs)
  in
  f [] exp

let assoc modified exp =
  let rec f t0 =
    let mk desc = { t0 with desc } in
    let get_lets t =
      let rec f t = match t.desc with
        | Let (pv, t1, t2) ->
            let lets, body = f t2 in
            (pv,t1)::lets, body
        | _ -> [], t
      in
      f t
    in
    let add_lets lets body =
      if lets <> [] then modified := true;
      List.fold_right (fun (pv,t) e ->
          mklet ~loc:Location.none pv t e) lets body
    in
    match t0.desc with
    | Let (pv, t1, t2) ->
        (* let x =
             let y = ey in
             ex
           in e

           => let y = ey in
              let x = ex in
              e
        *)
        let t1 = f t1 in
        let t2 = f t2 in
        let lets, body = get_lets t1 in
        add_lets lets { t0 with desc= Let (pv, body, t2) }
    | Var _ | Const _ | Nil | IML_None | AssertFalse -> t0
    (* XXX many are already in K normal form therefore f t is likely t *)
    | Contract_create _ -> t0
    | App (t, ts) ->
        (* XXX t and ts are variables.  Should cause no change *)
        let lets, body = get_lets & f t in
        add_lets lets & mk & App (body, List.map f ts)
    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (c, t) -> mk & Fun (c, f t)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (f t1, p1, f t2, p2, f t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (f t1, p1, p2, f t2, f t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (f t1, f t2, p, f t3)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Seq (t1, t2) -> mk & Seq (f t1, f t2)
    | Set ts -> mk & Set (List.map f ts)
    | Map kvs -> mk & Map (List.map (fun (k,v) -> f k, f v) kvs)
    | BigMap kvs -> mk & BigMap (List.map (fun (k,v) -> f k, f v) kvs)
  in
  f exp

let noloc = Location.none

let alpha_conv env t =
  let rec f env t =
    let g = f env in
    let mk desc = { t with desc } in
    let rename_pv env pv =
        let id = pv.desc in
        let id' = Ident.rename id in
        let env = (id,id')::env in
        env, { pv with desc= id' }
    in
    match t.desc with
    | Var id ->
        begin match List.assoc_opt id env with
          | None -> t
          | Some id' -> { t with desc= Var id' }
        end
    | Const _ | Nil | IML_None | AssertFalse -> t
    | Contract_create (cs, l, t1, t2, t3) ->
        mk & Contract_create (cs, l, g t1, g t2, g t3)
    | IML_Some t -> mk & IML_Some (g t)
    | Left t -> mk & Left (g t)
    | Right t -> mk & Right (g t)
    | Assert t -> mk & Assert (g t)
    | Fun (pat, t) ->
        let env, pat = rename_pv env pat in
        mk & Fun ( pat, f env t)
    | Let (p, t1, t2) ->
        let env, p = rename_pv env p in
        mk & Let (p, g t1, f env t2)
    | Cons (t1, t2) -> mk & Cons (g t1, g t2)
    | Pair (t1, t2) -> mk & Pair (g t1, g t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (g t1, g t2, Some (g t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (g t1, g t2, None)
    | Switch_or (t1, p1, t2, p2, t3) ->
        let env, p1 = rename_pv env p1 in
        let env, p2 = rename_pv env p2 in
        mk & Switch_or (g t1, p1, f env t2, p2, f env t3)
    | Switch_cons (t1, p1, p2, t2, t3) ->
        let env, p1 = rename_pv env p1 in
        let env, p2 = rename_pv env p2 in
        mk & Switch_cons (g t1, p1, p2, f env t2, f env t3)
    | Switch_none (t1, t2, p, t3) ->
        let env, p = rename_pv env p in
        mk & Switch_none (g t1, f env t2, p, f env t3)
    | App (t, ts) -> mk & App (g t, List.map g ts)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map g ts)
    | Seq (t1, t2) -> mk & Seq (g t1, g t2)
    | Set ts  -> mk & Set (List.map g ts)
    | Map tts -> mk & Map (List.map (fun (k,v) -> g k, g v) tts)
    | BigMap tts -> mk & BigMap (List.map (fun (k,v) -> g k, g v) tts)
  in
  f env t

let inline modified exp =
  let rec f env t0 =
    let g = f env in
    let mk desc = { t0 with desc } in
    match t0.desc with
    | Let (({desc=i} as _pv), {desc= Fun(pvp,body)}, {desc=App ({desc=Var i'}, t::ts)}) when i = i' ->
        (* let i = fun p -> body in i t ts ...

           We have many of these around the pattern match compilation and want to
           stop code side explosion by keeping the original definition.

           =>  let p = t in let j = body in j ts
        *)
        assert (List.for_all (fun t ->
            match t.desc with
            | Var x -> i' <> x
            | _ -> false) (t::ts));

        let j = Varname.create "j" body.typ in
        let pvj = {desc=j; typ= body.typ; attrs=(); loc=body.loc} in
        let vj = mkvar ~loc:noloc (j,body.typ) in

        g
        & mklet ~loc:noloc pvp t
        & mklet ~loc:noloc pvj body
        & mk & App (vj, ts)

    | Let (pv, t1, t2) ->
        let t1 = g t1 in
        begin match t1.desc with
          | Fun (pv', body) ->
              mk & Let (pv, t1, f ((pv.desc, `Fun (pv',body))::env) t2)
          (* How about Const (List .. | Pair _ | Left _ | Right _ ) ? *)
          | Pair (e1, e2) ->
              mk & Let (pv, t1, f ((pv.desc, `Pair (e1, e2))::env) t2)
          | Left e ->
              mk & Let (pv, t1, f ((pv.desc, `Left e)::env) t2)
          | Right e ->
              mk & Let (pv, t1, f ((pv.desc, `Right e)::env) t2)
          | Cons (e1,e2) ->
              mk & Let (pv, t1, f ((pv.desc, `Cons (e1, e2))::env) t2)
          | Nil ->
              mk & Let (pv, t1, f ((pv.desc, `Nil)::env) t2)
          | IML_Some e ->
              mk & Let (pv, t1, f ((pv.desc, `Some e)::env) t2)
          | IML_None ->
              mk & Let (pv, t1, f ((pv.desc, `None)::env) t2)
          | _ ->
              mk & Let (pv, t1, g t2)
        end
    | App (t, []) -> g t
    | App (({desc= Var id} as tf), ((t'::ts') as ts)) ->
        begin match List.assoc_opt id env with
          | Some (`Fun (pv, body)) ->
              modified := true;
              (* let f = fun pv -> body in ....  f t' ts'

                 let f = fun pv -> body in ...   let pv' = t' in let x = body' in x ts'
                 (pv,body) must be alpha converted
                 (let x = body in x) is required to keep K normal form
              *)
              let id = pv.desc in
              let id' = Ident.rename id in
              let pv = { pv with desc= id' } in
              let body = alpha_conv [(id,id')] body in
              let body =
                match body.desc with
                | Var _ -> body
                | _ ->
                    let i = Varname.create "k" body.typ in
                    mklet ~loc:body.loc {desc=i; typ= body.typ; attrs=(); loc=body.loc} body
                    & mkvar ~loc:body.loc (i, body.typ)
              in
              g (mk & Let (pv, t', mk & App (body, ts')))
          | _ ->
              let ts = List.map g ts in
              mk & App (tf, ts)
        end

    | App (t, ts) -> mk & App (g t, List.map g ts)

    | Prim ("fst", _, {desc= Var id}::ts') ->
        begin match List.assoc_opt id env with
          | Some (`Pair (e1, _)) ->
              modified := true;
              mk & App (e1, ts')
          | _ -> t0
        end
    | Prim ("snd", _, {desc= Var id}::ts') ->
        begin match List.assoc_opt id env with
          | Some (`Pair (_, e2)) ->
              modified := true;
              mk & App (e2, ts')
          | _ -> t0
        end

    | Switch_or (({desc= Var id} as t1), pl, tl, pr, tr) ->
        begin match List.assoc_opt id env with
          | Some (`Left e) ->
              modified := true;
              g & mk & Let (pl, e, g tl)
          | Some (`Right e) ->
              modified := true;
              g & mk & Let (pr, e, g tr)
          | _ ->
              mk & Switch_or (g t1, pl, g tl, pr, g tr)
        end

    | Switch_cons (({desc= Var id} as t1), p1, p2, t2, t3) ->
        begin match List.assoc_opt id env with
          | Some (`Cons (e1,e2)) ->
              modified := true;
              g & mk & Let (p1, e1, mk & Let (p2, e2, g t2))
          | Some `Nil ->
              modified := true;
              g t3
          | _ -> mk & Switch_cons (g t1, p1, p2, g t2, g t3)
        end

    | Switch_none (({desc= Var id} as t1), t2, p, t3) ->
        begin match List.assoc_opt id env with
          | Some (`Some e) ->
              modified := true;
              g & mk & Let (p, e, g t3)
          | Some `None ->
              modified := true;
              g t2
          | _ -> mk & Switch_none (g t1, g t2, p, g t3)
        end

    | Switch_or _ | Switch_cons _ | Switch_none _ -> assert false

    | Var _ | Const _ | Nil | IML_None | AssertFalse -> t0

    | Contract_create (s, l, t1, t2, t3) -> mk & Contract_create (s, l, g t1, g t2, g t3)
    | IML_Some t -> mk & IML_Some (g t)
    | Left t -> mk & Left (g t)
    | Right t -> mk & Right (g t)
    | Assert t -> mk & Assert (g t)
    | Fun (c, t) -> mk & Fun (c, g t)
    | Cons (t1, t2) -> mk & Cons (g t1, g t2)
    | Pair (t1, t2) -> mk & Pair (g t1, g t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (g t1, g t2, Some (g t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (g t1, g t2, None)

    | Prim (a, b, ts) -> mk & Prim (a, b, List.map g ts)
    | Seq (t1, t2) -> mk & Seq (g t1, g t2)
    | Set ts -> mk & Set (List.map g ts)
    | Map kvs -> mk & Map (List.map (fun (k,v) -> g k, g v) kvs)
    | BigMap kvs -> mk & BigMap (List.map (fun (k,v) -> g k, g v) kvs)
  in
  f [] exp

(* Eliminate [(fun [@fun_tmp] x -> ...) e] => [let x = e in ...],
   introduced at pattern match compilation.
*)
let inline_pmatch exp =
  let rec f t0 =
    let mk desc = { t0 with desc } in
    match t0.desc with
    | App (t, ts) ->
        let t = f t in
        let ts = List.map f ts in
        begin match t, ts with
          | {desc= Fun (pv,body)}, t :: ts ->
              mk & App (mklet ~loc:t0.loc pv t body, ts)
          | _, [] -> t
          | _ -> mk  & App (t, ts)
        end

    | Let (pv, t1, t2) -> mk & Let (pv, f t1, f t2)

    | Switch_or (t1, pl, tl, pr, tr) ->
        mk & Switch_or (f t1, pl, f tl, pr, f tr)

    | Switch_cons (t1, p1, p2, t2, t3) ->
        mk & Switch_cons (f t1, p1, p2, f t2, f t3)

    | Switch_none (t1, t2, p, t3) ->
        mk & Switch_none (f t1, f t2, p, f t3)

    | Var _ | Const _ | Nil | IML_None | AssertFalse -> t0

    | Contract_create (s, l, t1, t2, t3) -> mk & Contract_create (s, l, f t1, f t2, f t3)
    | IML_Some t -> mk & IML_Some (f t)
    | Left t -> mk & Left (f t)
    | Right t -> mk & Right (f t)
    | Assert t -> mk & Assert (f t)
    | Fun (c, t) -> mk & Fun (c, f t)
    | Cons (t1, t2) -> mk & Cons (f t1, f t2)
    | Pair (t1, t2) -> mk & Pair (f t1, f t2)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (f t1, f t2, Some (f t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (f t1, f t2, None)

    | Prim (a, b, ts) -> mk & Prim (a, b, List.map f ts)
    | Seq (t1, t2) -> mk & Seq (f t1, f t2)
    | Set ts -> mk & Set (List.map f ts)
    | Map kvs -> mk & Map (List.map (fun (k,v) -> f k, f v) kvs)
    | BigMap kvs -> mk & BigMap (List.map (fun (k,v) -> f k, f v) kvs)
  in
  f exp

let may_have_effect t =
  let rec f t = match t.desc with
    | Const _ | Nil | IML_None | Var _ -> false
    | Fun _ -> false

    (* args are variables or constants *)
    | IML_Some _ | Left _ | Right _
    | Cons _ | Pair _
    | Contract_create _
    | Set _
    | Map _
    | BigMap _ -> false

    | Assert _
    | AssertFalse
    | App _ -> true

    | Prim (s, _, _) ->
        begin match List.assoc_opt s Primitives.primitives with
          | None -> assert false
          | Some (is_pure, _, _) -> not is_pure
        end

    | IfThenElse (_, t1, None) -> f t1
    | IfThenElse (_, t1, Some t2)
    | Let (_, t1, t2)
    | Switch_or (_, _, t1, _, t2)
    | Switch_cons (_, _, _, t1, t2)
    | Switch_none (_, t1, _, t2)
    | Seq (t1, t2) -> f t1 || f t2
  in
  f t

let elim modified exp =
  let module S = Set.Make(struct type t = Ident.t let compare = compare end) in
  let (+) = S.union in
  let rec f t0 =
    let mk desc = { t0 with desc } in
    match t0.desc with
    | Let (pv, t1, t2) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        if S.mem pv.desc ids2 || may_have_effect t1 then
          ids1 + S.remove pv.desc ids2,
          mk & Let (pv, t1, t2)
        else begin
          modified := true;
          S.remove pv.desc ids2, t2
        end
    | Var id -> S.singleton id, t0
    | Const _ | Nil | IML_None | AssertFalse -> S.empty, t0
    | Contract_create (a, b, t1, t2, t3) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        let ids3, t3 = f t3 in
        ids1 + ids2 + ids3,
        mk & Contract_create (a, b, t1, t2, t3)
    | App (t, ts) ->
        let ids, t = f t in
        let idss, ts = List.split & List.map f ts in
        S.unions (ids :: idss), mk & App (t, ts)
    | IML_Some t ->
        let ids, t = f t in
        ids, mk & IML_Some t
    | Left t ->
        let ids, t = f t in
        ids, mk & Left t
    | Right t ->
        let ids, t = f t in
        ids, mk & Right t
    | Assert t ->
        let ids, t = f t in
        ids, mk & Assert t
    | Fun (c, t) ->
        let ids, t = f t in
        ids, mk & Fun (c, t)
    | Cons (t1, t2) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        ids1 + ids2, mk & Cons (t1, t2)
    | Pair (t1, t2) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        ids1 + ids2, mk & Pair (t1, t2)
    | IfThenElse (t1, t2, Some t3) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        let ids3, t3 = f t3 in
        ids1 + ids2 + ids3, mk & IfThenElse (t1, t2, Some t3)
    | IfThenElse (t1, t2, None) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        ids1 + ids2, mk & IfThenElse (t1, t2, None)
    | Switch_or (t1, p1, t2, p2, t3) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        let ids3, t3 = f t3 in
        ids1 + ids2 + ids3, mk & Switch_or (t1, p1, t2, p2, t3)
    | Switch_cons (t1, p1, p2, t2, t3) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        let ids3, t3 = f t3 in
        ids1 + ids2 + ids3, mk & Switch_cons (t1, p1, p2, t2, t3)
    | Switch_none (t1, t2, p, t3) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        let ids3, t3 = f t3 in
        ids1 + ids2 + ids3, mk & Switch_none (t1, t2, p, t3)
    | Prim (a, b, ts) ->
        let idss, ts = List.split & List.map f ts in
        S.unions idss, mk & Prim (a, b, ts)
    | Seq (t1, t2) ->
        let ids1, t1 = f t1 in
        let ids2, t2 = f t2 in
        ids1 + ids2, mk & Seq (t1, t2)
    | Set ts ->
        let idss, ts = List.split & List.map f ts in
        S.unions idss, mk & Set ts
    | Map kvs ->
        let idss, kvs =
          List.split & List.map (fun (k,v) ->
              let idsk, k = f k in
              let idsv, v = f v in
              idsk + idsv, (k,v)) kvs
        in
        S.unions idss, mk & Map kvs
    | BigMap kvs ->
        let idss, kvs =
          List.split & List.map (fun (k,v) ->
              let idsk, k = f k in
              let idsv, v = f v in
              idsk + idsv, (k,v)) kvs
        in
        S.unions idss, mk & BigMap kvs
  in
  snd & f exp


module VMap = Map.Make(struct type t = Ident.t let compare = compare end)

(* only_free : We only count free vars or not *)
let count_variables only_free t =
  let incr v typ st = match VMap.find_opt v st with
    | None -> VMap.add v (1,typ) st
    | Some (n,_) -> VMap.add v (n+1,typ) st (* hope type sare identical *)
  in
  let remove pv st = if only_free then VMap.remove pv.desc st else st in
  let rec f t st = match t.desc with
    | Var id -> incr id t.typ st

    | Const _ | Nil | IML_None | AssertFalse -> st

    | IML_Some t | Left t | Right t | Assert t -> f t st
    | Fun (pv, t) -> remove pv & f t st
    | Let (pv, t1, t2) -> remove pv & f t1 & f t2 st

    | Cons (t1, t2) | Pair (t1, t2) | Seq (t1, t2)
    | IfThenElse (t1, t2, None) -> f t1 & f t2 st

    | Contract_create (_, _, t1, t2, t3)
    | IfThenElse (t1, t2, Some t3) -> f t1 & f t2 & f t3 st

    | Switch_or (t1, pv1, t2, pv2, t3)
    | Switch_cons (t1, pv1, pv2, t2, t3) ->
        remove pv1 & remove pv2 & f t1 & f t2 & f t3 st

    | Switch_none (t1, t2, pv1, t3) ->
        remove pv1 & f t1 & f t2 & f t3 st

    | App (t, ts) -> List.fold_right f (t::ts) st
    | Prim (_, _, ts)
    | Set ts  -> List.fold_right f ts st
    | Map tts
    | BigMap tts ->
        List.fold_right (fun (t1,t2) st -> f t1 & f t2 st) tts st
  in
  f t VMap.empty

let unknorm exp =
  let vmap = count_variables false exp in
  let expanded_to_non_storable =
    let cache = ref VMap.empty in
    fun id t ->
      match VMap.find_opt id !cache with
      | None ->
          let res =
            VMap.exists (fun _ (_, typ) -> not & Michelson.Type.is_storable typ)
            & count_variables true t
          in
(*
          Format.eprintf "%s is not expandable inside fun@."
            (Ident.unique_name id);
*)
          cache := VMap.add id res !cache;
          res
      | Some res -> res
  in
  let rec f depth env t0 =
    let g = f depth env in
    let mk desc = { t0 with desc } in
    match t0.desc with
    | Var _ when t0.typ = Michelson.Type.tyUnit -> (* (x : unit) => () *)
        mkunit ~loc:t0.loc ()

    | Let (pv, t1, t2) when pv.typ = Michelson.Type.tyUnit ->
        begin match t1.desc with
          | Const Unit -> g t2 (* let unit = () in t2 => t2 *)
          | _ -> mk & Seq (g t1, g t2) (* let unit = e in t2 => e; t2 *)
        end

    | Let (pv, ({ desc=Prim ("Contract.self", _, _)} as t1), t2) ->
        (* We cannot expand it *)
        mk & Let (pv, t1, g t2)

    | Let ({desc= v}, t1, { desc= Var v' }) when v = v' ->
        (* let x = e in x   =>   e *)
        g t1

    | Let (pv, t1, t2) ->
        begin match VMap.find_opt pv.desc vmap with
          | _ when may_have_effect t1 -> mk & Let (pv, g t1, g t2)
          | Some (1,_) ->
              (* XXX If application is really pure, we should expand it... *)
              let t1 = g t1 in
              let env = (pv.desc,t1)::env in
              (* We cannot simply remove Let since some variables cannot be expanded ... *)
              mk & Let (pv, t1, f depth env t2)
          | None -> g t2
          | _ -> mk & Let (pv, g t1, g t2)
        end
    | App (t, ts) ->
        mk & App (g t, List.map g ts)
    | Var id ->
        begin match List.assoc_opt id env with
          | Some t ->
              if depth > 1 &&
                 expanded_to_non_storable id t
              then t0
              else f depth env t
          | None -> t0
        end
    | Fun (c, t) -> mk & Fun (c, f (depth + 1) env t)
    | Const _ | Nil | IML_None | AssertFalse -> t0
    | IML_Some t -> mk & IML_Some (g t)
    | Left t -> mk & Left (g t)
    | Right t -> mk & Right (g t)
    | Assert t -> mk & Assert (g t)
    | Cons (t1, t2) -> mk & Cons (g t1, g t2)
    | Pair (t1, t2) -> mk & Pair (g t1, g t2)
    | Contract_create (s, l, t1, t2, t3) ->
        mk & Contract_create (s, l, g t1, g t2, g t3)
    | IfThenElse (t1, t2, Some t3) -> mk & IfThenElse (g t1, g t2, Some (g t3))
    | IfThenElse (t1, t2, None) -> mk & IfThenElse (g t1, g t2, None)
    | Switch_or (t1, p1, t2, p2, t3) -> mk & Switch_or (g t1, p1, g t2, p2, g t3)
    | Switch_cons (t1, p1, p2, t2, t3) -> mk & Switch_cons (g t1, p1, p2, g t2, g t3)
    | Switch_none (t1, t2, p, t3) -> mk & Switch_none (g t1, g t2, p, g t3)
    | Prim (a, b, ts) -> mk & Prim (a, b, List.map g ts)
    | Seq (t1, t2) -> mk & Seq (g t1, g t2)
    | Set ts -> mk & Set (List.map g ts)
    | Map kvs -> mk & Map (List.map (fun (k,v) -> g k, g v) kvs)
    | BigMap kvs -> mk & BigMap (List.map (fun (k,v) -> g k, g v) kvs)
  in
  f 0 [] exp

let unknormalize exp = elim (ref false) & unknorm exp
