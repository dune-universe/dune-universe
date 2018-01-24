open Ast_405

open Ast_mapper
open Ppxx.Utils
open Ppxx.Helper
open Ppxx.Compilerlib
open Parsetree
open Longident
open Location

let is_m s =
  match Longident.parse s with
  | Lident "m" -> Some None
  | Ldot (l, "m") -> Some (Some l)
  | _ -> None
  | exception _ -> None

let is_do e = match e.pexp_desc with
  | Pexp_ident {txt=Lident "do_"} -> Some None
  | Pexp_ident {txt=Ldot (lid, "do_"); loc} -> Some (Some {txt=lid; loc})
  | _ -> None

let is_ext_do s =
  match Longident.parse s with
  | Lident "do" -> Some None
  | Ldot (l, "do") -> Some (Some l)
  | _ -> None
  | exception _ -> None

let is_do_clause e = match e.pexp_desc with
  | Pexp_extension ({txt; loc}, PStr [{pstr_desc= Pstr_eval (e,_)}]) ->
      begin match is_ext_do txt with
      | None -> None
      | Some None -> Some (None, e)
      | Some (Some lid) -> Some (Some {txt=lid; loc}, e)
      end
  | Pexp_sequence (e1, e2) ->
      (* e1; e2; e3   is  e1; (e2; e3) *)
      begin match is_do e1 with
      | None -> None
      | Some lo -> Some (lo, e2)
      end
  | Pexp_construct ({txt=Lident "::"}, Some ({pexp_desc=Pexp_tuple [e;_]}))
      when is_do e <> None ->
  (* [ [ ...; do_; ..] ]: [do_] inside a list, this is tricky
     therefore we should print a nice error message *)
      raise_errorf ~loc:e.pexp_loc "ppx_monadic: do_ inside a list. Probably you need parens around your do_ sequence like [ e; (do_; e; ..; e); e]"
  | _ ->
      match is_do e with
      | None -> None
      | Some _ ->
          raise_errorf ~loc:e.pexp_loc "ppx_monadic: do_ must be followed by ;"

let desugar_do e =
  let open Monadic in
  let rec f = function
    | End e -> e
    | Bind (Some p, e, t, loc, loc', loc'') ->
        (* p <-- e; t *)
        let bind = with_loc loc & fun () -> [%expr bind] in
        let bind_e = with_loc loc' & fun () -> [%expr [%e bind] [%e e]] in
        let t = f t in
        with_loc loc'' & fun () ->
          [%expr [%e bind_e] (fun [%p p] -> [%e t])]
    | Bind (None, e, t, loc, _loc', loc'') ->
        (* e; t *)
        let bind = with_loc loc & fun () -> [%expr bind] in
        let t = f t in
        with_loc loc'' & fun () ->
          [%expr
              [%e bind] [%e e] (fun () -> [%e t])
          ]
    | Let (g, t) ->
        g & f t
  in
  f & parse None e

let extend super =
  let expr self e =
    match is_do_clause e with
    | Some (None, e) ->
        (* Format.eprintf "do_ clause: %a@." Pprintast.expression e; *)
        self.expr self & desugar_do e
    | Some (Some lid, e) ->
        (* Format.eprintf "do_ clause: %a@." Pprintast.expression e; *)
        (* self.expr self & Exp.open_ Fresh lid & desugar_do e *)
        let m_dot s = Exp.ident {txt= Ldot (lid.txt, s); loc=Location.none} in
        (* let in_m s = Exp.open_ Override lid & Exp.var s in *)
        [%expr
         let bind   = [%e m_dot "bind"] in
         let return = [%e m_dot "return"] in
         (* the following is to surpress Warning 26 *)
         let _ = bind in
         let _ = return in
         (* I believe having fail is not a good idea... *)
         [%e self.expr self & desugar_do e]
        ]

    | None ->
        match e.pexp_desc with
        | Pexp_extension ({txt=x; loc}, PStr [{ pstr_desc= Pstr_eval ({ pexp_desc = Pexp_let(rec_flag, vbs, e')}, _attr) }]) ->
            (* let%xxx p = e1 in e2 *)
            begin match is_m x with
            | None -> super.expr self e
            | Some lidopt ->
                if rec_flag = Recursive then
                  raise_errorf ~loc:e'.pexp_loc "ppx_monadic: let%%m cannot be recursive";
                let bind = match lidopt with
                  | None -> [%expr bind]
                  | Some l -> Exp.ident ~loc {txt= Ldot (l, "bind"); loc}
                in
                let e' = self.expr self e' in
                match vbs with
                | [] -> assert false
                | [vb] ->
                    [%expr
                        [%e bind] [%e vb.pvb_expr] (fun [%p vb.pvb_pat] -> [%e e'])
                    ]
                | _ ->
                    (*
                        (* Code A *)
                        let%m p1 = e1
                        and   p2 = e2
                        in e

                      is not equal to

                        (* Code B *)
                        let%m p1 = e1 in
                        let%m p2 = e2 in
                        e

                      which is equivalent with the following:

                        bind e1 (fun p1 ->
                          bind e2 (fun p2 ->
                            e))

                      Code A should be translated to the following:

                        let ppx_monadic_v1 = e1
                        and ppx_monadic_v2 = e2
                        in
                        bind ppx_monadic_v1 (fun p1 ->
                          bind ppx_monadic_v2 (fun p2 ->
                            e))
                    *)
                    let pevbs = List.map (fun vb ->
                      let e,p =
                        Ppxx.Helper.ExpPat.var ~loc:vb.pvb_loc
                          (Name.make_unique "p_p_x__m_o_n_a_d_i_c")
                      in
                      p,e,vb) vbs
                    in
                    List.fold_right (fun (p,_,vb) st ->
                      [%expr
                        let [%p p] = [%e vb.pvb_expr] in [%e st]
                      ]) pevbs
                    & List.fold_right (fun (_,e,vb) st ->
                      [%expr
                          [%e bind] [%e e] (fun [%p vb.pvb_pat] -> [%e st])
                      ]) pevbs e'
            end
        | Pexp_extension ({txt=x; loc}, PStr [{ pstr_desc= Pstr_eval (({ pexp_desc = Pexp_match(e', cases)} as e0), _attr) }]) ->
            (* match%m e with cases  =>  bind e (function cases) *)
            begin match is_m x with
            | None -> super.expr self e
            | Some lidopt ->
                let bind = match lidopt with
                  | None -> with_loc loc & fun () -> [%expr bind]
                  | Some l -> Exp.ident ~loc {txt= Ldot (l, "bind"); loc}
                in
                let e0' = self.expr self { e0 with pexp_desc = Pexp_function cases } in
                self.expr self &
                  with_loc e0.pexp_loc & fun () ->
                  [%expr
                      [%e bind] [%e e'] [%e e0']
                  ]
            end
        | _ -> super.expr self e
  in
  { super with expr }
