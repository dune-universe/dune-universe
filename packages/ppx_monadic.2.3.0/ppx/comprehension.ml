open Ast_405

open Parsetree
open Ppxx.Helper
open Ppxx.Utils
open Monadic
open Location

let rec desugar = function
  | End e -> [%expr return [%e e]]
  | Bind (Some p, e, t, loc, _loc', loc'') ->
      (* p <-- e; t *)
      let bind = with_loc loc & fun () -> [%expr bind] in
      with_loc loc'' & fun () ->
        [%expr
            [%e bind] [%e e] (function
              | [%p p] when true -> [%e desugar t]
              | _ -> mzero)
        ]
  | Bind (None, e, t, _loc, _loc', loc'') ->
      (* e; t *)
      with_loc loc'' & fun () ->
        [%expr
            if [%e e] then [%e desugar t] else mzero
        ]
  | Let (f, t) -> f & desugar t

let is_comp s =
  match Longident.parse s with
  | Lident "comp" -> Some None
  | Ldot (l, "comp") -> Some (Some l)
  | _ -> None
  | exception _ -> None

let is_barred e = match e.pexp_desc with
  | Pexp_apply ({pexp_desc = Pexp_ident {txt=Lident "||"}},
                [(Nolabel,e1);
                 (Nolabel,e2)]) -> Some (e1, e2)
  | _ -> None

let is_head_barred e = match e.pexp_desc with
  | Pexp_sequence (e1, e2) ->
      begin match is_barred e1 with
      | None -> None
      | Some (ret, e1) -> Some (ret, { e with pexp_desc = Pexp_sequence (e1, e2) })
      end
  | _ ->
      begin match is_barred e with
      | None -> None
      | Some (ret, e1) -> Some (ret, e1)
      end

let is_comprehension e = match e.pexp_desc with
  | Pexp_extension({txt}, PStr[{ pstr_desc= Pstr_eval(e, _attr) }]) ->
      begin match is_comp txt with
      | None -> None
      | Some lidopt ->
          match is_head_barred e with
          | None -> 
              raise_errorf ~loc:e.pexp_loc "ppx_monadic: [%%comp e || ..] requires ||"
          | Some (ret, seq) -> Some (lidopt, ret, seq)
      end
  | _ -> None

open Ast_mapper

let extend super =
  let expr self e = 
    match is_comprehension e with
    | None -> super.expr self e
    | Some (None, ret, seq) ->
        [%expr
            let mzero = [] in
            let bind e f = List.concat (List.map f e) in
            let return x = [x] in
            let _ = mzero in
            let _ = bind in
            let _ = return in
            [%e self.expr self & desugar & Monadic.parse (Some ret) seq ]
        ]
    | Some (Some lid, ret, seq) -> 
        let m_dot s = Exp.ident {txt= Ldot (lid, s); loc=Location.none} in
        [%expr
         let mzero =  [%e m_dot "mzero" ] in
         let bind =   [%e m_dot "bind" ] in
         let return = [%e m_dot "return" ] in
         let _ = mzero in
         let _ = bind in
         [%e self.expr self & desugar & Monadic.parse (Some ret) seq ]
        ]
  in
  { super with expr }
