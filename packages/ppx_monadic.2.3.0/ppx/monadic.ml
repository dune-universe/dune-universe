open Ast_405
open Parsetree
open Ppxx.Helper
open Location

(* Monadic bind syntax sugar can be reusable
   also for pattern guards and comprehensions *)
     
type t =
  | End of expression
  | Bind of pattern option * expression * t * Location.t (* <-- *) * Location.t (* p <-- e *) * Location.t (* p <-- e; .. *)
  | Let of (expression -> expression) * t

let is_bind e = match e.pexp_desc with
  | Pexp_apply({ pexp_desc= Pexp_ident({txt=Lident "<--"; loc}) }, [(Nolabel,p); (Nolabel,e)]) ->
      (* p <-- e *)
      begin match Pat.of_expr p with
      | `Ok p -> Some (p, e, loc, e.pexp_loc)
      | `Error e ->
          raise_errorf ~loc:e.pexp_loc "The left hand side of <-- must be a pattern"
      end
  | Pexp_apply({ pexp_desc= Pexp_ident({txt=Lident "<--"}) }, _) ->
      raise_errorf ~loc:e.pexp_loc "Syntax error of <--. It must have the form p <-- e"
  | _ -> None

let is_unit e = match e.pexp_desc with
  | Pexp_construct ({txt=Lident "()"}, None) -> true
  | _ -> false

(* parse a bind sequence

   * [e]: the sequence 
   * [final]: somethings follows after [e] 
*)
let rec parse final e = match e.pexp_desc with
  | Pexp_sequence ( u, ({ pexp_desc= Pexp_sequence( e1, e2 ) } as e')) when is_unit u ->
      (* (); e1; e2   ==   e1; e2'  *)
      Let ( (fun e2' -> { e' with pexp_desc = Pexp_sequence (e1, e2') }),
            parse final e2 )

  | Pexp_sequence ( u, e ) when is_unit u -> 
      (* <(); e> => e *) 
      begin match final with
      | None -> End e
      | Some f -> End (Exp.sequence e f)
      end

  | Pexp_sequence ({ pexp_desc = Pexp_sequence _; pexp_loc=loc }, _) ->
      (* <do_; ..; (e1; e2); ..> => error *) 
      raise_errorf ~loc "The clause cannot take a nested sequence. It must be flattened"
    
  | Pexp_sequence (e1, e2) ->
      begin match is_bind e1 with
      | Some (p, e', loc, loc') ->
          (* <p <-- e1; e2> => bind e1 (fun p -> <e2>)  *)
          Bind (Some p, e', parse final e2, loc, loc', e.pexp_loc)
      | None ->
          (* <e1; e2> => bind e1 (fun () -> <e2>) *)
          Bind (None, e1, parse final e2,
                { e1.pexp_loc
                  with loc_start = e1.pexp_loc.loc_end;
                       loc_end = e2.pexp_loc.loc_start },
                { e1.pexp_loc
                  with loc_end = e2.pexp_loc.loc_start },
                e.pexp_loc)
      end

  | Pexp_let(rf, vbs, e2) ->
      (* <let p = e1 in e2> => let p = e1 in <e2> *)
      Let ( (fun e2 -> { e with pexp_desc = Pexp_let (rf, vbs, e2) }),
            parse final e2 )

  | Pexp_letmodule(s, mexp, e2) ->
      (* <let module M = mexp in e2> => let module M = mexp in <e2> *)
      Let ( (fun e2 -> { e with pexp_desc = Pexp_letmodule(s, mexp, e2) }),
            parse final e2 )

  | Pexp_open(ov, lid, e2) ->
      (* <let open M in e2> => let open M in <e2> *)
      Let ( (fun e2 -> { e with pexp_desc = Pexp_open(ov, lid, e2) }),
            parse final e2)

  | Pexp_extension ( ext,
                     PStr [({ pstr_desc= Pstr_eval (e2, attr) } as sitem)]) ->
      (* <let%m p = e1 in e2> => let%m p = e1 in <e2> 
            
         If you do not want ppx_monadic invades in your extension,
         use (); [%your_ext ..];
      *)
      Let ( (fun e2 -> 
        { e with pexp_desc =
            Pexp_extension ( ext,
                             PStr [{ sitem with pstr_desc = Pstr_eval ( e2, attr) }]) } ),
            parse final e2)

  | _ ->
      match is_bind e with
      | Some (p, e', loc, loc') ->
          begin match final with
          | None ->
              (* p <-- e' <end> *)
              raise_errorf ~loc:e.pexp_loc
                "bind (p <-- e) cannot appear at the end of this clause"
          | Some f ->
              (* ex. the end of a comprehension *)
              Bind (Some p, e', End f, loc, loc', e.pexp_loc) 
          end
      | None ->
          match final with
          | None -> End e
          | Some f ->
              (* ex. the end of a comprehension *)
              Bind (None, e, End f,
                    { e.pexp_loc with loc_start = e.pexp_loc.loc_end },
                    e.pexp_loc,
                    e.pexp_loc)
