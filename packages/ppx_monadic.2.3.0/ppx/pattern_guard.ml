open Ast_405

open Parsetree
open Ppxx.Helper
open Ppxx.Utils

(*

| p when ... -> e'

=>   | p when 
           (Option.do_;
              ...  (* where non binding expr [e] is considered as  [if e then None else return ()] *)
              return (res := Some e')) <> None
       -> match !res with Some v -> v | _ -> assert false

*)


(*
In Pure OCaml with extension

w ::= when a

a ::= -> e
    | e ; a
    | p <-- e ; a   (or [%p? p] <-- e; a )

desugar (-> e) = 
    res := Some e; true -> (match !res with Some v -> v | None -> assert false)

desugar (e ; a) =
    e && e'
    where
      e' = desugar a

desugar (p <-- e ; a) =
    (match e with p -> e' | _ -> false)
    where
      e' = desugar a

And outside of match with pattern guards, we have:

let res = ref None in match ..
*)

open Monadic

let rec need_desugar_guard = function
  | End _ -> false
  | Bind (None, _e, End _e', _loc, _loc', _loc'') ->
      (* when e -> e' *)
      false
  | Bind _ ->
      (* when e1; e2 -> _e' *)
      true
  | Let (_, t) -> need_desugar_guard t

let rec desugar_guard r = function
  | End e ->
      [%expr [%e r] := Some [%e e]; true]
  | Bind (Some p, e, t, _loc, _loc', loc'') ->
      (* p <-- e; t *)
      with_loc loc'' & fun () ->
        [%expr
            match [%e e] with
            | [%p p] when true (* to surpress possible warning 11 of the default case *) -> [%e desugar_guard r t]
            | _ -> false
        ]
  | Bind (None, e, t, loc, _loc', loc'') ->
      (* e; t *)
      let and_ = with_loc loc & fun () -> [%expr (&&)] in
      with_loc loc'' & fun () ->
        [%expr [%e and_] [%e e] [%e desugar_guard r t] ]
  | Let (f, t) ->
      f & desugar_guard r t

let desugar_case r case =
  match case.pc_guard with
  | None -> false, case
  | Some g ->
      let g = Monadic.parse (Some case.pc_rhs) g in
      if not (need_desugar_guard g) then false, case
      else 
        true,
        { case with
          pc_guard = Some (desugar_guard r g);
          pc_rhs = [%expr match ![%e r] with Some v -> v | None -> assert false]
        }

let desugar_cases cs =
  let n = Name.make_unique "p_a_t_t_e_r_n__g_u_a_r_d" in
  let r = Exp.var n in
  let bs, cs' = List.(split & map (desugar_case r) cs) in
  let need_ref = List.(fold_left (||) (hd bs) (tl bs)) in
  if not need_ref then None
  else
    Some (fun f -> 
      [%expr
       let [%p Pat.var' n] = ref None in
       [%e f cs']
      ])
  
open Ast_mapper

let extend super = 
  let expr self e =
    match e.pexp_desc with
    | Pexp_match (e', cs) ->
        begin match desugar_cases cs with
        | None -> super.expr self e
        | Some f ->
            (* We must be very careful about how to process
               desugared match, since its cases contain
               [r := e; true], which must not be treated as do-action.
            *)
            f (fun cs -> super.expr self { e with pexp_desc = Pexp_match( e', cs ) } )
        end
    | Pexp_try (e', cs) ->
        begin match desugar_cases cs with
        | None -> super.expr self e
        | Some f ->
            f (fun cs -> super.expr self { e with pexp_desc = Pexp_try( e', cs ) } )
        end
    | Pexp_function cs ->
        begin match desugar_cases cs with
        | None -> super.expr self e
        | Some f ->
            f (fun cs -> super.expr self { e with pexp_desc = Pexp_function cs })
        end
    | _ -> super.expr self e
  in
  { super with expr }
