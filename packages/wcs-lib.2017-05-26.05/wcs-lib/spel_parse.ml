(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

open Spel_t
open Spel_sedlexer_j

exception LexError of string

let error default msg = Log.error "Spel parsing" default msg
let warning default msg = Log.warning "Spel parsing" msg; default

type state = BodyLexing | ExprLexing

(** {6 Expression lexer} *)
let expr_lexer buff lb =
  Spel_sedlexer_j.token buff lb

let mk_expr_lexer () =
  expr_lexer (Spel_util.string_buff ())

(** {6 lexing} *)
let body_lexer st buff lb =
  begin match !st with
  | BodyLexing ->
      let tk = Spel_sedlexer_j.body buff lb in
      begin match tk with
      | Spel_parser_j.OPENEXPR _ -> st := ExprLexing ; tk
      | _ -> tk
      end
  | ExprLexing ->
      let tk = Spel_sedlexer_j.token buff lb in
      begin match tk with
      | Spel_parser_j.CLOSEEXPR -> st := BodyLexing ; tk
      | _ -> tk
      end
  end

let mk_body_lexer () =
  body_lexer (ref BodyLexing) (Spel_util.string_buff ())

(** {6 desugaring} *)

let rec fold_expr f e =
  let desc =
    begin match e.expr_desc with
    | E_lit l -> E_lit l
    | E_prop (e, s) -> E_prop (fold_expr f e, s)
    | E_prop_catch (e, s) ->
        E_prop_catch (fold_expr f e, s)
    | E_get (e1, e2) ->
        E_get (fold_expr f e1,
               fold_expr f e2)
    | E_list el ->
        E_list (List.map (fold_expr f) el)
    | E_new_array (t, ilo, elo) ->
        begin match elo with
        | None ->
            E_new_array (t, ilo,
                         None)
        | Some el ->
            E_new_array (t, ilo,
                         Some (List.map (fold_expr f) el))
        end
    | E_new (t, el) ->
        E_new (t, List.map (fold_expr f) el)
    | E_call (eo, s, el) ->
        begin match eo with
        | None ->
            E_call (None, s,
                    List.map (fold_expr f) el)
        | Some e ->
            E_call (Some (fold_expr f e), s,
                    List.map (fold_expr f) el)
        end
    | E_call_catch (eo, s, el) ->
        begin match eo with
        | None ->
            E_call_catch (None, s,
                          List.map (fold_expr f) el)
        | Some e ->
            E_call_catch (Some (fold_expr f e), s,
                          List.map (fold_expr f) el)
        end
    | E_op (op, el) ->
        E_op (op, List.map (fold_expr f) el)
    | E_conditional (e1, e2, e3) ->
        E_conditional (fold_expr f e1,
                       fold_expr f e2,
                       fold_expr f e3)
    | E_ident s -> E_ident s
    (* WCS extensions *)
    | E_anything_else -> E_anything_else
    | E_context -> E_context
    | E_conversation_start -> E_conversation_start
    | E_entities -> E_entities
    | E_input -> E_input
    | E_intents -> E_intents
    | E_output -> E_output
    | E_variable s -> E_variable s
    | E_intent s -> E_intent s
    | E_entity (s1, s2) -> E_entity (s1, s2)
    (* Fallback *)
    | E_error s -> E_error s
    end
  in
  let desc = f desc in
  let loc = e.expr_loc in
  let text = e.expr_text in
  Spel_util.mk_expr_full desc loc text

let refresh_expr_text e =
  begin match e.expr_text with
  | None -> ()
  | Some t ->
      e.expr_text <- Some (Spel_print.to_string e)
  end

let desugar_spel = ref false

let desugar_desc e =
  begin match e with
  (* Shorthand syntax for variables *)
  | E_variable (v, None) ->
      E_get (Spel_util.mk_expr E_context,
             Spel_util.mk_expr (E_lit (L_string v)))
  | E_variable (v, Some s) ->
      E_op (Op_eq,
            [Spel_util.mk_expr
               (E_get (Spel_util.mk_expr E_context,
                       Spel_util.mk_expr (E_lit (L_string v))));
             Spel_util.mk_expr
               (E_lit (L_string s))])
  | _ -> e
  end

let desugar e =
  fold_expr desugar_desc e

let resugar_spel = ref false

let resugar_desc e =
  begin match e with
  (* Shorthand syntax for variables *)
  | E_get ({ expr_desc = E_context },
           { expr_desc = E_lit (L_string s) }) ->
      E_variable (s,None)
  (* XXX Keeps equality intact when resugaring
     | E_op (Op_eq,
          [{ expr_desc = E_get ({ expr_desc = E_context },
                                { expr_desc = (E_lit (L_string v)) }) };
           { expr_desc = E_lit (L_string s) }]) ->
      E_variable (v, Some s) XXX *)
  | _ -> e
  end

let resugar e =
  fold_expr resugar_desc e

let sugarer e =
  let e = if !desugar_spel then desugar e else e in
  let e = if !resugar_spel then resugar e else e in
  e

(** {6 parsers} *)

let fix_empty_condition ocond =
  begin match ocond with
  | Some cond -> cond
  | None -> Spel_util.mk_expr (E_lit (L_boolean false))
  end

let expression_from_file f =
  fix_empty_condition
    (Spel_util.uparse_file
       Spel_parser_j.condition_main (mk_expr_lexer ()) f)

let expr_from_file s =
  sugarer (expression_from_file s)

let expression_from_string s =
  begin try
    let parsed =
      Spel_util.uparse_string
        Spel_parser_j.condition_main
        (mk_expr_lexer ())
        s
    in
    let ast = fix_empty_condition parsed in
    ast.expr_text <- Some s;
    ast
  with
  | LexError msg ->
      warning (Spel_util.mk_expr_text (E_error msg) (Some s))
        (Format.sprintf "[%s] in expression: '%s'" msg s)
  | _ ->
      warning
        (Spel_util.mk_expr_text
           (E_error "Parse error in expression")
           (Some s))
        (Format.sprintf "error in expression: '%s'" s)
  end

let expr_from_string s =
  sugarer (expression_from_string s)

let expression_from_quoted_file f =
  Spel_util.uparse_file Spel_parser_j.body_main (mk_body_lexer ()) f

let quoted_expr_from_file s =
  sugarer (expression_from_quoted_file s)

let expression_from_quoted_string s =
  begin try
    let ast =
      Spel_util.uparse_string
        Spel_parser_j.body_main
        (mk_body_lexer ())
        s
    in
    ast.expr_text <- Some s;
    ast
  with
  | LexError msg ->
      warning
        (Spel_util.mk_expr_text (E_error msg) (Some s))
        (Format.sprintf "[%s] in text: '%s'" msg s)
  | _ ->
      warning
        (Spel_util.mk_expr_text (E_error "Parse error in text") (Some s))
        (Format.sprintf "in text: '%s'" s)
  end

let quoted_expr_from_string s =
  sugarer (expression_from_quoted_string s)


