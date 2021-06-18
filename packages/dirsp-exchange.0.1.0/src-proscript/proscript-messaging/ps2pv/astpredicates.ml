(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)

(** Tests on the ProScript AST that can be used for compiling or translating ProScript *)

type letin_check_context =
  { last_statement_in_letin_sequence : bool
  ; within_conditional : bool
  }

(** @param skip_nested_functions If true, then do process any nested functions as having no impact on the
      passed-in function's safety. Use this flag to partially translate huge ProScript/JavaScript code that
      has many nested functions, or to avoid the exponential big-O if you are visiting nested functions in
      the AST later *)
type letin_check_options = { skip_nested_functions : bool }

let enable_debugging = false

let is_object_list_composed_only_of_properties (ol : Ast.object_prop_t list) =
  List.for_all
    (function
      | e ->
        ( match e with
        | `Property _pv, _loc -> true
        | _ -> false ) )
    ol


let is_array_composed_only_of_element_accesses (a : Ast.expression_t list) =
  List.for_all
    (function
      | `Property _ef, _loc -> true
      | _ -> false )
    a


module VariableAssignmentCounts = struct
  include Map.Make (String)

  let merge_counts a b =
    union (fun _varname cnt1 cnt2 -> Some (cnt1 + cnt2)) a b
end

module Ast_style = struct
  type t =
    | LetIn of int VariableAssignmentCounts.t
    | LetInSemicolonCapable of int VariableAssignmentCounts.t
    | NotLetIn of int VariableAssignmentCounts.t

  let to_string = function
    | LetIn _l -> "LetIn"
    | LetInSemicolonCapable _l -> "LetInSemicolonCapable"
    | NotLetIn _l -> "NotLetIn"


  let compose st_a st_b =
    match (st_a, st_b) with
    | LetInSemicolonCapable cnt1, LetInSemicolonCapable cnt2 ->
        LetInSemicolonCapable VariableAssignmentCounts.(merge_counts cnt1 cnt2)
    | ( (LetIn cnt1 | LetInSemicolonCapable cnt1)
      , (LetIn cnt2 | LetInSemicolonCapable cnt2) ) ->
        LetIn VariableAssignmentCounts.(merge_counts cnt1 cnt2)
    | NotLetIn cnt1, (LetIn cnt2 | LetInSemicolonCapable cnt2 | NotLetIn cnt2)
      ->
        NotLetIn VariableAssignmentCounts.(merge_counts cnt1 cnt2)
    | (LetIn cnt1 | LetInSemicolonCapable cnt1), NotLetIn cnt2 ->
        NotLetIn VariableAssignmentCounts.(merge_counts cnt1 cnt2)


  let compose3 st_a st_b st_c = compose st_a st_b |> compose st_c

  let add_variable_assignment st varname : t =
    let add_one cnt =
      VariableAssignmentCounts.(merge_counts cnt (singleton varname 1))
    in
    match st with
    | LetIn cnt -> LetIn (add_one cnt)
    | LetInSemicolonCapable cnt -> LetInSemicolonCapable (add_one cnt)
    | NotLetIn cnt -> NotLetIn (add_one cnt)


  let for_all f l =
    let g a b = compose a (f b) in
    List.fold_left g (LetInSemicolonCapable VariableAssignmentCounts.empty) l
end

(** Can we rewrite the AST by replacing all imperative statements with the functional OCaml "let in" form?

    {1 Preconditions}

    The AST must be Javascript in strict mode (that is, all variables are declared before they are used).

    {1 Why use 'let in' form?}

    OCaml can model mutable fields in record types, which maps well to ProScript code (at least the code used in KBB2017).
    The "let in" form works great with mutable fields and keeps the translated code easy to review and looking like
    somewhat idiomatic OCaml (although we always choose statement-by-statement equivalence if there is a conflict between
    idioms and equivalence).

    One alternative to "let in" is to translate all of the ProScript functions to use mutable "ref".
    "ref" rewriting is fairly complicated and may never be implemented. Another alternative is to translate
    ProScript into monadic style; again, that may never be implemented. It is preferable to
    simply constrain the ProScript to have more functional functions (aka. be less imperative).

    The following functions can be rewritten with "let in":

    [
      function() { a(); b(); c(); d() }
    ]

    [
      function() { a(); if (b()) { c(); } else { d(); }; e(); }
    ]

    [
      function() { var v = a(); v = v + b(); v = v - c(); return v * d() }
    ]

    Those become:

    [
      begin let () = a () in let () = b () in let () = c () in d () end
    ]

    [
      begin let () = a () in ( if b () then c () else d () ) in e () end
    ]

    [
      begin let v = a () in let v = v + b () in let v = v - c () in v * d () end
    ]

    The following ProScript cannot be rewritten:

    [
      function() { var v = a(); for (i in b()) { v += c(); }; return v + d() }
    ]

    As long as **you cannot skip over any local variable mutations**, then we can use "let in".
    And mutations through mutable records are fine to use with "let in".

    {1 Logic}

    R1. We descend into all Ast.statement_t and all Ast.expression_t because statements can interrupt sequential
        top-down execution flow; statements can possibly result in local variable mutations skipped, and expressions
        can contain statements.
    R2. We evaluate all Ast.expression_t within conditionals (ex. `If) to see whether they contain
        local variable mutations. A local variable mutation within a conditional statement is not safe to rewrite in
        "let in" form. However local variable mutations in the main flow of a function are safe!
    R3. Any use of `Return occurs that is not the last _statement_ of a sequential statement list is not
        safe to rewrite. For clarity, there is no impact on safety if a conditional like `If is the last
        statement.
        The Ast.stm_t that have a "statement_t list" are:
        a) `Block which has one; b) `Switch which has two.
        Note: This rule is too strict and reflects a bit of laziness (see R0). Many or perhaps
        all functions can be rewritten so that an "if (a) {return b} ...other code..."
        becomes "if (a) {return b} else {...other code..}". In other words, the `Return can be rewritten
        to be within the last statement.
    R4. Throwing exceptions should be safe for "let in", as OCaml already supports exceptions within an OCaml
        expression. But OCaml does support Lwt and Async libraries where we need to consider
        coroutines + exceptions. To err on the side of caution (R0), we just say that throwing exceptions
        is unsafe until we can foolproof detect the absence of coroutines.
    R0. We err on conservatism ... we say "not safe for rewriting" if we don't know how or simply don't
        have the bandwidth to deal with a ProScript statement or expression.    

    The logic is a recursive evaluation of those rules. We short-circuit return
    if we run into any rules says is not rewritable.

    There is also one idiomatic OCaml pattern we'd like to follow. If and only if there are no
    local variable mutations then we can write a letin based function of form [let ... in let ... in ...] as the
    semicolon form [... ; ...; ...]. We return {!LetInSemicolonCapable} in this special case.
    That leads to more rules ...

    R5. If there are any local variable mutations, then not safe for rewriting with semicolons.
*)
let rec characterize_ast_style (ast : Ast.t) (opts : letin_check_options) :
    Ast_style.t =
  (* Helper for R3 *)
  let rec test_statements_while_distinguishing_last_statement
      (ctx : letin_check_context)
      (test : letin_check_context -> Ast.stm_t -> Ast_style.t) = function
    | [] -> Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty
    | [ (hd, _loc) ] ->
        test { ctx with last_statement_in_letin_sequence = true } hd
    | (hd, _loc) :: tl ->
        Ast_style.compose
          (test { ctx with last_statement_in_letin_sequence = true } hd)
          (test_statements_while_distinguishing_last_statement ctx test tl)
  in
  (* Core Logic: is_*_safe *)
  let rec characterize_source_t ctx = function
    | [] -> Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty
    | (`FunctionDeclaration childf, _loc) :: _tl ->
        (* R1. A function declared within another function is a brand-new scope. *)
        let id_opt, _args, childast = childf in
        if opts.skip_nested_functions
        then
          if Option.is_some id_opt
          then
            Ast_style.LetInSemicolonCapable
              (VariableAssignmentCounts.singleton (Option.get id_opt) 1)
          else Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty
        else characterize_ast_style childast opts
    | [ (`Statement (stm, _loc2), _loc) ] ->
        (* R1. Descend into all statements *)
        (* R3. Distinguish last statement in sequence from others *)
        characterize_statement_t
          { ctx with last_statement_in_letin_sequence = true }
          stm
    | (`Statement (stm, st_loc), _loc) :: tl ->
        (* R1. Descend into all statements *)
        (* R3. Distinguish last statement in sequence from others *)
        let hd_result =
          characterize_statement_t
            { ctx with last_statement_in_letin_sequence = false }
            stm
        in
        if enable_debugging
        then
          Printf.printf
            "characterize_source_t %s result=%s\n"
            (Lexerror.get_error st_loc "code")
            (Ast_style.to_string hd_result) ;
        Ast_style.compose hd_result (characterize_source_t ctx tl)
  and characterize_statement_t ctx = function
    | `Empty | `Debugger -> LetInSemicolonCapable VariableAssignmentCounts.empty
    | `Return expression_opt ->
        (* R3. Return as last statement is OK! *)
        (* R1. Descend into all expressions *)
        if enable_debugging
        then
          Printf.printf
            "characterize_statement_t last_statement_in_letin_sequence=%b\n"
            ctx.last_statement_in_letin_sequence ;
        if ctx.last_statement_in_letin_sequence
        then (
          match expression_opt with
          | None -> LetInSemicolonCapable VariableAssignmentCounts.empty
          | Some expression ->
              let expr, _loc = expression in
              let result = characterize_expression_t ctx expr in
              if enable_debugging
              then
                Printf.printf
                  "characterize_statement_t Return result=%s\n"
                  (Ast_style.to_string result) ;
              result )
        else (
          if enable_debugging
          then Printf.printf "characterize_statement_t Return NotLetIn\n" ;
          NotLetIn VariableAssignmentCounts.empty )
    | `Throw _ ->
        (* R4. Throwing an exception is unsafe (for now) *)
        if enable_debugging
        then Printf.printf "characterize_statement_t Throw NotLetIn\n" ;
        Ast_style.NotLetIn VariableAssignmentCounts.empty
    | `Continue _ | `Break _ | `Try _ | `Label _ ->
        (* R1. Anything that can cause a jump in sequential execution flow is not safe. *)
        if enable_debugging
        then
          Printf.printf
            "characterize_statement_t Continue,Break,Try,Label NotLetIn\n" ;
        Ast_style.NotLetIn VariableAssignmentCounts.empty
    | `Do _ | `While _ | `For _ | `Forin _ ->
        (* R0. Conservative laziness.
           We in fact could accept loops as long as they didn't have local variable mutations
           inside the loop.
        *)
        if enable_debugging
        then
          Printf.printf "characterize_statement_t Do,While,For,Forin NotLetIn\n" ;
        Ast_style.NotLetIn VariableAssignmentCounts.empty
    | `Block statement_l ->
        (* R1. Sequential execution is OK, if child statements are OK *)
        (* R3. Distinguish last statement in sequence from others *)
        test_statements_while_distinguishing_last_statement
          ctx
          characterize_statement_t
          statement_l
    | `Expression expression ->
        (* R1. Descend into all expressions *)
        let expr, _loc = expression in
        characterize_expression_t ctx expr
    | `If (expression, if_statement, else_statement_opt) ->
        (* R1. Consider statement branches and consider the expression *)
        (* R2. A local variable mutation within a conditional is unsafe *)
        let ctx = { ctx with within_conditional = true } in
        Ast_style.compose
          (let expr, _loc = expression in
           characterize_expression_t ctx expr )
          (let if_stmt, _loc = if_statement in
           Ast_style.compose
             (characterize_statement_t ctx if_stmt)
             (Option.fold
                ~none:
                  (Ast_style.LetInSemicolonCapable
                     VariableAssignmentCounts.empty )
                ~some:(function
                  | else_stmt, _loc -> characterize_statement_t ctx else_stmt )
                else_statement_opt ) )
    | `Switch (expression, default_statement_l_opt, case_expr_statement_l_l) ->
        (* R1. Consider statement branches and consider the expression *)
        (* R2. A local variable mutation within a conditional is unsafe *)
        let ctx = { ctx with within_conditional = true } in
        let expr, _loc = expression in
        Ast_style.compose3
          (characterize_expression_t ctx expr)
          (Option.fold
             ~none:
               (Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty)
             ~some:
               ((* R3. Distinguish last statement in sequence from others *)
                test_statements_while_distinguishing_last_statement
                  ctx
                  characterize_statement_t )
             default_statement_l_opt )
          (Ast_style.for_all
             (function
               | _expr, statement_l ->
                   (* R3. Distinguish last statement in sequence from others *)
                   test_statements_while_distinguishing_last_statement
                     ctx
                     characterize_statement_t
                     statement_l )
             case_expr_statement_l_l )
    | `Const id_expression_opt_l | `Declaration id_expression_opt_l ->
        (* R1. Descend into all expressions *)
        Ast_style.for_all
          (function
            | id, None ->
                Ast_style.LetInSemicolonCapable
                  (VariableAssignmentCounts.singleton id 1)
            | id, Some (expr, _loc) ->
                Ast_style.add_variable_assignment
                  (characterize_expression_t ctx expr)
                  id )
          id_expression_opt_l
    | `With _ ->
        (* R0. Conservative laziness. This will rarely be used. Too difficult to reason about what objects
            are being accessed, so punt *)
        if enable_debugging
        then Printf.printf "characterize_statement_t With NotLetIn\n" ;
        Ast_style.NotLetIn VariableAssignmentCounts.empty
  and characterize_expression_t ctx = function
    | `This | `Null | `Undefined | `Elision | `Bool _ | `Byte _ | `Number _
     |`String _ | `Regexp _ | `Identifier _ ->
        (* Primitive values are safe *)
        Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty
    | `Array expression_l | `Sequence expression_l ->
        (* R1. Descend into all expressions *)
        Ast_style.for_all
          (function
            | expr, _loc -> characterize_expression_t ctx expr )
          expression_l
    | `Object (object_prop_l : Ast.object_prop_t list) ->
        Ast_style.for_all
          (function
            | (_oprop : Ast.oprop_t), _loc ->
              ( match _oprop with
              | `Property (_id, ((expr : Ast.expr_t), _loc)) ->
                  (* R1. Descend into all expressions *)
                  characterize_expression_t ctx expr
              | `Getter (id, childf) | `Setter (id, childf) ->
                  (* R1. A function declared within another function is a brand-new scope. *)
                  let _id_opt, _args, childast = childf in
                  if opts.skip_nested_functions
                  then
                    Ast_style.LetInSemicolonCapable
                      (VariableAssignmentCounts.singleton id 1)
                  else characterize_ast_style childast opts ) )
          object_prop_l
    | `New (left_expression, right_expression_l_opt) ->
        (* R1. Descend into all expressions *)
        let left_expr, _loc = left_expression in
        Ast_style.compose
          (characterize_expression_t ctx left_expr)
          ( match right_expression_l_opt with
          | None ->
              Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty
          | Some right_expression_l ->
              Ast_style.for_all
                (function
                  | expr, _loc -> characterize_expression_t ctx expr )
                right_expression_l )
    | `Typeof expression
     |`Delete expression
     |`Void expression
     |`Plus expression
     |`Preincr expression
     |`Postincr expression
     |`Predecr expression
     |`Postdecr expression
     |`Minus expression
     |`Lnot expression
     |`Bnot expression ->
        (* R1. Descend into all expressions *)
        let expr, _loc = expression in
        characterize_expression_t ctx expr
    | `Function childf ->
        (* R1. A function declared within another function is a brand-new scope. *)
        let id_opt, _args, childast = childf in
        if opts.skip_nested_functions
        then
          if Option.is_some id_opt
          then
            Ast_style.LetInSemicolonCapable
              (VariableAssignmentCounts.singleton (Option.get id_opt) 1)
          else Ast_style.LetInSemicolonCapable VariableAssignmentCounts.empty
        else characterize_ast_style childast opts
    | `Conditional ((expr1, _loc1), (expr2, _loc2), (expr3, _loc3)) ->
        (* R1. Descend into all expressions *)
        (* R2. A local variable mutation within a conditional is unsafe. x ? y : z is a conditional *)
        let ctx = { ctx with within_conditional = true } in
        Ast_style.compose3
          (characterize_expression_t ctx expr1)
          (characterize_expression_t ctx expr2)
          (characterize_expression_t ctx expr3)
    | `Assign
        ((`Dot ((dot_expression, _loc2), _id), _loc1), (rhs_expression, _loc3))
      ->
        (* property mutation. ex. a.iv = Type_iv.assert(a.iv); *)
        (* R1. Descend into all expressions *)
        if enable_debugging
        then
          Printf.printf "characterize_expression_t Assign property mutation\n" ;
        Ast_style.compose
          (characterize_expression_t ctx dot_expression)
          (characterize_expression_t ctx rhs_expression)
    | `Assign ((lhs_expr, _lloc), (rhs_expr, _rloc))
     |`Ashassign ((lhs_expr, _lloc), (rhs_expr, _rloc)) ->
        (* R1. Descend into all expressions *)
        (* R2. A local variable mutation within a conditional is unsafe. We abort immediately if we are in a conditional *)
        (* R5. Semicolon style is not possible with semicolons *)
        let subresult =
          Ast_style.compose
            (characterize_expression_t ctx lhs_expr)
            (characterize_expression_t ctx rhs_expr)
        in
        let subresult =
          match lhs_expr with
          | `Identifier id -> Ast_style.add_variable_assignment subresult id
          | _ -> subresult
        in
        let is_property_assignment =
          match lhs_expr with
          | `Property _ -> true
          | _ -> false
        in
        if enable_debugging
        then
          Printf.printf
            "characterize_expression_t Assign,Ashassign subresult=%s (before)\n"
            (Ast_style.to_string subresult) ;
        let result =
          if ctx.within_conditional
          then
            match subresult with
            | LetIn cnt | LetInSemicolonCapable cnt | NotLetIn cnt ->
                Ast_style.NotLetIn cnt
          else
            (* Downgrade LetInSemicolonCapable to LetIn for local variable assignments only. Property assignments are fine! *)
            match subresult with
            | Ast_style.LetInSemicolonCapable cnt ->
                if is_property_assignment
                then subresult
                else Ast_style.LetIn cnt
            | _ -> subresult
        in
        if enable_debugging
        then (
          Printf.printf
            "characterize_expression_t Assign,Ashassign result=%s (after)\n"
            (Ast_style.to_string result) ;
          result )
        else result
    | `Lor (lhs_expression, rhs_expression)
     |`Land (lhs_expression, rhs_expression) ->
        (* R1. Descend into all expressions *)
        (* R2. A local variable mutation within a conditional is unsafe. || and && are short-circuit conditionals *)
        let ctx = { ctx with within_conditional = true } in
        Ast_style.compose
          (let lhs_expr, _lloc = lhs_expression in
           characterize_expression_t ctx lhs_expr )
          (let rhs_expr, _rloc = rhs_expression in
           characterize_expression_t ctx rhs_expr )
    | `Property (lhs_expression, rhs_expression)
     |`In (lhs_expression, rhs_expression)
     |`Instanceof (lhs_expression, rhs_expression)
     |`Add (lhs_expression, rhs_expression)
     |`Sub (lhs_expression, rhs_expression)
     |`Multiply (lhs_expression, rhs_expression)
     |`Mod (lhs_expression, rhs_expression)
     |`Divide (lhs_expression, rhs_expression)
     |`Lsh (lhs_expression, rhs_expression)
     |`Rsh (lhs_expression, rhs_expression)
     |`Ash (lhs_expression, rhs_expression)
     |`Bor (lhs_expression, rhs_expression)
     |`Band (lhs_expression, rhs_expression)
     |`Bxor (lhs_expression, rhs_expression)
     |`Equal (lhs_expression, rhs_expression)
     |`Lt (lhs_expression, rhs_expression)
     |`Gt (lhs_expression, rhs_expression)
     |`Le (lhs_expression, rhs_expression)
     |`Ge (lhs_expression, rhs_expression)
     |`Sequal (lhs_expression, rhs_expression) ->
        (* R1. Descend into all expressions *)
        Ast_style.compose
          (let lhs_expr, _lloc = lhs_expression in
           characterize_expression_t ctx lhs_expr )
          (let rhs_expr, _rloc = rhs_expression in
           characterize_expression_t ctx rhs_expr )
    | `Dot (expression, _id) ->
        (* R1. Descend into all expressions *)
        let expr, _loc = expression in
        let result = characterize_expression_t ctx expr in
        if enable_debugging
        then
          Printf.printf
            "characterize_expression_t Dot(expr=%s, id=%s)\n"
            (Ast_style.to_string result)
            _id ;
        result
    | `Call (target_expression, args_expression_l) ->
        (* R1. Descend into all expressions *)
        let target_expr, _loc = target_expression in
        let target_result = characterize_expression_t ctx target_expr in
        if enable_debugging
        then
          Printf.printf
            "characterize_expression_t Call target_result=%s\n"
            (Ast_style.to_string target_result) ;
        Ast_style.compose
          target_result
          (Ast_style.for_all
             (function
               | expr, _loc -> characterize_expression_t ctx expr )
             args_expression_l )
  in
  characterize_source_t
    { last_statement_in_letin_sequence = false; within_conditional = false }
    ast
