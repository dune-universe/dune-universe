open Expr
open Imperative

let rec used_variables_in_expr (e : expr) : string list =
  match e with
  | Expr_const _ -> []
  | Expr_ref(name, _) -> [name]
  | Expr_unop(_, e1) -> used_variables_in_expr e1
  | Expr_binop(_, e1, e2)
  | Expr_index_eq_ne(_, _, e1, e2) ->
     used_variables_in_expr e1 @ used_variables_in_expr e2
  | Expr_if(e1, e2, e3) ->
     used_variables_in_expr e1 @ used_variables_in_expr e2 @ used_variables_in_expr e3


(** The following three recursive functions form an attribute grammar for removing unused locals:
    They take:
      1) the set of variables used in subsequent steps
    They return (if any):
      1) the modified step list/step/sequence
      2) the set of variables used in that modified step list/step/statement AND subsequently *)
let rec remove_unused_locals_in_steps (steps : step list) (used_locals : string list) : step list * string list =
  match steps with
  | [] -> ([], used_locals)
  | first :: rest ->
     let rest', used_locals' = remove_unused_locals_in_steps rest used_locals in
     begin
       match remove_unused_locals_in_step first used_locals' with
       | None -> rest', used_locals'
       | Some (first', used_locals'') -> (first' :: rest'), used_locals''
     end
and remove_unused_locals_in_step (step : step) (used_locals : string list) : (step * string list) option =
  match step with
  | Step_let(name, _repr, _unit, expr) ->
     if List.mem name used_locals then
       Some (step, used_variables_in_expr expr @ used_locals)
     else
       None
  | Step_do(statement) ->
     begin
       match remove_unused_locals_in_statement statement used_locals with
       | None -> None
       | Some (statement', used_locals') -> Some (Step_do statement', used_locals')
     end
and remove_unused_locals_in_statement (statement : statement) (used_locals : string list) : (statement * string list) option =
  match statement with
  | Statement_assign(lhs, expr)
  | Statement_increment(lhs, expr) ->
     begin
       match lhs with
       | Lhs_local(name, _repr) ->
          if List.mem name used_locals then
            Some (statement, used_variables_in_expr expr @ used_locals)
          else
            None
       | _ -> Some (statement, used_variables_in_expr expr @ used_locals)
     end
  | Statement_scale(lhs, expr) ->
     begin
       match lhs with
       | Lhs_local(name, _repr) ->
          if List.mem name used_locals then
            Some (statement, used_variables_in_expr expr @ used_locals)
          else
            None
       | _ -> Some (statement, used_variables_in_expr expr @ used_locals)
     end
  | Statement_for(index, range_name, body) ->
     begin
       match remove_unused_locals_in_statement body used_locals with
       | None -> None
       | Some (body', used_locals') -> Some (Statement_for(index, range_name, body'), used_locals')
     end
  | Statement_block body ->
     begin
       match remove_unused_locals_in_steps body used_locals with
       | [],_ -> None
       | body', used_locals' -> Some (Statement_block body', used_locals')
     end

let remove_unused_locals_in_procedure (p : procedure) : procedure =
  let used_locals = match p.procedure_return_value with
    | None -> []
    | Some(expr, _repr, _unit) -> used_variables_in_expr expr
  in
  let simplified_body, _used_locals = remove_unused_locals_in_steps p.procedure_body used_locals in
  { p with
    procedure_body = simplified_body
  }

let remove_unused_locals_in_module (m : module_) : module_ =
  { m with
    module_procedures = m.module_procedures |> List.map (fun (name, proc) ->
                                                         (name, remove_unused_locals_in_procedure proc));
  }
