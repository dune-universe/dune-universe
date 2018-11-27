open Expr
open Unit
open Syntax_tree

type dumb_user = {
    dumb_user_channel: out_channel;
    mutable dumb_user_error_count: int;
  }

let dumb_user_error (u: dumb_user) (s: string): unit
    =
  Printf.fprintf u.dumb_user_channel "error: %s\n" s;
  u.dumb_user_error_count <- u.dumb_user_error_count + 1

let make_dumb_user ch =
  { dumb_user_channel = ch;
    dumb_user_error_count = 0; }


let check_that_names_are_unique (u: dumb_user) (item_type: string) (names: string list): unit
    =
  let seen = Hashtbl.create 10 in
  names |> List.iter
    (fun name ->
      if Hashtbl.mem seen name then
        dumb_user_error u (Printf.sprintf "Multiple definitions of %s `%s'" item_type name)
      else
        Hashtbl.add seen name ()
    )


let check_for_multiple_definitions (u: dumb_user) (s: specification)
    =
  s.specification_ranges    |> List.map fst |> check_that_names_are_unique u "range";
  s.specification_variables |> List.map fst |> check_that_names_are_unique u "variable";
  s.specification_goals     |> List.map fst |> check_that_names_are_unique u "proc"


exception Subscript_not_found of string

let range_name_of_subscript_name (v:variable) (d: definition_expr) (subscript_name: string)
    =
  try
    List.assoc subscript_name (List.rev (v.variable_subscripts @ d.definition_expr_summation_subscripts))
  with
    Not_found -> raise (Subscript_not_found subscript_name)

let check_reference (u: dumb_user) (s: specification) (d: definition_expr) (toplevel_variable_name: string) (toplevel_variable: variable) (reference_variable_name: string) (reference_subscripts: string list): unit
    =
  try
    let definition_variable = specification_find_variable s reference_variable_name in
    if List.length definition_variable.variable_subscripts <> List.length reference_subscripts then
      dumb_user_error u (Printf.sprintf "Wrong number of subscripts to variable %s in definition of %s" reference_variable_name toplevel_variable_name)
    else
      List.combine definition_variable.variable_subscripts reference_subscripts |> List.iteri
        (fun index ((_,definition_range_name), reference_subscript) ->
          try
            let reference_range_name = range_name_of_subscript_name toplevel_variable d reference_subscript in
            if definition_range_name <> reference_range_name then
              dumb_user_error u (Printf.sprintf "Range `%s' does not match expected range `%s' in subscript %i of `%s' in definition of `%s'" reference_range_name definition_range_name (index + 1) reference_variable_name toplevel_variable_name)
          with
            Subscript_not_found _ ->
              dumb_user_error u (Printf.sprintf "Undefined subscript `%s' in definition of `%s'" reference_subscript toplevel_variable_name))
  with
    Variable_not_found _ ->
      dumb_user_error u (Printf.sprintf "Undefined variable `%s' in definition of `%s'" reference_variable_name toplevel_variable_name)

let check_for_multiply_defined_subscripts (u: dumb_user) (toplevel_variable_name: string) (subscripts: string list)
    =
  let seen = Hashtbl.create 4 in
  let reported = Hashtbl.create 0 in
  subscripts |> List.iter (fun subscript ->
    if Hashtbl.mem reported subscript then
      ()
    else if Hashtbl.mem seen subscript then
      begin
        dumb_user_error u (Printf.sprintf "subscript `%s' defined multiple times in definition of `%s'" subscript toplevel_variable_name);
        Hashtbl.add reported subscript ()
      end
    else
      Hashtbl.add seen subscript ())

let check_range_names (u: dumb_user) (s: specification) (toplevel_variable_name: string) (range_names: string list)
    =
  range_names |> List.iter
    (fun range_name ->
      try ignore (specification_find_range s range_name)
      with Range_not_found _ ->
        dumb_user_error u (Printf.sprintf "Undefined range `%s' in definition of `%s'" range_name toplevel_variable_name))

let subscripts_in_variable_and_definition (v:variable) =
  match v.variable_definition with
  | Definition_given -> v.variable_subscripts
  | Definition_expr d -> v.variable_subscripts @ d.definition_expr_summation_subscripts

let check_variables (u: dumb_user) (s: specification)
    =
  s.specification_variables |> List.iter
    (fun (variable_name, v) ->
      let all_subscript_declarations = subscripts_in_variable_and_definition v in
      all_subscript_declarations |> List.map fst |> check_for_multiply_defined_subscripts u variable_name;
      all_subscript_declarations |> List.map snd |> check_range_names u s variable_name;
      match v.variable_definition with
      | Definition_given -> ()
      | Definition_expr d ->
          let subscripts_used_in_references = Hashtbl.create 4 in
          let rec check_expr = function
            | Expr_const _ -> ()
            | Expr_ref(ref_variable_name, ref_subscripts) ->
                check_reference u s d variable_name v ref_variable_name ref_subscripts;
                ref_subscripts |> List.iter (fun subscript -> Hashtbl.add subscripts_used_in_references subscript ())
            | Expr_unop(_, e1)    -> check_expr e1
            | Expr_binop(_, e1, e2) -> check_expr e1; check_expr e2
            | Expr_if(e1, e2, e3) -> check_expr e1; check_expr e2; check_expr e3
            | Expr_index_eq_ne(i1,i2,e1,e2) ->
                let r1 =
                  try
                    range_name_of_subscript_name v d i1
                  with
                    Subscript_not_found _ ->
                      dumb_user_error u (Printf.sprintf "Undefined subscript `%s' in definition of `%s'" i1 variable_name);
                      "?"
                in
                let r2 =
                  try
                    range_name_of_subscript_name v d i2
                  with
                    Subscript_not_found _ ->
                      dumb_user_error u (Printf.sprintf "Undefined subscript `%s' in definition of `%s'" i2 variable_name);
                      "?"
                in
                if r1 <> r2 then
                  dumb_user_error u (Printf.sprintf "subscript ranges `%s' and `%s' do not match for subscripts `%s' and `%s' in definition of `%s'" r1 r2 i1 i2 variable_name);
                check_expr e1;
                check_expr e2
          in
          check_expr d.definition_expr_summee;
          v.variable_subscripts @ d.definition_expr_summation_subscripts |> Utils.nub_list |> List.iter
            (fun (subscript, _range_name) ->
              if not (Hashtbl.mem subscripts_used_in_references subscript) then
                dumb_user_error u (Printf.sprintf "Subscript `%s' never used in definition of `%s'" subscript variable_name)))

let check_for_cyclic_definitions (u: dumb_user) (s: specification): unit
    =
  let dependencies = Hashtbl.create 10 in
  s.specification_variables |> List.iter
    (fun (defined_variable_name, v) ->
      match v.variable_definition with
      | Definition_given -> ()
      | Definition_expr d ->
          let rec visit_expr = function
            | Expr_const _ -> ()
            | Expr_ref(used_variable_name, _subscripts) -> Hashtbl.add dependencies defined_variable_name used_variable_name
            | Expr_unop(_, e1) -> visit_expr e1
            | Expr_binop(_, e1, e2) -> visit_expr e1; visit_expr e2
            | Expr_if(e1, e2, e3) -> visit_expr e1; visit_expr e2; visit_expr e3
            | Expr_index_eq_ne(_i1,_i2,e1,e2) -> visit_expr e1; visit_expr e2
          in
          visit_expr d.definition_expr_summee
      );
  let already_visited = Hashtbl.create 10 in
  let in_path = Hashtbl.create 10 in
  let rec dfs (variable_name: string): unit
      =
    if Hashtbl.mem already_visited variable_name then
      ()
    else begin
      if Hashtbl.mem in_path variable_name then
        dumb_user_error u (Printf.sprintf "Cyclic definition involving `%s'" variable_name)
      else begin
        Hashtbl.add in_path variable_name ();
        Hashtbl.find_all dependencies variable_name |> List.iter dfs;
        Hashtbl.remove in_path variable_name;
      end;
      Hashtbl.add already_visited variable_name ()
    end
  in
  s.specification_variables |> List.map fst |> List.iter dfs

let all_base_units_in_specification (s : specification): string list =
  s.specification_variables
  |> List.map (fun (_, v) -> v.variable_unit)
  |> List.map all_base_units_in_unit
  |> List.concat
  |> Utils.nub_list
 

let check_goal (u: dumb_user) (s: specification) (goal_name: string) (g: goal): unit =
  match g with
  | Goal_get variable_name
  | Goal_recompute variable_name -> begin
      try ignore (specification_find_variable s variable_name)
      with Variable_not_found _ ->
        dumb_user_error u (Printf.sprintf "Undefined variable `%s' in proc `%s'" variable_name goal_name)
    end
  | Goal_propagate_delta variable_names
  | Goal_set variable_names
  | Goal_increment variable_names -> begin
      check_that_names_are_unique u (Printf.sprintf "proc `%s''s variable reference" goal_name) variable_names;
      variable_names |> Utils.nub_list |> List.iter
                                            (fun variable_name ->
                                              try ignore (specification_find_variable s variable_name)
                                              with Variable_not_found _ ->
                                                dumb_user_error u (Printf.sprintf "Undefined variable `%s' in proc `%s'" variable_name goal_name))
    end
  | Goal_scale_unit base_unit -> begin
      if not (List.mem base_unit (all_base_units_in_specification s)) then
        dumb_user_error u (Printf.sprintf "proc `%s' wants to scale base unit `%s' which is not in any variable's unit" goal_name base_unit)
    end

let check_that_goals_are_unique (u: dumb_user) (s: specification): unit =
  let goal_to_names = Hashtbl.create 10 in
  s.specification_goals |> List.iter (fun (name, goal) ->
                                      Hashtbl.replace goal_to_names goal
                                                      (name :: try Hashtbl.find goal_to_names goal
                                                               with Not_found -> []));
  goal_to_names |> Hashtbl.iter (fun _goal names ->
                                 match names with
                                 | [] -> ()
                                 | [_name] -> ()
                                 | _ ->
                                    dumb_user_error u (Printf.sprintf "Procs %s have the same goal"
                                                                      (String.concat " and " names)))

let check_goals (u: dumb_user) (s: specification): unit
    =
  s.specification_goals |> List.iter (fun (goal_name, goal) -> check_goal u s goal_name goal);
  check_that_goals_are_unique u s

let check_specification (u: dumb_user) (s: specification): unit
    =
  check_for_multiple_definitions u s;
  check_for_cyclic_definitions u s;
  check_variables u s;
  check_goals u s

type checked_unit =
  | Checked_unit_ok of unit_
  | Checked_unit_zero
  | Checked_unit_bad

let join_checked_units (u: dumb_user) (operator: string) (toplevel_variable_name: string) (cu1: checked_unit) (cu2: checked_unit): checked_unit
    = 
  match cu1, cu2 with
  | Checked_unit_bad, cu2 -> cu2
  | cu1, Checked_unit_bad -> cu1
  | Checked_unit_zero, cu2 -> cu2
  | cu1, Checked_unit_zero -> cu1
  | Checked_unit_ok u1, Checked_unit_ok u2 ->
      if u1 <> u2 then
        begin
          dumb_user_error u
            (Printf.sprintf
               "unit error in %s: units of %s operands do not match: `%s' vs `%s'"
               toplevel_variable_name
               operator
               (string_of_unit u1)
               (string_of_unit u2));
          Checked_unit_bad
        end
      else
        Checked_unit_ok u1

let mul_checked_units (cu1 : checked_unit) (cu2 : checked_unit) : checked_unit =
  match cu1, cu2 with
  | Checked_unit_zero, _ -> Checked_unit_zero
  | _, Checked_unit_zero -> Checked_unit_zero
  | Checked_unit_ok u1, Checked_unit_ok u2 -> Checked_unit_ok(unit_mul u1 u2 |> unit_canonicalize)
  | Checked_unit_bad, _ -> Checked_unit_bad
  | _, Checked_unit_bad -> Checked_unit_bad

let div_checked_units (u: dumb_user) (cu1 : checked_unit) (cu2 : checked_unit) : checked_unit =
  match cu1, cu2 with
  | Checked_unit_zero, _ -> Checked_unit_zero
  | _, Checked_unit_zero -> dumb_user_error u "Division by zero"; Checked_unit_bad
  | Checked_unit_ok u1, Checked_unit_ok u2 -> Checked_unit_ok(unit_div u1 u2 |> unit_canonicalize)
  | Checked_unit_bad, _ -> Checked_unit_bad
  | _, Checked_unit_bad -> Checked_unit_bad

let unit_check_expr (u: dumb_user) (s: specification) (toplevel_variable_name: string) (e: expr): checked_unit =
  let rec check (e: expr): checked_unit
      =
    match e with
    | Expr_const 0.0 -> Checked_unit_zero
    | Expr_const _ -> Checked_unit_ok unit_one
    | Expr_ref(variable_name, _subscripts) ->
        begin try
          Checked_unit_ok((specification_find_variable s variable_name).variable_unit |> unit_canonicalize)
        with 
          Variable_not_found _ -> Checked_unit_bad
        end
    | Expr_unop(Unop_neg, e1) -> check e1
    | Expr_unop(Unop_abs, e1) -> check e1
    | Expr_binop(b, e1, e2) -> begin
        let cu1 = check e1 in
        let cu2 = check e2 in
        match b with
        | Binop_mul -> mul_checked_units cu1 cu2
        | Binop_div -> div_checked_units u cu1 cu2
        | Binop_add -> join_checked_units u "+" toplevel_variable_name cu1 cu2
        | Binop_sub -> join_checked_units u "-" toplevel_variable_name cu1 cu2
        | Binop_min -> join_checked_units u "min" toplevel_variable_name cu1 cu2
        | Binop_max -> join_checked_units u "max" toplevel_variable_name cu1 cu2
        | Binop_le -> ignore(join_checked_units u "<=" toplevel_variable_name cu1 cu2); Checked_unit_ok unit_one
        | Binop_ge -> ignore(join_checked_units u ">=" toplevel_variable_name cu1 cu2); Checked_unit_ok unit_one
        | Binop_lt -> ignore(join_checked_units u "<" toplevel_variable_name cu1 cu2); Checked_unit_ok unit_one
        | Binop_gt -> ignore(join_checked_units u ">" toplevel_variable_name cu1 cu2); Checked_unit_ok unit_one
      end
    | Expr_if(e1, e2, e3) ->
       ignore (join_checked_units u "?" toplevel_variable_name (check e1) (Checked_unit_ok unit_one));
       join_checked_units u ":" toplevel_variable_name (check e2) (check e3)
    | Expr_index_eq_ne(_,_,e1,e2) -> join_checked_units u "?:" toplevel_variable_name (check e1) (check e2)
  in
  check e

let unit_check_specification (u: dumb_user) (s: specification): unit
    =
  s.specification_variables |> List.iter
    (fun (variable_name, v) ->
      match v.variable_definition with
      | Definition_given -> ()
      | Definition_expr de ->
          let expr_unit = unit_check_expr u s variable_name de.definition_expr_summee in
          let decl_unit = Checked_unit_ok (v.variable_unit |> unit_canonicalize) in
          ignore (join_checked_units u "=" variable_name decl_unit expr_unit))

type result =
  | Ok
  | Failed_checks

let run (ch: out_channel) (s: specification): result
    =
  let u = make_dumb_user ch in
  check_specification u s;
  unit_check_specification u s;
  if u.dumb_user_error_count = 0 then
    Ok
  else
    Failed_checks
