open Ppx_core
open Ast_builder.Default

let explode_tuple_expr = function
  | { pexp_desc = Pexp_tuple exp_list; pexp_attributes = attrs; _ } ->
    assert_no_attributes attrs;
    exp_list
  | e -> [e]

let explode_tuple_patt = function
  | { ppat_desc = Ppat_tuple pat_list; ppat_attributes = attrs; _ } ->
    assert_no_attributes attrs;
    pat_list
  | p -> [p]

let decompose_tuples_in_case ~tuple_size case : (pattern list * expression) =
  let { pc_lhs; pc_rhs; pc_guard } = case in
  (match pc_guard with
   | Some guard ->
     Location.raise_errorf ~loc:guard.pexp_loc
       "guards are not supported in [%%optional ]"
   | _ -> ());
  let tp = explode_tuple_patt pc_lhs in
  if tuple_size <> List.length tp then
    Location.raise_errorf ~loc:pc_lhs.ppat_loc
      "Incorrect number of tuple components in match pattern";
  (tp, pc_rhs)

let convert_option_pattern_to_bool pattern =
  let loc = pattern.ppat_loc in
  match pattern with
  | [%pat? Some [%p? _]] -> [%pat? false]
  | [%pat? None]         -> [%pat? true]
  | [%pat? _]            -> [%pat? _]
  | _ ->
    Location.raise_errorf ~loc:pattern.ppat_loc
      "only None, Some and _ are supported in [%%optional ]"

let convert_option_pattern_to_flat_option pattern =
  let loc = pattern.ppat_loc in
  match pattern with
  | [%pat? Some [%p? _]] -> [%pat? `Some]
  | [%pat? None]         -> [%pat? `None]
  | [%pat? _]            -> [%pat? _]
  | _ ->
    Location.raise_errorf ~loc:pattern.ppat_loc
      "only None, Some and _ are supported in [%%optional ]"

let varname i = Printf.sprintf "__ppx_optional_e_%i" i
let evar ~loc i = evar ~loc (varname i)
let pvar ~loc i = pvar ~loc (varname i)

let define_matched_expression ~loc match_exp_tuple_components nested_expression =
  let cases =
    List.mapi match_exp_tuple_components ~f:(fun i expr ->
      let loc = expr.pexp_loc in
      value_binding ~loc ~pat:(pvar ~loc i) ~expr
    )
  in
  pexp_let ~loc
    Nonrecursive
    cases
    nested_expression

let variable_binding_for_pattern ~i pattern =
  let loc = pattern.ppat_loc in
  match pattern with
  | [%pat? Some [%p? x]] ->
    Some
      (value_binding
         ~loc
         ~pat:x
         ~expr:(eapply ~loc [%expr Optional_syntax.unchecked_value]
                  [evar ~loc i]))
  | [%pat? None]
  | [%pat? _] ->
    None
  | _ ->
    Location.raise_errorf ~loc:pattern.ppat_loc
      "only None, Some and _ are supported in [%%optional ]"

let expand_match ~loc:_ ~path:_ e =
  let match_exp, cases =
    match e with
    | {pexp_desc = Pexp_match (match_exp, cases); _ } ->
      match_exp, cases
    | _ ->
      Location.raise_errorf ~loc:e.pexp_loc
        "[%%optional ] must apply to a match statement"
  in
  let match_exp_tuple_components = explode_tuple_expr match_exp in
  let tuple_size = List.length match_exp_tuple_components in
  let decomposed_cases = List.map cases ~f:(decompose_tuples_in_case ~tuple_size) in
  (* This is the expression we will end up matching on in our output,
     it will look like
     (Optional_syntax.is_none a), (Optional_syntax.is_none b) ... *)
  let make_match ~convert =
    pexp_tuple
      ~loc:e.pexp_loc
      (List.mapi match_exp_tuple_components ~f:(fun i expr ->
         let loc = expr.pexp_loc in
         eapply ~loc (convert loc) [evar ~loc i]
       ))
  in
  let unsafe_some_vbs (ps : pattern list) =
    let rec loop i = function
      | [] -> []
      | pattern :: ps ->
        match variable_binding_for_pattern ~i pattern with
        | None -> loop (i + 1) ps
        | Some vb -> vb :: loop (i + 1) ps
    in
    loop 0 ps
  in
  let make_cases ~f =
    let single_case_is_none_pattern (ps : pattern list) =
      List.map ps ~f
      |> ppat_tuple ~loc:e.pexp_loc
    in
    List.map decomposed_cases ~f:(fun (pl,exp) ->
      case ~guard:None
        ~lhs:(single_case_is_none_pattern pl)
        ~rhs:(match (unsafe_some_vbs pl) with
          | [] -> exp
          | vbs ->
            pexp_let ~loc:exp.pexp_loc Nonrecursive
              vbs exp
        )
    )
  in
  let real_match =
    let is_none_expr =
      let convert loc = [%expr Optional_syntax.is_none] in
      make_match ~convert
    in
    let cases = make_cases ~f:convert_option_pattern_to_bool in
      define_matched_expression ~loc:match_exp.pexp_loc
        match_exp_tuple_components
        (pexp_match ~loc:e.pexp_loc is_none_expr cases)
  in
  (* This is so that a "not exhaustive" compiler error generates an error that looks like:
     Here is an example of a value that is not matched:
     `Some

     In addition to the more confusing compiler error it already generates:
     Here is an example of a value that is not matched:
     false
  *)
  let match_for_compiler_errors =
    let flat_option_expr_for_compiler_errors =
      let convert loc = [%expr (fun x -> if Optional_syntax.is_none x then `None else `Some)] in
      make_match ~convert
    in
    let cases = make_cases ~f:convert_option_pattern_to_flat_option in
    define_matched_expression ~loc:match_exp.pexp_loc
      match_exp_tuple_components
      (pexp_match ~loc:e.pexp_loc flat_option_expr_for_compiler_errors cases)
  in
  let loc = e.pexp_loc in
  pexp_ifthenelse ~loc [%expr false] match_for_compiler_errors (Some real_match)

let optional =
  Extension.declare "optional" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_match

let () =
  Ppx_driver.register_transformation "optional"
    ~extensions:[ optional ]
