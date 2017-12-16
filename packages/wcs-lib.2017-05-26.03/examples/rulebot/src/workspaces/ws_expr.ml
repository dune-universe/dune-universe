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

open Wcs_lib
open Cnl_t
open Cnl_builder
open Cnl_util

open Bmd_t

let binary_op_list = [Op_eq;Op_ne;Op_lt;Op_le;Op_gt;Op_ge;
                      Op_and;Op_or;Op_plus;Op_minus;Op_mult;
                      Op_div;Op_mod;Op_pow;Op_concat;Op_during]

let unary_op_list = [Op_not;Op_toString]

let aggregate_op_list = [A_total; A_avg]

type intent_desc = {
  intent_desc_node_name : string;
  intent_desc_name : string;
  intent_desc_output : string;
  intent_desc_examples : string list;
}

let mk_intent_desc node_name entity_name output examples = {
  intent_desc_node_name = node_name;
  intent_desc_output = output;
  intent_desc_name = "mk_" ^ entity_name;
  intent_desc_examples = examples;
}

let intent_desc_desc_of_cnl_binop binop =
  begin match binop with
  | Op_eq -> mk_intent_desc "Equal" "equal" "equality condition" [ "equal";
                                                                   "equality";
                                                                   "equality comparison";
                                                                   "equal values";
                                                                   "same value" ]
  | Op_ne -> mk_intent_desc "NotEqual" "notequal" "inequality condition" [ "notequal";
                                                                           "not equal";
                                                                           "inequality";
                                                                           "different";
                                                                           "not the same";
                                                                           "different values" ]
  | Op_lt -> mk_intent_desc "Lt" "lt" "less than comparison" [ "lt";
                                                               "less";
                                                               "lower";
                                                               "smaller";
                                                               "less than";
                                                               "lower than";
                                                               "smaller than";
                                                               "smaller value";
                                                               "strictly less" ]
  | Op_le -> mk_intent_desc "Le" "le" "less than or equal comparison" [ "le";
                                                                        "less or equal";
                                                                        "lower or equal";
                                                                        "smaller or equal";
                                                                        "less than or equal";
                                                                        "lower than or equal" ]
  | Op_gt -> mk_intent_desc "Gt" "gt" "greater than comparison" [ "gt";
                                                                  "greater";
                                                                  "more";
                                                                  "higher";
                                                                  "greater than";
                                                                  "more than";
                                                                  "higher than";
                                                                  "greater value";
                                                                  "strictly more" ]
  | Op_ge -> mk_intent_desc "Ge" "ge" "greater than or equal comparison" [ "ge";
                                                                           "greater or equal";
                                                                           "more or equal";
                                                                           "higher or equal";
                                                                           "greater than or equal";
                                                                           "more than or equal" ]
  | Op_and -> mk_intent_desc "And" "and" "Boolean conjunction" [ "and";
                                                                 "conjunction";
                                                                 "and condition";
                                                                 "conjunction condition";
                                                                 "two conditions";
                                                                 "also"; ]
  | Op_or -> mk_intent_desc "Or" "or" "Boolean disjunction" [ "or";
                                                              "disjunction";
                                                              "or condition";
                                                              "disjunction condition";
                                                              "either"; ]
  | Op_plus -> mk_intent_desc "Plus" "plus" "addition expression" [ "plus";
                                                                    "addition";
                                                                    "additive";
                                                                    "plus expression";
                                                                    "additive expression";
                                                                    "add";
                                                                  ]
  | Op_minus -> mk_intent_desc "Minus" "minus" "subtraction expression" [ "minus";
                                                                          "subtraction";
                                                                          "subtractive";
                                                                          "minus expression";
                                                                          "subtraction expression";
                                                                          "remove";
                                                                          "take away"; ]
  | Op_mult -> mk_intent_desc "Mult" "mult" "multiplication expression" [ "mult";
                                                                          "multiplication";
                                                                          "multiplicative";
                                                                          "mult expression";
                                                                          "multiplication expression"; ]
  | Op_div -> mk_intent_desc "Div" "div" "division expression" [ "div";
                                                                 "division";
                                                                 "divisive";
                                                                 "div expression";
                                                                 "division expression"; ]
  | Op_mod -> mk_intent_desc "Mod" "mod" "modulo expression" [ "mod";
                                                               "modulo";
                                                               "mod expression";
                                                               "modulo expression"; ]
  | Op_pow -> mk_intent_desc "Pow" "pow" "exponent expression" [ "pow";
                                                                 "power";
                                                                 "power of";
                                                                 "exponent";
                                                                 "exponentiation";
                                                                 "exponent expression"; ]
  | Op_concat -> mk_intent_desc "Concat" "concat" "string concatenation" [ "concat";
                                                                           "concatenation";
                                                                           "string concat";
                                                                           "string concatenation";
                                                                           "concat expression";
                                                                           "concatenation expression"; ]
  | Op_during -> mk_intent_desc "During" "during" "during expression" [ "during";
                                                                        "during expression";
                                                                        "time during";
                                                                        "time during expression"; ]
  end

let intent_desc_desc_of_cnl_unop unop =
  begin match unop with
  | Op_toString -> mk_intent_desc "ToString" "toString" "conversion to string" [ "toString";
                                                                                 "to string";
                                                                                 "convert to a string";
                                                                                 "give me a string";
                                                                                 "return a string";
                                                                                 "stringify"; ]
  | Op_not -> mk_intent_desc "Not" "not" "not expression" [ "not";
                                                            "complement";
                                                            "if not";
                                                            "boolean negation";
                                                            "negation";
                                                            "invert"; ]
  end

let intent_desc_desc_of_cnl_aggregate aggop =
  begin match aggop with
  | A_total -> mk_intent_desc "Total" "total" "total aggregate" [ "total";
                                                                  "sum";
                                                                  "sum of";
                                                                  "sum over";
                                                                  "add all"]
  | A_avg -> mk_intent_desc "Average" "average" "average aggregate" [ "average";
                                                                      "mean";
                                                                      "arithmetic mean"
                                                                    ]
  end


let intent_of_intent_desc desc =
  Wcs.intent desc.intent_desc_name
    ~examples: desc.intent_desc_examples
    ()

let make_binary_intent binop =
  let intent_desc = intent_desc_desc_of_cnl_binop binop in
  intent_of_intent_desc intent_desc

let make_unary_intent unop =
  let intent_desc = intent_desc_desc_of_cnl_unop unop in
  intent_of_intent_desc intent_desc

let make_aggregate_intent aggop =
  let intent_desc = intent_desc_desc_of_cnl_aggregate aggop in
  intent_of_intent_desc intent_desc

let binary_intents = List.map make_binary_intent binary_op_list

let unary_intents = List.map make_unary_intent unary_op_list

let aggregate_intents = List.map make_aggregate_intent aggregate_op_list

let intents = let open Wcs in [
  intent "mk_binary"
    ~examples: [ "binary";
                 "binary expression";
                 "binary operator";
                 "binary operation";
                 "two";
                 "two inputs";
                 "two operands"]
    ();
  intent "mk_integer"
    ~examples: [ "int";
                 "integer";
                 "integer value";
                 "int value";
                 "numeral" ]
    ();
  Wcs.intent "mk_real"
    ~examples: [ "real";
                 "rational";
                 "float";
                 "double";
                 "floating point";
                 "floating point number";
                 "double precision";
               ]
    ();
  Wcs.intent "mk_number"
    ~examples: [ "number";
                 "numeric";
               ]
    ();
  Wcs.intent "mk_literal"
    ~examples: [ "constant";
                 "constant literal";
                 "constant value";
                 "fixed value";
                 "literal";
                 "literal value" ]
    ();
  Wcs.intent "mk_new"
    ~examples: [ "create entity";
                 "create event";
                 "create new";
                 "create object";
                 "new";
                 "new entity";
                 "new event";
                 "new object" ]
    ();
  Wcs.intent "mk_prop"
    ~examples: [ "attribute";
                 "attribute access";
                 "attribute lookup";
                 "field";
                 "field access";
                 "field lookup";
                 "field name";
                 "lookup";
                 "property";
                 "get field"]
    ();
  intent "mk_aggregate"
    ~examples: [ "aggregate";
                 "reduce";
                 "reduction";]
    ();
  intent "mk_string"
    ~examples: [ "string";
                 "string constant";
                 "string literal";
                 "string value";
                 "text";
                 "text value" ]
    ();
  intent "mk_enum"
    ~examples: [ "enum";
                 "enumeration";
                 "enumeration value";
                 "alternative" ]
    ();
  intent "mk_boolean"
    ~examples: [ "boolean";
                 "boolean constant";
                 "boolean literal";
                 "boolean value";
                 "bool";
                 "bool constant";
                 "bool literal";
                 "bool value";
               ]
    ();
  intent "mk_this"
    ~examples: [ "this";
                 "this entity";
                 "this event";
                 "this expression";
                 "this object";
                 "this stuffs" ]
    ();
  intent "mk_unary"
    ~examples: [ "negation";
                 "one";
                 "one input";
                 "single parameter";
                 "toString";
                 "unary";
                 "unary operator" ]
    ();
  intent "mk_variable"
    ~examples: [ "defined variable";
                 "definition";
                 "lookup";
                 "named entity";
                 "variable";
                 "variable access";
                 "variable lookup" ]
    ();
  Ws_common.intent_help
]@ binary_intents @ unary_intents @ aggregate_intents


let ws_expr bmd =
  let entities = [
    Ws_common.entity_boolean
  ; Wcs.sys_number
  ]@ (Bmd_to_wcs_entities.entities_of_bmd bmd) in

  let integer_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                         let _expr_integer_with_value =
                           let expr = mk_int_as_string_f "@sys-number" in
                           add_dialog_node "Integer with value"
                             ~conditions: "#mk_integer && entities['sys-number']"
                             ~text: "An integer with value @sys-number"
                             ~context: (json_replace "L_int_as_string" "L_int" (* XXX Hack XXX *)
                                          (Ctx.set_expr `Null "expr" expr))
                             () in
                         let expr_integer =
                           add_dialog_node "Integer"
                             ~conditions: "#mk_integer"
                             ~text: "What's the integer value?"
                             () in
                         let _expr_integer_value =
                           let expr = mk_int_as_string_f "@sys-number" in
                           add_dialog_node "Integer value"
                             ~parent: expr_integer
                             ~conditions: "entities['sys-number']"
                             ~text: "An integer with value @sys-number"
                             ~context: (json_replace "L_int_as_string" "L_int" (* XXX Hack XXX *)
                                          (Ctx.set_expr `Null "expr" expr))
                             () in
                         add_usage "Integer" "what integer" expr_integer ["integers"]
                       end in

  let real_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                      let _expr_real_with_value =
                        let expr = mk_float_as_string_f "@sys-number" in
                        add_dialog_node "Real with value"
                          ~conditions: "#mk_real && entities['sys-number']"
                          ~text: "A real with value @sys-number"
                          ~context: (json_replace "L_real_as_string" "L_real" (* XXX Hack XXX *)
                                       (Ctx.set_expr `Null "expr" expr))
                          () in
                      let expr_real =
                        add_dialog_node "Real"
                          ~conditions: "#mk_real"
                          ~text: "What's the real numeric value?"
                          () in
                      let _expr_real_value =
                        let expr = mk_float_as_string_f "@sys-number" in
                        add_dialog_node "Real numeric value"
                          ~parent: expr_real
                          ~conditions: "entities['sys-number']"
                          ~text: "A real (floating point) number with value @sys-number"
                          ~context: (json_replace "L_real_as_string" "L_real" (* XXX Hack XXX *)
                                       (Ctx.set_expr `Null "expr" expr))
                          () in
                      add_usage "Real" "what real number" expr_real ["real (floating point) numbers"]
                    end in

  let number_subdialogs = [
    integer_dialog;
    real_dialog;
  ] in
  let number_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                        let expr_number =
                          add_dialog_node "Number"
                            ~conditions: "#mk_number"
                            ~text: "Is that an integer or a real (floating point) number?"
                            ()
                        in
                        add_sub_dialogs (Some expr_number) number_subdialogs ;
                        add_usage "Number" "what kind of number" expr_number ["integers"; "real (floating point) numbers"];
                      end in

  let boolean_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                         let _expr_literal_boolean_literal_with_value =
                           let expr = mk_boolean_as_string_f "@boolean" in
                           add_dialog_node "Boolean literal with value"
                             ~conditions: "#mk_boolean && entities['boolean']"
                             ~context: (json_replace "L_boolean_as_string" "L_boolean" (* XXX Hack XXX *)
                                          (Ctx.set_expr `Null "expr" expr))
                             ()
                         in
                         let _expr_literal_boolean_value =
                           let expr = mk_boolean_as_string_f "@boolean" in
                           add_dialog_node "Boolean value"
                             ~conditions: "entities['boolean']"
                             ~context: (json_replace "L_boolean_as_string" "L_boolean" (* XXX Hack XXX *)
                                          (Ctx.set_expr `Null "expr" expr))
                             () in
                         let expr_literal_boolean_literal =
                           add_dialog_node "Boolean literal"
                             ~conditions: "#mk_boolean"
                             ~text: "Is that a true or false boolean?"
                             ()
                         in
                         let _expr_literal_boolean_literal_value =
                           let expr = mk_boolean_as_string_f "@boolean" in
                           add_dialog_node "Boolean literal value"
                             ~parent: expr_literal_boolean_literal
                             ~conditions: "entities['boolean']"
                             ~context: (json_replace "L_boolean_as_string" "L_boolean" (* XXX Hack XXX *)
                                          (Ctx.set_expr `Null "expr" expr))
                             () in
                         add_usage "Boolean" "which boolean" expr_literal_boolean_literal ["true"; "false"]
                       end in
  let string_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                        let _expr_string_with_content =
                          let expr = mk_string_f "$has_string" in
                          add_dialog_node "String with content"
                            ~conditions: "#mk_string && $has_string"
                            ~context: (Ctx.set_expr `Null "expr" expr)
                            () in
                        let expr_string =
                          add_dialog_node "String"
                            ~conditions: "#mk_string"
                            ~text: "What is the content of that string?"
                            () in
                        let _expr_string_content =
                          let expr = mk_string_f "<? input.text ?>" in
                          add_dialog_node "String content"
                            ~parent: expr_string
                            ~text: "A string with content '<? input.text ?>'"
                            ~context: (Ctx.set_expr `Null "expr" expr)
                            () in
                        ()
                      end in
  let enum_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                      let _expr_enum =
                        let expr = mk_enum_f "@enum" in
                        add_dialog_node "Enumeration"
                          ~conditions: "#mk_enum && entities['enum']"
                          ~context: (Ctx.set_expr `Null "expr" expr)
                          () in
                      let expr_enum_missing =
                        add_dialog_node "Enumeration - missing enum"
                          ~conditions: "#mk_enum"
                          ~text: "Ok, what enumeration value do you want to use?"
                          () in
                      let _expr_enum_fillin =
                        let expr = mk_enum_f "@enum" in
                        add_dialog_node "Enumeration - fill enum"
                          ~parent:expr_enum_missing
                          ~conditions: "entities['enum']"
                          ~context: (Ctx.set_expr `Null "expr" expr)
                          () in
                      add_usage "Enum" "which enumeration value" expr_enum_missing (Ws_common.bmd_enumerations bmd);
                    end in
  let literal_subdialogs = [
    integer_dialog;
    real_dialog;
    number_dialog;
    boolean_dialog;
    string_dialog;
    enum_dialog
  ] in
  let literal_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                         let expr_literal =
                           add_dialog_node "Literal"
                             ~conditions: "#mk_literal"
                             ~text: "What kind of literal value? (int, string, ...)"
                             () in
                         add_sub_dialogs (Some expr_literal) literal_subdialogs ;
                         add_usage "Literal" "what kind of literal" expr_literal ["integers"; "real (floating point) numbers"; "strings"; "booleans"; "enumerations"];
                       end in
  let variable_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                          let _expr_variable_with_content =
                            let expr = mk_var_f "$has_string" in
                            add_dialog_node "Variable with content"
                              ~conditions: "#mk_variable && $has_string"
                              ~context: (Ctx.set_expr `Null "expr" expr)
                              () in
                          let expr_variable =
                            add_dialog_node "Variable"
                              ~conditions: "#mk_variable"
                              ~text: "What's the variable's name?"
                              () in
                          let _expr_variable_name =
                            let expr = mk_var_f "<? input.text ?>" in
                            add_dialog_node "Variable name"
                              ~parent: expr_variable
                              ~text: "A variable with name '<? input.text ?>'"
                              ~context: (Ctx.set_expr `Null "expr" expr)
                              () in
                          () ;
                        end in
  let property_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                          let expr_prop =
                            let expr = mk_get_f (mk_expr_undefined()) "@field" in
                            dialog_node "Property"
                              ~conditions: "#mk_prop && entities['field']"
                              ~context: (Ctx.set_expr `Null "expr" expr)
                              ()
                          in add_node expr_prop ;

                          let expr_prop_missing =
                            dialog_node "Property - missing field"
                              ~conditions: "#mk_prop"
                              ~text: "Ok, what property do you want to access?"
                              ()
                          in add_node expr_prop_missing ;

                          let expr_prop_fillin =
                            let expr = mk_get_f (mk_expr_undefined()) "@field" in
                            dialog_node "Property - fill field"
                              ~parent:expr_prop_missing
                              ~conditions: "entities['field']"
                              ~context: (Ctx.set_expr `Null "expr" expr)
                              ()
                          in add_node expr_prop_fillin ;

                          add_usage "Prop" "which field" expr_prop_missing (Ws_common.bmd_fields bmd);
                        end in
  let aggregate_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                           let expr_agg_missing_both =
                             dialog_node "Aggregate - missing both"
                               ~conditions: "#mk_aggregate"
                               ~text: "Ok, we are creating an aggregate operation.  What aggregation operator should we use?"
                               ()
                           in add_node expr_agg_missing_both ;

                           let mk_agg_nodes op =
                             let op_desc = intent_desc_desc_of_cnl_aggregate op in
                             let agg_dname = "Aggregate _"^op_desc.intent_desc_node_name ^ "_ " in

                             let expr_agg_full =
                               let expr = mk_aggregate_f (mk_expr_undefined()) op "@field" in
                               dialog_node (agg_dname ^"full")
                                 ~conditions: ("#"^op_desc.intent_desc_name ^ " && entities['field']")
                                 ~context: (Ctx.set_expr `Null "expr" expr)
                                 () in
                             add_node expr_agg_full ;
                             let expr_agg_missing_field =
                               dialog_node (agg_dname ^"_ - missing field")
                                 ~conditions: ("#"^op_desc.intent_desc_name)
                                 ~text: ("Ok, we are creating a "^ op_desc.intent_desc_output ^" operation.  What field are we looking at?")
                                 () in
                             add_node expr_agg_missing_field ;

                             let expr_agg_missing_field_field =
                               let expr = mk_aggregate_f (mk_expr_undefined()) op "@field" in
                               dialog_node (agg_dname ^"- missing field - field")
                                 ~parent:expr_agg_missing_field
                                 ~conditions: "entities['field']"
                                 ~context: (Ctx.set_expr `Null "expr" expr)
                                 () in
                             add_node expr_agg_missing_field_field ;

                             add_usage (agg_dname ^"field") "which field" expr_agg_missing_field (Ws_common.bmd_fields bmd);

                             let expr_agg_missing_both_op_and_field =
                               let expr = mk_aggregate_f (mk_expr_undefined()) op "@field" in
                               dialog_node (agg_dname ^"- missing both - op and field")
                                 ~parent:expr_agg_missing_both
                                 ~conditions: ("#" ^ op_desc.intent_desc_name ^ " && entities['field']")
                                 ~context: (Ctx.set_expr `Null "expr" expr)
                                 () in
                             add_node expr_agg_missing_both_op_and_field ;

                             let expr_agg_missing_both_op =
                               dialog_node (agg_dname ^"- missing both - op")
                                 ~parent:expr_agg_missing_both
                                 ~conditions: ("#"^op_desc.intent_desc_name)
                                 ~text: ("Ok, we are creating a "^ op_desc.intent_desc_output ^ " operation.  What field are we looking at?")
                                 () in
                             add_node expr_agg_missing_both_op ;

                             let expr_agg_missing_both_op_field =
                               let expr = mk_aggregate_f (mk_expr_undefined()) op "@field" in
                               dialog_node (agg_dname ^"- missing both - op - field")
                                 ~parent:expr_agg_missing_both_op
                                 ~conditions: "entities['field']"
                                 ~context: (Ctx.set_expr `Null "expr" expr)
                                 () in
                             add_node expr_agg_missing_both_op_field

                           ; add_usage (agg_dname ^"both field") "which field" expr_agg_missing_both_op (Ws_common.bmd_fields bmd)

                           in List.iter mk_agg_nodes aggregate_op_list ;

                           add_usage "Aggregate" "wich aggregate operation" expr_agg_missing_both
                             (List.map (fun aggop -> (intent_desc_desc_of_cnl_aggregate aggop).intent_desc_output) aggregate_op_list);
                         end in
  let binary_operators_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                                  let expr_binary binop =
                                    let intent_desc = intent_desc_desc_of_cnl_binop binop in
                                    let expr = mk_binop_f binop in
                                    dialog_node intent_desc.intent_desc_node_name
                                      ~conditions: ("#" ^ intent_desc.intent_desc_name)
                                      ~text: ("Alright, let's build an " ^ intent_desc.intent_desc_output)
                                      ~context: (Ctx.set_expr `Null "expr" expr)
                                      ()
                                  in List.iter (fun b -> add_node (expr_binary b))
                                       binary_op_list;
                                end in
  let binary_operator_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                                 let expr_binary_operator =
                                   add_dialog_node "Binary operator"
                                     ~conditions: "#mk_binary"
                                     ~text: "Ok, which binary operator are we looking at?"
                                     ()
                                 in
                                 add_sub_dialogs (Some expr_binary_operator) [binary_operators_dialog] ;
                                 add_usage "Binary operator" "what type of binary operator" expr_binary_operator
                                   (List.map (fun binop -> (intent_desc_desc_of_cnl_binop binop).intent_desc_output) binary_op_list);
                               end in
  let unary_operators_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                                 let expr_unary unop =
                                   let intent_desc = intent_desc_desc_of_cnl_unop unop in
                                   let expr = mk_unop_f unop in
                                   dialog_node intent_desc.intent_desc_node_name
                                     ~conditions: ("#" ^ intent_desc.intent_desc_name)
                                     ~text: ("Alright, let's build an " ^ intent_desc.intent_desc_output)
                                     ~context: (Ctx.set_expr `Null "expr" expr)
                                     ()
                                 in List.iter (fun b -> add_node (expr_unary b))
                                      unary_op_list;
                               end in
  let unary_operator_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                                let expr_unary_operator =
                                  add_dialog_node "Unary operator"
                                    ~conditions: "#mk_unary"
                                    ~text: "Ok, which unary operator are we looking at?"
                                    () in
                                add_sub_dialogs (Some expr_unary_operator) [unary_operators_dialog] ;
                                add_usage "Unary operator" "what type of unary operator" expr_unary_operator
                                  (List.map (fun unop -> (intent_desc_desc_of_cnl_unop unop).intent_desc_output) unary_op_list);
                              end in
  let this_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                      let _expr_this =
                        let expr = mk_this_f "@entity" in
                        add_dialog_node "this"
                          ~conditions: "#mk_this && @entity"
                          ~context: (Ctx.set_expr `Null "expr" expr)
                          () in
                      let expr_this_missing =
                        add_dialog_node "this - missing event"
                          ~conditions: "#mk_this"
                          ~text: "Ok, what event are you talking about?"
                          () in
                      let _expr_this_fillin =
                        let expr = mk_this_f "@entity" in
                        add_dialog_node "this - fill event"
                          ~parent:expr_this_missing
                          ~conditions: "@entity"
                          ~context: (Ctx.set_expr `Null "expr" expr)
                          () in
                      add_usage "This" "which entity" expr_this_missing (Ws_common.bmd_entities bmd)
                    end in
  let new_dialog = Ws_common.make_child_dialog begin fun (module SD) -> let open SD in
                     let expr_new =
                       dialog_node "new entity"
                         ~conditions: "#mk_new"
                         ~text: "Ok, what event would you like to create?"
                         () in

                     let expr_new_with_entity ename fnames =
                       let expr = mk_new_event_for_concept_f ename fnames in
                       dialog_node ("new_with_" ^ ename)
                         ~conditions: ("#mk_new && entities['entity']?.contains('" ^ ename ^ "')")
                         ~context: (Ctx.set_expr `Null "expr" expr)
                         ()
                     in
                     let expr_new_for_entity ename fnames =
                       let expr = mk_new_event_for_concept_f ename fnames in
                       dialog_node ("new_missing_" ^ ename)
                         ~parent:expr_new
                         ~conditions: ("entities['entity']?.contains('" ^ ename ^ "')")
                         ~context: (Ctx.set_expr `Null "expr" expr)
                         ()
                     in
                     (* returns a pair of top level combined node and child node *)
                     let expr_news ename fnames =
                       (expr_new_with_entity ename fnames, expr_new_for_entity ename fnames) in

                     let add_expr_new_for_bmd_concept concept =
                       let (ename,_,ctype) = concept.concept_desc in
                       begin match ctype.type_desc with
                       | BT_rec rec_desc ->
                           [(expr_news ename (List.map fst rec_desc))]
                       | _ -> []
                       end
                     in
                     let add_expr_new_for_bmd =
                       let concepts = bmd.schema_desc in
                       List.concat (List.map add_expr_new_for_bmd_concept concepts)

                     in let (top_new_nodes, child_new_nodes) = List.split add_expr_new_for_bmd
                     in add_nodes top_new_nodes ;
                     add_node expr_new ;
                     add_nodes child_new_nodes ;
                     add_usage "New" "which entity" expr_new (Ws_common.bmd_entities bmd);
                   end in
  let top_subdialogs =
    [literal_dialog]@
    literal_subdialogs@[
      variable_dialog;
      property_dialog;
      aggregate_dialog;
      binary_operator_dialog;
      binary_operators_dialog;
      unary_operator_dialog;
      unary_operators_dialog;
      this_dialog;
      new_dialog;
    ] in
  let ws_nodes = Ws_common.make_top_dialog begin fun (module SD) -> let open SD in
                   add_sub_dialogs None top_subdialogs ;

                   let _expr_select_field_expr =
                     add_dialog_node "Select field expression"
                       ~text: "What is the $field of the $entity ($kind$n)?"
                       ~conditions: "conversation_start && $field"
                       () in
                   let expr_select_expr =
                     add_dialog_node "Select expression"
                       ~text: "What is $prompt ($kind$n)?"
                       ~conditions: "conversation_start"
                       () in
                   add_usage_toplevel "Expression" "what type of expression" expr_select_expr [
                     "literals";
                     "variables";
                     "property (field) access";
                     "aggregates";
                     "binary operators";
                     "unary operators";
                     "this access";
                     "creating a new object";
                   ]
                 end in
  Wcs.workspace "rulebot-expr"
    ~description: "Build expressions for rules"
    ~entities: entities
    ~intents: intents
    ~dialog_nodes: ws_nodes
    ()
