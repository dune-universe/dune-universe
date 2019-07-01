module String = Stdcompat.String
open Expansion_type

let is_var_expr v = Str.string_match Regex.for_is_var_expr v 0


let variable_expression_of_string str =
  (* Detect a composite value *)
  match Str.string_match Regex.compsite_from_var_name str 0 with
  | true -> (
      let new_name = Str.matched_group 1 str in
      Template.create_variable_expression new_name ~value_modifier:Composite
    )
  | false -> (
      match Str.string_match Regex.trim_from_var_name str 0 with
      (* -1 will cause sub to raise Invalid_argument, and return the full var *)
      | false -> Template.create_variable_expression str ~value_modifier:NoModifier
      | true -> (
          let new_name = Str.matched_group 1 str in
          let trim_num = Str.matched_group 2 str |> int_of_string in
          Template.create_variable_expression new_name ~value_modifier:(Prefix trim_num)
        )
    )

let part_of_string str =
  match is_var_expr str with
  | false -> Template.Literal str
  | true ->
    let _ = Str.string_match Regex.for_prefix str 0 in
    let expansion_type = Str.matched_group 1 str |> expansion_type_of_string in
    let expression_strings = Str.matched_group 2 str |> String.split_on_char ',' in
    let variable_expressions = List.map variable_expression_of_string expression_strings in
    Template.Expression (Template.create_expression expansion_type variable_expressions)


let template_of_string str =
  let rec aux index template =
    if Str.string_match Regex.for_tokens str index && index < (String.length str) then
      let new_index = Str.match_end () in
      let part = Str.matched_string str |> part_of_string in
      aux new_index (Template.add_part part template)
    else
      template
  in
  aux 0 Template.empty
