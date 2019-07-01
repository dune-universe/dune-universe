module String = Stdcompat.String
module Buffer = Stdcompat.Buffer


type variable = [
  | `String of string
  | `List of string list
  | `Assoc of (string * string) list
]

(* Encoding *)
let encode_char c =
  Char.code c
  |> Printf.sprintf "%%%X"

let encode_str rex =
  Str.global_substitute
    rex
    (fun m -> String.get (Str.matched_string m) 0
              |> encode_char)

(* This is the same as a standard encodeUriComponent, but also encoding ! *)
let uri_encode_reserved = encode_str Regex.for_encode_reserved

let uri_encode_full = encode_str Regex.for_encode_full

let trim_var var modifier =
  let open Template in
  match modifier with
  | Composite | NoModifier -> var
  | Prefix trim -> (
      try
        String.sub var 0 trim
      with
      | Invalid_argument _ -> var
    )

let trim_and_encode_var expr_type modifier var =
  let open Expansion_type in
  let trimmed_var = trim_var var modifier in
  match expr_type with
  | Fragment
  | Reserved -> uri_encode_full trimmed_var
  | _ -> uri_encode_reserved trimmed_var


let simple_expr buff _ var =
  Buffer.add_string buff var

let form_query buff var_name var =
  Buffer.add_string buff var_name; Buffer.add_char buff '='; Buffer.add_string buff var

let path_parameter buff var_name var =
  Buffer.add_string buff var_name;
  match var with
  | "" -> ()
  | var -> Buffer.add_char buff '='; Buffer.add_string buff var

let determine_expr_function buff expr_type =
  let open Expansion_type in
  buff
  |> match expr_type with
  | FormQuery | FormQueryContinuation -> form_query
  | PathParameter -> path_parameter
  | _ -> simple_expr

let rec combine_list_of_vars ?buff:(buff=Buffer.create 10) expr_type modifier = function
  | [] -> ""
  | var::[] -> (
      trim_and_encode_var expr_type modifier var |> Buffer.add_string buff;
      Buffer.contents buff
    )
  | var::rest -> (
      trim_and_encode_var expr_type modifier var |> Buffer.add_string buff;
      Buffer.add_char buff ',';
      combine_list_of_vars ~buff expr_type modifier rest
    )

let combine_assoc_of_vars expr_type trim vars =
  vars
  |> List.fold_left (fun acc (a, b) -> a::b::acc) []
  |> combine_list_of_vars expr_type trim

let apply_standard_modifier buff sep_str f var_name variables expr_type modifier =
  let resolved_var = match List.assoc var_name variables with
    | `String var -> trim_and_encode_var expr_type modifier var
    | `List vars -> combine_list_of_vars expr_type modifier vars
    | `Assoc vars -> combine_assoc_of_vars expr_type modifier vars
  in
  f var_name resolved_var;
  Buffer.add_char buff sep_str

let apply_composite_modifier buff sep_str f var_name variables expr_type modifier =
  match List.assoc var_name variables with
  | `String var -> (
      let resolved_var = trim_and_encode_var expr_type modifier var in
      f var_name resolved_var;
      Buffer.add_char buff sep_str
    )
  | `List vars -> List.iter (fun var ->
      let resolved_var = trim_and_encode_var expr_type modifier var in
      f var_name resolved_var;
      Buffer.add_char buff sep_str
    ) vars

  | `Assoc vars -> List.iter (fun (var_name, var) ->
      let resolved_var = trim_and_encode_var expr_type modifier var in
      form_query buff var_name resolved_var;
      Buffer.add_char buff sep_str
    ) vars


let add_var_to_buffer buffer variables expansion_type sep_str f variable_expression =
  try
    let name = Template.get_variable_expression_name variable_expression in
    let value_modifier = Template.get_variable_expression_modifier variable_expression in
    match value_modifier with
    | Composite -> apply_composite_modifier buffer sep_str f name variables expansion_type value_modifier
    | _ -> apply_standard_modifier buffer sep_str f name variables expansion_type value_modifier
  with
  | Not_found -> ()

let add_op_to_buffer buffer = function
  | Expansion_type.Reserved -> ()
  | expansion_type ->
    Expansion_type.string_of_expansion_type expansion_type |> Buffer.add_string buffer

let resolve_expression buffer variables expression =
  let expansion_type = Template.get_expansion_type expression in
  let variable_expressions = Template.get_variable_expressions expression in
  let sep_str = Expansion_type.separator_for_expansion_type expansion_type in
  let f = determine_expr_function buffer expansion_type in
  add_op_to_buffer buffer expansion_type;
  List.iter (add_var_to_buffer buffer variables expansion_type sep_str f) variable_expressions;
  match Buffer.length buffer - 1 with
  | -1 | 0 -> ()
  | len -> Buffer.truncate buffer len


let resolve_part variables part =
  let open Template in
  let buffer = Buffer.create 10 in
  let _ = match part with
    | Literal str -> Buffer.add_string buffer str
    | Expression expr -> resolve_expression buffer variables expr
  in
  buffer

let expand_template ~template ~variables =
  let buffer = Buffer.create 10 in
  List.iter (fun part ->
      resolve_part variables part
      |> Buffer.add_buffer buffer
    ) (Template.parts_of_t template);
  Buffer.contents buffer


let expand_template_with_strings ~template ~variables =
  expand_template
    ~template
    ~variables:(
      List.map
        (fun (var_name, var) -> (var_name, `String var))
        variables
    )

let expand_template_with_lists ~template ~variables =
  expand_template
    ~template
    ~variables:(
      List.map
        (fun (var_name, var) -> (var_name, `List var))
        variables
    )

let expand_template_with_assoc_list ~template ~variables =
  expand_template
    ~template
    ~variables:(
      List.map
        (fun (var_name, var) -> (var_name, `Assoc var))
        variables
    )
