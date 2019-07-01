module Buffer = Stdcompat.Buffer

type value_modifier =
  | NoModifier
  | Prefix of int (* {var:N} *)
  | Composite (* {var*} *)

type variable_expression = {
  name: string;
  value_modifier: value_modifier;
}

type expression = {
  expansion_type: Expansion_type.t;
  variable_expressions: variable_expression list;
}

type template_part =
  | Literal of string
  | Expression of expression

type t = {
  parts: template_part list
}

let string_of_value_modifier = function
  | NoModifier -> ""
  | Prefix i -> Printf.sprintf ":%i" i
  | Composite -> "*"


let string_of_variable_expression { name; value_modifier; } =
  name ^ (string_of_value_modifier value_modifier)


let create parts = { parts = List.rev parts }

let create_expression expansion_type variable_expressions = {
  expansion_type;
  variable_expressions
}

let create_single_expression expansion_type variable_expression = {
  expansion_type;
  variable_expressions = [variable_expression]
}

let create_variable_expression ?value_modifier:(value_modifier = NoModifier) name = {
  name;
  value_modifier
}

let empty = { parts = [] }

let add_part part t = { parts = part::t.parts }

let add_literal lit = add_part (Literal lit)

let add_expression expansion_type variable_expressions =
  Expression (create_expression expansion_type variable_expressions)
  |> add_part

let add_single_expression expansion_type variable_expression =
  Expression (create_single_expression expansion_type variable_expression)
  |> add_part


let parts_of_t { parts; } = List.rev parts

let get_expansion_type { expansion_type; _ } = expansion_type
let get_variable_expressions { variable_expressions; _ } = variable_expressions

let get_variable_expression_name { name; _ } = name
let get_variable_expression_modifier { value_modifier; _ } = value_modifier

let part_is_literal = function
  | Literal _ -> true
  | _ -> false
let part_is_expression = function
  | Expression _ -> true
  | _ -> false

let get_variable_names { parts; } =
  List.filter part_is_expression parts
  |> List.map (function
      | Expression e -> (
          get_variable_expressions e
          |> List.map get_variable_expression_name
        )
      | _ -> assert false
    )
  |> List.rev
  |> List.flatten

let buffer_of_expression { expansion_type; variable_expressions; } =
  let b = Buffer.create 10 in
  if List.length variable_expressions = 0 then
    b
  else begin
    Buffer.add_string b (Expansion_type.string_of_expansion_type expansion_type);
    List.iter (fun ve ->
        Buffer.add_string b (string_of_variable_expression ve);
        Buffer.add_char b ','
      ) variable_expressions;
    Buffer.truncate b ((Buffer.length b) - 1);
    b
  end

let string_of_template { parts; } =
  let b = Buffer.create 10 in
  List.rev parts
  |> List.iter (function
      | Literal s -> Buffer.add_string b s
      | Expression e -> (
          Buffer.add_char b '{';
          Buffer.add_buffer b (buffer_of_expression e);
          Buffer.add_char b '}'
        )
    );
  Buffer.contents b
