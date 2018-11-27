open Expr
open Imperative
open Pretty_printer

(* Generate C definitions of functions *)

let c_representation : representation -> string = function
  | Representation_float32 -> "float"
  | Representation_float64 -> "double"

let c_unit (u : unit_) : string =
  let positive, negative = List.partition (fun (_name,power) -> power > 0) u in
  let buf = Buffer.create 20 in
  let sep = ref "" in
  Printf.bprintf buf "unit(";
  positive |> List.iter (fun (name, power) ->
                         for _ = 1 to power do
                           Printf.bprintf buf "%s%s" (!sep) name;
                           sep := "*"
                         done);
  if !sep = "" then Printf.bprintf buf "1";
  negative |> List.iter (fun (name, power) ->
                         for _ = 1 to -power do
                           Printf.bprintf buf "/%s" name
                         done);
  Printf.bprintf buf ")";
  Buffer.contents buf

let c_literal_suffix : representation -> string = function
  | Representation_float32 -> "f"
  | Representation_float64 -> ""

let print_c_reference (p: pretty_printer) ~(name: string) ~(indices: string list) =
  pretty_printer_print p name;
  indices |> List.iter (fun dim -> pretty_printer_print p (Printf.sprintf "[%s]" dim))

let print_c_function_declaration (p: pretty_printer) ~(function_name: string) ~(index_args: string list) ~(value_args: (string * representation * unit_) list) ~(return_value: (expr * representation * unit_) option): unit
  =
  let return_type : string = match return_value with
    | None -> "void"
    | Some(_expr, repr, unit_) -> c_unit unit_ ^ " " ^ c_representation repr
  in
  pretty_printer_print p (Printf.sprintf "static %s %s(" return_type function_name);
  let sep = ref "" in
  index_args |> List.iter (fun index_arg ->
    pretty_printer_print p (Printf.sprintf "%sint %s" (!sep) index_arg);
    sep := ", ");
  value_args |> List.iter (fun (value_arg, representation, unit_) ->
    pretty_printer_print p (Printf.sprintf "%s%s %s %s" (!sep) (c_unit unit_) (c_representation representation) value_arg);
    sep := ", ");
  if !sep = "" then pretty_printer_print p "void";
  pretty_printer_print p ")"

type c_binop =
  | C_binop_plus
  | C_binop_minus
  | C_binop_star
  | C_binop_slash
  | C_binop_le
  | C_binop_ge
  | C_binop_lt
  | C_binop_gt

let c_binop_string = function
  | C_binop_plus -> "+"
  | C_binop_minus -> "-"
  | C_binop_star -> "*"
  | C_binop_slash -> "/"
  | C_binop_le -> "<="
  | C_binop_ge -> ">="
  | C_binop_lt -> "<"
  | C_binop_gt -> ">"

type c_precedence =
  | C_precedence_parenthesized
  | C_precedence_comparative
  | C_precedence_additive_lhs
  | C_precedence_additive_rhs
  | C_precedence_multiplicative_lhs
  | C_precedence_multiplicative_rhs
  | C_precedence_unary

let c_binop_precedences = function
  | C_binop_plus -> C_precedence_additive_lhs, C_precedence_additive_rhs
  | C_binop_minus -> C_precedence_additive_lhs, C_precedence_additive_rhs
  | C_binop_star -> C_precedence_multiplicative_lhs, C_precedence_multiplicative_rhs
  | C_binop_slash -> C_precedence_multiplicative_lhs, C_precedence_multiplicative_rhs
  | C_binop_le | C_binop_ge | C_binop_lt | C_binop_gt -> C_precedence_comparative, C_precedence_comparative

type binop_printing_style =
  | Infix of c_binop
  | Binary_function of string

let rec print_c_expr (p: pretty_printer) (outer_precedence: c_precedence) (result_representation: representation) (e: expr): unit =
  match e with
  | Expr_const 0.0 -> pretty_printer_print p "0"
  | Expr_const f -> pretty_printer_print p (string_of_float f ^ c_literal_suffix result_representation)
  | Expr_ref(name, subscripts) -> print_c_reference p ~name ~indices:subscripts
  | Expr_unop(Unop_neg, e1) ->
      pretty_printer_print p "-";
      print_c_expr p C_precedence_unary result_representation e1
  | Expr_unop(Unop_abs, e1) ->
      pretty_printer_print p (match result_representation with
                              | Representation_float32 -> "fabsf("
                              | Representation_float64 -> "fabs(");
      print_c_expr p C_precedence_parenthesized result_representation e1;
      pretty_printer_print p ")"
  | Expr_binop(b, e1, e2) -> begin
      let printing_style =
        match b with
        | Binop_add -> Infix(C_binop_plus)
        | Binop_sub -> Infix(C_binop_minus)
        | Binop_mul -> Infix(C_binop_star)
        | Binop_div -> Infix(C_binop_slash)
        | Binop_min -> Binary_function(match result_representation with
                                       | Representation_float32 -> "fminf"
                                       | Representation_float64 -> "fmin")
        | Binop_max -> Binary_function(match result_representation with
                                       | Representation_float32 -> "fmaxf"
                                       | Representation_float64 -> "fmax")
        | Binop_le -> Infix(C_binop_le)
        | Binop_ge -> Infix(C_binop_ge)
        | Binop_lt -> Infix(C_binop_lt)
        | Binop_gt -> Infix(C_binop_gt)
      in
      match printing_style with
      | Infix c_binop -> print_c_binop_expr p outer_precedence result_representation e1 c_binop e2
      | Binary_function f ->
         pretty_printer_print p f;
         pretty_printer_print p "(";
         print_c_expr p C_precedence_parenthesized result_representation e1;
         pretty_printer_print p ", ";
         print_c_expr p C_precedence_parenthesized result_representation e2;
         pretty_printer_print p ")"
    end
  | Expr_if(e1, e2, e3) ->
     pretty_printer_print p "(";
     print_c_expr p C_precedence_parenthesized Representation_float64 e1;
     pretty_printer_print p " ? ";
     print_c_expr p C_precedence_parenthesized result_representation e2;
     pretty_printer_print p " : ";
     print_c_expr p C_precedence_parenthesized result_representation e3;
     pretty_printer_print p ")"
  | Expr_index_eq_ne(i1, i2, e1, e2) ->
     pretty_printer_print p (Printf.sprintf "(%s == %s ? " i1 i2);
     print_c_expr p C_precedence_parenthesized result_representation e1;
     pretty_printer_print p " : ";
     print_c_expr p C_precedence_parenthesized result_representation e2;
     pretty_printer_print p ")"

and print_c_binop_expr (p: pretty_printer) (outer_precedence: c_precedence) (result_representation : representation) (e1: expr) (c_binop: c_binop) (e2: expr): unit
    =
  let inner_precedence_lhs, inner_precedence_rhs = c_binop_precedences c_binop in
  let lowest_precedence = min inner_precedence_lhs inner_precedence_rhs in
  if lowest_precedence < outer_precedence then pretty_printer_print p "(";
  print_c_expr p inner_precedence_lhs result_representation e1;
  pretty_printer_print p " ";
  pretty_printer_print p (c_binop_string c_binop);
  pretty_printer_print p " ";
  print_c_expr p inner_precedence_rhs result_representation e2;
  if lowest_precedence < outer_precedence then pretty_printer_print p ")"



let open_c_for_loop (p: pretty_printer) (m: module_) ~(index: string) ~(range_name: string)
    =
  let r:range = module_find_range m range_name in
  pretty_printer_open_block p (Printf.sprintf "for (int %s = 0; %s < %s; %s++) {" index index r.range_c_name index)

let close_c_for_loop p
    =
  pretty_printer_close_block p "}"

let print_lhs (p: pretty_printer) (lhs: lhs): unit =
  match lhs with
  | Lhs_global(variable_name, variable_subscripts) ->
    print_c_reference p ~name:variable_name ~indices:variable_subscripts;
  | Lhs_local(variable_name, _representation) ->
    pretty_printer_print p variable_name

let lhs_representation (m: module_) (lhs: lhs): representation =
  match lhs with
  | Lhs_global(variable_name, _variable_subscripts) ->
     (module_find_variable m variable_name).variable_representation
  | Lhs_local(_variable_name, representation) ->
     representation

let rec print_step (p: pretty_printer) (m: module_) (step: step): unit =
  match step with
  | Step_let(variable_name, representation, unit_, expr) ->
    pretty_printer_print p (c_unit unit_);
    pretty_printer_print p " ";
    pretty_printer_print p (c_representation representation);
    pretty_printer_print p " ";
    pretty_printer_print p variable_name;
    pretty_printer_print p " = ";
    print_c_expr p C_precedence_parenthesized representation expr;
    pretty_printer_println p ";";
  | Step_do(statement) -> print_statement p m statement
and print_statement (p: pretty_printer) (m: module_) (statement: statement): unit =
  match statement with
  | Statement_assign(lhs, expr) ->
    print_lhs p lhs;
    pretty_printer_print p " = ";
    print_c_expr p C_precedence_parenthesized (lhs_representation m lhs) expr;
    pretty_printer_println p ";"
  | Statement_increment(lhs, expr) ->
    print_lhs p lhs;
    pretty_printer_print p " += ";
    print_c_expr p C_precedence_parenthesized (lhs_representation m lhs) expr;
    pretty_printer_println p ";"
  | Statement_scale(lhs, expr) ->
    print_lhs p lhs;
    pretty_printer_print p " *= ";
    print_c_expr p C_precedence_parenthesized (lhs_representation m lhs) expr;
    pretty_printer_println p ";"
  | Statement_for(subscript_name, range_name, body) ->
    open_c_for_loop p m ~index:subscript_name ~range_name:range_name;
    begin
      match body with
      | Statement_block steps -> List.iter (print_step p m) steps
      | _ -> print_statement p m body
    end;
    close_c_for_loop p
  | Statement_block(body) ->
    pretty_printer_open_block p "{";
    body |> List.iter (print_step p m);
    pretty_printer_close_block p "}"

let print_procedures (p : pretty_printer) (m : module_) : unit =
  m.module_procedures |> List.iter
      (fun (procedure_name, proc) ->
        print_c_function_declaration p ~function_name:procedure_name ~index_args:proc.procedure_index_args ~value_args:proc.procedure_value_args ~return_value:proc.procedure_return_value;
        pretty_printer_open_block p " {";
        proc.procedure_index_args |> List.iter (fun index_arg -> pretty_printer_println p (Printf.sprintf "(void) %s;" index_arg));
        proc.procedure_value_args |> List.iter (fun (value_arg, _, _) -> pretty_printer_println p (Printf.sprintf "(void) %s;" value_arg));
        proc.procedure_body |> List.iter (print_step p m);
        begin match proc.procedure_return_value with
        | None -> ()
        | Some(expr, representation, _) ->
           pretty_printer_print p "return ";
           print_c_expr p C_precedence_parenthesized representation expr;
           pretty_printer_println p ";"
        end;
        pretty_printer_close_block p "}";
      )

let print_global_variable (p : pretty_printer) (m : module_) (variable_name : string) (v : variable) (linkage : string): unit =
  pretty_printer_print p linkage;
  let dimensions =
    v.variable_dimensions |> List.map (fun range_name -> (module_find_range m range_name).range_c_name)
  in
  if dimensions <> [] then pretty_printer_print p "ALIGN ";
  pretty_printer_print p (c_unit v.variable_unit);
  pretty_printer_print p " ";
  pretty_printer_print p (c_representation v.variable_representation);
  pretty_printer_print p " ";
  print_c_reference p ~name:variable_name ~indices:dimensions;
  pretty_printer_println p ";"

let print_global_variables (p : pretty_printer) (m : module_) : unit =
  m.module_variables
  |> List.iter
       (fun (variable_name, v) ->
         let linkage = match v.variable_linkage with
           | Linkage_extern -> "extern "
           | Linkage_public -> ""
           | Linkage_private -> "static "
         in
         print_global_variable p m variable_name v linkage)

let print_module (p : pretty_printer) (m : module_) : unit =
  print_global_variables p m;
  print_procedures p m

let linkage_is_externally_visible = function
  | Linkage_extern -> true
  | Linkage_public -> true
  | Linkage_private -> false

let print_header_global_variables (p : pretty_printer) (m : module_) : unit =
  m.module_variables
  |> List.filter (fun (_, v) -> linkage_is_externally_visible v.variable_linkage)
  |> List.iter
       (fun (variable_name, v) ->
         print_global_variable p m variable_name v "extern ")

let print_module_header (p : pretty_printer) (m : module_) : unit =
  print_header_global_variables p m
