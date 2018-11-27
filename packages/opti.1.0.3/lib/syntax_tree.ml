open Expr
open Unit

(* Ranges *)

type range = { range_c_name: string }

(* Representations of values *)

type representation =
  | Representation_float32
  | Representation_float64

(* Variables and their definitions *)

type subscript = Subscript of string

type definition_expr =
    { definition_expr_summation_subscripts: (string * string) list;
      definition_expr_summee: expr }

type linkage =
  | Linkage_extern
  | Linkage_public
  | Linkage_private
  | Linkage_phantom

type definition =
  | Definition_given
  | Definition_expr of definition_expr

type variable = {
  variable_linkage: linkage;
  variable_subscripts: (string * string) list;
  variable_unit: unit_;
  variable_representation: representation;
  variable_definition: definition;
}


(* Computational goals *)

type goal =
  | Goal_get of string
  | Goal_recompute of string
  | Goal_propagate_delta of string list
  | Goal_set of string list
  | Goal_increment of string list
  | Goal_scale_unit of string

(* Complete specifications *)

type specification = {
    specification_ranges: (string * range) list;
    specification_variables: (string * variable) list;
    specification_goals: (string * goal) list;
  }


exception Range_not_found of string
exception Variable_not_found of string
exception Goal_not_found of string

let specification_find_range (s: specification) (range_name: string): range
    =
  try List.assoc range_name s.specification_ranges
  with
    Not_found -> raise (Range_not_found range_name)


let specification_find_variable (s: specification) (variable_name: string): variable
    =
  try List.assoc variable_name s.specification_variables
  with
    Not_found -> raise (Variable_not_found variable_name)


let specification_find_goal (s: specification) (goal_name: string): goal
    =
  try List.assoc goal_name s.specification_goals
  with
    Not_found -> raise (Goal_not_found goal_name)


let specification_add_range range_name range (s: specification)
    =
  { specification_ranges = (range_name,range) :: s.specification_ranges;
    specification_variables = s.specification_variables;
    specification_goals = s.specification_goals;
  }

let specification_add_variable variable_name variable (s: specification)
    =
  { specification_ranges = s.specification_ranges;
    specification_variables = (variable_name, variable) :: s.specification_variables;
    specification_goals = s.specification_goals;
  }

let specification_add_goal goal_name goal (s: specification)
    =
  { specification_ranges = s.specification_ranges;
    specification_variables = s.specification_variables;
    specification_goals = (goal_name,goal) :: s.specification_goals;
  }

let empty_specification =
  { specification_ranges = [];
    specification_variables = [];
    specification_goals = []; }











let map_subscripts_in_definition_expr (f: string -> string) (de: definition_expr)
    =
  { definition_expr_summation_subscripts = de.definition_expr_summation_subscripts |> List.map (fun (subscript_name, range_name) -> f subscript_name, range_name);
    definition_expr_summee = map_subscripts_in_expr f de.definition_expr_summee; }

let map_subscripts_in_definition (f: string -> string) (d: definition)
    =
  match d with
  | Definition_given -> Definition_given
  | Definition_expr de -> Definition_expr(map_subscripts_in_definition_expr f de)

let map_subscripts_in_variable (f: string -> string) (v: variable)
    =
  { variable_linkage = v.variable_linkage;
    variable_subscripts = v.variable_subscripts |> List.map (fun (subscript_name, range_name) -> f subscript_name, range_name);
    variable_unit = v.variable_unit;
    variable_representation = v.variable_representation;
    variable_definition = map_subscripts_in_definition f v.variable_definition; }

