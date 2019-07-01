type value_modifier =
  | NoModifier
  | Prefix of int (** \{var:N\} *)
  | Composite (** \{var*\} *)
(** A modifier for a variable *)

type variable_expression
(** A variable in an expression, optionally containing a prefix or composite modifier *)

type expression
(** A URI template expression. This can be expanded into a real string when the URI is templated *)

type template_part =
  | Literal of string (** A standard string that will not be expanded *)
  | Expression of expression (** An expandable expression *)

(** A part of a template *)

type t
(** The type of a template *)

val empty : t
(** The empty template *)


val add_part : template_part -> t -> t
(** [add_part part template] appends [part] to the end of [template] *)

val add_literal : string -> t -> t
(** [add_literal lit template] appends literal [lit] to the end of [template] *)

val add_expression : Expansion_type.t -> variable_expression list -> t -> t
(** [add_expression ex_typ v_exps template] appends an expression with expansion type [ex_typ] and the expressions [v_exps] to the end of [template] *)

val add_single_expression : Expansion_type.t -> variable_expression -> t -> t
(** [add_single_expression ex_typ v_exp template] appends an expression with expansion type [ex_typ] and the single expression [v_exp] to the end of [template] *)


val create : template_part list -> t
(** [create parts] creates a template with [parts] in order *)

val create_expression : Expansion_type.t -> variable_expression list -> expression
(** [create_expression ex_typ v_exps] creates an expression with expansion type [ex_typ] and the expressions [v_exps] *)

val create_single_expression : Expansion_type.t -> variable_expression -> expression
(** [create_single_expression ex_typ v_exps] creates an expression with expansion type [ex_typ] and the single expression [v_exp] *)

val create_variable_expression : ?value_modifier:value_modifier -> string -> variable_expression
(** [create_variable_expression name] creates a variable expression with the given [name]
    @param value_modifier creates variable expression with a value modifier *)


val parts_of_t : t -> template_part list
(** Returns a list of the parts of the given template *)


val get_expansion_type : expression -> Expansion_type.t
(** Returns the expansion type of the given expression *)

val get_variable_expressions : expression -> variable_expression list
(** Returns the variable expressions associated with the given expression *)


val get_variable_expression_name : variable_expression -> string
(** Returns the name of the given variable_expression *)

val get_variable_expression_modifier : variable_expression -> value_modifier
(** Returns the value_modifier of the given variable_expression *)


val part_is_literal : template_part -> bool
(** Returns true if the tempalte_part is a literal *)

val part_is_expression : template_part -> bool
(** Returns true if the tempalte_part is an expression *)


val get_variable_names : t -> string list
(** Returns a list of the names of the variables in the expression *)

val string_of_value_modifier : value_modifier -> string
(** Returns the string representing the value_modifier, see the {!constructor:value_modifier} for examples *)

val string_of_variable_expression : variable_expression -> string
(** Returns the string representation of the variable_expression.
    This is the name of the variable expression concatinated with the result of {!val:string_of_value_modifier}

    Example results: "var" "var:5" "var*" *)

val string_of_template : t -> string
(** Returns the uri template string of the given template *)
