type init_list_form =
  | Syntactic
      (** Syntactic form: only explicitly written values appear in
          initialization lists. *)
  | Semantic
      (** Semantic form: values that are not explicitly written in
          initialization lists appear as
          {!value:Clang__ast.ImplicitValueInitExpr}. *)

(** {!type:Options.t} stores flags that change the construction of the
    abstract syntax tree. Beware that the nodes that are ignored by default
    can differ from one version of Clang to the other. *)
type t = {
    ignore_implicit_cast : bool;
    (** Ignore implicit cast nodes in expressions.
        See {!const:Clang__ast.Cast} for examples. *)

    ignore_paren : bool;
    (** Ignore parenthese nodes in expressions.
        See {!type:Clang__ast.expr} for examples. *)

    ignore_paren_in_types : bool;
    (** Ignore parenthese nodes in types.
        See {!type:Clang__ast.qual_type} for examples. *)

    ignore_expr_with_cleanups : bool;

    ignore_materialize_temporary_expr : bool;

    ignore_bind_temporary_expr : bool;

    convert_integer_literals : bool;
    (** Convert integer literals into {!constr:Clang__ast.Int}.
        See {!constr:Clang__ast.IntegerLiteral} for examples. *)

    convert_floating_literals : bool;
    (** Convert floating literals into {!constr:Clang__ast.Float}.
        See {!constr:Clang__ast.FloatingLiteral} for examples. *)

    init_list_form : init_list_form;
    (** Select between default form (syntactic or semantic) for initialization
        lists.
        See {!constr:Clang__ast.ImplicitValueInitExpr} for examples.
        Default: [Syntactic]. *)
  }

let default = {
  ignore_implicit_cast = true;
  ignore_paren = true;
  ignore_paren_in_types = true;
  ignore_expr_with_cleanups = true;
  ignore_materialize_temporary_expr = true;
  ignore_bind_temporary_expr = true;
  convert_integer_literals = true;
  convert_floating_literals = true;
  init_list_form = Syntactic;
}
