(** {2 Low-level interface } *)

(** The module includes {!module:Clang__bindings} which contains the
auto-generated wrappers over [libclang] and some extensions defined in
[libclang_extensions.h]. *)
include module type of struct
  include Clang__bindings
end

(** The module includes {!module:Clang__compat} which contains
    compatibility implementations for some functions and type
    definitions that are missing in old versions of Clang API. *)
include module type of struct
  include Clang__compat
end

(** {2 Parsing files and strings } *)

val parse_file : ?index:cxindex ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit
(** [parse_file ?index ?command_line_args ?unsaved_files ?options filename]
  parses file [filename] and returns its translation unit.
    This function is equivalent to {!val:Clang.parse_translation_unit2},
    with, by default, a fresh [index]
    (created by {!val:Clang.create_index}[ true true]),
    an empty list for [command_line_args] and [unsaved_files], and default
    [options]
    (obtained by {!val:Clang.default_editing_translation_unit_options}).
    See also {!val:Clang.Ast.parse_file} which returns a pattern-matchable
    representation of the AST. *)

val parse_file_res : ?index:cxindex ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) result
(** Equivalent to {!val:parse_file} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

val parse_string : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> cxtranslationunit
(** [parse_string ?index ?filename ?command_line_args ?unsaved_files ?options contents]
  parses string [contents] and returns its translation unit.
    This function calls {!val:Clang.parse_file} with an unsaved file called
    [filename] (by default, [<string>.c]) with [contents]: this unsaved file
    is consed to the list [unsaved_files] (by default, an empty list).
    Note that [filename] should have the [.cpp] suffix to parse C++ code
    (for instance, [<string>.cpp]).
    See also {!val:Clang.Ast.parse_string} which returns a pattern-matchable
    representation of the AST. *)

val parse_string_res : ?index:cxindex -> ?filename:string ->
  ?command_line_args:string list ->
    ?unsaved_files:cxunsavedfile list ->
      ?options:Cxtranslationunit_flags.t ->
        string -> (cxtranslationunit, cxerrorcode) result
(** Equivalent to {!val:parse_string} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

(** {2 Abstract syntax tree} *)

module Ast : sig
  (** The module includes {!module:Clang__ast} which contains the declaration
      of the abstract syntax tree. Since the abstract syntax tree is a pure
      type declaration without value definition, the declaration is written in
      a separate module, written in an implementation file (.ml) without
      interface file (.mli)).
   *)
  include module type of struct
    include Clang__ast
  end

  (** {!type:Options.t} stores flags that change the construction of the
      abstract syntax tree. Beware that the nodes that are ignored by default
      can differ from one version of Clang to the other. *)
  module Options : sig
    type t = {
        ignore_implicit_cast : bool [@default true];
        (** Ignore implicit cast nodes in expressions.
            See {!const:Clang__ast.Cast} for examples. *)

        ignore_paren : bool [@default true];
        (** Ignore parenthese nodes in expressions.
            See {!type:Clang__ast.expr} for examples. *)

        ignore_paren_in_types : bool [@default true];
        (** Ignore parenthese nodes in types.
            See {!type:Clang__ast.qual_type} for examples. *)

        convert_integer_literals : bool [@default true];
        (** Convert integer literals into {!constr:Clang__ast.Int}.
            See {!constr:Clang__ast.IntegerLiteral} for examples. *)

        convert_floating_literals : bool [@default true];
        (** Convert floating literals into {!constr:Clang__ast.Float}.
            See {!constr:Clang__ast.FloatingLiteral} for examples. *)
      }
          [@@deriving make]
  end

  val parse_file : ?index:cxindex ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> translation_unit
  (** [parse_file ?index ?command_line_args ?unsaved_files ?clang_options ?options filename]
  parses file [filename] and returns its translation unit.
    This function is equivalent to {!val:Clang.parse_file} (where [options]
      becomes [clang_options]), but returns the high-level representation
   of the translation unit (as obtained by {!val:of_cxtranslationunit}). *)

  val parse_file_res : ?index:cxindex ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> (translation_unit, cxerrorcode) result
  (** Equivalent to {!val:parse_file} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

  val parse_string : ?index:cxindex -> ?filename:string ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> translation_unit
  (** [parse_string ?index ?filename ?command_line_args ?unsaved_files ?clang_options ?options contents]
  parses string [contents] and returns its translation unit.
    This function is equivalent to {!val:Clang.parse_string} (where [options]
      becomes [clang_options]), but returns the high-level representation
   of the translation unit (as obtained by {!val:of_cxtranslationunit}). *)

  val parse_string_res : ?index:cxindex -> ?filename:string ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> (translation_unit, cxerrorcode) result
  (** Equivalent to {!val:parse_string_res} but returns a [result] instead of
    raising [Failure _] if parsing fails. *)

  val of_cxtranslationunit : ?options:Options.t -> cxtranslationunit ->
    translation_unit
  (** [of_cxtranslationunit ?options tu] translates [tu] into its high-level
      representation. *)

  val node : ?decoration:decoration -> ?cursor:cxcursor ->
    ?location:source_location -> ?qual_type:qual_type -> 'a -> 'a node
  (** [node ?decoration desc] returns a node with the given [desc] value and
      [decoration]. [decoration] can be given by one of the three
      following forms: (1) a value for [?decoration],
      or (2) a value for [?cursor], or (3) by
      either a value for [location], or a value for [qual_type], or both.
      These three forms cannot be mixed, otherwise [Invalid_arg _] is
      raised. *)

  val cursor_of_decoration : decoration -> cxcursor
  (** [cursor_of_decoration decoration] returns the cursor associated to
      [decoration] if any, or the null cursor otherwise (as returned by
      {!val:get_null_cursor}). *)

  val cursor_of_node : 'a node -> cxcursor
  (** [cursor_of_node node] is equivalent to
      {!val:cursor_of_decoration}[ node.decoration]. *)

  val location_of_decoration : decoration -> source_location
  (** [location_of_decoration decoration] returns the location associated to
      [decoration] if any, or the location of the null cursor otherwise
      (as returned by {!val:get_null_cursor}). *)

  val location_of_node : 'a node -> source_location
  (** [location_of_node node] is equivalent to
      {!val:location_of_decoration}[ node.decoration]. *)

  val get_presumed_location : source_location -> concrete_location
  (** [get_presumed_location location] returns the concrete location
      associated to [location]. If [location] is libclang's, then
      this function calls {!val:Clang.get_presumed_location} (which
      honors [#] line directive). *)

  val get_expansion_location : source_location -> concrete_location
  (** [get_expansion_location location] returns the concrete location
      associated to [location]. If [location] is libclang's, then
      this function calls {!val:Clang.get_expansion_location} (which
      ignores [#] line directive). *)

  val string_of_elaborated_type_keyword : elaborated_type_keyword -> string
  (** Alias for {!val:ext_elaborated_type_get_keyword_spelling}:
      returns the keyword as a string, ["struct"], ["union"], ["enum"], ... *)

  val string_of_unary_operator_kind : unary_operator_kind -> string
  (** Alias for {!val:ext_unary_operator_get_opcode_spelling}:
      returns the operator as a string, ["++"], ["+"], ["&"], ... *)

  val string_of_binary_operator_kind : binary_operator_kind -> string
  (** Alias for {!val:ext_binary_operator_get_opcode_spelling}:
      returns the operator as a string, ["+"], ["="], ["<<"], ... *)

  val literal_of_int : int -> integer_literal
  (** [literal_of_int i] returns the integer literal [i]. *)

  val int64_of_literal_opt : integer_literal -> Int64.t option
  (** [int64_of_literal_opt x] returns [Some i] if [x] is representable as
      a 64-bit integer value [i], or [None] otherwise. *)
  
  val int64_of_literal : integer_literal -> Int64.t
  (** [int64_of_literal x] returns [i] if [x] is representable as
      a 64-bit integer value [i], or raises [Failure _] otherwise. *)
  
  val int_of_literal_opt : integer_literal -> int option
  (** [int_of_literal_opt x] returns [Some i] if [x] is representable as
      an integer value [i], or [None] otherwise. *)
  
  val int_of_literal : integer_literal -> int
  (** [int_of_literal x] returns [i] if [x] is representable as
      an integer value [i], or raises [Failure _] otherwise. *)
  
  val string_of_integer_literal : integer_literal -> string
  (** [string_of_integer_literal f] is an alias for
      {!val:Clang__bindings.ext_int_to_string}, radix 10 and signed. *)
  
  val literal_of_float : float -> floating_literal
  (** [literal_of_float f] returns the floating literal [f]. *)

  val float_of_literal : floating_literal -> float
  (** [float_of_cxfloat f] is an alias for
      {!val:Clang__bindings.ext_float_convert_to_double}. *)
  
  val string_of_floating_literal : floating_literal -> string
  (** [string_of_float_literal f] is an alias for
      {!val:Clang__bindings.ext_float_to_string}. *)
end

(** AST types as ordered types. *)
module Type : sig
  type t = Ast.qual_type

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_node : ?options:Ast.Options.t -> 'a Ast.node -> t
  (** [of_node ?options node] returns the type associated to [node].
      If [node] comes from libclang's AST, the function calls
      {!val:get_cursor_type} to the associated cursor.
      If [node] has been constructed and decorated with a type [ty],
      [ty] is returned.
      If [node] has been constructed without type decoration,
      [Invalid_arg _] is raised.
      It is equivalent to
      [Clang.Type.of_decoration ?options (Clang.Ast.decoration_of_node node)].*)

  val make : ?const:bool -> ?volatile:bool -> ?restrict:bool ->
    Ast.type_desc -> t
  (** [make ?const ?volatile ?restrict desc] returns a type from
      a description, associated to an invalid libclang's type.
      This function can be useful to construct types from OCaml
      programs, for instance to describe a program transformation. *)

  val of_cxtype : ?options:Ast.Options.t -> cxtype -> t
  (** [of_cxtype ?options ty] translates [ty] into its high-level
      representation. *)

  val of_cursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] returns the type associated to [cu]. *)

  val of_decoration : ?options:Ast.Options.t -> Ast.decoration -> t
  (** [of_decoration ?options d] returns the type associated to [d]. *)

  val iter_fields : ?options:Ast.Options.t ->
    (Ast.decl -> unit) -> t -> unit
  (** [iter_fields ?options f ty] calls [f] over all the declaration nodes
      of the fields belonging to the record type [ty]
      (either a struct or union).
      It is equivalent to
     [Clang.iter_type_fields (fun d -> f (Clang.Decl.of_cursor ?options d)) ty.cxtype].
   *)

  val list_of_fields : ?options:Ast.Options.t -> t -> Ast.decl list
  (** [list_of_fields ?options f ty] returns the list of all the declaration
      nodes of the fields belonging to the record type [ty] (either a struct
      or union).
      It is equivalent to
     [List.map (Clang.Decl.of_cursor ?options) (Clang.list_of_type_fields ty.cxtype)].
   *)

  val get_declaration : ?options:Ast.Options.t -> t -> Ast.decl
  (** [get_declaration ?options ty] returns the declaration node of [ty].
      It is equivalent to
      [Clang.Decl.of_cxcursor ?options (Clang.get_type_declaration ty.cxtype)].
   *)

  val get_typedef_underlying_type : ?options:Ast.Options.t -> t -> t
  (** [get_declaration ?options ty] returns the underlying type of a typedef
      [ty].
      It is equivalent to
      [Clang.Type.of_cxtype ?options (Clang.get_typedef_decl_underlying_type (Clang.get_type_declaration ty.cxtype))]. *)

  val get_align_of : t -> int
  (** [get_align_of ty] returns the alignment of [ty] in bytes.
      It is equivalent to [Clang.type_get_align_of ty.cxtype]. *)

  val get_size_of : t -> int
  (** [get_align_of ty] returns the size of [ty] in bytes.
      It is equivalent to [Clang.type_get_size_of ty.cxtype]. *)
end

(** AST expressions as ordered types. *)
module Expr : sig
  type t = Ast.expr

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to an expression. *)
end

(** AST statements as ordered types. *)
module Stmt : sig
  type t = Ast.stmt

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a statement. *)
end

(** AST declarations as ordered types. *)
module Decl : sig
  type t = Ast.decl

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a declaration. *)

  val get_typedef_underlying_type : ?options:Ast.Options.t -> t -> Type.t
  (** [get_declaration ?options decl] returns the underlying type of a
      typedef [decl].
      It is equivalent to
      [Clang.Type.of_cxtype ?options (Clang.get_typedef_decl_underlying_type (Clang.Ast.cursor_of_node decl))]. *)

  val get_field_bit_width : t -> int
  (** [get_field_bit_width d] returns the bit width of the field
      declaration [d]. *)
end

(** AST enumeration constants as ordered types. *)
module Enum_constant : sig
  type t = Ast.enum_constant

  val equal : t -> t -> bool

  val compare : t -> t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a enumeration constant. *)

  val get_value : t -> int
  (** [get_value c] returns the value associated to the constant [c].*)
end

(** {2 Iterators } *)

val iter_children : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_children f cur] calls [f] over all the direct child nodes of
    [cur]. *)

val list_of_children : cxcursor -> cxcursor list
(** [list_of_children cur] returns the list of all the direct child nodes of
    [cur]. *)

val iter_type_fields : (cxcursor -> unit) -> cxtype -> unit
(** [iter_type_fields f ty] calls [f] over all the declaration nodes of the
    fields belonging to the record type [ty] (either a struct or union).
    See also {!val:Clang.Type.iter_fields} for a higher-level interface. *)

val list_of_type_fields : cxtype -> cxcursor list
(** [list_of_type_fields f ty] returns the list of all the declaration nodes
    of the fields belonging to the record type [ty] (either a struct or
    union). *)

(** {2 Integer conversions } *)

val int64_of_cxint_opt : cxint -> Int64.t option
(** [int64_of_cxint_opt x] returns [Some i] if [x] is representable as
    a 64-bit integer value [i], or [None] otherwise. *)

val int64_of_cxint : cxint -> Int64.t
(** [int64_of_cxint x] returns [i] if [x] is representable as
    a 64-bit integer value [i], or raises [Failure _] otherwise. *)

val int_of_cxint_opt : cxint -> int option
(** [int_of_cxint_opt x] returns [Some i] if [x] is representable as
    an integer value [i], or [None] otherwise. *)

val int_of_cxint : cxint -> int
(** [int_of_cxint x] returns [i] if [x] is representable as
    an integer value [i], or raises [Failure _] otherwise. *)

val string_of_cxint : cxint -> string
(** [string_of_cxint f] is an alias for
    {!val:Clang__bindings.ext_int_to_string}, radix 10 and signed. *)

(** {2 Floating conversions } *)

val float_of_cxfloat : cxfloat -> float
(** [float_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_convert_to_double}. *)

val string_of_cxfloat : cxfloat -> string
(** [string_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_to_string}. *)

(** {2 Error management } *)

val string_of_cxerrorcode : cxerrorcode -> string
(** [string_of_cxerrorcode ec] returns a message describing [ec]. *)

val seq_of_diagnostics : cxtranslationunit -> cxdiagnostic Seq.t
(** [seq_of_diagnostics tu] returns the diagnostics (warnings and errors)
    produced for the given translation unit *)

val is_error : cxdiagnosticseverity -> bool
(** [is_error d] returns whether [d] is [Error] or [Fatal]. *)

val has_error : cxtranslationunit -> bool
(** [has_error tu] returns whether the translation unit [tu] produced an
    error. *)
