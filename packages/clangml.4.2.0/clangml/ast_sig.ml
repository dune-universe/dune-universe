open Clang__bindings

open Clang__types

open Clang__utils

(** Common part of AST node signatures *)
module type S = sig
  type t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hash : t -> int

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  module Hashtbl : Hashtbl.S with type key = t
end

module type CustomS = sig
module Node : Clang__ast.NodeS

module Ast : sig
  (** The module includes {!module:Clang__ast} which contains the declaration
      of the abstract syntax tree. Since the abstract syntax tree is a pure
      type declaration without value definition, the declaration is written in
      a separate module, written in an implementation file (.ml) without
      interface file (.mli)).
   *)
  include module type of struct
    include Clang__ast.Common

    include Clang__ast.Custom (Node)
  end

  (** The following functions provides convenient ways to build some AST
      nodes. *)

  val var : ?linkage:linkage_kind -> ?var_init:expr ->
    ?constexpr:bool -> ?attributes:attribute list -> string -> qual_type ->
    var_decl_desc

  val function_decl : ?linkage:linkage_kind -> ?body:stmt -> ?deleted:bool ->
    ?constexpr:bool -> ?inline_specified:bool -> ?inlined:bool ->
    ?nested_name_specifier:nested_name_specifier ->
    ?attributes:attribute list -> function_type ->
    declaration_name -> function_decl

  val function_type : ?calling_conv:calling_conv -> ?parameters:parameters ->
    ?exception_spec:exception_spec -> ?ref_qualifier:cxrefqualifierkind ->
    qual_type -> function_type

  val parameters : ?variadic:bool -> parameter list -> parameters

  val parameter : ?default:expr -> qual_type -> string -> parameter_desc

  val ident_ref : ?nested_name_specifier:nested_name_specifier ->
    ?template_arguments:template_argument list -> declaration_name -> ident_ref

  val identifier_name : ?nested_name_specifier:nested_name_specifier ->
    ?template_arguments:template_argument list -> string -> ident_ref

  val new_instance : ?placement_args:expr list -> ?array_size:expr ->
    ?init:expr -> ?args:expr list -> qual_type -> expr_desc

  val delete : ?global_delete:bool -> ?array_form:bool -> expr -> expr_desc

  val enum_decl : ?complete_definition: bool -> ?attributes:attribute list ->
    string -> enum_constant list -> decl_desc

  val if_ :
      ?init:stmt -> ?condition_variable:var_decl -> ?else_branch:stmt -> expr ->
        stmt -> stmt_desc

  (** {!type:Options.t} stores flags that change the construction of the
      abstract syntax tree. Beware that the nodes that are ignored by default
      can differ from one version of Clang to the other. *)
  module Options : module type of struct include Clang__ast_options end

  val parse_file : ?index:cxindex ->
    ?command_line_args:string list ->
      ?unsaved_files:cxunsavedfile list ->
        ?clang_options:Cxtranslationunit_flags.t ->
          ?options:Options.t ->
            string -> translation_unit
  (** [parse_file ?index ?command_line_args ?unsaved_files ?clang_options
      ?options filename]
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
  (** [parse_string ?index ?filename ?command_line_args ?unsaved_files
      ?clang_options ?options contents]
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
    ?location:source_location -> ?qual_type:qual_type -> 'a Node.t -> 'a node
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

  val tokens_of_node : 'a node -> string array
  (** [tokens_of_node node] returns the token at the beginning of [node] if
      available. *)

  val concrete_of_source_location :
      location_kind -> source_location -> concrete_location
  (** [concrete_of_source_location kind location] returns the concrete location
      associated to [location].
      If [location] is concrete, it is returned directly.
      If [location] is libclang's, [concrete_of_cxsourcelocation] is called. *)

  val seq_of_diagnostics : translation_unit -> cxdiagnostic Seq.t
  (** [seq_of_diagnostics tu] returns the diagnostics
      (notes, warnings, errors, ...)
      produced for the given translation unit *)

  val format_diagnostics :
    ?pp:((Format.formatter -> unit -> unit)
         -> Format.formatter -> unit -> unit) ->
    cxdiagnosticseverity list -> Format.formatter ->
      translation_unit -> unit
  (** [format_diagnostics ?pp severities fmt tu] formats the
      diagnostics produced for the given translation unit. Only the diagnostics,
      the severity of which is listed in [severities] are displayed.
      If there is a printer given in [pp], then this printer is called once if
      and only if there is at least one diagnostic to display, and [pp] should
      call the printer passed in its first argument to display the diagnostics.
      In the case there is no diagnostic to display, nothing is printed. *)

  val has_severity : cxdiagnosticseverity list -> translation_unit -> bool
  (** [has_severity l tu] returns whether the translation unit [tu] produced a
      diagnostic, the severity of which belongs to [l]. *)

  include module type of struct
    include Clang__ast_utils
  end
  end
(** AST types. *)
module Type : sig
  type t = Ast.qual_type [@@deriving refl]

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
    Ast.type_desc Node.t -> t
  (** [make ?const ?volatile ?restrict desc] returns a type from
      a description, associated to an invalid libclang's type.
      This function can be useful to construct types from OCaml
      programs, for instance to describe a program transformation. *)

  val of_cxtype : ?options:Ast.Options.t -> cxtype -> t
  (** [of_cxtype ?options ty] translates [ty] into its high-level
      representation. *)

  val of_type_loc : ?options:Ast.Options.t -> clang_ext_typeloc -> t
  (** [of_type_loc ?options ty] translates [ty] into its high-level
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
     [Clang.iter_type_fields (fun d -> f (Clang.Decl.of_cursor ?options d))
       ty.cxtype].
   *)

  val list_of_fields : ?options:Ast.Options.t -> t -> Ast.decl list
  (** [list_of_fields ?options f ty] returns the list of all the declaration
      nodes of the fields belonging to the record type [ty] (either a struct
      or union).
      It is equivalent to
     [List.map (Clang.Decl.of_cursor ?options) (Clang.list_of_type_fields
       ty.cxtype)].
   *)

  val get_declaration : ?options:Ast.Options.t -> t -> Ast.decl
  (** [get_declaration ?options ty] returns the declaration node of [ty].
      It is equivalent to
      [Clang.Decl.of_cxcursor ?options (Clang.get_type_declaration ty.cxtype)].
   *)

  val get_typedef_underlying_type :
    ?options:Ast.Options.t -> ?recursive:bool -> t -> t
  (** [get_typedef_underlying_type ?options ?recursive ty] returns the
      underlying type of [ty] if [ty] is a typedef, and [ty] otherwise.
      If [recursive] is [true]
      (default: [false]), typedefs are followed until the underlying type is
      not a typedef. *)

  val get_align_of : t -> int
  (** [get_align_of ty] returns the alignment of [ty] in bytes.
      It is equivalent to [Clang.type_get_align_of ty.cxtype]. *)

  val get_size_of : t -> int
  (** [get_align_of ty] returns the size of [ty] in bytes.
      It is equivalent to [Clang.type_get_size_of ty.cxtype]. *)

  val get_offset_of : t -> string -> int
  (** [get_offset_of ty field] returns the offset of [field] in the elaborated
      type [ty] in bits. *)

  include S with type t := t
end

(** AST expressions as ordered types. *)
module Expr : sig
  type t = Ast.expr [@@deriving refl]

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to an expression. *)

  val get_definition : t -> cxcursor
  (** [get_definition e] retrieves a cursor that describes the definition of
      the entity referenced by [e]. Returns a [NULL] cursor of [e] has no
      corresponding definition. *)

  type radix = Decimal | Octal | Hexadecimal | Binary [@@deriving refl]

  val radix_of_integer_literal : t -> radix option
  (** [radix_of_integer_literal e] returns the radix of the integer literal [e]
      if available. Note that, by convention, [0] is octal. *)

  val parse_string :
      ?index:cxindex -> ?clang_options:Cxtranslationunit_flags.t ->
        ?options:Ast.Options.t -> ?filename:string -> ?line:int ->
          ?context:Clang__ast.decl list -> string ->
            t option * Ast.translation_unit
  (** [parse_string ?index ?clang_options ?options ?filename ?line ?context
      contents] parses string [contents] as a C expression and returns
      [(o, tu)] where [o] is [Some e] if [contents] has been successfully parsed
      as the expression [e], and [tu] is the translation unit created for
      parsing. [tu] can be used to retrieve diagnostics if any.
      [context] provides some declaration context.
      [filename] and [line] specifies respectively the file name and the line
      number to use in diagnostics. *)

  include S with type t := t
end

(** AST statements. *)
module Stmt : sig
  type t = Ast.stmt [@@deriving refl]

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a statement. *)

  include S with type t := t
end

(** AST not-transformed types. *)
module Type_loc : sig
  type t = Ast.type_loc [@@deriving refl]

  val to_qual_type : ?options:Ast.Options.t -> t -> Type.t

  include S with type t := t
end

(** AST declarations. *)
module Decl : sig
  type t = Ast.decl [@@deriving refl]

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a declaration. *)

  val get_typedef_underlying_type :
    ?options:Ast.Options.t -> ?recursive:bool -> t -> Type.t
  (** [get_typedef_underlying_type ?options ?recursive decl] returns the
      underlying type of a typedef [decl].
      If [recursive] is [true] (default: [false]), typedefs are followed until
      the underlying type is not a typedef. *)

  val get_field_bit_width : t -> int
  (** [get_field_bit_width d] returns the bit width of the field
      declaration [d]. *)

  val get_size_expr : ?options:Ast.Options.t -> t -> Expr.t
  (** [get_size_expr ?options d] returns the expression specifying the size
      of the array declared by [d], and fails if [d] is not an array
      declaration. *)

  val get_type_loc : ?options:Ast.Options.t -> t -> Type_loc.t

  val get_canonical : t -> cxcursor
  (** [get_canonical d] retrieves the canonical cursor declaring an entity. *)

  include S with type t := t
end

(** AST parameters. *)
module Parameter : sig
  type t = Ast.parameter [@@deriving refl]

  val get_size_expr : ?options:Ast.Options.t -> t -> Expr.t
  (** [get_size_expr ?options p] returns the expression specifying the size
      of the array declared by [p], and fails if [p] is not an array
      parameter. *)

  val get_type_loc : ?options:Ast.Options.t -> t -> Type_loc.t

  include S with type t := t
end

(** AST enumeration constants. *)
module Enum_constant : sig
  type t = Ast.enum_constant [@@deriving refl]

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a enumeration constant. *)

  val get_value : t -> int
  (** [get_value c] returns the value associated to the constant [c].*)

  include S with type t := t
end

(** AST translation units. *)
module Translation_unit : sig
  type t = Ast.translation_unit [@@deriving refl]

  val make : ?filename:string -> Ast.decl list -> Ast.translation_unit_desc

  include S with type t := t
end
end
