(** {2 Low-level interface } *)

(** The module includes {!module:Clang__bindings} which contains the
auto-generated wrappers over [libclang] and some extensions defined in
[libclang_extensions.h]. *)

module Bindings = Clang__bindings

include module type of struct
  include Bindings
end

(** The module includes {!module:Clang__compat} which contains
    compatibility implementations for some functions and type
    definitions that are missing in old versions of Clang API. *)
include module type of struct
  include Clang__compat
end

module Types = Clang__types

include module type of struct
  include Types
end

include module type of struct
  include Clang__utils
end

module Command_line = Clang__command_line

val version : unit -> cxversion
(** [version ()] is the Clang version. *)

val includedir : string
(** [includedir] is equal to the path to Clang [include/] directory, i.e.,
    [`llvm-config --includedir`/../lib/clang/`llvm-config --version`/include],
    where [llvm-config] commands has been evaluated when [clangml] has been
    configured.
    This directory contains [stddef.h] and other compiler-specific headers,
    and it is common to pass
    [Clang.Command_line.include_directory Clang.includedir]
    to Clang command-line. *)

val default_include_directories : unit -> string list
(** [default_include_directories ()] is a list of include directories that are
    common to pass to Clang command-line. The list contains {!val:includedir}.
 *)

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

  (** The following functions provides convenient ways to build some AST
      nodes. *)

  val var : ?linkage:linkage_kind -> ?var_init:expr ->
    ?constexpr:bool -> string -> qual_type -> var_decl_desc

  val function_decl : ?linkage:linkage_kind -> ?body:stmt -> ?deleted:bool ->
    ?constexpr:bool -> ?nested_name_specifier:nested_name_specifier ->
    function_type -> declaration_name -> function_decl

  val function_type : ?calling_conv:calling_conv -> ?parameters:parameters ->
    ?exception_spec:exception_spec -> qual_type -> function_type

  val parameters : ?variadic:bool -> parameter list -> parameters

  val parameter : ?default:expr -> qual_type -> string -> parameter_desc

  val ident_ref : ?nested_name_specifier:nested_name_specifier ->
    declaration_name -> ident_ref

  val identifier_name : ?nested_name_specifier:nested_name_specifier ->
    string -> ident_ref

  val new_instance : ?placement_args:expr list -> ?array_size:expr ->
    ?init:expr -> ?args:expr list -> qual_type -> expr_desc

  val delete : ?global_delete:bool -> ?array_form:bool -> expr -> expr_desc

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

  val concrete_of_cxsourcelocation : location_kind -> cxsourcelocation -> concrete_location
  (** [concrete_of_cxsourcelocation kind location] returns the concrete location
      associated to [location]. [kind] selects whether
      {!val:Clang.get_presumed_location} (which ignores [#] line directive)
      or {!val:Clang.get_expansion_location} (which honors [#] line directive)
      is called. *)

  val concrete_of_source_location : location_kind -> source_location -> concrete_location
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
      and only if there is at least one diagnostic to display, and [pp] should call
      the printer passed in its first argument to display the diagnostics.
      In the case there is no diagnostic to display, nothing is printed. *)

  val has_severity : cxdiagnosticseverity list -> translation_unit -> bool
  (** [has_severity l tu] returns whether the translation unit [tu] produced a
      diagnostic, the severity of which belongs to [l]. *)

  include module type of struct
    include Clang__ast_utils
  end
end

(** AST types as ordered types. *)
module Type : sig
  type t = Ast.qual_type

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

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to an expression. *)
end

(** AST statements as ordered types. *)
module Stmt : sig
  type t = Ast.stmt

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a statement. *)
end

(** AST declarations as ordered types. *)
module Decl : sig
  type t = Ast.decl

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

  val of_cxcursor : ?options:Ast.Options.t -> cxcursor -> t
  (** [of_cxcursor ?options cu] translates [cu] into its high-level
      representation, supposing that [cu] points to a enumeration constant. *)

  val get_value : t -> int
  (** [get_value c] returns the value associated to the constant [c].*)
end

module Translation_unit : sig
  type t = Ast.translation_unit

  val make : ?filename:string -> Ast.decl list -> Ast.translation_unit_desc
end
