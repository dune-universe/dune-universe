[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]

open Clang__bindings

open Clang__types

val string_of_language : language -> string

val language_of_string : string -> language

val language_of_string_opt : string -> language option

val suffix_of_language : language -> string

val extern_of_language : language -> string

val string_of_cxx_access_specifier : cx_cxxaccessspecifier -> string

(** {2 Compatibility layer} *)

[%%meta Metapp.Sigi.of_list (
  if Clangml_config.version >= { major = 3; minor = 5; subminor = 0 } then
    []
  else [%sig:
    type cxerrorcode =
      | Failure
      | Crashed
      | InvalidArguments
      | ASTReadError
    (** Error codes introduced in clang 3.5, declared here for compatibility.
        Only {!constr:Failure} will be used. *)

    val parse_translation_unit2 :
      cxindex -> string -> string array -> cxunsavedfile array ->
        Cxtranslationunit_flags.t -> (cxtranslationunit, cxerrorcode) result
    (** Compatibility wrapper for [parse_translation_unit2].
        In case of error, [Error Failure] will be returned. *)])]

val predefined_expr_get_function_name : cxcursor -> cxcursor -> string

[%%meta Metapp.Sigi.of_list (
  if Clangml_config.version >= { major = 3; minor = 7; subminor = 0 } then
    []
  else [%sig:
    type cxvisitorresult =
      | Break
      | Continue

    val type_visit_fields : cxtype -> (cxcursor -> cxvisitorresult) -> bool])]

val include_attributed_types : Cxtranslationunit_flags.t

val type_non_null : clang_ext_attrkind

[%%meta Metapp.Sigi.of_list (
  if Clangml_config.version.major >= 8 then
    []
  else [%sig:
    val type_get_modified_type : cxtype -> cxtype])]

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

val iter_decl_context : (cxcursor -> unit) -> cxcursor -> unit
(** [iter_decl_context f c] calls [f] over all the declaration nodes of
    declaration context [c]. *)

val list_of_decl_context : cxcursor -> cxcursor list
(** [list_of_decl_context f ty] returns the list of all the declaration nodes
    of declaration context [c]. *)

(** {2 Integer or floating point types} *)
val is_integer : cxtypekind -> bool
(** [is_integer ty] returns true if [ty] is a built-in integer type. *)

val is_unsigned_integer : cxtypekind -> bool
(** [is_unsigned_integer ty] returns true if [ty] is a built-in integer type
    that is unsigned. *)

val is_signed_integer : cxtypekind -> bool
(** [is_signed_integer ty] returns true if [ty] is a built-in integer type
    that is signed. *)

val is_floating_point : cxtypekind -> bool
(** [is_floating_point ty] returns true if [ty] is a built-in floating-point
    type. *)

(** {2 Integer conversions } *)

val int64_of_cxint_opt : ?signed:bool -> cxint -> Int64.t option
(** [int64_of_cxint_opt ~signed x] returns [Some i] if [x] is representable as
    a 64-bit integer value [i], or [None] otherwise.
    [signed] specifies if [x] is signed (default: true). *)

val int64_of_cxint : ?signed:bool -> cxint -> Int64.t
(** [int64_of_cxint x] returns [i] if [x] is representable as
    a 64-bit integer value [i], or raises [Invalid_argument _] otherwise.
    [signed] specifies if [x] is signed (default: true).*)

val int_of_cxint_opt : ?signed:bool -> cxint -> int option
(** [int_of_cxint_opt x] returns [Some i] if [x] is representable as
    an integer value [i], or [None] otherwise.
    [signed] specifies if [x] is signed (default: true). *)

val int_of_cxint : ?signed:bool -> cxint -> int
(** [int_of_cxint x] returns [i] if [x] is representable as
    an integer value [i], or raises [Invalid_argument _] otherwise.
    [signed] specifies if [x] is signed (default: true). *)

val string_of_cxint : ?signed:bool -> cxint -> string
(** [string_of_cxint f] is an alias for
    {!val:Clang__bindings.ext_int_to_string}, radix 10.
    [signed] specifies if [x] is signed (default: true). *)

(** {2 Floating conversions } *)

val float_of_cxfloat_opt : cxfloat -> float option
(** [float_of_cxfloat_opt x] returns [Some f] if [x] is a floating-point
    value with either IEEE single or double semantics,
    or [None] otherwise. *)

val float_of_cxfloat : cxfloat -> float
(** [float_of_cxfloat x] returns [f] if [x] is a floating-point
    value with either IEEE single or double semantics,
    or raises [Invalid_argument _] otherwise. *)

val string_of_cxfloat : cxfloat -> string
(** [string_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_to_string}. *)

(** {2 Error management } *)

val string_of_cxerrorcode : cxerrorcode -> string
(** [string_of_cxerrorcode ec] returns a message describing [ec]. *)

val seq_of_diagnostics : cxtranslationunit -> cxdiagnostic Seq.t
(** [seq_of_diagnostics tu] returns the diagnostics
    (notes, warnings, errors, ...)
    produced for the given translation unit *)

val format_diagnostics :
  ?pp:((Format.formatter -> unit -> unit)
       -> Format.formatter -> unit -> unit) ->
  cxdiagnosticseverity list -> Format.formatter ->
  cxtranslationunit -> unit
(** [format_diagnostics ?pp severities fmt tu] formats the
    diagnostics produced for the given translation unit. Only the diagnostics,
    the severity of which is listed in [severities] are displayed.
    If there is a printer given in [pp], then this printer is called once if
    and only if there is at least one diagnostic to display, and [pp] should call
    the printer passed in its first argument to display the diagnostics.
    In the case there is no diagnostic to display, nothing is printed. *)

val error : cxdiagnosticseverity list
(** [error] contains the severities [Error] and [Fatal]. *)

val warning_or_error : cxdiagnosticseverity list
(** [warning_or_error] contains the severities [Warning], [Error] and [Fatal]. *)

val not_ignored_diagnostics : cxdiagnosticseverity list
(** [not_ignored_diagnostics] contains the severities [Note], [Warning],
    [Error] and [Fatal]. *)

val all_diagnostics : cxdiagnosticseverity list
(** [all_diagnostics] contains the severities [Ignored], [Note], [Warning],
    [Error] and [Fatal]. *)

val has_severity : cxdiagnosticseverity list -> cxtranslationunit -> bool
(** [has_severity l tu] returns whether the translation unit [tu] produced a
    diagnostic, the severity of which belongs to [l]. *)

val cursor_get_translation_unit : cxcursor -> cxtranslationunit
(** [cursor_get_translation_unit cursor] returns the translation unit
    associated to [cursor]. *)

val sourcelocation_get_translation_unit :
    cxsourcelocation -> cxtranslationunit
(** [sourcelocation_get_translation_unit location] returns the translation
    unit associated to [location]. *)

val binary_of_overloaded_operator_kind :
    clang_ext_overloadedoperatorkind -> clang_ext_binaryoperatorkind
