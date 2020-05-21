open Clang__bindings

open Clang__ast

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

val int64_of_literal_opt : ?signed:bool -> integer_literal -> Int64.t option
(** [int64_of_literal_opt x] returns [Some i] if [x] is representable as
    a 64-bit integer value [i], or [None] otherwise. *)

val int64_of_literal : ?signed:bool -> integer_literal -> Int64.t
(** [int64_of_literal x] returns [i] if [x] is representable as
    a 64-bit integer value [i], or raises [Failure _] otherwise. *)

val int_of_literal_opt : ?signed:bool -> integer_literal -> int option
(** [int_of_literal_opt x] returns [Some i] if [x] is representable as
    an integer value [i], or [None] otherwise. *)

val int_of_literal : ?signed:bool -> integer_literal -> int
(** [int_of_literal x] returns [i] if [x] is representable as
    an integer value [i], or raises [Failure _] otherwise. *)

val string_of_integer_literal : ?signed:bool -> integer_literal -> string
(** [string_of_integer_literal f] is an alias for
    {!val:Clang__bindings.ext_int_to_string}, radix 10 and signed. *)

val print_integer_literal : ?signed:bool -> integer_literal -> unit

val output_integer_literal :
    ?signed:bool -> out_channel -> integer_literal -> unit

val pp_print_integer_literal :
    ?signed:bool -> Format.formatter -> integer_literal -> unit

val literal_of_float : float -> floating_literal
(** [literal_of_float f] returns the floating literal [f]. *)

val float_of_literal : floating_literal -> float
(** [float_of_cxfloat f] is an alias for
    {!val:Clang__bindings.ext_float_convert_to_double}. *)

val string_of_floating_literal : floating_literal -> string
(** [string_of_float_literal f] is an alias for
    {!val:Clang__bindings.ext_float_to_string}. *)

val print_floating_literal : floating_literal -> unit

val output_floating_literal : out_channel -> floating_literal -> unit

val pp_print_floating_literal : Format.formatter -> floating_literal -> unit

val languages_of_ids : Clang_ext_languageids.t -> languages

val language_of_ids : Clang_ext_languageids.t -> Clang__types.language

val ids_of_languages : languages -> Clang_ext_languageids.t

val ids_of_language :  Clang__types.language -> Clang_ext_languageids.t
