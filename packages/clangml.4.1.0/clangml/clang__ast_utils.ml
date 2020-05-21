open Clang__bindings

open Clang__ast

open Clang__utils

let string_of_elaborated_type_keyword =
  ext_elaborated_type_get_keyword_spelling

let string_of_unary_operator_kind = ext_unary_operator_get_opcode_spelling

let string_of_binary_operator_kind = ext_binary_operator_get_opcode_spelling

let literal_of_int i = Int i

let int64_of_literal_opt ?signed i =
  match i with
  | Int i -> Some (Int64.of_int i)
  | CXInt i -> int64_of_cxint_opt ?signed i

let int64_of_literal ?signed i =
  match i with
  | Int i -> Int64.of_int i
  | CXInt i -> int64_of_cxint ?signed i

let int_of_literal_opt ?signed i =
  match i with
  | Int i -> Some i
  | CXInt i -> int_of_cxint_opt ?signed i

let int_of_literal ?signed i =
  match i with
  | Int i -> i
  | CXInt i -> int_of_cxint ?signed i

let string_of_integer_literal ?signed i =
  match i with
  | Int i -> string_of_int i
  | CXInt i -> string_of_cxint ?signed i

let print_integer_literal ?signed i =
  match i with
  | Int i -> print_int i
  | CXInt i -> print_string (string_of_cxint ?signed i)

let output_integer_literal ?signed channel i =
  match i with
  | Int i -> output_string channel (string_of_int i)
  | CXInt i -> output_string channel (string_of_cxint ?signed i)

let pp_print_integer_literal ?signed fmt i =
  match i with
  | Int i -> Format.pp_print_int fmt i
  | CXInt i -> Format.pp_print_string fmt (string_of_cxint ?signed i)

let literal_of_float f = Float f

let float_of_literal f =
  match f with
  | Float f -> f
  | CXFloat f -> float_of_cxfloat f

let string_of_floating_literal f =
  match f with
  | Float f -> string_of_float f
  | CXFloat f -> string_of_cxfloat f

let print_floating_literal f =
  match f with
  | Float f -> print_float f
  | CXFloat f -> print_string (string_of_cxfloat f)

let output_floating_literal channel f =
  match f with
  | Float f -> output_string channel (string_of_float f)
  | CXFloat f -> output_string channel (string_of_cxfloat f)

let pp_print_floating_literal fmt f =
  match f with
  | Float f -> Format.pp_print_float fmt f
  | CXFloat f -> Format.pp_print_string fmt (string_of_cxfloat f)

let languages_of_ids e =
  { c = Clang_ext_languageids.(subset c e);
    cxx = Clang_ext_languageids.(subset cxx e) }

let language_of_ids e : Clang__types.language =
  if e = Clang_ext_languageids.c then
    C
  else if e = Clang_ext_languageids.cxx then
    CXX
  else
    invalid_arg (Printf.sprintf "language_of_ids: %d" (Obj.magic e))

let ids_of_languages li =
  Clang_ext_languageids.
    ((if li.c then c else zero) +
     (if li.cxx then cxx else zero))

let ids_of_language (l : Clang__types.language) =
  match l with
  | C -> Clang_ext_languageids.c
  | CXX -> Clang_ext_languageids.cxx
  | _ -> invalid_arg "ids_of_language"
