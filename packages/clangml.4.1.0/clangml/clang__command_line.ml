let include_directory directory =
  "--include-directory=" ^ directory

let include_barrier = "--include-barrier"

let language_of_string lang =
  "--language=" ^ lang

let language lang =
  language_of_string (Clang__utils.string_of_language lang)

let standard_of_string std =
  "--std=" ^ std

let standard_of_clang std =
  standard_of_string (Clang__bindings.ext_lang_standard_get_name std)

let standard std =
  standard_of_clang (Standard.to_clang std)

let trigraphs = "-trigraphs"
