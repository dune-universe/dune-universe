[%%metapackage "metapp"]
[%%metadir "config/.clangml_config.objs/byte"]

open Clang__bindings

[%%meta Metapp.Stri.of_list (
  if Clangml_config.version >= { major = 7; minor = 0; subminor = 0 } then [%str
    external get_token : cxtranslationunit -> cxsourcelocation ->
      cxtoken option = "clang_getToken_wrapper"
    (** Get the raw lexical token starting with the given location. *)]
  else [])]

external tokenize : cxtranslationunit -> cxsourcerange -> cxtoken array =
    "clang_tokenize_wrapper"
(** Tokenize the source code described by the given range into raw lexical
    tokens. *)
