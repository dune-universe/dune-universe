type token

type typ = {
    num_of_groups : int;
    named_groups : (string * int) list;
    callouts : int list; (* 255 : default, not in the list *)
    named_callouts : (string * int) list; (* caml extension *)
  }

val from_string : string -> string * token list * string
(** Parse "regexp". Returns regexp in string and tokens, and its flags *)

val replace_from_string : string -> string * token list * string
  (* parse "regexp/replace".
    return (token * regexp * replace) 
  *)

val type_regexp : token list -> typ

val string_of_tokens : typ -> token list -> string 
