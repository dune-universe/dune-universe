(*                                                          * 
 * all these exceptions feels very weird                    *
 * i'm not sure if there's a more ocaml-y way or whatever   *
 * (i'm not good with error handling)                       *
 * TODO: contemplate                                        * 
 *                                                          *)

exception Unexpected_newline ;;
exception Unexpected_eof ;;
exception Empty_key ;;
exception Invalid_escape_char ;;

type token =
    | Equals
    | BraceOpen
    | BraceClosed
    | Separator

    | Comment   of string

    | Key       of string
    | Value     of string

    | EOF
;;

val string_of_token : token     -> string
val tokenize        : char list -> token list
val tokenize_str    : string    -> token list
