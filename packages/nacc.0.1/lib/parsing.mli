(**************************************************************************)
(*                           _   _    _    ____ ____                      *)
(*                          | \ | |  / \  / ___/ ___|                     *)
(*                          |  \| | / _ \| |  | |                         *)
(*                          | |\  |/ ___ \ |__| |___                      *)
(*                          |_| \_/_/   \_\____\____|                     *)
(*                                                                        *)
(*                                                                        *)
(*                        Copyright (c) 2020 - CodeAnon                   *)
(**************************************************************************)

type 'a parser

val pure : 'a -> 'a parser

val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser

val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser

val ( <* ) : 'a parser -> 'b parser -> 'a parser

val ( *> ) : 'a parser -> 'b parser -> 'b parser

val ( <|> ) : 'a parser -> 'a parser -> 'a parser

val do_parse : 'a parser -> string -> 'a option

val ( --> ) : string -> 'a parser -> ('a * string) option

val ( <-- ) : 'a parser -> string -> ('a * string) option

val many : 'a parser -> 'a list parser

val some : 'a parser -> 'a list parser

val check : (char -> bool) -> char parser

val ( ~~ ) : (string -> ('a * string) option) -> 'a parser
