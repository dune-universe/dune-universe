type location = Lexing.position*Lexing.position

type associativity =
  | Left
  | Right
  | NonAss

type fixity =
  | Prefix
  | Infix of (int * associativity)

type term =
  | Var of string
  | Cst of string
  | App of (term*term)
  | Abs of (string*term)

type token =
  | Term of term
  | Op of (string * fixity)

val next : token list -> (token option * token list)

val parse_sequence : token list -> token option -> token list -> term

val to_string : term -> string

module SMap : Map.S with type key = string 
                                
type sig_info = fixity SMap.t

val test_sig : sig_info

val get_fixity : string -> sig_info -> fixity
