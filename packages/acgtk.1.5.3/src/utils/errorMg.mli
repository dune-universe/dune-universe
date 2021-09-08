type location = Lexing.position * Lexing.position

(** [update_loc lexbuf name] update the position informations for the
    lexer *)
val update_loc : ?filename:string -> Lexing.lexbuf -> unit

module type E =
  sig
    type t
    val to_string : t -> string
  end
  
  
module type ERROR =
  sig

    type bracket =
      | Round
      | Square
      | Curly
  
    type lex_error =
      | Unstarted_comment
      | Unstarted_bracket
      | Mismatch_parentheses of bracket
      | Unclosed_comment
      | Expect of string
      | Bad_token

    type synt_error
      
    type error =
      | SyntError of synt_error
      | LexError of lex_error
      | SysError of string

    (** The exception that should be raised when an error occur *)
    exception Error of (error * location)

    (** [error_msg e ~filename] returns a string describing the error
       [e] while the file [filename] is being processed *)
    val error_msg : ?filename:string -> (error * location) -> string

    val empty_bracket_stack : (bracket * location) list
    
    val push_bracket : bracket -> location -> (bracket * location) list -> (bracket * location) list

    val pop_bracket : bracket -> location -> (bracket * location) list -> (bracket * location) list
      
    val check_brackets : (bracket * location) list -> unit
      
  end
  

module Make (E:E) : ERROR with type synt_error = E.t
