type range = {
    start_pos : Lexing.position;
    end_pos : Lexing.position;
  }

exception Syntax_error of range * string

type delimiter_kind = Open_codoc | Comment | String | String_ident of string | Square_bracket

type open_delimiter = {
    position : Lexing.position;
    kind : delimiter_kind;
    mutable warned : bool;
  }

type 'a context = {
    out_channel : 'a;
    delimiter_stack : open_delimiter Stack.t;
    warnings : (range * string) Queue.t;
    mutable important_warnings : bool;
  }

val main : out_channel context -> Lexing.lexbuf -> unit

val mismatched_delimiters : 'a context -> open_delimiter -> Lexing.position -> unit
