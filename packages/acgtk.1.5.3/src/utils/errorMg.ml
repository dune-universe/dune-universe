type location = Lexing.position * Lexing.position

let update_loc ?filename lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match filename with
    | None -> pos.Lexing.pos_fname
    | Some s -> s
  in
    lexbuf.Lexing.lex_curr_p <- { pos with
			     Lexing.pos_fname = new_file;
			     Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
			     Lexing.pos_bol = pos.Lexing.pos_cnum;
			 }

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
  


              
  
module Make(E:E) =
  struct

    type bracket =
      | Round
      | Square
      | Curly

    let kind_to_char = function
      | Round -> '('
      | Square -> '['
      | Curly -> '{'
      
    type lex_error =
      | Unstarted_comment
      | Unstarted_bracket
      | Mismatch_parentheses of bracket
      | Unclosed_comment
      | Expect of string
      | Bad_token

    let lex_error_to_string = function
      | Unstarted_comment -> "Syntax error: No comment opened before this closing of comment"
      | Unstarted_bracket -> "Syntax error: No bracket opened before this right bracket"
      | Unclosed_comment -> "Syntax error: Unclosed comment"
      | Mismatch_parentheses k -> Printf.sprintf "Syntax error: Unclosed parenthesis '%c'" (kind_to_char k)
      | Expect s -> Printf.sprintf "Syntax error: %s expected" s
      | Bad_token -> "Lexing error: no such token allowed"
                   



      
    type synt_error = E.t
      
    type error =
      | SyntError of synt_error
      | LexError of lex_error
      | SysError of string

                   
    exception Error of (error * location)

    let compute_comment_for_location (pos1,pos2) =
      let line2 = pos2.Lexing.pos_lnum in
      let col2 = pos2.Lexing.pos_cnum - pos2.Lexing.pos_bol in
      let pos1 = pos1 in
      let line1 = pos1.Lexing.pos_lnum in
      let col1 = pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol in
      if line1=line2 then
        Printf.sprintf "line %d, characters %d-%d" line2 col1 col2
      else
        Printf.sprintf "line %d, character %d to line %d, character %d" line1 col1 line2 col2
      
            
    let error_msg ?filename (err,loc)  =
      let input =
        match filename with
        | None -> ""
        | Some f -> Printf.sprintf "File \"%s\"," f in
      let msg =
        match err with
        | LexError e -> lex_error_to_string e
        | SysError e -> e
        | SyntError e -> E.to_string e in
      Printf.sprintf
        "%s%s\n%s"
        input
        (compute_comment_for_location loc)
        msg

    
  let empty_bracket_stack = []

  let push_bracket br loc stack =
    (br,loc)::stack

  let pop_bracket br l stack=
    match stack with
    | [] -> raise (Error (LexError Unstarted_bracket, l))
    | (k,_)::tl when k = br -> tl
    | (k,l)::_ -> raise (Error (LexError (Mismatch_parentheses k),l))

  let check_brackets stack =
    match stack with
    | [] -> ()
    | (k,loc)::_ -> raise (Error (LexError (Mismatch_parentheses k),loc))

      
  end
