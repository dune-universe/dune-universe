module Lexicalizer =
struct
  (* The string buffering machinery *)
  (* copied from genlex.ml*)
  
(*  let initial_buffer = String.create 32 *)
  let initial_buffer = String.make 32 ' '
    
  let buffer = ref initial_buffer
  let bufpos = ref 0
    
  let reset_buffer () = buffer := initial_buffer; bufpos := 0
    
  let store c =
    if !bufpos >= String.length !buffer then
      begin
        let newbuffer = String.make (2 * !bufpos) ' ' in
          String.blit !buffer 0 newbuffer 0 !bufpos; buffer := newbuffer
      end;
    String.set !buffer !bufpos c;
    incr bufpos
      
  let get_string () =
    let s = String.sub !buffer 0 !bufpos in buffer := initial_buffer; s
      

  (*The lexer*)

  type pos = P of int* int

  let first_pos = P(1,0)

  type token = 
      Ident of string * pos 
      | Str of string * pos
      | INIT of pos
      | EQ of pos
      | NEQ of pos
      | SEP of pos
      | LPAR of pos 
      | RPAR of pos 
      | COMMA of pos
      | PERIOD of pos
      | SEMICOL of pos

  let col pos = let P(line,col) = pos in P(line,col+1)

  let line pos = let P(line,col) = pos in P(line+1,0)                                

  let print_pos pos = 
    let P(line,col) = pos in
      "Line "^(string_of_int line)^", col "^(string_of_int col)

  let char_failure pos c= 
    failwith ((print_pos (col pos))^": invalid character '"^(String.make 1 c)^"'")

  let init_pos = P(1,0)
                                         
  let rec next_token stream pos= 
    match Stream.peek stream  with
        Some (' ' | '\t') ->
          Stream.junk stream; next_token stream (col pos)
      | Some ('\n') ->
          Stream.junk stream; next_token stream (line pos)
      | Some ('A'..'Z' | 'a'..'z'| '_' | '\192'..'\255' as c) ->
          Stream.junk stream; reset_buffer() ; store c; 
          let pos = col pos in
            ident stream pos pos
      | Some('=') -> 
          let pos = col pos in 
            Stream.junk stream; Some(EQ(pos),pos)
      | Some('~') ->
          let pos = col pos in
            Stream.junk stream; ineq stream pos
      | Some ('"') ->
          Stream.junk stream ; reset_buffer () ; 
          let pos = col pos in
            string stream pos pos
      | Some ('(') ->
          Stream.junk stream ; maybe_comment stream (col pos)
      | Some (')') ->
          Stream.junk stream ; Some(RPAR(pos),pos)
      | Some (',') ->
          let pos = col pos in
            Stream.junk stream ; Some(COMMA (pos),pos)
      | Some (':') -> 
          Stream.junk stream ; maybe_sep stream (col pos)
      | Some (';') ->
          let pos = col pos in
            Stream.junk stream ; Some(SEMICOL (pos), pos)
      | Some ('.') ->
          let pos = col pos in
            Stream.junk stream ; Some(PERIOD (pos), pos)
      | Some(c) -> char_failure pos c
      | None -> None
  and ineq stream pos = 
    match Stream.peek stream with
        Some('=') -> 
          Stream.junk stream; Some(NEQ (pos), col(pos))
      |_ -> 
         failwith ((print_pos pos)^": '=' expected.")
  and
      ident stream init_pos last_pos =
    match Stream.peek stream with
        Some
          ('a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '-' as c) ->
            Stream.junk stream; store c; ident stream init_pos (col last_pos)
      |_ -> 
         let s = get_string() in
           (if s = "init"
           then 
             Some(INIT(init_pos), last_pos)
             else
               ((*if s="eq"
                  then
                  failwith ((print_pos (init_pos))^": invalid identifier, the name 'eq' is reserved.")
                  else*)
                 Some(Ident (get_string(),init_pos), last_pos)
               )
           )
  and
      string stream init_pos last_pos =
    match Stream.peek stream with
        Some
          ('A'..'Z' | 'a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '-' as c) ->
            Stream.junk stream; store c; string stream init_pos (col last_pos)
      |  Some ('"') ->
           Stream.junk stream ; 
           Some(Str (get_string(),init_pos), (col last_pos))
      | Some c -> char_failure last_pos c
      |_ -> raise Stream.Failure
  and
      maybe_sep stream pos = 
    match Stream.peek stream with
        Some('-') -> 
          Stream.junk stream ; Some(SEP (pos), (col pos))
      | Some(c) -> char_failure (col pos) c
      |_ -> raise Stream.Failure
  and
      maybe_comment stream pos = 
    match Stream.peek stream with
        Some ('*') ->
          Stream.junk stream;
          let pos = 
            comment stream pos
          in
            next_token stream pos
      |_ -> Some (LPAR (pos),pos)
  and 
      comment stream pos = 
    match Stream.peek stream with
        Some '*' -> Stream .junk stream ; maybe_end_comment stream (col pos)
      | Some ('\n') ->
          Stream.junk stream; comment stream (line pos)
      | Some (c) ->
          Stream.junk stream; comment stream (col pos)
      |_ -> raise Stream.Failure
  and 
      maybe_end_comment stream pos = 
    match Stream.peek stream with
        Some ')' -> Stream.junk stream ; (col pos)
      | Some '*' -> Stream.junk stream ; maybe_end_comment stream (col pos)
      | Some (c) -> Stream.junk stream ; comment stream (col pos)
      |_ -> raise Stream.Failure

end
