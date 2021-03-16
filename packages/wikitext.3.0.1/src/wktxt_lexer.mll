{
  (**/**)
  open Wktxt_parser

  let newline = ref true
  let last_def_term_line = ref 0
  let last_def_term_depth = ref 0
  let in_table = ref false
  let header_isopen = ref false

  (* Retourne soit [String str], soit [token], suivant si
    on est en début de ligne ou pas.
  *)
  let token_or_str (str, token) =
    if !newline then begin
      newline := false ;
      token
    end
    else
      STRING str

  let str_or_token (str, token) =
    if !newline then begin
      newline := false ;
      STRING str
    end
    else
      token

  let newline_token_or_str (str, token) lexbuf =
    Lexing.new_line lexbuf ;
    if !newline then
      token
    else begin
      newline := true ;
      STRING str
    end

  let table_or_str (str, token) =
    match token with
    | TABLE_START ->
      in_table := true ;
      token
    | TABLE_END when !in_table ->
      in_table := false;
      token
    | _ when !in_table ->
      token
    | _ -> STRING str

  let update_lex_new_line lexbuf ch =
    if ch = '\n' then Lexing.new_line lexbuf else ()

  let get_lex_line lexbuf =
    let pos = lexbuf.Lexing.lex_start_p in
    pos.Lexing.pos_lnum

  (* https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements *)
  let html_block =
    [ "address"
    ; "article"
    ; "aside"
    ; "blockquote"
    ; "details"
    ; "dialog"
    ; "dd"
    ; "div"
    ; "dl"
    ; "dt"
    ; "fieldset"
    ; "figcaption"
    ; "figure"
    ; "footer"
    ; "form"
    ; "h1"
    ; "h2"
    ; "h3"
    ; "h4"
    ; "h5"
    ; "h6"
    ; "header"
    ; "hgroup"
    ; "hr"
    ; "li"
    ; "main"
    ; "nav"
    ; "ol"
    ; "p"
    ; "pre"
    ; "section"
    ; "table"
    ; "ul"
    ]

}

let hrule = "----" ['-']*
let ws = [ ' ' '\t']
let wordchar = [^ '\'' '=' '*' '#' '\n' '[' ':' ';' '|' '!' '{' '}' '<']

let table_start = "{|"
let table_end = "|}"
let table_title = "|+"
let table_new_line = "|-" ['-']*
let cell_inline = "||"
let header_cell_inline = "!!"
let cell = "|"
let header_cell = "!"


(*
  When editing these lexing rules, do not forget to use and update the newline reference.
*)
rule nowiki buf = parse
  | "</nowiki>" {
      newline := false;
      let s = Buffer.contents buf in
      String.iter (update_lex_new_line lexbuf) s ;
      NOWIKI (s)
    }
  | _ as c {
      update_lex_new_line lexbuf c ;
      Buffer.add_char buf c;
      nowiki buf lexbuf
    }

and comment = parse
  | "-->" {
      newline := false;
      main lexbuf
    }

  | _ as c {
      update_lex_new_line lexbuf c ;
      comment lexbuf
    }

and main = parse
  | "<nowiki>" {
    nowiki (Buffer.create 1024) lexbuf
  }
  | "<!--" {
      comment lexbuf
    }
  | ("<" '/'? (['A'-'Z''a'-'z']+ as t) [^'>']* ">" as s) {
      newline := false;
      String.iter (update_lex_new_line lexbuf) s ;
      if List.mem (String.lowercase_ascii t) html_block
      then HTML_BLOCK s
      else STRING s
    }
  | "<" {
      newline := false ;
      STRING "<"
    }
  | (ws* table_title ws*) as s {
      let tok = token_or_str (s, TABLE_TITLE) in table_or_str (s, tok)
    }
  | (ws* table_start ws*) as s '\n' {
      let tok = newline_token_or_str (s, TABLE_START) lexbuf in table_or_str (s, tok)
    }
  | (ws* table_end ws*) as s {
      let tok = token_or_str (s, TABLE_END) in table_or_str (s, tok)
    }
  | (ws* table_new_line ws*) as s '\n' {
      let tok = newline_token_or_str (s, TABLE_NEW_LINE) lexbuf in table_or_str (s, tok)
    }
  | (ws* cell_inline ws*) as s {
      let tok = str_or_token (s, TABLE_CELL TableCell) in table_or_str (s, tok)
    }
  | (ws* header_cell_inline ws*) as s {
      let tok = str_or_token (s, TABLE_CELL TableHeader) in table_or_str (s, tok)
    }
  | (ws* cell ws*) as s {
      let tok = token_or_str (s, TABLE_CELL TableCell) in table_or_str (s, tok)
    }
  | (ws* header_cell ws*) as s {
      let tok = token_or_str (s, TABLE_CELL TableHeader) in table_or_str (s, tok)
    }
  | (ws* (':'* ';') as s1 ws*) as s2 {
      token_or_str (s2, DEFLIST (Term, String.length s1))
    }
  | (ws* ':'+ as s1 ws*) as s2 {
      token_or_str (s2, DEFLIST (Description, String.length s1))
    }
  | (ws* '='+ as s1 ws*) as s2 {
      header_isopen := true ;
      token_or_str (s2, HEADER (String.length s1))
    }
  | (ws* '*'+ as s1 ws*) as s2 {
      token_or_str (s2, LIST (Unordered, String.length s1))
    }
  | (ws* '#'+ as s1 ws*) as s2 {
      token_or_str (s2, LIST (Ordered, String.length s1))
    }
  | (ws* hrule ws*) as s {
      token_or_str (s, HRULE )
    }
  | (ws* as sp1) ('='+ as s) (ws* as sp2) ('\n' | eof) {
      newline := true ;
      Lexing.new_line lexbuf ;
      if !header_isopen then begin
        header_isopen := false ;
        HEADER (String.length s)
      end
      else
        STRING (sp1 ^ s ^ sp2)
    }
  | ws* '\n' as s {
      newline_token_or_str (s, EMPTYLINE) lexbuf
    }
  | ('['+ as pre) ([^ '[' ']' '\n' ]+ as s) (']'+ as post) {
      newline := false;
      LINK (String.sub pre 1 (String.length pre - 1)
            ^ s
            ^ String.sub post 1 (String.length post - 1))
    }
  | '\''+ as s {
    newline := false ;
    match String.length s with
    | 2 -> ITALIC
    | 3 -> BOLD
    | 5 -> BOLDITALIC
    | _ -> STRING s
  }
  | wordchar+ as s {
      newline := false ;
      STRING s
    }
  | _ as c {
      newline := false ;
      CHAR c
  }
  | eof {
      EOF
    }
