{
  open Format
  open Lexing
  open Xdot_parser


  type error = 
    | IllegalCharacter of char
    | UnterminatedComment
    | UnterminatedString

  exception Error of error

  let report fmt = function
    | IllegalCharacter c -> fprintf fmt "illegal character %c" c
    | UnterminatedComment -> fprintf fmt "unterminated comment"
    | UnterminatedString -> fprintf fmt "unterminated string"


  (* lexical errors *)

  let keywords = Hashtbl.create 97
  let () = 
    List.iter 
      (fun (x,y) -> Hashtbl.add keywords x y)
      [ 
        "digraph", DIGRAPH;
        "graph", GRAPH;
      ]
	       
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let string_buf = Buffer.create 1024

  let char_for_backslash = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | c -> c

}

let newline = '\n'
let space = [' ' '\t' '\r']
let lalpha = ['a'-'z' '_']
let ualpha = ['A'-'Z']
let alpha = lalpha | ualpha
let digit = ['0'-'9']
let ident = alpha (alpha | digit | '\'')*
let decimal_literal = ['0'-'9']+
let int = '-'? decimal_literal
let float = '-'? decimal_literal ('.' decimal_literal)?


rule token = parse
  | newline 
      { newline lexbuf; token lexbuf }
  | space+  
      { token lexbuf }
  | "mlpost_node" (int as i) {NODE (int_of_string i)} 
  | ident as id  
      { try Hashtbl.find keywords id with Not_found -> IDENT id }
  | float as f { FLOAT (float_of_string f) }
  | "\""
      { STRING (string lexbuf) }
  | "'"
      { QUOTE }
  | ","
      { COMMA }
  | "("
      { LEFTPAR }
  | ")"
      { RIGHTPAR }
  | ":"
      { COLON }
  | ";"
      { SEMICOLON }
  | "->"
      { ARROW }
  | "<->"
      { LRARROW }
  | "."
      { DOT }
  | "|"
      { BAR }
  | "="
      { EQUAL }
  | "["
      { LEFTSQ }
  | "]"
      { RIGHTSQ }
  | "{"
      { LEFTAC }
  | "}"
      { RIGHTAC }
  | eof 
      { EOF }
  | _ as c
      { raise (Error (IllegalCharacter c)) }

and pos = parse
  | newline { newline lexbuf; token lexbuf }
  | (float as x) {FLOAT (float_of_string x)}
  | space+ {SPACE}
  | ',' {COMMA}
  | eof { EOF }
  | _ as c
      { raise (Error (IllegalCharacter c)) }


and string = parse
  | "\""
      { let s = Buffer.contents string_buf in
	Buffer.clear string_buf; 
	s }
  | "\\" (_ as c)
      { Buffer.add_char string_buf (char_for_backslash c); string lexbuf }
  | newline 
      { newline lexbuf; Buffer.add_char string_buf '\n'; string lexbuf }
  | eof
      { raise (Error UnterminatedString) }
  | _ as c
      { Buffer.add_char string_buf c; string lexbuf }

{

open Xdot_ast

let (*parse_string_with*) ps_with parse s =
  let lexbuf = Lexing.from_string s in
  try
    parse pos lexbuf
  with Error e -> 
    Format.eprintf "mlpost_dot error (pos,path,bb) : %a@." report e;
    exit 1
    | Parsing.Parse_error ->
        let pstart = lexeme_start_p lexbuf in
        let pend = lexeme_end_p lexbuf in
        Format.eprintf 
          "mlpost_dot(pos) parsing error at line %d, characters %d-%d@." 
          pstart.pos_lnum 
          (pstart.pos_cnum - pstart.pos_bol) 
          (pend.pos_cnum - pend.pos_bol);
        exit 1
    | e -> 
    Format.printf "mlpost_dot error in pos, path or bounding_box : %s@."
        (Printexc.to_string e);
    exit 1

let xdot_type digraph =
  let bounding_box = ref None in
  let nodes = ref [] in
  let edges = ref [] in
  List.iter (function
               | None -> ()
               | Some Graph l -> 
                   begin try
                     bounding_box := 
                       Some (ps_with Xdot_parser.bounding_box
                       (List.assoc "bb" l))
                   with Not_found -> () end
               | Some Node (i,l) ->
                   begin try
                     let p = 
                       ps_with Xdot_parser.pos (List.assoc "pos" l) in
                     nodes := (i,p)::!nodes 
                   with Not_found -> () end
               | Some Edge (i1,i2,l) -> 
                   begin try
                     let p = ps_with Xdot_parser.path (List.assoc "pos" l) in
                     edges := (i1,i2,p)::!edges 
                   with Not_found -> () end) digraph;
  let bounding_box = match !bounding_box with
    | None -> 
        Format.eprintf "Dot doesn't give any bounding box!! Please report to mlpost authors@.";
        exit 1
    | Some bb -> bb in
  { bounding_box = bounding_box;
    edges        = !edges;
    nodes        = !nodes}

let main f =
  try
    let digraph = Xdot_parser.file token f in
    xdot_type digraph
  with Error e -> 
    Format.eprintf "mlpost_dot error : %a@." report e;
    exit 1
    | Parsing.Parse_error ->
        let pstart = lexeme_start_p f in
        let pend = lexeme_end_p f in
        Format.eprintf "parsing error at line %d, characters %d-%d@." 
          pstart.pos_lnum 
          (pstart.pos_cnum - pstart.pos_bol) 
          (pend.pos_cnum - pend.pos_bol);
        exit 1

let node_name id = Format.sprintf "mlpost_node%i" id

}


(*
Local Variables: 
compile-command: "unset LANG; make -C ../.. contrib"
End: 
*)

