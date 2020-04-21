(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

{
type extended_token = Parser.token*(bool*bool)
              (* [token, [can_start_line,can_end_line]] *)

let parse_token (tok: extended_token): Parser.token = fst tok

let is_start_token (tok:extended_token): bool = fst (snd tok)

let is_end_token   (tok:extended_token): bool = snd (snd tok)


let last_is_endtok: bool ref      = ref false

let last_pos: Lexing.position ref = ref Lexing.dummy_pos

let cached_tok: (extended_token * Lexing.position) option ref
    = ref None

let initialize (filename:string) (lexbuf:Lexing.lexbuf): unit =
  let open Lexing in
  lexbuf.lex_curr_p <-
      {lexbuf.lex_curr_p with pos_fname = filename};
  last_is_endtok := false;
  last_pos       := lexbuf.lex_curr_p;
  cached_tok     := None



let info_of_pos (pos:Lexing.position): Support.info =
  let open Lexing in
  let line = pos.pos_lnum
  and col  = pos.pos_cnum - pos.Lexing.pos_bol + 1
  and fn   = pos.pos_fname
  in
  Support.FINFO (line,col,fn)

let info_of_last_pos (): Support.info =
  info_of_pos !last_pos


let info_of_lexbuf (lexbuf:Lexing.lexbuf): Support.info =
  let pos = Lexing.lexeme_start_p lexbuf in
  info_of_pos pos


let illegal_token (tok:string) (lexbuf:Lexing.lexbuf) =
  Support.error_info
    (info_of_lexbuf lexbuf)
    ("Illegal token: " ^ tok)



let keyword_table = Hashtbl.create 53

let _ =
  List.iter (fun (kwd,tok) -> Hashtbl.add keyword_table kwd tok)
    [
     ("Result",    Parser.KWResult);

     ("agent",     Parser.KWagent);
     ("all",       Parser.KWall);
     ("and",       Parser.KWand);
     ("as",        Parser.KWas);
     ("assert",    Parser.KWassert);
     ("case",      Parser.KWcase);
     ("check",     Parser.KWcheck);
     ("class",     Parser.KWclass);
     ("create",    Parser.KWcreate);
     ("deferred",  Parser.KWdeferred);
     ("do",        Parser.KWdo);
     ("else",      Parser.KWelse);
     ("elseif",    Parser.KWelseif);
     ("end"  ,     Parser.KWend);
     ("ensure",    Parser.KWensure);
     ("false",     Parser.KWfalse);
     ("feature",   Parser.KWfeature);
     ("from",      Parser.KWfrom);
     ("ghost",     Parser.KWghost);
     ("if",        Parser.KWif);
     ("mod",       Parser.KWmod);
     ("immutable", Parser.KWimmutable);
     ("import",    Parser.KWimport);
     ("in",        Parser.KWin);
     ("inherit",   Parser.KWinherit);
     ("inspect",   Parser.KWinspect);
     ("invariant", Parser.KWinvariant);
     ("local",     Parser.KWlocal);
     ("mutable",   Parser.KWmutable);
     ("not",       Parser.KWnot);
     ("note",      Parser.KWnote);
     ("old",       Parser.KWold);
     ("or",        Parser.KWor);
     ("orif",      Parser.KWorif);
     ("redefine",  Parser.KWredefine);
     ("rename",    Parser.KWrename);
     ("require",   Parser.KWrequire);
     ("some",      Parser.KWsome);
     ("then",      Parser.KWthen);
     ("true",      Parser.KWtrue);
     ("undefine",  Parser.KWundefine);
     ("use",       Parser.KWuse);
     ("variant",   Parser.KWvariant);
     ("via",       Parser.KWvia);
     ("while",     Parser.KWwhile);

     ("->",        Parser.ARROW);
     (":=",        Parser.ASSIGN);
     ("|",         Parser.BAR);
     ("^",         Parser.CARET);
     (":",         Parser.COLON);
     ("==>",       Parser.DARROW);
     ("||",        Parser.DBAR);
     ("::",        Parser.DCOLON);
     ("/",         Parser.DIVIDE);
     ("=",         Parser.EQ);
     ("~",         Parser.EQV);
     (">=",        Parser.GE);
     ("> ",        Parser.GT);
     ("<=",        Parser.LE);
     ("<",         Parser.LT);
     ("-",         Parser.MINUS);
     ("/=",        Parser.NEQ);
     ("/~",        Parser.NEQV);
     ("+",         Parser.PLUS);
     ("*",         Parser.TIMES);
   ]


let kwtoken id =
  let kw = Hashtbl.find keyword_table id
  in
  match kw with
    Parser.KWrequire -> kw, (true,false)
  | Parser.KWdeferred-> kw, (true,false)
  | Parser.KWensure  -> kw, (true,false)
  | Parser.KWassert  -> kw, (true,false)
  | Parser.KWall     -> kw, (true,false)
  | Parser.KWclass   -> kw, (true,false)
  | Parser.KWend     -> kw, (false,true)
  | Parser.KWnot     -> kw, (true,false)
  | Parser.KWold     -> kw, (true,false)
  | Parser.KWsome    -> kw, (true,false)
  | Parser.KWif      -> kw, (true,false)
  | Parser.KWinspect -> kw, (true,false)
  | Parser.KWtrue    -> kw, (true,true)
  | Parser.KWfalse   -> kw, (true,true)
  | Parser.KWResult  -> kw, (true,true)
  | _                -> kw, (false,false)



}





(* Rules *)

rule next_token = parse

  [' ' '\t'] +    { next_token lexbuf }

| "-- " [^'\n']*  { next_token lexbuf }

| "{:"            { comment 0 lexbuf }

| '\n'            { Lexing.new_line lexbuf; Parser.NEWLINE, (false,false) }

| ','             { Parser.COMMA,    (false,false) }

| '.'             { Parser.DOT,      (false,false) }

| '!'             { Parser.EXCLAM,   (false,false) }

| '{'             { Parser.LBRACE,   (true,false)  }

| '['             { Parser.LBRACKET, (true,false)  }

| '('             { Parser.LPAREN,   (true,false)  }

| '?'             { Parser.QMARK,    (false,false) }

| '}'             { Parser.RBRACE,   (false,true)  }

| ']'             { Parser.RBRACKET, (false,true)  }

| ')'             { Parser.RPAREN,   (false,true)  }

| ';'             { Parser.SEMICOL true,  (false,false) }

| '_'             { Parser.USCORE,   (true,true) }


| "/in"           { Parser.NOTIN,    (false,false) }


| ['-' '~' '='] ['+' '-' '/' '*' '<' '>' '=' '~' ':' '#' '|' '^']*
    ('>' | ">*" | ">+" ) | "|-" | "|-"
    as op {
  try
    kwtoken op
  with Not_found ->
    let sym = Support.ST.symbol op in
    Parser.RELOP sym, (false,false)
}

| ['+' '-' '/' '*' '<' '>' '=' '~' ':' '#' '|' '^']+ as op {
  try
    kwtoken op
  with
    Not_found ->
      let len = String.length op in
      assert (0 < len);
      let last = op.[len-1]
      and sym  = Support.ST.symbol op
      in
      if last = ':'
      then Parser.ROPERATOR  sym, (false,false)
      else Parser.OPERATOR sym, (false,false)
}


| ['0'-'9']+ as num { Parser.NUMBER (Support.ST.symbol num), (true,true) }


| ['A'-'Z'] ['A'-'Z' '0'-'9' '_']* as id {
  try
    kwtoken id
  with
    Not_found ->
      Parser.UIDENTIFIER (Support.ST.symbol id), (true,true)
}


| ['a'-'z'] ['a'-'z' '0'-'9' '_']* as id {
  try
    kwtoken id
  with
    Not_found -> Parser.LIDENTIFIER (Support.ST.symbol id), (true,true)
}


| ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']* as id {
  try
    kwtoken id
  with
    Not_found ->
      illegal_token id lexbuf
}

| eof   { Parser.EOF, (false,false) }


| _  as ch   {
  illegal_token  (String.make 1 ch) lexbuf
}

and comment level = parse

  ':'           { comment level lexbuf }

| '{'           { comment level lexbuf }

| [^':' '\n' '{']+   { comment level lexbuf }

| '\n'          { Lexing.new_line lexbuf; comment level lexbuf }

| eof           {
    Support.error_info
        (info_of_lexbuf lexbuf)
        "End of file within comment"
}
| "{:"          { comment (level+1) lexbuf}

| ":}"          { if level = 0 then
                    next_token lexbuf
                  else
                    comment (level-1) lexbuf}


(* Trailer *)
{


let nextorcached_token lexbuf =
  match !cached_tok with
    None ->
      let tok = next_token lexbuf
      in
      tok, Lexing.lexeme_start_p lexbuf
  | Some(t,p) ->
      cached_tok := None;
      t,p

let some_cached () =
  match !cached_tok with None -> false | Some(_,_) -> true

let cache_next lexbuf =
  assert  (not (some_cached ()));
  let tok = next_token lexbuf
  in
  cached_tok := Some (tok, Lexing.lexeme_start_p lexbuf);
  tok

let return_tok tok isend pos =
  last_is_endtok := isend;
  last_pos       := pos;
  tok

let rec token lexbuf =
  let tok,pos = nextorcached_token lexbuf
  in
  match parse_token tok with
    Parser.NEWLINE ->
      if !last_is_endtok then
        let tok = cache_next lexbuf
        in
        if is_start_token tok then
          (last_is_endtok:=false;
           return_tok (Parser.SEMICOL false) false pos)
        else
          token lexbuf
      else
        token lexbuf

  | _ ->
      return_tok (fst tok) (is_end_token tok) pos
}
