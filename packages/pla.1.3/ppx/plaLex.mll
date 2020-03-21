(* The MIT License (MIT)
Copyright (c) 2016 Leonardo Laguna Ruiz

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

{

open Lexing
open Pla_tokens

(** Updates the location of the lexbuf*)
let updateLocation lexbuf (line:int) (chars:int) : unit =
   let pos = lexbuf.lex_curr_p in
   lexbuf.lex_curr_p <- { pos with
                          pos_lnum = pos.pos_lnum + line;
                          pos_bol = pos.pos_cnum - chars;
                        }

let addBuff (buffer:Buffer.t) (acc:s list) =
   if Buffer.length buffer > 0 then
      let txt = Buffer.contents buffer in
      let ()  = Buffer.clear buffer in
      T(txt) :: acc
   else acc

}

let newline = ('\010' | '\013' | "\013\010")
let startid = ['A'-'Z' 'a'-'z' '_']
let idchar = ['A'-'Z' 'a'-'z' '_' '0'-'9' '.']

rule tokenizeStr buffer acc = parse
  | newline
    { updateLocation lexbuf 1 0; (* Increases the line *)
      let acc' = N :: addBuff buffer acc in
      tokenizeStr buffer acc' lexbuf
    }

  | "<#>"
    {
      let acc' = N :: addBuff buffer acc in
      tokenizeStr buffer acc' lexbuf
    }

  | "<#"
    {
      let start_pos = Lexing.lexeme_start_p lexbuf in
      let acc' = addBuff buffer acc in
      let acc' = tokenizeVar buffer acc' start_pos lexbuf in
      tokenizeStr buffer acc' lexbuf
    }

  | _
    {
      Buffer.add_string buffer (lexeme lexbuf);
      tokenizeStr buffer acc lexbuf
    }
  | eof
    {
      addBuff buffer acc
    }


and tokenizeVar buffer acc start_pos = parse
  | startid idchar *
    {
      Buffer.add_string buffer (lexeme lexbuf);
      tokenizeVar buffer acc start_pos lexbuf
    }

  | "#>"
    {
      let end_pos = Lexing.lexeme_end_p lexbuf in
      let loc = Location.{ loc_start = start_pos; loc_end = end_pos; loc_ghost = false } in
      let txt = Buffer.contents buffer in
      Buffer.clear buffer;
      V(txt,Template,loc) :: acc
    }

  | "#+>"
    {
      let end_pos = Lexing.lexeme_end_p lexbuf in
      let loc = Location.{ loc_start = start_pos; loc_end = end_pos; loc_ghost = false } in
      let txt = Buffer.contents buffer in
      Buffer.clear buffer;
      O :: V(txt,Template,loc) :: I :: acc
    }
  | "#s>"
    {
      let end_pos = Lexing.lexeme_end_p lexbuf in
      let loc = Location.{ loc_start = start_pos; loc_end = end_pos; loc_ghost = false } in
      let txt = Buffer.contents buffer in
      Buffer.clear buffer;
      V(txt,String,loc) :: acc
    }
  | "#i>"
    {
      let end_pos = Lexing.lexeme_end_p lexbuf in
      let loc = Location.{ loc_start = start_pos; loc_end = end_pos; loc_ghost = false } in
      let txt = Buffer.contents buffer in
      Buffer.clear buffer;
      V(txt,Int,loc) :: acc
    }
  | "#f>"
    {
      let end_pos = Lexing.lexeme_end_p lexbuf in
      let loc = Location.{ loc_start = start_pos; loc_end = end_pos; loc_ghost = false } in
      let txt = Buffer.contents buffer in
      Buffer.clear buffer;
      V(txt,Float,loc) :: acc
    }
  | _ { failwith "Invalid token" }

{

let tokenize (str:string) : s list =
  let lexbuf = Lexing.from_string str in
  let buffer = Buffer.create 128 in
  tokenizeStr buffer [] lexbuf
  |> List.rev


}