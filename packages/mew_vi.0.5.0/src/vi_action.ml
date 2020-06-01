(*
 * vi_action.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


type insert=
  | Append of string (* a *)
  | AppendEol of string (* A *)
  | Insert of string (* i *)
  | InsertBol of string (* I *)
  | Newline_below of string (* o *)
  | Newline_above of string (* O *)

type motion=
  (* left right *)
  | Left (* h *)
  | Right (* l *)
  | Right_nl (* l, including newline *)
  | Line_FirstChar (* 0 *)
  | Line_FirstNonBlank (* ^ *)
  | Line_LastChar (* $ *)
  | Line_LastChar_nl (* $ *)
  | Line_LastNonBlank (* g_ *)
  | Line_LastNonBlank_nl (* g_ *)

  (* up down *)
  | Upward (* k *)
  | Downward (* j *)
  | GotoLine (* gg or G *)
  | GotoLine_first (* gg *)
  | GotoLine_last (* G *)

  (* word *)
  | Word (* w *)
  | WORD (* W *)
  | Word_end (* e *)
  | WORD_end (* E *)
  | Word_back (* b *)
  | WORD_back (* B *)
  | Word_back_end (* ge *)
  | WORD_back_end (* gE *)

  (* line *)
  | Line

  (* occurrence *)
  | Occurrence_inline of string
  | Occurrence_inline_back of string
  | Occurrence_inline_till of string
  | Occurrence_inline_till_back of string

  (* text object *)
  | Sentence_backword (* ( *)
  | Sentence_forward (* ) *)
  | Paragraph_backward (* { *)
  | Paragraph_forward (* } *)

  (* text object selection *)
  | Word_include (* aw *)
  | Word_inner (* iw *)
  | WORD_include (* aW *)
  | WORD_inner (* iW *)
  | Sentence_include (* as *)
  | Sentence_inner (* is *)
  | Paragraph_include (* ap *)
  | Paragraph_inner (* ip *)
  | Parenthesis_include (* a( a) *)
  | Parenthesis_inner (* i( i) *)
  | Bracket_include (* a[ a] *)
  | Bracket_inner (* i[ i] *)
  | AngleBracket_include (* a< a> *)
  | AngleBracket_inner (* i< i> *)
  | Brace_include (* a{ a} *)
  | Brace_inner (* i{ i} *)
  | Quote_include of string
  | Quote_inner of string

  (* match *)
  | Match

type register= string

type t=
  | Insert of insert * int
  | Motion of motion * int
  | Delete of register * motion * int
  | Change of register * motion * int
  | Join of int
  | Undo of int
  | Paste_before of register * int
  | Paste_after of register * int
  | Yank of register * motion * int
  | DeleteSelected of register
  | YankSelected of register
  | ChangeMode of Mode.Name.t

