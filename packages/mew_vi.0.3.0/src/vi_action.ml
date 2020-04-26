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
  | Left of int (* h *)
  | Right of int (* l *)
  | Right_nl of int (* l, including newline *)
  | Line_FirstChar of int (* 0 *)
  | Line_FirstNonBlank of int (* ^ *)
  | Line_LastChar of int (* $ *)
  | Line_LastChar_nl of int (* $ *)
  | Line_LastNonBlank of int (* g_ *)
  | Line_LastNonBlank_nl of int (* g_ *)

  (* up down *)
  | Upward of int (* k *)
  | Downward of int (* j *)
  | GotoLine of int (* gg or G *)
  | GotoLine_first (* gg *)
  | GotoLine_last (* G *)

  (* word *)
  | Word of int (* w *)
  | WORD of int (* W *)
  | Word_end of int (* e *)
  | WORD_end of int (* E *)
  | Word_back of int (* b *)
  | WORD_back of int (* B *)
  | Word_back_end of int (* ge *)
  | WORD_back_end of int (* gE *)

  (* line *)
  | Line

  (* occurrence *)
  | Occurrence_inline of string
  | Occurrence_inline_back of string
  | Occurrence_inline_till of string
  | Occurrence_inline_till_back of string

  (* text object *)
  | Sentence_backword of int (* ( *)
  | Sentence_forward of int (* ) *)
  | Paragraph_backward of int (* { *)
  | Paragraph_forward of int (* } *)

  (* text object selection *)
  | Word_include of int (* aw *)
  | Word_inner of int (* iw *)
  | WORD_include of int (* aW *)
  | WORD_inner of int (* iW *)
  | Sentence_include of int (* as *)
  | Sentence_inner of int (* is *)
  | Paragraph_include of int (* ap *)
  | Paragraph_inner of int (* ip *)
  | Parenthesis_include of int (* a( a) *)
  | Parenthesis_inner of int (* i( i) *)
  | Bracket_include of int (* a[ a] *)
  | Bracket_inner of int (* i[ i] *)
  | AngleBracket_include of int (* a< a> *)
  | AngleBracket_inner of int (* i< i> *)
  | Brace_include of int (* a{ a} *)
  | Brace_inner of int (* i{ i} *)
  | Quote_include of (string * int)
  | Quote_inner of (string * int)

  (* match *)
  | Match

type t=
  | Insert of insert * int
  | Motion of motion * int
  | Delete of motion * int
  | Change of motion * int
  | Join of int
  | Undo of int
  | Paste_before of int
  | Paste_after of int
  | Yank of motion * int
  | ChangeMode of Mode.Name.t

