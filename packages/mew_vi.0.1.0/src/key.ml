(*
 * key.ml
 * -----------
 * Copyright : (c) 2019 - 2020, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of mew_vi.
 *)


type code =
  | Char of string
  | Enter
  | Escape
  | Tab
  | Up
  | Down
  | Left
  | Right
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Next_page
  | Prev_page
  | Home
  | End
  | Insert
  | Delete
  | Backspace

let code_to_int=
  let bin_to_int bin=
    let len= String.length bin in
    let rec calc s pos acc=
      if pos < len then
        calc s (pos+1) (acc + (int_of_char s.[pos]) lsl (pos * 8))
      else
        acc
    in
    calc bin 0 0
  in
  function
    | Enter-> 1
    | Escape-> 2
    | Tab-> 3
    | Up-> 4
    | Down-> 5
    | Left-> 6
    | Right-> 7
    | F1-> 8
    | F2-> 9
    | F3-> 10
    | F4-> 11
    | F5-> 12
    | F6-> 13
    | F7-> 14
    | F8-> 15
    | F9-> 16
    | F10-> 17
    | F11-> 18
    | F12-> 19
    | Next_page-> 20
    | Prev_page-> 21
    | Home-> 22
    | End-> 23
    | Insert-> 24
    | Delete-> 25
    | Backspace-> 26
    | Char bin-> 27 + (bin_to_int bin)

let code_to_string= function
  | Char bin-> Printf.sprintf "Char %s" bin
  | Enter-> "Enter"
  | Escape-> "Escape"
  | Tab-> "Tab"
  | Up-> "Up"
  | Down-> "Down"
  | Left-> "Left"
  | Right-> "Right"
  | F1-> "F1"
  | F2-> "F2"
  | F3-> "F3"
  | F4-> "F4"
  | F5-> "F5"
  | F6-> "F6"
  | F7-> "F7"
  | F8-> "F8"
  | F9-> "F9"
  | F10-> "F10"
  | F11-> "F11"
  | F12-> "F12"
  | Next_page-> "Next_page"
  | Prev_page-> "Prev_page"
  | Home-> "Home"
  | End-> "End"
  | Insert-> "Insert"
  | Delete-> "Delete"
  | Backspace-> "Backspace"

type t = {
  control: bool;
  meta: bool;
  shift: bool;
  code : code;
}

let t_to_int t=
  let b= function true -> 1 | false -> 0 in
  (code_to_int t.code, [b t.control; b t.meta; b t.shift])

let t_to_string t= Printf.sprintf "{ %s%s%s%s }"
  (code_to_string t.code)
  (if t.control then "; control" else "")
  (if t.meta then "; meta" else "")
  (if t.shift then "; shift" else "")

type modifier=
  | Control
  | Meta
  | Shift

let compare_code t1 t2= compare t1.code t2.code
let compare_modifier t1 t2=
  let c= compare t1.control t2.control in
  if c <> 0 then c else
  let c= compare t1.meta t2.meta in
  if c <> 0 then c else
  compare t1.shift t2.shift


module Modifiers = Set.Make(
  struct
    type t= modifier
    let compare= compare
  end)

type modifiers= Modifiers.t

let compare t1 t2=
  let c= compare_code t1 t2 in
  if c <> 0 then c
  else compare_modifier t1 t2

let control key = key.control
let meta key = key.meta
let shift key = key.shift
let code key = key.code

let create ~code ~modifiers=
  { control= Modifiers.mem Control modifiers;
    meta= Modifiers.mem Meta modifiers;
    shift= Modifiers.mem Shift modifiers;
    code
  }

let create_modifiers= Modifiers.of_list

let modifiers t=
  let l= [] in
  let l= if t.control then Control::l else l in
  let l= if t.meta then Meta::l else l in
  let l= if t.shift then Shift::l else l in
  Modifiers.of_list l

let modifier ~key ~modifier=
  match modifier with
  | Control-> key.control
  | Meta-> key.meta
  | Shift-> key.shift

let equal k1 k2= compare k1 k2 = 0

let hash k= let code, modifier= t_to_int k in
  Mew.Key.hash code modifier

let to_string= t_to_string
