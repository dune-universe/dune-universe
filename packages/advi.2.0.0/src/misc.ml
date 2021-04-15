(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* General type abbrevs. *)
type file_name = string;;
type dir_name = string;;
type line_number = int;;

type modifiers = int;;
type mouse_x = int;;
type mouse_y = int;;
type button_pressed = bool;;
type key_pressed = bool;;

(* Fatal error in advi's code. *)
exception Fatal_error of string;;
let fatal_error x = raise (Fatal_error x);;

let handle_fatal_error f () =
  try f () with Fatal_error s -> prerr_endline s; exit 1;;

(* To emit a warning. *)
let emit_warning mes =
  Printf.fprintf stderr "Warning: %s" mes;
  prerr_newline ()
;;

(* To set up and output warnings. *)
let set_warnings, warning =
  let warnings = ref true in
  (fun b -> warnings := b),
  (fun s -> if !warnings then emit_warning s)
;;

(* Debugging. *)
let forward_debug_endline =
  ref (function (_ : string) -> failwith "undefined forward debug_endline")
;;

let debug_endline s = (!forward_debug_endline s : unit);;

let set_forward_debug_endline f = forward_debug_endline := f;;

let debug_stop s =
  if false then begin
   prerr_string (s ^ " --> Press return to go on");
   flush stderr;
   let _ = input_line stdin in
   prerr_endline "Ok"
  end
;;

(* Pushing events in the advi's events queue. *)
let forward_push_char_event =
  ref (fun (c : char) ->
         failwith "undefined forward push_char_event")
;;
let forward_push_key_event =
  ref (fun (c : char) (ms : modifiers) ->
         failwith "undefined forward push_key_event")
;;
let forward_push_mouse_event =
  ref (fun (x : mouse_x) (y : mouse_y) (b : button_pressed) ->
         failwith "undefined forward push_mouse_event")
;;
let forward_push_full_event =
  ref (fun (c : char) (ms : modifiers) (k : key_pressed)
           (x : mouse_x) (y : mouse_y) (b : button_pressed) ->
         failwith "undefined forward push_full_event")
;;

let push_char_event c = (!forward_push_char_event c : unit);;
let push_key_event c ms = (!forward_push_key_event c ms : unit);;
let push_mouse_event x y b = (!forward_push_mouse_event x y b : unit);;
let push_full_event c ms k x y b =
  (!forward_push_full_event c ms k x y b : unit)
;;

let set_forward_push_char_event f = forward_push_char_event := f;;
let set_forward_push_key_event f = forward_push_key_event := f;;
let set_forward_push_mouse_event f = forward_push_mouse_event := f;;
let set_forward_push_full_event f = forward_push_full_event := f;;

(* To round properly a float to an int (round it to the nearest integer). *)
let round_pos x = int_of_float (x +. 0.5);;
let round_neg x = int_of_float (x -. 0.5);;

let round x = if x >= 0. then round_pos x else round_neg x;;

(* Reverse and filters list l according to f. (Pour faire plaisir à Gérard) *)
let reverse_filter f l =
  let rec filter res = function
    | [] -> res
    | a :: l -> if f a then filter (a :: res) l else filter res l in
  filter [] l
;;

let reverse_map f l =
  let rec map res = function
    | [] -> res
    | a :: l -> map (f a :: res) l in
  map [] l
;;

(* Concat of List-1 and List-2 is  2-tsiL :: List-1 *)
let rec reverse_concat l1 = function
  | [] -> l1
  | a :: b -> reverse_concat (a :: l1) b
;;

(* Strings auxiliaries. *)
let string_prefix char s =
 let i = String.index s char in
 String.sub s 0 (i + 1)
;;

let string_suffix char s =
 let l = String.length s in
 let i = String.rindex s char in
 String.sub s i (l - i)
;;

let filename_extension = string_suffix '.';;

(* Check if string [s] contains string [p] at occurrence [i],
   or, equivalently, test if we have:
   String.sub s i (String.length p) = p. *)
let contains_string_from s i p =
 let lp = String.length p
 and ls = String.length s in
 let rec has i j =
   j >= lp ||
   i < ls && s.[i] = p.[j] && has (i + 1) (j + 1) in
 i >= 0 && has i 0
;;

let contains_string s p =
 let ls = String.length s in
 let rec contains i =
   i < ls &&
   (contains_string_from s i p || contains (i + 1)) in
 contains 0
;;

let has_prefix pre s = contains_string_from s 0 pre;;
let has_suffix suf s =
  contains_string_from s (String.length s - String.length suf) suf
;;

exception Match;;

let get_suffix pre str =
  let lpre = String.length pre in
  let lstr = String.length str in
  if has_prefix pre str then String.sub str lpre (lstr - lpre)
  else raise Match
;;

let rec split_string s p start =
  let len = String.length s
  and i = ref start in
  while !i < len && p s.[!i] do incr i done;
  if !i >= len then [] else
  begin
    let i0 = !i in
    while !i < len && not (p s.[!i]) do incr i done;
    let i1 = !i in
    String.sub s i0 (i1 - i0) :: split_string s p i1
  end
;;

let zap_to_char c s =
  let len = String.length s
  and i = ref 0 in
  while !i < len && s.[!i] <> c do incr i done;
  let i0 = !i + 1 in
  if i0 >= len then "" else String.sub s i0 (len - i0)
;;

let int_of_float_of_string s =
  try int_of_string s with _ -> truncate (float_of_string s)
;;

let is_digit c = c >= '0' && c <= '9';;

let string_substitute_var f s =
  let b = Buffer.create (String.length s * 2) in
  let rec loop ib =
    Scanf.bscanf ib "%c" (function
    | '@'
    | '!' (* Keeping this obsolete syntax. *) as c ->
        Scanf.bscanf ib "%c" (fun id ->
        begin
          try Buffer.add_string b (f id) with
          | Not_found -> Buffer.add_char b c; Buffer.add_char b id
        end;
        loop ib)
    | c -> Buffer.add_char b c; loop ib) in
  try loop (Scanf.Scanning.from_string s) with
  | _ -> Buffer.contents b
;;

let string_replace pat templ str =
  let result = Buffer.create (String.length str * 2) in
  let patlen = String.length pat in
  let find pat str at =
    let rec find_aux pos =
      if String.sub str pos patlen = pat then pos
      else find_aux (pos + 1) in
    try find_aux at with _ -> raise Not_found in
  let rec replace pos =
    try
      let fpos = find pat str pos in
      Buffer.add_string result (String.sub str pos (fpos - pos));
      Buffer.add_string result templ;
      replace (fpos + patlen)
    with
    | Not_found ->
        Buffer.add_string result
          (String.sub str pos (String.length str - pos));
        Buffer.contents result in
  replace 0
;;

(* lift a function to an option type *)
let lift f a = match a with None -> () | Some v -> f v;;
