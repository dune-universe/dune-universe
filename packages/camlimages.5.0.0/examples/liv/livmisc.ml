(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: livmisc.ml,v 1.3 2004/09/23 07:20:20 weis Exp $ *)

let may f = function
  | Some v -> f v
  | None -> ()

let color_merge (r1, g1, b1) (r2, g2, b2) max cntr =
  (r1 * (max - cntr) + r2 * cntr) / max,
  (g1 * (max - cntr) + g2 * cntr) / max,
  (b1 * (max - cntr) + b2 * cntr) / max

let remove_space s =
  let s =
    let pos = ref 0 in
    while !pos < String.length s
       && List.mem s.[!pos] [' '; '\t'] do incr pos done;
    if !pos = 0 then s
    else String.sub s !pos (String.length s - !pos) in
  let l = String.length s in
  let pos = ref (l - 1) in
  while !pos >= 0 && List.mem s.[!pos] [' '; '\t'] do decr pos done;
  if !pos = l - 1 then s else String.sub s 0 (succ !pos)

let get_extension s =
  try
    let dotpos = String.rindex s '.' in
    String.sub s 0 dotpos,
    String.sub s (dotpos + 1)  (String.length s - dotpos - 1)
  with
  | _ -> s, ""

let normalize_filename file =
  let is_absolute = not (Filename.is_relative file) in
  let tkns = Mstring.split_str (function '/' -> true | _ -> false) file in
  let tkns =
    List.fold_left
      (fun acc -> function
       | "." -> acc
       | ".." ->
         begin try List.tl acc with
         | _ -> if is_absolute then acc else ".." :: acc end
       | tkn -> tkn :: acc)
      [] tkns in
  (if is_absolute then "/" else "") ^
  Mstring.catenate_sep "/" (List.rev tkns)

type 'a result =
   | Ok of 'a
   | Exn of exn

let after f g =
  match
    let r = try Ok (f ()) with | e -> Exn e in
    g ();
    r
  with
  | Ok r -> r
  | Exn e -> raise e

let (!!) = Lazy.force

let string_tail str len = String.sub str (String.length str - len) len
