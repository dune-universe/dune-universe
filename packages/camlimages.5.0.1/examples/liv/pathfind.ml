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

(* $Id: pathfind.ml,v 1.11 2004/09/23 07:20:20 weis Exp $ *)

open Unix

(* Tilde substitution *)


(* skip to next / *)
let rec next_slash s n =
  if n >= String.length s || s.[n] = '/'
  then n
  else next_slash s (succ n)

let tilde_subst s =
  try
    if s = "" || s.[0] <> '~' then s else
      let len = String.length s in
      if len = 1 then Sys.getenv "HOME" else
      match s.[1] with
      | '/' -> Filename.concat (Sys.getenv "HOME") (String.sub s  2  (len - 2))
      | _ ->
        let final = next_slash s 1 in
        let user = String.sub s 1 (pred final) in
        let pwnam = getpwnam user in
        if succ final >= len then pwnam.pw_dir else
          Filename.concat pwnam.pw_dir
            (String.sub s (succ final) (len - succ final))
  with
  | Unix_error (_, _, _) -> s
  | Sys_error _ -> s
  | Not_found -> s

let find pathlist s =
  if s.[0] = '/' then
    if Sys.file_exists s then s else raise Not_found
  else begin
    let f = ref "" in
    try
      List.iter
       (fun path ->
          f := Filename.concat (tilde_subst path) s;
          (* prerr_endline ("tring "^ !f); *)
          if Sys.file_exists !f then raise Exit)
       pathlist;
      raise Not_found
    with
    | Exit -> !f
  end

