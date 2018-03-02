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

(* $Id: cache.ml,v 1.5 2008/06/16 22:35:42 furuse Exp $ *)

type ('a, 'b) elt = {
    key : 'a;
    data : 'b;
    at_remove : 'b -> unit;
    time : float;
  }

type ('a, 'b) t = ('a, 'b) elt option array

let create size = Array.make size None

let find_pos t key =
  let found = ref 0 in
  try
    for i = 0 to (Array.length t - 1) do
      match t.(i) with
      | None -> ()
      | Some {key = key'} when key = key' -> found := i; raise Exit
      | _ -> ()
    done;
    raise Not_found
  with
  | Exit -> !found

let find t key =
  match t.(find_pos t key) with
  | Some elt -> elt.data
  | _ -> assert false

let rename t key newkey =
  try
    let pos = find_pos t key in
    let data = match t.(pos) with Some d -> d | _ -> assert false in
    t.(pos) <- Some {data with key = newkey}
  with
  | Not_found -> ()

let find_empty_or_eldest t =
  let found = ref None in
  begin try
    for i = 0 to (Array.length t - 1) do
      match t.(i) with
      | None -> found := Some (i, None); raise Exit
      | Some elt ->
          match !found with
          | None -> found := Some (i, Some elt)
          | Some (_j, Some elt') when elt.time < elt'.time ->
            found := Some (i, Some elt)
          | _ -> ()
    done
  with Exit -> () end;
  match !found with
  | Some (i, _) -> i
  | None -> raise Not_found

let add t key data at_remove =
  let slot =
    try find_pos t key with Not_found ->
      try find_empty_or_eldest t with Not_found -> 0 
  in
  begin match t.(slot) with
  | Some elt -> elt.at_remove elt.data
  | None -> ()
  end;
  t.(slot) <- Some {key = key; data = data; at_remove= at_remove; time = Unix.time ()}
