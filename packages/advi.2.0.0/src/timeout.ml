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

(* Simple timeout handler *)

module Timeout = struct
  type t = {
      at : float;
      callback : (unit -> unit);
      stamp : int;
    }
  let compare t t' = compare t.at t'.at
end;;

module TimeoutSet = Set.Make(Timeout);;

open Timeout;;

type t = Timeout.t;;

let set = ref TimeoutSet.empty;;
let stamp = ref 0;;

let callback () =
  let rec callback () =
    let min = TimeoutSet.min_elt !set in
    let now = Unix.gettimeofday () in
    let wait = min.at -. now in
    (* Be careful: if wait < 1e-7, timer will never be raised... *)
    if wait < 0.001 then
      begin
        set := TimeoutSet.remove min !set;
        min.callback ();
        callback ()
      end
    else
      ignore
        (Unix.setitimer Unix.ITIMER_REAL
           {Unix.it_interval = 0.0; Unix.it_value = wait}) in
  try callback () with Not_found -> ();;

let init () =
  (* Already existing itimer's handler is disabled! *)
  ignore (Sys.signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> callback ())));
  set := TimeoutSet.empty;
  stamp := 0;;

let add sec cbk =
  if sec < 0.001 then raise (Invalid_argument "Timeout.add");
  let start = Unix.gettimeofday () in
  let at = start +. sec in
  let mystamp = !stamp in
  incr stamp;
  let timeout = { at = at; callback = cbk; stamp = mystamp} in
  set := TimeoutSet.add timeout !set;
  callback ();
  timeout;;

let rec repeat sec cbk =
  ignore
    (add sec
       (fun () ->
          try cbk (); repeat sec cbk with
          | exn -> repeat sec cbk; raise exn));;

let remove timeout =
  if not (TimeoutSet.mem timeout !set) then raise Not_found;
  set := TimeoutSet.remove timeout !set;
  callback ();;

