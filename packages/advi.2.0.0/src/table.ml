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

type 'a status =
   | Unknown
   | Known of 'a
   | Error of exn;;

type 'a t = {
   table : 'a status array;
   build : int -> 'a;
   (* extension for japanese characters (id > 255) *)
   hash : (int, 'a) Hashtbl.t;
};;

let make f = {
  table = Array.make 256 Unknown;
  build = f;
  hash = Hashtbl.create 1031;
};;

let get tbl n =
  if n < 0 || n > 0xFF then begin
    try Hashtbl.find tbl.hash n with Not_found ->
      let v = tbl.build n in
      Hashtbl.add tbl.hash n v;
      v
  end else
  let table = tbl.table in
  match table.(n) with
  | Known v -> v
  | Error e -> raise e
  | Unknown ->
      try
        let v = tbl.build n in
        table.(n) <- Known v; v
      with e ->
        table.(n) <- Error e;
        raise e;;
