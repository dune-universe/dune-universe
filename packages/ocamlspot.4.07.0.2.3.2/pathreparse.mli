(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Spot

val get :
  string                     (* source file name *)
  -> Region.t      (* the spot region *)
  -> Position.t    (* cursor pos in the region *)
  -> Path.t         (* the path found at the region *)
  -> (Path.t * Region.t) option  (* sub path found *)
