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

(* extend the original module *)
open Ident

(* extend the original Ident module *)
  
let name id =
  let binding_time = binding_time id in
  Name.create (name id) binding_time

let () = reinit ()
      
let create_with_stamp ?(global=false) name stamp =
  let current_time = Ident.current_time () in
  reinit ();
  Ident.set_current_time stamp;
  let stamp' = Ident.current_time () in
  assert (stamp = stamp');
  let id = (if global then Ident.create_persistent else Ident.create) name in
  Ident.set_current_time current_time;
  id

let format ppf id = Format.pp_print_string ppf (name id)

let parse s =
  let s, pos = Name.parse s in
  (* CR jfuruse: actually 0 is global and should be printed as 'G'
     Current 'G' means -1 *)
  create_with_stamp ~global:(pos=0) s pos

