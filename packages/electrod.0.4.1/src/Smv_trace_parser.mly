/*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2019 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************/

%parameter <D : sig
  val base : (Name.t, Tuple_set.t) CCList.Assoc.t 
end>

%{

(* fixes issue between Menhir and dune *)
(* https://github.com/ocaml/dune/issues/2450 *)
module Libelectrod = struct end  

  open Containers

  (* vendored from Containers (BSD license allows this): 
    its prototype changd in 2.4, so copy a version to
    compile with many versions of Containers.  *)
  let rec search_set eq acc l x ~f = match l with
    | [] -> f x None acc
    | (x',y')::l' ->
      if eq x x'
      then f x (Some y') (List.rev_append acc l')
      else search_set eq ((x',y')::acc) l' x ~f

  let update ~eq f x l =
    search_set eq [] l x
      ~f:(fun x opt_y rest ->
        match f opt_y with
          | None -> rest (* drop *)
          | Some y' -> (x,y') :: rest)
  (* END vendoring *)

  
  (* converts a list containing pairs (name, tuple) in a list of pairs (name,
     set of tuples), i.e. gathers (in [acc]) all tuples corresponding to a given
     name in a same set. The list is nonempty. *)
  let upd tuple = function
    | None -> Some (Tuple_set.singleton tuple)
    | Some ts -> Some (Tuple_set.add tuple ts)
    
  let convert_name_tuple_l ntl =
    let rec walk acc = function
      | [] -> acc
      (* if None: means that atom was FALSE, else TRUE *)
      | None::tl -> walk acc tl
      | Some (name, tuple)::tl ->
         begin
           (* Msg.debug (fun m -> m "conv (%a, %a)" Name.pp name Tuple.pp tuple); *)
           let acc2 = update ~eq:Name.equal (upd tuple) name acc in
           walk acc2 tl
         end
    in walk D.base ntl


  (* From what we gathered, we should remove the last state from the returned
     trace (it is the loop-target state), and if the first state is the target,
     it is not said so we have to tag it as a loop-target by ourselves. *)
  let met_one_loop = ref false

  let rec remove_last = function
    | [] -> assert false
    | [_] -> []
    | hd::tl -> hd :: remove_last tl
    
       
%}
  
%start <Outcome.states> trace



%%

%public trace:
states = state+ EOF 
    {
      remove_last states
    }

state:
 loop = iboption(LOOP) STATE ntl = atomic*
    {
      let valu = Outcome.valuation @@ convert_name_tuple_l ntl in
      if loop && not !met_one_loop then (* nuXmv may report several loop states, we only keep one *)
        (met_one_loop := true;
         Outcome.loop_state valu)
      else
        Outcome.plain_state valu
    }

atomic:
 ATOMIC EQUAL FALSE
    { None }                
| v = ATOMIC EQUAL TRUE
    { Some v }
    
////////////////////////////////////////////////////////////////////////
// MENHIR MACROS
////////////////////////////////////////////////////////////////////////
  
%public %inline iboption(X):
(* empty *)
{ false }
| X
    { true }


    
