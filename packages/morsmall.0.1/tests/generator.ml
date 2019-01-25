(******************************************************************************)
(*                                                                            *)
(*                                  Morsmall                                  *)
(*                       A concise AST for POSIX shell                        *)
(*                                                                            *)
(*   Copyright (C) 2017  Yann Régis-Gianas, Ralf Treinen, Nicolas Jeannerod   *)
(*                                                                            *)
(*   This program is free software: you can redistribute it and/or modify     *)
(*   it under the terms of the GNU General Public License as published by     *)
(*   the Free Software Foundation, either version 3 of the License, or        *)
(*   (at your option) any later version.                                      *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU General Public License for more details.                             *)
(*                                                                            *)
(*   You should have received a copy of the GNU General Public License        *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                            *)
(******************************************************************************)

open Morsmall.AST
open Morsmall.Location

let dummy_lexing_position =
  { pos_fname = "dummy" ;
    pos_lnum = 0 ;
    pos_bol = 0 ;
    pos_cnum = 0 }

let dummy_locate f x =
  { value = f x ;
    position = { start_p = dummy_lexing_position ;
                 end_p = dummy_lexing_position } }

type 'a p_array = (int * 'a) array

let choose (a : 'a p_array) : 'a =
  let p_max = Array.fold_left (fun p_tot (p,_) -> p_tot+p) 0 a in
  let n = Random.int p_max in
  let p_tot = ref 0 in
  let v_found = ref (snd (a.(0))) in
  for i = 0 to Array.length a - 1 do
    let (p, v) = a.(i) in
    if !p_tot <= n && n < !p_tot + p then
      v_found := v;
    p_tot := !p_tot + p
  done;
  !v_found

(* Parameters *)

type parameters =
  { depth : int }

let default_parameters =
  { depth = 10 }

let d p = { depth = p.depth - 1 } (* { p with depth = p.depth - 1 } *)

(* Generator helper functions *)

(* let g_bool ~prob =
 *   Random.float 1. < prob *)

let g_option ~prob inhabitant =
  if Random.float 1. < prob then
    Some (inhabitant ())
  else
    None

let rec g_list ~prob ~limit inhabitant =
  if limit > 0 && Random.float 1. < prob then
    (inhabitant ()) :: g_list ~prob ~limit:(limit - 1) inhabitant
  else
    []

(* Our generators *)

let rec g_word_component p : word_component =
  choose
    [| 1, (fun _ -> Literal "foo") ;
       1, (fun _ -> Variable ("x", NoAttribute)) ;
       (if p.depth <= 0 then 0 else 1),
       (fun p -> Subshell (g_program p)) ;
       1, (fun _ -> GlobAll) ;
       1, (fun _ -> GlobAny) |]
    (d p)

and g_word p =
  g_word_component (d p)
  :: g_list ~prob:0.9 ~limit:4 (fun () -> g_word_component (d p))

and g_word' p =
  dummy_locate g_word p

and g_name _p =
  "blah" (*FIXME*)

and g_pattern p =
  g_word (d p) :: g_list ~prob:0.8 ~limit:4 (fun () -> g_word (d p))

and g_pattern' p =
  dummy_locate g_pattern p

and g_assignment p =
  { variable = choose [|1,"x";2,"y";3,"z";4,"choucroute"|] ;
    word = g_word (d p) }

and g_assignment' p =
  dummy_locate g_assignment p

and g_descr _p =
  Random.int 10

and g_redirection_kind _p =
  choose
    [| 1, Output ;
       1, OutputDuplicate ;
       1, OutputAppend ;
       1, OutputClobber ;
       1, Input ;
       1, InputDuplicate ;
       1, InputOutput |]

and g_program p =
  g_list ~prob:0.5 ~limit:3
    (fun () -> g_command' (d p))
  
and g_command p =
  if p.depth <= 0 then
    g_simple_command (d p)
  else
    choose
      [| 1, g_simple_command ;
         1, (fun p -> Async (g_command (d p))) ;
         1, (fun p -> Seq (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> And (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> Or (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> Not (g_command' (d p))) ;
         1, (fun p -> Pipe (g_command' (d p), g_command' (d p))) ;
         1, (fun p -> Subshell (g_command' (d p))) ;
         1, g_for_clause ;
         1, g_case_clause ;
         1, g_if_clause ;
         1, g_while_clause ;
         1, g_until_clause ;
         1, g_function_definition ;
         1, g_redirection ;
         1, g_here_document |]
      (d p)

and g_command' p =
  dummy_locate g_command p

and g_simple_command p =
  let assignments =
    g_list ~prob:0.5 ~limit:5
      (fun () -> g_assignment' (d p))
  in
  let words =
    g_list ~prob:0.7 ~limit:10
      (fun () -> g_word' (d p))
  in
  if assignments = [] && words = [] then
    g_simple_command p
  else
    Simple (assignments, words)

and g_for_clause p =
  For (
      "x",
      g_option ~prob:0.8 (fun () -> g_list ~prob:0.8 ~limit:10 (fun () -> g_word (d p))),
      g_command' (d p)
    )

and g_case_clause p =
  Case (
      g_word (d p),
      g_list ~prob:0.7 ~limit:5 (fun () -> g_case_item' (d p) )
    )

and g_case_item p =
  (
    g_pattern' (d p),
    g_option ~prob:0.9 (fun () -> g_command' (d p))
  )

and g_case_item' p =
  dummy_locate g_case_item p

and g_if_clause p =
  If (
      g_command' (d p),
      g_command' (d p),
      g_option ~prob:0.6 (fun () -> g_command' (d p))
    )

and g_while_clause p =
  While (
      g_command' (d p),
      g_command' (d p)
    )

and g_until_clause p =
  Until (
      g_command' (d p),
      g_command' (d p)
    )

and g_function_definition p =
  Function (
      g_name (d p),
      g_command' (d p)
    )

and g_redirection p =
  Redirection (
      g_command' (d p),
      g_descr (d p),
      g_redirection_kind (d p),
      g_word (d p)
    )

and g_here_document p =
  HereDocument (
      g_command' (d p),
      g_descr (d p),
      dummily_located (g_word (d p) @ [Literal "\n"])
    )
