(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: geometry.ml,v 1.1 2006/11/28 15:43:28 rousse Exp $*)

(* geometry setting *)
type size =
   | Scale of float
   | Pixel of int
   | Guess

type aspect_opts =
   | Keep_at_most
   | Keep_at_least
   | Dont_keep

type resize_switch =
   | Always
   | Bigger_only
   | Smaller_only

type from =
   | TopLeft
   | BottomRight
   | Center

type position =
   | AtPixel of from * int
   | AtScale of from * float

type t = {
    geom_width : int;
    geom_height : int;
    geom_x : int;
    geom_y : int;
  }

type spec = {
    spec_width : size;
    spec_height : size;
    spec_aspect : aspect_opts;
    spec_switch : resize_switch;
    spec_x : int;
    spec_y : int;
  }

let compute spec orgw orgh =
  let w, h =
    match spec.spec_width, spec.spec_height, spec.spec_aspect  with
    | Scale s, Guess, asp when asp <> Dont_keep ->
      truncate (float orgw *. s), truncate (float orgh *. s)
    | Guess, Scale s, asp when asp <> Dont_keep  ->
      truncate (float orgw *. s), truncate (float orgh *. s)
    | Scale sw, Scale sh, _ (* asp is ignored *) ->
      truncate (float orgw *. sw), truncate (float orgh *. sh)
    | Pixel w, Guess, asp when asp <> Dont_keep ->
      let s = float w /. float orgw in w, truncate (float orgh *. s)
    | Guess, Pixel h, asp when asp <> Dont_keep ->
      let s = float h /. float orgh in truncate (float orgw *. s), h
    | Pixel w, Pixel h, _ (* asp is ignored *) -> w, h
    | _ -> raise (Invalid_argument "Geometry.compute") in

  let scalew = float w /. float orgw
  and scaleh = float h /. float orgh in

  let scalew', scaleh' =
    match spec.spec_aspect with
    | Keep_at_most ->
      if scalew < scaleh then scalew, scalew else scaleh, scaleh
    | Keep_at_least ->
      if scalew < scaleh then scaleh, scaleh else scalew, scalew
    | Dont_keep -> scalew, scaleh in

  let scalew'', scaleh'' =
    match spec.spec_switch with
    | Always -> scalew', scaleh'
    | Bigger_only when scalew' >= 1.0 && scaleh' >= 1.0 -> scalew', scaleh'
    | Smaller_only when scalew' <= 1.0 && scaleh' <= 1.0 -> scalew', scaleh'
    | _ -> 1.0, 1.0 in

  let w' = if scalew = scalew'' then w else truncate (float orgw *. scalew'')
  and h' = if scaleh = scaleh'' then h else truncate (float orgh *. scaleh'') in

  { geom_width = w';
    geom_height = h';
    geom_x = spec.spec_x;
    geom_y = spec.spec_y; }
