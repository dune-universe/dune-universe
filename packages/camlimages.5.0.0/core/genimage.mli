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

(* $Id: genimage.mli,v 1.6 2009/03/01 11:24:10 furuse Exp $ *)

open Image_intf

(** Low level image creator *)
module MakeRawImage : functor (E : ENCODE) -> (RAWIMAGE with type elt = E.t)

module Make : functor (RI : RAWIMAGE) ->
              functor (CON : CONTAINER with type rawimage = RI.t) ->
  IMAGE with type t = CON.container
	and  type elt = RI.elt

module MakeIndexed(RI:RAWIMAGE with type elt = int)
    (CON:CONTAINER_INDEXED with type rawimage = RI.t) :
  IMAGEINDEXED with type t = CON.container
               and  type elt = int
	       and  type mapelt = CON.mapelt

