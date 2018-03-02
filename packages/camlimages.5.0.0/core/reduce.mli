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
module type REDUCER = sig 
  val find_nearest : Color.rgb Color.map -> Color.rgb -> int 
end

module ErrorDiffuse : functor(R : REDUCER) -> sig 
  val f : Rgb24.t -> Color.rgb Color.map -> Index8.t 
end

val error_diffuse : Rgb24.t -> Color.rgb Color.map -> Index8.t
