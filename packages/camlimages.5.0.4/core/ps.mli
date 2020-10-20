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

(* $Id: ps.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

type bounding_box = (int * int * int * int) option
  (** The bounding box of a postscript image. *)

val load : string -> Images.load_option list -> Images.t
  (** Loads a postscript image. *)

val save : string -> Images.save_option list -> Images.t -> unit
  (** Save a full-color image in the Encapuslated PS format file.
     Raises [Invalid_argument] if the image is not a full-color image. *)

val load_ps : string -> bounding_box -> Images.load_option list -> Images.t
  (** Loads a postscript image within a bounding box. *)

val get_bounding_box : string -> bounding_box
  (** Return the bounding box option of the postscript image inside the file
      argument. *)
