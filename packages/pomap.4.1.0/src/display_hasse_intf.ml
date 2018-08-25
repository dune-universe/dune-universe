(*
   HASSE - Library for generating Hasse-diagrams with the dot-utility

   Copyright (C) 2001-2002  Markus Mottl  (OEFAI)
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place --- Suite 330, Boston, MA 02111-1307, USA.
*)

(** Default specification for drawing with the DOT-utility. *)
module type DEFAULT_SPEC = sig
  (** DOT-options (see "man dot") *)

  val name : string
  val label : string
  val size_x : float
  val size_y : float
  val ratio : float
  val rotation : float
  val center : bool

  val top_attr : string
  (** Node attribute string for top nodes, e.g. "shape = box" *)

  val bot_attr : string
  (** Node attribute string for bottom nodes *)

  val top_bot_attr : string
  (** Node attribute string for top/bottom nodes *)

  val edge_attr : string
  (** Edge attribute string, e.g. "color = blue" *)
end

(** Specification for drawing Hasse-diagrams. *)
module type SPEC = sig
  include DEFAULT_SPEC

  type el
  type (+'a) node

  val pp_node_attr : Format.formatter -> el node -> unit
  (** [pp_node_attr ppf node] prints attributes of [node] to the
      pretty-printer [ppf]. *)
end

(** Interface for drawing Hasse-diagrams. *)
module type DISPLAY_HASSE = sig
  type pomap

  val fprintf : Format.formatter -> pomap -> unit
    (** [fprintf ppf pm] prints partially ordered map [pm] to the
        pretty-printer [ppf]. *)

  val printf : pomap -> unit
    (** [printf ppf pm] prints partially ordered map [pm] to [stdout]. *)
end
