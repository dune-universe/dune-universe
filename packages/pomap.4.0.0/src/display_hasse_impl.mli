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

open Display_hasse_intf
open Pomap_intf

(** Default specification than can be used by the pretty-printer for
    Hasse-diagrams. Just [include] it into some module and override the
    defaults as required. *)
module DefaultSpec :
sig
  include DEFAULT_SPEC
  val pp_node_attr : Format.formatter -> 'a -> unit
end

(** Functor that generates a pretty-printer for Hasse-diagrams from
    a partially ordered map and a pretty-printer specification. See the
    {!Display_hasse_intf.DISPLAY_HASSE}-interface for documentation. *)
module Make
  (POMap : POMAP)
  (Spec : SPEC with type (+'a) node = 'a POMap.node)
  : DISPLAY_HASSE with type pomap = Spec.el POMap.pomap
