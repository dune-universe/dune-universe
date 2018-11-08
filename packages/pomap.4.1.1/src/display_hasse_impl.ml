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
open Format

module DefaultSpec = struct
  let name = "poset"
  let label = "Partially Ordered Set"
  let size_x = 8.27
  let size_y = 11.69
  let ratio = 0.9
  let rotation = 90.0
  let center = true

  let pp_node_attr ppf _ = pp_print_string ppf "label = \"\""

  let make_attr color = sprintf "shape = box, color = %s" color
  let top_attr = make_attr "blue"
  let bot_attr = make_attr "red"
  let top_bot_attr = make_attr "green"

  let edge_attr = "dir = back"
end

module Make (POMap : POMAP) (Spec : SPEC with type (+'a) node = 'a POMap.node) =
struct
  open POMap
  open Store
  open Spec

  type pomap = el POMap.pomap

  let pp_node ppf ix node =
    fprintf ppf "@\nnode%d [ %a ]" (Ix.int_of_ix ix) Spec.pp_node_attr node

  let pp_node_edge ppf src dst_ix =
    fprintf ppf "@\nnode%d -> node%d" src (Ix.int_of_ix dst_ix)

  let pp_ix_edges ppf src dsts = Ix.Set.iter (pp_node_edge ppf src) dsts
  let pp_node_attr ppf n attr = fprintf ppf "@\nnode%d [ %s ]" n attr

  let pp_node_edges ppf ix node =
    let n = Ix.int_of_ix ix and prds = get_prds node in
    match Ix.Set.is_empty prds, Ix.Set.is_empty (get_sucs node) with
    | true, true -> pp_node_attr ppf n Spec.top_bot_attr
    | true, false -> pp_node_attr ppf n Spec.bot_attr
    | false, true -> pp_node_attr ppf n Spec.top_attr; pp_ix_edges ppf n prds
    | false, false -> pp_ix_edges ppf n prds

  let pp_nodes ppf pm = POMap.iteri (pp_node ppf) pm
  let pp_edges ppf pm = POMap.iteri (pp_node_edges ppf) pm

  let fprintf ppf pm =
    fprintf ppf
      "\
        @[<2>digraph %s {@\n\
          label = \"%s\"@\n\
          size = \"%f,%f\"@\n\
          ratio = %f@\n\
          rotate = %f@\n\
          center = %c\n\
          @\n\
          edge [ %s ]\n\
          %a\n\
          %a@]@\n\
        }@."
      name
      label
      size_x
      size_y
      ratio
      rotation
      (if center then '1' else '0')
      edge_attr
      pp_nodes pm
      pp_edges pm

  let printf pm = fprintf std_formatter pm
end
