(*
    This file is part of easy_logging.

    easy_logging is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    easy_logging is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with easy_logging.  If not, see <https://www.gnu.org/licenses/>.
*)


module type Data_type =
sig
  type t
  val make : string -> t option -> t
  val root : t
end

module MakeTree(D: Data_type) =
struct

  module SMap = Map.Make ( struct type t = string
      let compare = String.compare end)

  type t = Node of D.t * t SMap.t

  type _t = {mutable data : t}

  let internal = {data= Node (D.root, SMap.empty )}

  let make_part_name a b =
    let open Printf in
    if a = "" then b else sprintf "%s.%s" a b

  let get s =
    let path = String.split_on_char '.' s in
    let rec get_aux path (current_node :t) current_name : (D.t * t) =
      match path, current_node with
      | h :: [], Node (l, children) ->
        if SMap.mem h children
        then
          let Node (logger,_) = SMap.find h children in
          (logger, Node (l, children))
        else
          let logger = D.make (make_part_name current_name h) (Some l) in
          let new_leaf = Node (logger, SMap.empty) in
          (logger, Node (l, SMap.add h new_leaf  children))

      | h :: t, Node (l, children) ->
        if SMap.mem h children
        then
          let logger, new_child =
            get_aux t (SMap.find h children) (make_part_name current_name h)
          in
          (logger, Node (l, SMap.add h new_child children))
        else
          let temp_logger = D.make (make_part_name current_name h) (Some l) in
          let new_leaf = Node (temp_logger, SMap.empty) in
          let logger, new_child =
            get_aux t new_leaf (make_part_name current_name h)
          in
          (logger, Node (l, SMap.add h new_child children))

      | [], Node (l, children) -> l, Node (l, children)
    in
    let res, new_item = get_aux path internal.data ""
    in internal.data <- new_item;
    res

end
