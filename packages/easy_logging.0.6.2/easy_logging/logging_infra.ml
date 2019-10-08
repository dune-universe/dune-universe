
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
