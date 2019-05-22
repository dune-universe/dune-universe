open Batteries;;

open Pds_reachability_work;;
open Pds_reachability_work_collection;;

module Work_stack : Work_collection_template = functor(W : Work_type) ->
struct
  module W = W;;
  type work_collection = W.t list
    [@@deriving eq, ord, show]
  ;;
  let empty = [];;
  let offer work coll = work::coll;;
  let take coll =
    match coll with
    | [] -> ([], None)
    | h::t -> (t, Some h)
  ;;
  let is_empty coll = (List.is_empty coll);;
  let size = List.length;;
  let enum = List.enum;;
  let to_yojson coll = `List (List.map W.to_yojson coll);;
end;;
