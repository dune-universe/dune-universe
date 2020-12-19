open! Core_kernel
module Page_id = String

module Pagination = struct
  type t =
    | Before of Page_id.t
    | After of Page_id.t
  [@@deriving sexp]
end

type 'a t =
  { children : 'a list
  ; after : Page_id.t option
  }
[@@deriving sexp, fields]

let map t ~f = { t with children = List.map t.children ~f }

let of_json convert_element json =
  let fail s = raise_s [%message s (json : Json.t)] in
  let assoc =
    match json with
    | `O list -> list
    | _ -> fail "Expected JSON map when creating [Listing.t]"
  in
  let data =
    match List.Assoc.find assoc "data" ~equal:String.equal with
    | Some data -> data
    | None -> fail "Missing field \"data\""
  in
  let data =
    match data with
    | `O list -> list
    | _ -> fail "Expected \"data\" to be an object"
  in
  let data =
    match String.Map.of_alist data with
    | `Ok map -> map
    | `Duplicate_key key -> fail (sprintf "Duplicate key: \"%s\"" key)
  in
  let children =
    match Map.find data "children" with
    | Some (`A l) -> l
    | Some _ -> fail "Expected \"children\" to be a list"
    | None -> fail "Missing key \"children\""
  in
  let children = List.map children ~f:convert_element in
  let after =
    match Map.find data "after" with
    | None | Some `Null -> None
    | Some (`String s) -> Some (Page_id.of_string s)
    | Some _ -> fail "Expected \"after\" to be a string"
  in
  { children; after }
;;
