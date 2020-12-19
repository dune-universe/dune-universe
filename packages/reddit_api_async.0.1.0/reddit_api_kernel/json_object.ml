open! Core_kernel
include Json_object_intf

module Utils = struct
  type t = Json.t String.Map.t [@@deriving sexp, bin_io]

  let field_map = ident
  let of_json = Json.get_map
  let to_json t = `O (Map.to_alist t)
  let get_field = Map.find

  let get_field_exn t field =
    match Map.find t field with
    | Some value -> value
    | None -> raise_s [%message "Missing JSON field" (t : t) (field : string)]
  ;;

  let optional_field name convert t =
    match Map.find t name with
    | None | Some `Null -> None
    | Some v -> Some (convert v)
  ;;

  let required_field name convert t = convert (get_field_exn t name)
  let ( >> ) f g x = g (f x)

  let or_null f json =
    match json with
    | `Null -> None
    | json -> Some (f json)
  ;;

  let int = Json.get_int
  let float = Json.get_float
  let bool = Json.get_bool
  let string = Json.get_string
  let username = string >> Username.of_string
  let subreddit_name = string >> Subreddit_name.of_string
  let time = float >> Time_ns.Span.of_sec >> Time_ns.of_span_since_epoch
  let uri = string >> Uri.of_string
end

include Utils

module Make_kinded (Param : Kinded_param) = struct
  include Param

  let of_json json =
    match Option.try_with (fun () -> Ezjsonm.find json [ "kind" ]) with
    | None -> Param.of_data_field json
    | Some (`String kind) ->
      (match String.equal Param.kind kind with
      | true -> Param.of_data_field (Ezjsonm.find json [ "data" ])
      | false ->
        raise_s
          [%message
            "Unexpected JSON object kind"
              ~expected:(Param.kind : string)
              (json : Json.value)])
    | Some kind ->
      raise_s
        [%message
          "JSON object kind is not a string" (kind : Json.value) (json : Json.value)]
  ;;

  let to_json t = `O [ "kind", `String Param.kind; "data", Param.to_data_field t ]
end

module Make_kinded_simple (Param : sig
  val kind : string
end) =
Make_kinded (struct
  type t = Utils.t

  let kind = Param.kind
  let of_data_field = of_json
  let to_data_field = to_json
end)
