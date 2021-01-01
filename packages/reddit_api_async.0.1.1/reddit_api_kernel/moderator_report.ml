open! Core_kernel

type t =
  { moderator : Username.t option
  ; report : string
  }

let of_json json =
  let report, moderator =
    Json.get_pair
      Json.get_string
      Json_object.Utils.(string >> Username.of_string_or_deleted)
      json
  in
  { report; moderator }
;;
