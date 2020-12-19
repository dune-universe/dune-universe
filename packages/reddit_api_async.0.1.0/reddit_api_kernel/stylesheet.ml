open! Core_kernel

module Image = struct
  include Json_object.Utils

  let of_json json =
    match json with
    | `O alist -> String.Map.of_alist_exn alist
    | _ -> raise_s [%message "Unexpected stylesheet image json" (json : Json.t)]
  ;;

  let url = required_field "url" uri
  let link = required_field "link" string
  let name = required_field "name" string
end

include Json_object.Utils

include Json_object.Make_kinded_simple (struct
  let kind = "stylesheet"
end)

let images = required_field "images" (Json.get_list Image.of_json)
let subreddit_id = required_field "subreddit_id" (string >> Thing.Subreddit.Id.of_string)
let stylesheet_text = required_field "stylesheet" string
