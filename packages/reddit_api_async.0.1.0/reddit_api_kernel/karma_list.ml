open! Core_kernel

module Entry = struct
  include Json_object.Utils

  let of_json json =
    match json with
    | `O alist -> String.Map.of_alist_exn alist
    | _ -> raise_s [%message "Invalid [Karma_list.Entry] JSON" (json : Json.t)]
  ;;

  let to_json t = `O (Map.to_alist t)
  let subreddit = required_field "sr" subreddit_name
  let link_karma = required_field "link_karma" int
  let comment_karma = required_field "comment_karma" int
end

type t = Entry.t list [@@deriving sexp]

include Json_object.Make_kinded (struct
  type nonrec t = t

  let of_data_field json =
    match json with
    | `A entries -> List.map entries ~f:Entry.of_json
    | _ -> raise_s [%message "Invalid [Karma_list] JSON" (json : Json.t)]
  ;;

  let to_data_field t = `A (List.map t ~f:Entry.to_json)
  let kind = "KarmaList"
end)
