open! Core_kernel

type t = string

include Identifiable.Make (struct
  include String.Caseless

  let module_name = "Subreddit_name"
  let to_string = ident

  let of_string string =
    let try_prefix = List.find_map ~f:(fun prefix -> String.chop_prefix string ~prefix) in
    match try_prefix [ "u/"; "/u/" ] with
    | Some username -> "u_" ^ username
    | None -> try_prefix [ "r/"; "/r/" ] |> Option.value ~default:string
  ;;
end)

let user_subreddit username = of_string ("u_" ^ Username.to_string username)
let all = "all"
let combine = String.concat ~sep:"+"
