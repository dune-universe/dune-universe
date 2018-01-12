open Base
open Soup

type metatags = (string, string list, String.comparator_witness) Map.t

type grouped_metatags = {
  twitter: metatags;
  opengraph: metatags;
  opengraph_ordered: (string * string) list;  
}

let name_attr n = attribute "name" n |> Option.value ~default:""

let content_attr n = attribute "content" n |> Option.value ~default:""

let pair_of_node_with_prefix prefix node = 
  let name = name_attr node in
  let content = content_attr node in
  if String.is_prefix name (prefix ^ ":") then Some((name, content)) else None

let group_by_name pairs =
  (** we know for sure pairs is not empty, so hd_exn is fine *)
  let (name, _ ) = List.hd_exn pairs in
  (name, List.map ~f:snd pairs)

let tags_of_group prefix tags = 
  tags |> Soup.to_list
  |> List.filter_map ~f:(pair_of_node_with_prefix prefix)
  |> List.sort ~cmp:(fun (n1, _) (n2, _) -> String.compare n1 n2)
  |> List.group ~break:(fun (n1, _) (n2, _) -> not (String.equal n1 n2))
  |> List.map ~f:(group_by_name)
  |> Map.of_alist_exn (module String)

let ordered_tags prefix tags =
  tags |> Soup.to_list
  |> List.filter_map ~f:(pair_of_node_with_prefix prefix)

let group tags = { twitter = tags_of_group "twitter" tags;
                   opengraph = tags_of_group "og" tags;
                   opengraph_ordered = ordered_tags "og" tags }

let last_of groups ~key = Map.find groups key 
                          |> Option.map ~f:List.last_exn

let or_empty maybe = Option.value maybe ~default:""