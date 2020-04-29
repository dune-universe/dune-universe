(* [String.escaped] but not escaping newlines *)
let escaped_literal_newlines s =
  let lines = String.split_on_char '\n' s in
  String.concat "\n" (List.map String.escaped lines)

let data_to_body data =
  data
  |> List.map (fun (key, value) -> (key, [ value ]))
  |> Uri.encoded_of_query |> Cohttp_lwt.Body.of_string

let json_to_body json = json |> Yojson.to_string |> Cohttp_lwt.Body.of_string
