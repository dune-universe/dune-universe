external get : unit -> (int * int) option = "ocaml_terminal_size_get"

let get_rows () =
  match get () with
  | Some (rows, _) -> Some rows
  | None -> None

let get_columns () =
  match get () with
  | Some (_, columns) -> Some columns
  | None -> None
