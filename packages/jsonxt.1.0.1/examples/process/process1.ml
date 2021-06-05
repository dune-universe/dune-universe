open Jsonxt.Basic.Process

let extract_titles json =
  [json]
    |> filter_member "pages"
    |> flatten
    |> filter_member "title"
    |> filter_string

let () =
  let json = Jsonxt.Basic.of_file "test.json" in
  let titles = extract_titles json in
  let s = String.concat "\n" titles in
  print_endline s
