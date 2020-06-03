open Camlon
let test f g s =
  let o =
    match Ocaml.Parser.from_string s with
    | [o] -> o
    | _ -> assert false
  in
  match f o with
  | Ok o -> Format.eprintf "Ok: %a@." (Ocaml.format_with g) o
  | Error e -> Format.eprintf "Error: %a@." Ocaml_conv.format_error e
