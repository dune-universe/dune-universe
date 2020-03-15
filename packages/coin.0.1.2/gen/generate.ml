let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let with_ic path f a =
  try
    let ic = open_in path in
    let rs = f ic a in
    close_in ic ; Ok rs
  with _ -> error_msgf "Invalid filename: %s" path

let with_oc path f a =
  try
    let oc = open_out path in
    let rs = f oc a in
    close_out oc ; rs
  with _ -> error_msgf "Invalid filename: %s" path

let of_file path =
  with_ic path
    (fun ic (parser, lexer) ->
       let lexbuf = Lexing.from_channel ic in
       parser lexer lexbuf)
    (Format_a_parser.file, Format_a_lexer.token)

module Map = Koi8.Map

let pp_list ~sep:pp_sep pp_data ppf lst =
  let rec go = function
    | [] -> ()
    | [ x ] -> Format.fprintf ppf "%a" pp_data x
    | x :: r -> Format.fprintf ppf "%a%a@ " pp_data x pp_sep () ; go r in
  go lst

let pp_array ppf lst =
  let sep ppf () = Format.pp_print_string ppf ";" in
  Format.fprintf ppf "[|@[<hov>@ %a@ @]|]" (pp_list ~sep Format.pp_print_int) lst

let produce oc database =
  let ppf = Format.formatter_of_out_channel oc in
  let res = Array.init 256 (fun idx -> match Map.find idx database with
      | (cp, _) -> cp
      | exception Not_found -> (-1)) |> Array.to_list in
  Format.fprintf ppf "let map = %a\n%!" pp_array res; Ok ()

let ( >>= ) x f = match x with
  | Ok x -> f x
  | Error err -> Error err

let parse source destination =
  of_file source >>= fun src ->
  Koi8.extract src >>= fun maps ->
  with_oc destination produce maps

let exit_success = 0
let exit_failure = 1

let report = function
  | Ok () -> exit exit_success
  | Error (`Msg err) -> Format.eprintf "%s: %s.\n%!" Sys.argv.(0) err

let () = match Sys.argv with
  | [| _; source; destination; |] ->
    if Sys.file_exists source
    then report (parse source destination)
    else ( Format.eprintf "%s source destination\n%!" Sys.argv.(0) ; exit exit_failure )
  | _ ->
    Format.eprintf "%s source destination\n%!" Sys.argv.(0) ; exit exit_failure
