open Cohttp
open Lwt.Infix
open Yojson

let headers api_key =
  Header.of_list [("X-Textrazor-Key", api_key); ("Accept-Enconding", "gzip")]

let parse_response (response, body) =
    let%lwt contents = Cohttp_lwt.Body.to_string body in
    let json = Safe.from_string contents in
    let result = if Response.status response |> Code.code_of_status |> Code.is_success
      then Ok (Safe.Util.member "response" json)
      else Error (Safe.Util.member "error" json |> Safe.Util.to_string)
    in Lwt.return result

let get url api_key =
  Lwt_main.run (
    Cohttp_lwt_unix.Client.get url ~headers:(headers api_key) >>=
    parse_response
  )

let post_form url ?(params = []) api_key =
  Lwt_main.run (
    Cohttp_lwt_unix.Client.post_form url ~headers:(headers api_key) ~params >>=
    parse_response
  )