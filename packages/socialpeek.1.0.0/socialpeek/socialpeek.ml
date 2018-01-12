open Base
open Lwt
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Soup

module Metatags = Metatags
module Twitter = Twitter
module Opengraph = Opengraph

type error = Error_code of int 
           | Missing_location_header

let rec get_page_content uri = 
  Client.get uri >>= fun (resp, body) ->
  let code = Response.status resp |> Code.code_of_status in
  if Code.is_redirection code then
    let location = resp |> Response.headers |> Header.get_location in
    match location with
    | Some(uri) -> get_page_content uri
    | None -> return (Error Missing_location_header)
  else if Code.is_success code then
    Body.to_string body >>= fun body -> return (Ok body)
  else
    return (Error (Error_code code))

let get_html url = get_page_content (Uri.of_string url)
                   |> Lwt_main.run

let from_html html = html |> Soup.parse |> (tags "meta") |> Metatags.group

let from_url url = get_html url |> Result.map ~f:from_html