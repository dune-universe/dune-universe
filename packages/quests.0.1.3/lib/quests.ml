open Lwt

let rec request meth ?params ?data ?headers ?auth ?(follow_redirects = true) url
    =
  let { Request_utils.meth; body; request_headers; uri } =
    Request_utils.make_request_data meth ?data ?params ?headers ?auth url
  in
  Cohttp_lwt_unix.Client.call meth ?body ~headers:request_headers uri
  >>= fun (response, body) ->
  Response_utils.process (response, body) request meth ~uri ?params ?data
    ?headers ?auth ~request_headers ~follow_redirects

let get ?params ?data ?headers ?auth = request `GET ?params ?data ?headers ?auth

let post ?params ?data ?headers ?auth =
  request `POST ?params ?data ?headers ?auth

let put ?params ?data ?headers ?auth = request `PUT ?params ?data ?headers ?auth

let delete ?params ?data ?headers ?auth =
  request `DELETE ?params ?data ?headers ?auth

module Request = Request
module Response = Response
module Session = Session

module Header = Cohttp.Header
