type t = {
  secure: bool;
  use_eu_endpoint: bool
}

let create ?(secure=true) ?(use_eu_endpoint=false) () =
  {secure; use_eu_endpoint}

let scheme t =
  if t.secure then "https" else "http"

let host t =
  if t.use_eu_endpoint then "api-eu.textrazor.com" else "api.textrazor.com"

let url t =
  Uri.make ~scheme:(scheme t) ~host:(host t)
    ?userinfo:None ?port:None ?fragment:None