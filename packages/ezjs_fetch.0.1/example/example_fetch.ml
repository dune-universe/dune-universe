open Ezjs_fetch.Fetch
open Js_of_ocaml

class type test = object
  method hash : Js.js_string Js.t Js.prop
end

let () = Lwt.async @@ fun () ->
  Lwt.bind
    (fetch "https://api.dunscan.io/v4/head" to_js)
    (function
      | Error s ->
        Lwt.return @@ Firebug.console##log_2 (Js.string "Error") (Js.string s)
      | Ok (r : test Js.t response) ->
        Lwt.return @@ Firebug.console##log r.body##.hash)
