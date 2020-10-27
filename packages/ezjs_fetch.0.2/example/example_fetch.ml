open Ezjs_min
open Ezjs_fetch_lwt

class type test = object
  method hash : js_string t prop
end

let () = Lwt.async @@ fun () ->
  Lwt.bind
    (fetch "https://api.dunscan.io/v4/head" to_js)
    (function
      | Error e->
        Lwt.return @@ Firebug.console##log e
      | Ok (r : test t response) ->
        Lwt.return @@ Firebug.console##log r.body##.hash)
