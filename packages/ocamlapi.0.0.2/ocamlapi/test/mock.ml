open Core
open Cohttp

module Io = struct
    
    type 'a t = Wrapper of 'a

    let run_synchronously = function Wrapper x -> x

    let return x = Wrapper x
end

module Body = struct

    type t = Wrapper of string | Empty

    let empty = Empty

    let of_string x = Wrapper x

    let to_string =
        function
        | Empty -> ""
        | Wrapper s -> s

    let equal b1 b2 =
        match b1, b2 with
        | Empty, Empty -> true
        | Wrapper x, Wrapper y -> String.equal x y
        | _ -> false

end

module Exceptions = struct
    exception Bad_request of string
end

let respond_success_string str =
    (Response.make ~status:(Code.status_of_code 200) (),
                 Body.of_string str)
                |> Io.return

module Router = Ocamlapi.Make (Cohttp.Request)
                              (Body)
                              (Cohttp.Response)
                              (Io)

module Custom_config = struct
    
    let default_exn_handler ?vars:_ exn =
        let status, body =
            match exn with
            | Exceptions.Bad_request _msg -> `Bad_request, Body.empty 
            | _ -> `Internal_server_error, Body.of_string "An unexpected error occurred" in
        Io.return (Response.make ~status (), body)

    let default_fallback ?vars:_ _req _body =
        (* TODO: add an example of using a variable. *)
        Io.return (Response.make ~status:`Not_found (), Body.empty)
end

module Router_with_custom_config = Ocamlapi.Make_with_config (Cohttp.Request)
                                                             (Body)
                                                             (Cohttp.Response)
                                                             (Io)
                                                             (Custom_config)
