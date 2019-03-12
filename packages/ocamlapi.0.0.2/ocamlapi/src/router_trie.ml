open Core
open S.Route_exceptions



module Make_with_config (Req: Cohttp.S.Request)
                        (B: S.Body)
                        (Resp: Cohttp.S.Response)
                        (IO: S.Io)
                        (Config : S.Router_config with
                                    type req := Req.t and
                                    type body := B.t and
                                    type resp := Resp.t and
                                    type 'a io := 'a IO.t) = struct    
    include Config
   
    type callback = ?vars: string String.Table.t -> Req.t -> B.t -> (Resp.t * B.t) IO.t
    type exn_handler = ?vars:string Core.String.Table.t -> exn -> (Resp.t * B.t) IO.t
    type route = string * (Cohttp.Code.meth * callback) list

    type t = { router : callback String.Table.t Path_trie.path_trie
             ; exn_handler : exn_handler
             ; fallback : callback }

    module type Routes = sig
        val routes : route list
    end

    let create ?exn_handler:(exn_handler = Config.default_exn_handler)
               ?fallback_response:(fallback_response = Config.default_fallback)
               (routes: route list) : (t, exn) Result.t  =
        let trie = Path_trie.empty () in
        let rec insert =
            function
            | [] -> Ok () 
            | (path, callbacks)::routes ->
                    begin
                        List.fold_until callbacks
                                        ~init:(String.Table.create ())
                                        ~f:(fun callback_map (meth, callback) ->
                                            let key = Cohttp.Code.string_of_method meth in
                                            match Hashtbl.add callback_map ~key:key ~data:callback with
                                            | `Ok -> Continue (callback_map)
                                            | `Duplicate -> Stop (Error meth))
                                        ~finish:(fun map -> Ok map)
                        |> function
                           | Ok callback_map -> begin
                                match Path_trie.insert_path trie path callback_map with
                                | `Ok -> insert routes
                                | `Duplicate -> Error (DuplicateRouteTemplate path)
                                end
                           | Error meth -> Error (DuplicateHttpMethod meth)
                    end in
        try
            insert routes
            |> Result.map
                ~f:(fun () ->
                    { router=trie
                    ; exn_handler=exn_handler
                    ; fallback=fallback_response }) 
        with err -> Error err 

    let create_exn ?exn_handler:(exn_handler = Config.default_exn_handler)
                   ?fallback_response:(fallback_response = Config.default_fallback)
                   (routes: route list) : t =
        match create ~exn_handler ~fallback_response routes with
        | Ok trie -> trie
        | Error err -> raise err

    let create_from_modules ?exn_handler:(exn_handler = Config.default_exn_handler)
                            ?fallback_response:(fallback_response = Config.default_fallback)
                            (modules : (module Routes) list) =
        List.map modules ~f:(fun (module R) -> R.routes)
        |> List.concat
        |> create ~exn_handler ~fallback_response

    let create_from_modules_exn ?exn_handler:(exn_handler = Config.default_exn_handler)
                                ?fallback_response:(fallback_response = Config.default_fallback)
                                (modules : (module Routes) list) =
        List.map modules ~f:(fun (module R) -> R.routes)
        |> List.concat
        |> create_exn ~exn_handler ~fallback_response

    let dispatch ({ router; exn_handler; fallback } : t) req body =
        Req.uri req
        |> Uri.path
        |> Path_trie.matches router
        |> Option.bind
            ~f:(fun (vars, callbacks) ->
                Req.meth req
                |> Cohttp.Code.string_of_method
                |> Hashtbl.find callbacks
                |> Option.map ~f:(fun callback -> vars, callback))
        |> Option.map
            ~f:(fun (vars, callback) ->
                try
                   callback ~vars:vars req body
                with err -> exn_handler ~vars:vars err)
        |> function
           | Some res -> res
           | None -> fallback req body
end

module Make (Req: Cohttp.S.Request)
            (B: S.Body)
            (Resp: Cohttp.S.Response)
            (IO: S.Io) = struct
    
    module Default_config = struct

        let default_exn_handler ?vars:_ _ = 
            IO.return (Resp.make ~status:`Internal_server_error (), B.empty)

        let default_fallback ?vars:_ _req _body =
            IO.return (Resp.make ~status:`Not_found (), B.empty)
    end

    include Make_with_config (Req) (B) (Resp) (IO) (Default_config)
end
