open Hiredis_value

type status = C.status =
    | OK
    | ERR of string option

module Reader = struct
    type t = {
        r_handle : C.reader;
        mutable freed : bool;
    }

    let release r =
        if not r.freed then
            let () = C.redis_reader_free r.r_handle in
            r.freed <- true

    let create () =
        let r = {r_handle = C.redis_reader_create (); freed = false} in
        Gc.finalise release r; r

    let feed r s = C.redis_reader_feed r.r_handle s
    let get_reply r = C.redis_reader_get_reply r.r_handle
end

let rec encode_string = function
    | Nil -> "*-1\r\n"
    | Error e ->
        if String.contains e '\n'
        then raise Invalid_value
        else Printf.sprintf "-%s\r\n" e
    | Integer i ->
        Printf.sprintf ":%Ld\r\n" i
    | String s ->
        Printf.sprintf "$%d\r\n%s\r\n" (String.length s) s
    | Array arr ->
        let l = Array.map encode_string arr
            |> Array.to_list
            |> String.concat "" in
        Printf.sprintf "*%d\r\n%s" (Array.length arr) l
    | Status s ->
        if String.contains s '\n'
        then raise Invalid_value
        else Printf.sprintf "+%s\r\n" s

let decode_string s =
    let r = Reader.create () in
    match Reader.feed r s with
    | OK -> Reader.get_reply r
    | ERR s -> failwith (match s with
        | Some s -> s
        | None -> "invalid encoding")

module Client = struct
    type t = {
        c_handle : C.context;
        mutable c_freed : bool;
        c_scripts : (string, string) Hashtbl.t;
    }

    let error_string ctx =
        C.redis_context_errstr ctx.c_handle

    let close ?close_fd:(close_fd=true) ctx =
        let res = if ctx.c_freed then ()
        else if close_fd then C.redis_context_free ctx.c_handle
        else C.redis_context_free_keep_fd ctx.c_handle
        in ctx.c_freed <- true; res

    let to_fd ctx =
        C.redis_context_to_fd ctx.c_handle

    let of_fd ?scripts:(scripts=Hashtbl.create 16) ?close_fd:(close_fd=true) fd =
        let ctx = {
            c_handle = C.redis_context_of_fd fd;
            c_freed = false;
            c_scripts = scripts;
        } in
        Gc.finalise (fun x ->
            close ~close_fd x) ctx; ctx

    let reconnect ctx =
        C.redis_context_reconnect ctx.c_handle

    let set_timeout ctx s us =
        C.redis_context_set_timeout ctx.c_handle s us

    let enable_keepalive ctx =
        C.redis_context_enable_keepalive ctx.c_handle

    let append_command ctx arr =
        C.redis_context_append_command ctx.c_handle arr

    let append_command_v ctx arr =
        C.redis_context_append_command ctx.c_handle (Array.map to_string arr)

    let append_formatted ctx s =
        C.redis_context_append_formatted ctx.c_handle s

    let append_value ctx v =
        append_formatted ctx (encode_string v)

    let flush_buffer ctx =
        C.redis_context_flush_buffer ctx.c_handle

    let read_buffer ctx =
        C.redis_context_read_buffer ctx.c_handle

    let get_reply ctx =
        C.redis_context_get_reply ctx.c_handle

    let run ctx arr =
        C.redis_context_command ctx.c_handle arr

    let run_v ctx arr =
        run ctx (Array.map to_string arr)

    let connect ?scripts:(scripts=Hashtbl.create 16) ?auth ?nonblock:(nonblock=false) ?port host =
        let ctx = {
                c_handle = begin match port with
                    | Some port' -> C.redis_context_connect host port' nonblock
                    | None -> C.redis_context_connect_unix host nonblock
                    end;
                c_freed = false;
                c_scripts = scripts
            }
        in Gc.finalise (fun x ->
            close x) ctx;
        match auth with
        | Some s -> ignore (run ctx [| "AUTH"; s |]); ctx
        | None -> ctx

    let load_script ctx name s =
        let hash = to_string (run ctx [| "SCRIPT"; "LOAD"; s |]) in
        Hashtbl.replace ctx.c_scripts name hash

    let call_script ctx name numkeys args =
        let hash = Hashtbl.find ctx.c_scripts name in
        Array.of_list @@
        (["EVALSHA"; hash; string_of_int numkeys ] @ args)
        |> run ctx

    let call_script_v ctx name numkeys args =
        let hash = Hashtbl.find ctx.c_scripts name in
        Array.of_list @@
        ([String "EVALSHA"; String hash; String (string_of_int numkeys) ] @ args)
        |> run_v ctx
end

module Pool = struct
    type t = Client.t Lwt_pool.t

    let create ?port host n =
        Lwt_pool.create n (fun () ->
            Lwt.return (Client.connect ?port host))

    let use pool fn =
        Lwt_pool.use pool fn
end
