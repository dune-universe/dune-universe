include Resp_server_intf

module Auth = struct
  module String = struct
    type t = string

    let check auth args = Array.length args > 0 && args.(0) = auth
  end

  module User = struct
    type t = (string, string) Hashtbl.t

    let check auth args =
      if Array.length args < 2 then false
      else
        match Hashtbl.find_opt auth args.(0) with
        | Some p -> p = args.(1)
        | None -> false
  end
end

module Make
    (Server : SERVER)
    (Auth : AUTH)
    (Value : Resp.S
               with type Reader.ic = Server.ic
                and type Writer.oc = Server.oc) =
struct
  include Server
  module Value = Value
  module Auth = Auth

  let ( >>= ) = Lwt.( >>= )

  type client = { data : Client.t; ic : ic; oc : oc }

  type command = data -> client -> string -> int -> unit Lwt.t

  type t = {
    server : server;
    data : data;
    auth : Auth.t option;
    commands : (string, command) Hashtbl.t;
    default : string;
  }

  let ok { oc; _ } = Value.write oc (Simple_string "OK")

  let error { oc; _ } msg = Value.write oc (Error (Printf.sprintf "ERR %s" msg))

  let invalid_arguments client = error client "Invalid arguments"

  let send { oc; _ } x = Value.write oc x

  let recv { ic; _ } = Value.read ic

  let hashtbl_of_list l =
    let ht = Hashtbl.create (List.length l) in
    List.iter (fun (k, v) -> Hashtbl.replace ht (String.lowercase_ascii k) v) l;
    ht

  let create ?auth ?(commands = []) ?(default = "default") server data =
    let commands = hashtbl_of_list commands in
    { server; data; auth; commands; default }

  let check_auth auth args =
    match auth with Some auth -> Auth.check auth args | None -> true

  let split_command_s seq : string * string array =
    match seq () with
    | Seq.Nil -> invalid_arg "split_command_s"
    | Seq.Cons (x, next) ->
        let name = Resp.to_string_exn x |> String.lowercase_ascii in
        (name, Array.of_seq (Seq.map Resp.to_string_exn next))

  let rec discard_n client n =
    if n > 0 then Value.read client.ic >>= fun _ -> discard_n client (n - 1)
    else Lwt.return ()

  let finish client ~nargs used = discard_n client (nargs - used)

  let rec handle t (client : client) authenticated =
    let argc = ref 0 in
    Lwt.catch
      (fun () ->
        if not authenticated then handle_not_authenticated t client
        else
          Value.Reader.read_lexeme client.ic >>= function
          | Ok (`As n) -> (
              argc := n - 1;
              Value.read client.ic >>= function
              | Simple_string s | Bulk (`String s) ->
                  let s = String.lowercase_ascii s in
                  let f =
                    try Hashtbl.find t.commands s
                    with Not_found -> Hashtbl.find t.commands t.default
                  in
                  f t.data client s !argc >>= fun () -> handle t client true
              | _ ->
                  discard_n client !argc >>= fun () ->
                  error client "invalid commands name" >>= fun () ->
                  handle t client true)
          | Error e ->
              error client (Resp.string_of_error e) >>= fun () ->
              handle t client true
          | _ ->
              error client "invalid command format" >>= fun () ->
              handle t client true)
      (function
        | Resp.Exc exc ->
            error client (Resp.string_of_error exc) >>= fun () ->
            handle t client true
        | Not_found ->
            discard_n client !argc >>= fun () ->
            error client "command not found" >>= fun () -> handle t client true
        | Failure msg | Invalid_argument msg -> error client msg
        | End_of_file -> Lwt.return ()
        | exc -> raise exc)

  and handle_not_authenticated t client =
    Value.read client.ic >>= function
    | Array arr -> (
        let cmd, args = split_command_s arr in
        match (cmd, args) with
        | "auth", args ->
            if check_auth t.auth args then
              ok client >>= fun () -> handle t client true
            else
              error client "authentication required" >>= fun () ->
              handle t client false
        | _, _ ->
            error client "authentication required" >>= fun () ->
            handle t client false)
    | _ ->
        error client "authentication required" >>= fun () ->
        handle t client false

  let start t =
    run t.server (fun (ic, oc) ->
        let data = Client.init t.data in
        let client = { ic; oc; data } in
        handle t client (t.auth = None))
end
