(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module Value = Hiredis_value

open Unix

exception Invalid_encoding

module type BACKEND = sig
  type t
  type client
  val new_client: t -> client
end

module type AUTH = sig
  type t
  val check: t -> string array -> bool
end

module type SERVER = sig
  module Auth: AUTH
  module Backend: BACKEND

  type t

  val ok: Value.t option Lwt.t
  val error: string -> Value.t option Lwt.t
  val invalid_arguments: unit -> Value.t option Lwt.t

  type command =
    Backend.t ->
    Backend.client ->
    string ->
    Value.t array ->
    Value.t option Lwt.t

  val create:
    ?auth: Auth.t ->
    ?default: command ->
    ?commands: (string * command) list ->
    ?host: string ->
    ?tls_config: Conduit_lwt_unix.tls_server_key ->
    Conduit_lwt_unix.server ->
    Backend.t ->
    t Lwt.t

  val start:
    ?backlog: int ->
    ?timeout: int ->
    ?stop: unit Lwt.t ->
    ?on_exn: (exn -> unit) ->
    t ->
    unit Lwt.t
end

module Auth = struct
  module String = struct
    type t = string
    let check auth args =
      Array.length args > 0 && args.(0) = auth
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

let rec read_value ic =
  let open Value in
  Lwt_io.read_char ic >>= function
  | ':' ->
      Lwt_io.read_line ic >|= fun i ->
      let i = Int64.of_string i in
      Value.Integer i
  | '-' -> Lwt_io.read_line ic >|= fun line -> Error line
  | '+' -> Lwt_io.read_line ic >|= fun line -> Status line
  | '*' ->
      Lwt_io.read_line ic >>= fun i ->
      let i = int_of_string i in
      let arr = Array.make i Value.Nil in
      if i < 0 then Lwt.return Nil
      else
        let rec aux n =
          match n with
          | 0 -> Lwt.return_unit
          | n ->
              read_value ic >>= fun x ->
              arr.(i - n) <- x;
              aux (n - 1)
        in
        aux i >|= fun () ->
        Array arr
  | '$' ->
      Lwt_io.read_line ic >>= fun i ->
      let i = int_of_string i in
      if i < 0 then Lwt.return Nil
      else
        Lwt_io.read ~count:i ic >>= fun s ->
        Lwt_io.read_char ic >>= fun _ ->
        Lwt_io.read_char ic >|= fun _ ->
        String s
  | _ -> raise Invalid_encoding

let rec write_value oc x =
  let open Value in
  match x with
  | Nil -> Lwt_io.write oc "*-1\r\n"
  | Error e ->
    Lwt_io.write oc "-" >>= fun () ->
    Lwt_io.write oc e >>= fun () ->
    Lwt_io.write oc "\r\n"
  | Integer i ->
    Lwt_io.write oc ":" >>= fun () ->
    Lwt_io.write oc (Printf.sprintf "%Ld\r\n" i)
  | String s ->
    Lwt_io.write oc (Printf.sprintf "$%d\r\n" (String.length s)) >>= fun () ->
    Lwt_io.write oc s >>= fun () ->
    Lwt_io.write oc "\r\n"
  | Array arr ->
    Lwt_io.write oc (Printf.sprintf "*%d\r\n" (Array.length arr)) >>= fun () ->
    let rec write_all arr i =
      if i >= Array.length arr then Lwt.return_unit
      else write_value oc arr.(i) >>= fun () -> write_all arr (i + 1)
    in write_all arr 0
  | Status s ->
    Lwt_io.write oc "+" >>= fun () ->
    Lwt_io.write oc s >>= fun () ->
    Lwt_io.write oc "\r\n"

module Client = struct
  open Conduit_lwt_unix
  type t = flow * ic * oc

  let connect ?(ctx = default_ctx) ?tls_config ?port s =
    let client = match port with
      | None -> `Unix_domain_socket (`File s)
      | Some port ->
          (match tls_config with
          | Some cfg -> `TLS cfg
          | None -> `TCP (`IP (Ipaddr.of_string_exn s), `Port port))
    in
    Conduit_lwt_unix.connect ~ctx client

  let read (_, ic, _) = read_value ic
  let write (_, _, oc) x = write_value oc x

  let run (_, ic, oc) cmd = write_value oc (Value.Array (Array.map Value.string cmd)) >>= fun () -> read_value ic
  let run_v (_, ic, oc) cmd = write_value oc (Value.Array cmd) >>= fun () -> read_value ic
end

module Make(A: AUTH)(B: BACKEND): SERVER with module Backend = B and module Auth = A  = struct
  module Auth = A
  module Backend = B

  type command = Backend.t -> Backend.client -> string -> Value.t array -> Value.t option Lwt.t

  type t = {
    s_ctx: Conduit_lwt_unix.ctx;
    s_mode: Conduit_lwt_unix.server;
    s_tls_config: Conduit_lwt_unix.tls_server_key option;
    s_auth: Auth.t option;
    s_cmd: (string, command) Hashtbl.t;
    s_data: Backend.t;
    s_default: command option;
  }

  let error msg =
    Lwt.return_some (Value.error ("ERR " ^ msg))

  let invalid_arguments () =
    Lwt.return_some (Value.error "ERR invalid arguments")

  let ok = Lwt.return_some (Value.status "OK")

  type client = {
    c_in: Lwt_io.input_channel;
    c_out: Lwt_io.output_channel;
    c_data: B.client;
  }

  let create ?auth ?default ?commands ?host:(host="127.0.0.1") ?tls_config mode data =
    Conduit_lwt_unix.init ~src:host ?tls_server_key:tls_config () >|= fun ctx ->
    let commands = match commands with
    | Some s ->
        let ht = Hashtbl.create (List.length s) in
        List.iter (fun (k, v) -> Hashtbl.replace ht k v) s;
        ht
    | None -> Hashtbl.create 8 in
    {
      s_ctx = ctx;
      s_mode = mode;
      s_tls_config = tls_config;
      s_auth = auth;
      s_cmd = commands;
      s_data = data;
      s_default = default;
    }

  let check auth args =
    match auth with
    | Some a -> Auth.check a args
    | None -> true

  let split_command arr =
      try
        let cmd = Value.to_string arr.(0)
          |> String.lowercase_ascii in
        let args = Array.sub arr 1 (Array.length arr - 1) in
        cmd, args
      with
        | Value.Invalid_value -> "", [||]

  let determine_command srv cmd =
    match Hashtbl.find_opt srv.s_cmd cmd with
    | Some f -> Some f
    | None ->
      begin
        match srv.s_default with
        | Some f -> Some f
        | None -> None
      end

  let rec aux srv authenticated client =
    Lwt.catch (fun () -> read_value client.c_in >>= Lwt.return_some)
              (function
                | Invalid_encoding | End_of_file -> Lwt.return_none
                | x -> raise x) >>= function
    | Some (Array a) when Array.length a > 0 ->
      let cmd, args = split_command a in
      if authenticated then
        when_authenticated srv client cmd args
      else
        when_not_authenticated srv client cmd args
    | _ ->
      write_value client.c_out (Error "NOCOMMAND Invalid Command")

  and when_authenticated srv client cmd args =
    match determine_command srv cmd with
    | Some f ->
      begin
        f srv.s_data client.c_data cmd args >>= fun r ->
        match r with
        | Some res ->
            write_value client.c_out res >>= fun () ->
            aux srv true client
        | None -> Lwt.return_unit
      end
    | None ->
      (if cmd = "command" then
        let commands = Hashtbl.fold (fun k v dst -> Value.string k :: dst) srv.s_cmd [] in
        write_value client.c_out (Value.array (Array.of_list commands))
      else
        write_value client.c_out (Error "NOCOMMAND Invalid command")) >>= fun _ ->
        aux srv true client

    and when_not_authenticated srv client cmd args =
      let args =
        try
          Array.map Value.to_string args
        with Value.Invalid_value -> [||] in
      if cmd = "auth" && check srv.s_auth args then
        write_value client.c_out (Status "OK") >>= fun () ->
        aux srv true client
      else
        write_value client.c_out (Error "NOAUTH Authentication Required") >>= fun _ ->
        aux srv false client

  let rec handle srv flow ic oc =
    let client = {
      c_in = ic;
      c_out = oc;
      c_data = B.new_client srv.s_data;
    } in
    Lwt.catch
      (fun () -> aux srv (srv.s_auth = None) client)
      (fun _ -> Lwt_unix.yield ())

  let start ?backlog ?timeout ?stop ?on_exn srv =
    Conduit_lwt_unix.serve ?backlog ?timeout ?stop ?on_exn
      ~ctx:srv.s_ctx ~mode:srv.s_mode (handle srv)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, BACKEND OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
