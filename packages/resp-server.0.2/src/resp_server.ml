(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix
open Hiredis

open Unix

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

  val ok: Hiredis.value option Lwt.t
  val error: string -> Hiredis.value option Lwt.t
  val invalid_arguments: unit -> Hiredis.value option Lwt.t

  type command =
    Backend.t ->
    Backend.client ->
    string ->
    Hiredis.value array ->
    Hiredis.value option Lwt.t

  val create:
    ?auth: Auth.t ->
    ?default: command ->
    ?commands: (string * command) list ->
    ?host: string ->
    ?tls_config: Conduit_lwt_unix.tls_server_key ->
    Conduit_lwt_unix.server ->
    Backend.t ->
    t Lwt.t

  val run:
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

module Make(A: AUTH)(B: BACKEND): SERVER with module Backend = B and module Auth = A  = struct
  module Auth = A
  module Backend = B

  type command = Backend.t -> Backend.client -> string -> Hiredis.value array -> Hiredis.value option Lwt.t

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
    c_buf: bytes;
    c_reader: Hiredis.Reader.t;
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

  let buffer_size = 2048

  let rec write oc = function
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
        else write oc arr.(i) >>= fun () -> write_all arr (i + 1)
      in write_all arr 0
    | Status s ->
      Lwt_io.write oc "+" >>= fun () ->
      Lwt_io.write oc s >>= fun () ->
      Lwt_io.write oc "\r\n"

  let check auth args =
    match auth with
    | Some a -> Auth.check a args
    | None -> true

  let split_command arr =
      try
        let cmd = Hiredis.Value.to_string arr.(0)
          |> String.lowercase_ascii in
        let args = Array.sub arr 1 (Array.length arr - 1) in
        cmd, args
      with
        | Hiredis.Value.Invalid_value -> "", [||]

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
    Lwt_io.read_into client.c_in client.c_buf 0 buffer_size >>= fun n ->
    if n <= 0 then
      Lwt.return_unit
    else
      let s = (Bytes.sub client.c_buf 0 n |> Bytes.to_string) in
      let () = ignore (Reader.feed client.c_reader s) in
      match Reader.get_reply client.c_reader with
      | None ->
        aux srv authenticated client
      | Some (Array a) when Array.length a > 0 ->
        let cmd, args = split_command a in
        if authenticated then
          when_authenticated srv client cmd args
        else
          when_not_authenticated srv client cmd args
    | _ ->
      write client.c_out (Error "NOCOMMAND Invalid Command")

  and when_authenticated srv client cmd args =
    match determine_command srv cmd with
    | Some f ->
      begin
        f srv.s_data client.c_data cmd args >>= fun r ->
        match r with
        | Some res ->
            write client.c_out res >>= fun () ->
            aux srv true client
        | None -> Lwt.return_unit
      end
    | None ->
      (if cmd = "command" then
        let commands = Hashtbl.fold (fun k v dst -> Hiredis.Value.string k :: dst) srv.s_cmd [] in
        write client.c_out (Hiredis.Value.array (Array.of_list commands))
      else
        write client.c_out (Error "NOCOMMAND Invalid command")) >>= fun _ ->
        aux srv true client

    and when_not_authenticated srv client cmd args =
      let args =
        try
          Array.map Hiredis.Value.to_string args
        with Hiredis.Value.Invalid_value -> [||] in
      if cmd = "auth" && check srv.s_auth args then
        write client.c_out (Status "OK") >>= fun () ->
        aux srv true client
      else
        write client.c_out (Error "NOAUTH Authentication Required") >>= fun _ ->
        aux srv false client

  let rec handle srv flow ic oc =
    let r = Reader.create () in
    let buf = Bytes.make buffer_size ' ' in
    let client = {
      c_in = ic;
      c_out = oc;
      c_buf = buf;
      c_reader = r;
      c_data = B.new_client srv.s_data;
    } in
    Lwt.catch
      (fun () -> aux srv (srv.s_auth = None) client)
      (fun _ -> Lwt_unix.yield ())

  let run ?backlog ?timeout ?stop ?on_exn srv =
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
