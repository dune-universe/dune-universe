(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lwt.Infix

module Backend = struct
  type t = (string, string) Hashtbl.t
  type client = unit
  let new_client _ctx = ()
end

module Server = Resp_server.Make(Resp_server.Auth.String)(Backend)

let get (srv: Backend.t) (cli: Backend.client) (_: string) (args: Hiredis.value array) =
  begin match args with
  | [| String key |] ->
    begin
      match Hashtbl.find_opt srv key with
      | Some v -> Hiredis.Value.string v
      | None -> Hiredis.Value.nil
    end
  | _ -> Hiredis.Value.error "Invalid arguments"
  end
  |> Lwt.return_some

let del (srv: Backend.t) (cli: Backend.client) (_: string) (args: Hiredis.value array) =
  begin match args with
  | [| String key |] ->
      Hashtbl.remove srv key;
      Hiredis.Value.status "OK"
  | _ -> Hiredis.Value.error "Invalid arguments"
  end
  |> Lwt.return_some

let set (srv: Backend.t) (cli: Backend.client) (_: string) (args: Hiredis.value array) =
  begin match args with
  | [| String key; String value |] ->
      Hashtbl.replace srv key value;
      Hiredis.Value.status "OK"
  | _ -> Hiredis.Value.error "Invalid arguments"
  end
  |> Lwt.return_some

let done_ (srv: Backend.t) (cli: Backend.client) (_: string) (args: Hiredis.value array) =
  print_endline "Test complete, closing server";
  exit 0

let commands = [
  "get", get;
  "set", set;
  "del", del;
  "done", done_;
]

let () =
  match Lwt_unix.fork () with
  | n when n < 0 -> print_endline "Fork error"; exit 1
  | 0 ->
      Unix.sleep 1;
      let cli = Hiredis.Client.connect ~port:1234 "127.0.0.1" in
      let run = Hiredis.Client.run cli in
      assert (run [| "SET"; "abc"; "123" |] = Hiredis.Value.status "OK");
      assert (run [| "GET"; "abc" |] = Hiredis.Value.string "123");
      ignore (run [| "DONE" |]);
      exit 0
  | n ->
      let ht = Hashtbl.create 16 in
      Lwt_main.run (
        Server.create ~commands (`TCP (`Port 1234)) ht >>= fun server ->
          Server.run server)

(*---------------------------------------------------------------------------
   Copyright (c) 2018 Zach Shipko

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
