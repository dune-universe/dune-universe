(*
 * Copyright (C) 2011-2015 Citrix Inc
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let project_url = "http://github.com/xapi-project/nbd"

open Lwt

module Device = struct
  type id = [
    | `Nbd of Uri.t
    | `Local of string
  ]
  type t = [
    | `Nbd of Nbd_lwt_unix.Client.t
    | `Local of Block.t
  ]
  type 'a io = 'a Lwt.t
  type page_aligned_buffer = Cstruct.t
  type error = [
    Mirage_block.error
    | `Protocol_error of Nbd.Protocol.Error.t
  ]
  type write_error = [
    Mirage_block.write_error
    | `Protocol_error of Nbd.Protocol.Error.t
  ]
  let pp_error ppf = function
    | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
    | `Protocol_error e -> Fmt.string ppf (Nbd.Protocol.Error.to_string e)

  let pp_write_error ppf = function
    | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
    | `Protocol_error e -> Fmt.string ppf (Nbd.Protocol.Error.to_string e)

  let connect uri = match Uri.scheme uri with
    | Some "file" ->
      let path = Uri.path uri in
      ( Block.connect path >|= fun x ->
        `Local x )
    | Some "nbd" ->
      begin match Uri.host uri with
        | Some host ->
          let port = match Uri.port uri with None -> 10809 | Some x -> x in
          Nbd_lwt_unix.connect host port
          >>= fun channel ->
          Nbd_lwt_unix.Client.negotiate channel (Uri.to_string uri)
          >>= fun (t, _, _) ->
          return (`Nbd t)
        | None -> fail_with "Cannot connect to nbd without a host"
      end
    | _ -> fail_with "unknown scheme"

  type info = {
    read_write: bool;
    sector_size: int;
    size_sectors: int64;
  }

  let get_info = function
    | `Nbd t ->
      Nbd_lwt_unix.Client.get_info t
    | `Local t ->
      Block.get_info t

  let read t off bufs = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.read t off bufs
    | `Local t ->
      Block.read t off bufs
      >>= function
      | Result.Error `Disconnected -> Lwt.return (Result.Error `Disconnected)
      | Result.Error `Unimplemented -> Lwt.return (Result.Error `Unimplemented)
      | Result.Ok x -> Lwt.return (Result.Ok x)

  let write t off bufs = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.write t off bufs
    | `Local t ->
      Block.write t off bufs
      >>= function
      | Result.Error `Disconnected -> Lwt.return (Result.Error `Disconnected)
      | Result.Error `Unimplemented -> Lwt.return (Result.Error `Unimplemented)
      | Result.Ok x -> Lwt.return (Result.Ok x)

  let disconnect t = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.disconnect t
    | `Local t -> Block.disconnect t
end

open Common
open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [
  `S _common_options;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
  `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  Term.(pure Common.make $ debug $ verb)

module Impl = struct
  open Lwt
  open Nbd

  let require name arg = match arg with
    | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
    | Some x -> x

  let size host port export =
    let res =
      Nbd_lwt_unix.connect host port
      >>= fun client ->
      Client.negotiate client export in
    let (_,size,_) = Lwt_main.run res in
    Printf.printf "%Ld\n%!" size;
    `Ok ()

  let list common host port =
    let t =
      Nbd_lwt_unix.connect host port
      >>= fun channel ->
      Client.list channel
      >>= function
      | Result.Ok disks ->
        List.iter print_endline disks;
        return ()
      | Result.Error `Unsupported ->
        Printf.fprintf stderr "The server does not support the query function.\n%!";
        exit 1
      | Result.Error `Policy ->
        Printf.fprintf stderr "The server configuration does not permit listing exports.\n%!";
        exit 2 in
    `Ok (Lwt_main.run t)

  let serve common filename port =
    let filename = require "filename" filename in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
      Lwt_unix.bind sock sockaddr;
      Lwt_unix.listen sock 5;
      let rec loop () =
        Lwt_unix.accept sock
        >>= fun (fd, _) ->
        (* Background thread per connection *)
        let _ =
          let channel = Nbd_lwt_unix.of_fd fd in
          Server.connect channel ()
          >>= fun (name, t) ->
          Block.connect filename
          >>= fun b ->
          Server.serve t (module Block) b in
        loop () in
      loop () in
    Lwt_main.run t;
    `Ok ()

let mirror common filename port secondary =
  let filename = require "filename" filename in
  let secondary = require "secondary" secondary in
  let t =
    let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
    Lwt_unix.bind sock sockaddr;
    Lwt_unix.listen sock 5;
    let module M = Mirror.Make(Device)(Device) in
    ( Device.connect (Uri.of_string filename)
      >>= fun primary ->
      (* Connect to the secondary *)
      Device.connect (Uri.of_string secondary)
      >>= fun secondary ->
      let progress_cb = function
        | `Complete ->
          Printf.fprintf stderr "Mirror synchronised\n%!"
        | `Percent x ->
          Printf.fprintf stderr "Mirror %d %% complete\n%!" x in
      M.connect ~progress_cb primary secondary
    ) >>= fun m ->
    let rec loop () =
      Lwt_unix.accept sock
      >>= fun (fd, _) ->
      (* Background thread per connection *)
      let _ =
        let channel = Nbd_lwt_unix.of_fd fd in
        Server.connect channel ()
        >>= fun (name, t) ->
        Server.serve t (module M) m in
      loop () in
    loop () in
    Lwt_main.run t;
    `Ok ()
end

let size_cmd =
  let doc = "Return the size of a disk served over NBD" in
  let host =
    let doc = "Hostname of NBD server" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"hostname") in
  let port =
    let doc = "Remote port" in
    Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"port") in
  let export =
    let doc = "Name of the export" in
    Arg.(value & opt string "export" & info [ "export" ] ~doc ~docv:"export") in
  Term.(ret (pure Impl.size $ host $ port $ export)),
  Term.info "size" ~version:"1.0.0" ~doc

let serve_cmd =
  let doc = "serve a disk over NBD" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a server which allows a client to access a disk using NBD.";
  ] @ help in
  let filename =
    let doc = "Disk (file or block device) to expose" in
    Arg.(value & pos 0 (some file) None & info [] ~doc) in
  let port =
    let doc = "Local port to listen for connections on" in
    Arg.(value & opt int 10809 & info [ "port" ] ~doc) in
  Term.(ret(pure Impl.serve $ common_options_t $ filename $ port)),
  Term.info "serve" ~sdocs:_common_options ~doc ~man

let mirror_cmd =
  let doc = "serve a disk over NBD while mirroring" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a server which allows a client to access a disk using NBD.";
    `P "The server will pass I/O through to a primary disk underneath, while also mirroring the contents to a secondary.";
    `S "EXAMPLES";
  ] @ help in
  let filename =
    let doc = "URI naming the primary disk" in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  let secondary =
    let doc = "URI naming the secondary disk" in
    Arg.(value & pos 1 (some string) None & info [] ~doc) in
  let port =
    let doc = "Local port to listen for connections on" in
    Arg.(value & opt int 10809 & info [ "port" ] ~doc) in
  Term.(ret(pure Impl.mirror $ common_options_t $ filename $ port $ secondary)),
  Term.info "mirror" ~sdocs:_common_options ~doc ~man

let list_cmd =
  let doc = "list the disks exported by an NBD server" in
  let man = [
    `S "DESCRIPTION";
    `P "Queries a server and returns a list of known exports. Note older servers may not support the protocol option: this will result in an empty list.";
  ] @ help in
  let host =
    let doc = "Hostname of NBD server" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"hostname") in
  let port =
    let doc = "Remote port" in
    Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"port") in
  Term.(ret(pure Impl.list $ common_options_t $ host $ port)),
  Term.info "list" ~sdocs:_common_options ~doc ~man

let default_cmd =
  let doc = "manipulate NBD clients and servers" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "nbd-tool" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man

let cmds = [serve_cmd; list_cmd; size_cmd; mirror_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
