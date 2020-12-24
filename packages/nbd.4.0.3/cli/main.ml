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

open Lwt.Infix

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
          Lwt.return (`Nbd t)
        | None -> Lwt.fail_with "Cannot connect to nbd without a host"
      end
    | _ -> Lwt.fail_with "unknown scheme"

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
      | Result.Error `Disconnected -> Lwt.return_error `Disconnected
      | Result.Error `Unimplemented -> Lwt.return_error `Unimplemented
      | Result.Error _ -> Lwt.return_error `Disconnected
      | Result.Ok x -> Lwt.return_ok x

  let write t off bufs = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.write t off bufs
    | `Local t ->
      Block.write t off bufs
      >>= function
      | Result.Error `Disconnected -> Lwt.return_error `Disconnected
      | Result.Error `Unimplemented -> Lwt.return_error `Unimplemented
      | Result.Error `Is_read_only -> Lwt.return_error `Is_read_only
      | Result.Error _ -> Lwt.return_error `Disconnected
      | Result.Ok x -> Lwt.return_ok x

  let disconnect t = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.disconnect t
    | `Local t -> Block.disconnect t
end

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
  open Nbd

  let require name arg = match arg with
    | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
    | Some x -> x

  let require_str name arg =
    require name (if arg = "" then None else Some arg)

  let size host port export =
    let res =
      Nbd_lwt_unix.connect host port
      >>= fun client ->
      Client.negotiate client export in
    let (_,size,_) = Lwt_main.run res in
    Printf.printf "%Ld\n%!" size;
    `Ok ()

  let list _common host port =
    let t =
      Nbd_lwt_unix.connect host port
      >>= fun channel ->
      Client.list channel
      >>= function
      | Result.Ok disks ->
        List.iter print_endline disks;
        Lwt.return ()
      | Result.Error `Unsupported ->
        Printf.fprintf stderr "The server does not support the query function.\n%!";
        exit 1
      | Result.Error `Policy ->
        Printf.fprintf stderr "The server configuration does not permit listing exports.\n%!";
        exit 2 in
    `Ok (Lwt_main.run t)

  (* Helper function for use within this module *)
  let init_tls_get_server_ctx ~curve ~certfile ~ciphersuites no_tls =
    if no_tls then None
    else (
      let certfile = require_str "certfile" certfile in
      let ciphersuites = require_str "ciphersuites" ciphersuites in
      Some (Nbd_lwt_unix.TlsServer
              (Nbd_lwt_unix.init_tls_get_ctx ~curve ~certfile ~ciphersuites)
           )
    )

  let ignore_exn t () = Lwt.catch t (fun _ -> Lwt.return_unit)

  let serve _common filename port exportname certfile curve ciphersuites no_tls =
    let tls_role = init_tls_get_server_ctx ~curve ~certfile ~ciphersuites no_tls in
    let filename = require "filename" filename in
    let validate ~client_exportname =
      match exportname with
      | Some exportname when exportname <> client_exportname ->
        Lwt.fail_with
          (Printf.sprintf "Client requested invalid exportname %s, name of the export is %s" client_exportname exportname)
      | _ -> Lwt.return_unit
    in
    let handle_connection fd =
      Lwt.finalize
        (fun () ->
           Nbd_lwt_unix.with_channel fd tls_role
             (fun clearchan ->
                let offer = match exportname with
                  | None -> None
                  | Some exportname -> Some [exportname]
                in
                Server.with_connection
                  ?offer
                  clearchan
                  (fun client_exportname svr ->
                     validate ~client_exportname >>= fun () ->
                     Nbd_lwt_unix.with_block filename
                       (Server.serve svr ~read_only:false (module Block))
                  )
             )
        )
        (ignore_exn (fun () -> Lwt_unix.close fd))
    in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt.finalize
        (fun () ->
           Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
           let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
           Lwt_unix.bind sock sockaddr >>= fun () ->
           Lwt_unix.listen sock 5;
           let rec loop () =
             Lwt_unix.accept sock
             >>= fun (fd, _) ->
             (* Background thread per connection *)
             let _ =
               Lwt.catch
                 (fun () -> handle_connection fd)
                 (fun e -> Lwt_log.error_f "Caught exception %s while handling connection" (Printexc.to_string e))
             in
             loop ()
           in
           loop ()
        )
        (ignore_exn (fun () -> Lwt_unix.close sock))
    in
    Lwt_main.run t

  let mirror _common filename port secondary certfile curve ciphersuites no_tls =
    let tls_role = init_tls_get_server_ctx ~curve ~certfile ~ciphersuites no_tls in
    let filename = require "filename" filename in
    let secondary = require "secondary" secondary in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
      Lwt_unix.bind sock sockaddr >>= fun () ->
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
          let channel = Nbd_lwt_unix.cleartext_channel_of_fd fd tls_role in
          Server.connect channel ()
          >>= fun (_name, t) ->
          Server.serve t (module M) m in
        loop () in
      loop () in
    Lwt_main.run t
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

(* Used by both serve and mirror cmds *)
let certfile =
  let doc = "Path to file containing TLS certificate." in
  Arg.(value & opt string "" & info ["certfile"] ~doc)
let ciphersuites =
  let doc = "Set of ciphersuites for TLS (specified in the format accepted by OpenSSL, stunnel etc.)" in
  let parse_cipherstring_as_required = (fun s -> if s = "" then (failwith "ciphersuite is required") else `Ok s), Format.pp_print_string in
  Arg.(value & opt parse_cipherstring_as_required "" & info ["ciphersuites"] ~doc) (* cli is only used for debugging, so assume user is providing a good cipherstring *)
let curve =
  let doc = "EC curve to use" in
  Arg.(value & opt string "secp384r1" & info ["curve"] ~doc)

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
  let exportname =
    let doc = {|Export name to use when serving the file. If specified, clients
                will be able to list this export, and only this export name
                will be accepted. If unspecified, listing the exports will not
                be allowed, and all export names will be accepted when
                connecting.|}
    in
    Arg.(value & opt (some string) None & info ["exportname"] ~doc)
  in
  let no_tls =
    let doc = "Use NOTLS mode (refusing TLS) instead of the default FORCEDTLS." in
    Arg.(value & flag & info ["no-tls"] ~doc) in
  Term.(ret(pure Impl.serve $ common_options_t $ filename $ port $ exportname $ certfile $ curve $ ciphersuites $ no_tls)),
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
  let no_tls =
    let doc = "Serve using NOTLS mode (refusing TLS) instead of the default FORCEDTLS." in
    Arg.(value & flag & info ["no-tls"] ~doc) in
  Term.(ret(pure Impl.mirror $ common_options_t $ filename $ port $ secondary $ certfile $ curve $ ciphersuites $ no_tls)),
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

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
