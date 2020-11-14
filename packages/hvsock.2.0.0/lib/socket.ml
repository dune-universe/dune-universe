(*
 * Copyright (C) 2018 Docker Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type t = Unix.file_descr

type platform =
  | Windows
  | Linux
  | Mac
  | Unsupported of string

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let startswith prefix line =
  let prefix_length = String.length prefix
  and line_length = String.length line in
  prefix_length <= line_length && (String.sub line 0 prefix_length = prefix)

let platform = match Sys.os_type with
  | "Win32" -> Windows
  | "Unix" ->
    let read_line f =
      finally
        (fun () -> input_line f)
        (fun () -> close_in f) in
    begin
      try
        let line = read_line (open_in "/proc/version") in
        if startswith "Linux" line
        then Linux
        else Unsupported line
      with _ ->
        (* Maybe it's a Mac *)
        begin
          let line = read_line (Unix.open_process_in "uname") in
          if startswith "Darwin" line
          then Mac
          else Unsupported line
        end
    end
  | x -> Unsupported x    

exception Unsupported_platform of string

let create () = match platform with
  | Windows -> Af_hyperv.create ()
  | Linux -> Af_vsock.create ()
  | Mac -> Hyperkit.create ()
  | Unsupported x -> raise (Unsupported_platform x)

type port =
  | Port of Af_vsock.port
  | Serviceid of Af_hyperv.serviceid

let serviceid_of_port = function
  | Port x -> Printf.sprintf "%08lx-FACB-11E6-BD58-64006A7986D3" x
  | Serviceid x -> x

let port_of_port = function
  | Port x -> x
  | Serviceid x ->
    try
        Scanf.sscanf x "%08lx-FACB-11E6-BD58-64006A7986D3" (fun x -> x)
    with _ ->
        raise (Unsupported_platform "Generic service IDs are only supported on Windows")

type peer =
  | Any
  | CID of Af_vsock.cid
  | VMID of Af_hyperv.vmid
  | Hyperkit of string

type sockaddr = peer * port

let string_of_sockaddr (peer, port) =
  let string_of_peer = function
    | Any    -> "Any"
    | CID x  -> Printf.sprintf "CID %s" (Af_vsock.string_of_cid x)
    | VMID x -> Printf.sprintf "VMID %s" (Af_hyperv.string_of_vmid x)
    | Hyperkit x -> Printf.sprintf "Hyperkit %s" x in
  let string_of_port = function
    | Port x      -> Int32.to_string x
    | Serviceid x -> x in
  Printf.sprintf "Socket { peer = %s; port = %s }" (string_of_peer peer) (string_of_port port)

let sockaddr_of_uri uri =
  let strip_slash x =
    if x = ""
    then ""
    else
      if x.[0] = '/'
      then String.sub x 1 (String.length x - 1)
      else x in
match Uri.scheme uri, Uri.host uri, Uri.port uri, Uri.path uri with
  | Some "vsock", Some "", Some port, _ -> Any, Port (Int32.of_int port)
  | Some "vsock", Some cid, Some port, _ -> CID (Af_vsock.Id (Int32.of_string cid)), Port (Int32.of_int port)
  | Some "hvsock", Some "", _, serviceid -> Any, Serviceid (strip_slash serviceid)
  | Some "hvsock", Some vmid, _, serviceid ->
    let vmid = match Uuidm.of_string vmid with
    | None ->
      (* Attempt to look up the UUID from the name *)
      Af_hyperv.vmid_of_name vmid
    | Some vmid -> vmid in
    VMID (Af_hyperv.Id vmid), Serviceid (strip_slash serviceid)
  | Some "hyperkit", _, Some port, hyperkit_path ->
    (* Support relative paths which start with "/." *)
    let hyperkit_path =
      if String.length hyperkit_path >= 2 && String.sub hyperkit_path 0 2 = "/."
      then String.sub hyperkit_path 1 (String.length hyperkit_path - 1)
      else hyperkit_path in
    Hyperkit hyperkit_path, Port (Int32.of_int port)
  | _, _, _, _ -> invalid_arg "sockaddr_of_uri"

let vmid_of_peer = function
  | Any -> Af_hyperv.Wildcard
  | VMID x -> x
  | Hyperkit _ -> raise (Unsupported_platform "Hyperkit is only supported on Mac")
  | CID _ -> raise (Unsupported_platform "CIDs are only supported on Linux")

let cid_of_peer = function
  | Any -> Af_vsock.Any
  | CID x -> x
  | Hyperkit _ -> raise (Unsupported_platform "Hyperkit is only supported on Mac")
  | VMID _ -> raise (Unsupported_platform "VMIDs are only supported on Windows")

let path_of_peer = function
  | Hyperkit x -> x
  | _ -> raise (Unsupported_platform "Mac only supports Hyperkit")

let bind fd (peer, port) = match platform with
  | Windows -> Af_hyperv.bind fd { Af_hyperv.vmid = vmid_of_peer peer; serviceid = serviceid_of_port port }
  | Linux   -> Af_vsock.bind fd { Af_vsock.cid = cid_of_peer peer; port = port_of_port port }
  | Mac -> Hyperkit.bind fd { Hyperkit.hyperkit_path = path_of_peer peer; port = port_of_port port }
  | Unsupported x -> raise (Unsupported_platform x)

let accept fd = match platform with
  | Windows ->
    let fd', { Af_hyperv.vmid; serviceid } = Af_hyperv.accept fd in
    fd', (VMID vmid, Serviceid serviceid)
  | Linux ->
    let fd', { Af_vsock.cid; port } = Af_vsock.accept fd in
    fd', (CID cid, Port port)
  | Mac ->
    let fd', { Hyperkit.hyperkit_path; port } = Hyperkit.accept fd in
    fd', (Hyperkit hyperkit_path, Port port)
  | Unsupported x -> raise (Unsupported_platform x)

let to_hyperv (peer, port) =
  try
    let vmid = vmid_of_peer peer in
    let serviceid = serviceid_of_port port in
    Some { Af_hyperv.vmid; serviceid }
  with _ -> None

let to_vsock (peer, port) =
  try
    let cid = cid_of_peer peer in
    let port = port_of_port port in
    Some { Af_vsock.cid; port }
  with _ -> None

let to_hyperkit (peer, port) =
  try
    let hyperkit_path = path_of_peer peer in
    let port = port_of_port port in
    Some Hyperkit.({hyperkit_path; port })
  with _ -> None

let connect ?timeout_ms fd sockaddr = match platform, sockaddr with
  | Windows, (peer, port) ->
    let vmid = vmid_of_peer peer in
    let serviceid = serviceid_of_port port in
    Af_hyperv.connect ?timeout_ms fd { Af_hyperv.vmid; serviceid }
  | Linux, (peer, port) ->
    let cid = cid_of_peer peer in
    let port = port_of_port port in
    Af_vsock.connect ?timeout_ms fd { Af_vsock.cid; port }
  | Mac, (peer, port) ->
    let path = path_of_peer peer in
    let port = port_of_port port in
    Hyperkit.connect ?timeout_ms fd Hyperkit.({hyperkit_path = path; port = port})
  | Unsupported x, _ -> raise (Unsupported_platform x)

let read_into = Af_common.read_into
let writev = Af_common.writev
let shutdown_read fd = Unix.shutdown fd Unix.SHUTDOWN_RECEIVE
let shutdown_write fd = Unix.shutdown fd Unix.SHUTDOWN_SEND
let close = Unix.close
let listen = Unix.listen