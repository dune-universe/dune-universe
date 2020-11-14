(*
 * Copyright (C) 2016 Docker Inc
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

type vmid =
  | Wildcard
  | Children
  | Loopback
  | Parent
  | Id of Uuidm.t

type serviceid = string

type sockaddr = {
  vmid: vmid;
  serviceid: serviceid;
}

let string_of_sockaddr { vmid; serviceid } =
  let vmid = match vmid with
    | Wildcard -> "Wildcard"
    | Children -> "Children"
    | Loopback -> "Loopback"
    | Parent   -> "Parent"
    | Id x     -> Uuidm.to_string x in
  Printf.sprintf "AF_HYPERV { vmid = %s; serviceid = %s }" vmid serviceid

external get_wildcard: unit -> string = "stub_hvsock_wildcard"
let wildcard = get_wildcard ()

external get_children: unit -> string = "stub_hvsock_children"
let children = get_children ()

external get_loopback: unit -> string = "stub_hvsock_loopback"
let loopback = get_loopback ()

external get_parent: unit -> string = "stub_hvsock_parent"
let parent = get_parent ()

let string_of_vmid = function
  | Wildcard -> wildcard
  | Children -> children
  | Loopback -> loopback
  | Parent   -> parent
  | Id x     -> Uuidm.to_string x

let vmid_of_string x =
  if x = wildcard then Wildcard
  else if x = children then Children
  else if x = loopback then Loopback
  else if x = parent then Parent
  else match Uuidm.of_string x with
    | Some x -> Id x
    | None -> failwith ("Failed to parse VM GUID: " ^ x)

external do_socket: unit -> Unix.file_descr = "stub_hvsock_socket"

external do_bind: Unix.file_descr -> string -> string -> unit = "stub_hvsock_bind"

external do_accept: Unix.file_descr -> Unix.file_descr * string * string = "stub_hvsock_accept"

external do_connect_blocking: Unix.file_descr -> string -> string -> unit = "stub_hvsock_connect_blocking"
external do_connect_nonblocking: int -> Unix.file_descr -> string -> string -> unit = "stub_hvsock_connect_nonblocking"

let create = do_socket

let bind fd { vmid; serviceid } = do_bind fd (string_of_vmid vmid) serviceid

let accept fd =
  let new_fd, vmid, serviceid = do_accept fd in
  let vmid = vmid_of_string vmid in
  new_fd, { vmid; serviceid }

let connect ?timeout_ms fd { vmid; serviceid } =
  ( match timeout_ms with
    | None -> do_connect_blocking
    | Some t -> do_connect_nonblocking t ) fd (string_of_vmid vmid) serviceid

let read_into = Af_common.read_into
let writev = Af_common.writev

let shutdown_read fd = Unix.shutdown fd Unix.SHUTDOWN_RECEIVE
let shutdown_write fd = Unix.shutdown fd Unix.SHUTDOWN_SEND
let close = Unix.close
let listen = Unix.listen

let with_powershell script f =
  (* Avoid escaping problems by base64-encoding the script *)
  let encoded =
    let b = Buffer.create 100 in
    for i = 0 to String.length script - 1 do
      Uutf.Buffer.add_utf_16le b (Uchar.of_char script.[i])
    done;
    match Base64.encode (Buffer.contents b) with
    | Ok x -> x
    | Error (`Msg y) -> failwith ("Base64.encode failed unexpectedly: " ^ y) in

  let ic = Unix.open_process_in ("powershell.exe -Sta -NonInteractive -ExecutionPolicy RemoteSigned -EncodedCommand  "^ encoded) in
  let closed = ref false in
  try
    let result = f ic in
    begin match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> result
    | _ ->
      closed := true;
      Printf.fprintf stderr "Failed to run powershell script:\n%s\n" script;
      failwith "Failed to run powershell"
    end
  with e ->
    if not(!closed) then ignore(Unix.close_process_in ic);
    raise e

let vmid_of_name name =
  with_powershell (Printf.sprintf "(Get-VM %s).Id" name)
    (fun ic ->
      (* If not adminstrator this will fail with:
        Get-VM : You do not have the required permission to complete this task. Contact the administrator of the authorization
      *)
      let line = String.trim @@ input_line ic in
      if line <> "" then failwith line;
      let line = String.trim @@ input_line ic in
      if line <> "Guid" then failwith line;
      let line = String.trim @@ input_line ic in
      if line <> "----" then failwith line;
      let line = String.trim @@ input_line ic in
      match Uuidm.of_string line with
      | None -> failwith ("Failed to discover VM GUID of " ^ name)
      | Some x -> x
    )

let register_serviceid serviceid =
  let script = String.concat "\n" [
    (* Get-Item with a regexp doesn't spam the output if the key doesn't exist.
       Note [S]OFTWARE only matches SOFTWARE *)
    Printf.sprintf "if (!(Get-Item -Path \"HKLM:\\[S]OFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Virtualization\\GuestCommunicationServices\\%s\")) {" serviceid;
    Printf.sprintf "  $service = New-Item -Path \"HKLM:\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Virtualization\\GuestCommunicationServices\" -Name %s" serviceid;
    "  # Set a friendly name";
    "  $service.SetValue(\"ElementName\", \"https://github.com/mirage/ocaml-hvsock\")";
    "}";
  ] in
  with_powershell script (fun _ic -> ())
