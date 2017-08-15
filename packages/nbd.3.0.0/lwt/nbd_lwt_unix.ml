(*
 * Copyright (C) Citrix Systems Inc.
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
open Nbd
open Channel
open Lwt

let of_fd fd =
  let read = Lwt_cstruct.(complete (read fd)) in
  let write = Lwt_cstruct.(complete (write fd)) in
  let close () = Lwt_unix.close fd in
  { read; write; close }

let connect hostname port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname hostname
  >>= fun host_info ->
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port))
  >>= fun () ->
  return (of_fd socket)

module Client = Nbd.Client
module Server = Nbd.Server
