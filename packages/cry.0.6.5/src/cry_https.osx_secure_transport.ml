(*
 * Copyright 2003-2016 Savonet team
 *
 * This file is part of Ocaml-cry.
 *
 * Ocaml-cry is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-cry is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-cry; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Cry_common

let register fn =
  let connect_ssl ?timeout ?bind ~host sockaddr =
    let domain =
      match sockaddr with
        | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
        | Unix.ADDR_INET (_, _) -> Unix.PF_INET
    in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    let do_timeout = timeout <> None in
    let check_timeout () =
      match timeout with
        | Some timeout ->
            (* Block in a select call for [timeout] seconds. *)
            let _, w, _ = Unix.select [] [sock] [] timeout in
            if w = [] then raise (Error (Connect Timeout));
            Unix.clear_nonblock sock
        | None -> assert false
    in
    begin
      try
        if do_timeout then Unix.set_nonblock sock;
        Unix.connect sock sockaddr;
        if do_timeout then Unix.clear_nonblock sock
      with
      | Unix.Unix_error (Unix.EINPROGRESS, _, _) -> check_timeout ()
      | Unix.Unix_error (Unix.EWOULDBLOCK, _, _) when Sys.os_type = "Win32" ->
          check_timeout ()
      | exn ->
          begin
            try Unix.close sock with _ -> ()
          end;
          raise exn
    end;
    begin
      match bind with
      | None -> ()
      | Some s -> (
          try
            let bind_addr_inet = (Unix.gethostbyname s).Unix.h_addr_list.(0) in
            (* Seems like you need to bind on port 0 *)
            let bind_addr = Unix.ADDR_INET (bind_addr_inet, 0) in
            Unix.bind sock bind_addr
          with exn ->
            begin
              try Unix.close sock with _ -> ()
            end;
            raise exn )
    end;
    let ctx =
      SecureTransport.init SecureTransport.Client SecureTransport.Stream
    in
    SecureTransport.set_connection ctx sock;
    SecureTransport.set_peer_domain_name ctx host;
    SecureTransport.handshake ctx;
    let close () =
      SecureTransport.close ctx;
      Unix.close sock
    in
    let wait_for operation delay =
      let events () =
        match operation with
          | `Read -> Unix.select [sock] [] [] delay
          | `Write -> Unix.select [] [sock] [] delay
          | `Both -> Unix.select [sock] [sock] [] delay
      in
      let r, w, _ = events () in
      match operation with
        | `Read -> r <> []
        | `Write -> w <> []
        | `Both -> r <> [] || w <> []
    in
    {
      write = SecureTransport.write ctx;
      read = SecureTransport.read ctx;
      wait_for;
      close;
    }
  in
  fn connect_ssl
