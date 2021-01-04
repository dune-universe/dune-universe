(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(* lastfm protocol API for ocaml *)

module Http_ocamlnet = struct
  type request = Get | Post of string

  exception Http of string

  let default_timeout = ref 5.

  let request ?timeout ?(headers = []) ?(port = 80) ~host ~url ~request () =
    let timeout = match timeout with Some v -> v | None -> !default_timeout in
    let call =
      match request with
        | Get -> new Nethttp_client.get_call
        | Post _ -> new Nethttp_client.post_call
    in
    let pipeline = new Nethttp_client.pipeline in
    pipeline#set_options
      {
        (pipeline#get_options) with
        Nethttp_client.connection_timeout = timeout;
      };
    let http_headers = call#request_header `Base in
    let body = call#request_body in
    call#set_request_uri (Printf.sprintf "http://%s:%d%s" host port url);
    let headers =
      ("User-agent", Printf.sprintf "ocaml-lastfm/%s" Lastfm_constants.version)
      :: headers
    in
    http_headers#set_fields headers;
    begin
      match request with
      | Get -> ()
      | Post post ->
          body#set_value post;
          call#set_request_body body;
          http_headers#update_field "Content-length"
            (string_of_int (String.length post))
    end;
    call#set_request_header http_headers;
    pipeline#add call;
    try
      pipeline#run ();
      call#response_body#value
    with Nethttp_client.Http_protocol e | e ->
      pipeline#reset ();
      raise (Http (Printexc.to_string e))
end

module Audioscrobbler = Lastfm_generic.Audioscrobbler_generic (Http_ocamlnet)
module Radio = Lastfm_generic.Radio_generic (Http_ocamlnet)
