(*
 * Copyright 2007 Savonet team
 *
 * This file is part of OCaml-Vorbis.
 *
 * OCaml-Vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-Vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Nethttp_client

let _ =
  try
    let uri = Sys.argv.(1) in
    let tracks = Lastfm.Radio.get uri in
    let metas, track = List.hd tracks in
    Printf.printf "Got track: %s\n" track;
    List.iter (fun (a, b) -> Printf.printf "Metadata: %s: %s\n" a b) metas;
    let filename =
      Printf.sprintf "%s - %s.mp3"
        (List.assoc "artist" metas)
        (List.assoc "title" metas)
    in
    Printf.printf "Downloading track to: %s" filename;
    flush stdout;
    let pipeline = new pipeline in
    let get_call = new get track in
    get_call#set_response_body_storage (`File (fun () -> filename));
    pipeline#add get_call;
    pipeline#run ()
  with Lastfm.Radio.Error e ->
    Printf.printf "Failed: %s" (Lastfm.Radio.string_of_error e)
