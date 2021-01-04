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

let _ =
  try
    let uri = Sys.argv.(1) in
    let tracks = Lastfm.Radio.get uri in
    let metas, track = List.hd tracks in
    Printf.printf "Got track: %s\n" track;
    List.iter (fun (a, b) -> Printf.printf "Metadata: %s: %s\n" a b) metas
  with Lastfm.Radio.Error e ->
    Printf.printf "Failed: %s" (Lastfm.Radio.string_of_error e)
