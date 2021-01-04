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

open Lastfm.Audioscrobbler

let _ =
  try
    let user, pass, title, artist, length =
      (Sys.argv.(1), Sys.argv.(2), Sys.argv.(3), Sys.argv.(4), Sys.argv.(5))
    in
    let time = Unix.time () in
    let length = float_of_string length in
    let song = get_song ~time ~source:User ~length ~artist ~track:title () in
    let client = { Lastfm_generic.client = "tst"; version = "0.1" } in
    let login = { Lastfm_generic.user; password = pass } in
    do_np client login song;
    let failed = do_submit client login [song] in
    let print_failed (e, s) =
      Printf.printf "%s -- %s submission failed: %s" s.artist s.track
        (string_of_error e)
    in
    List.iter print_failed failed
  with Error e -> Printf.printf "Failed: %s\n" (string_of_error e)
