(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-flac.
 *
 * Ocaml-flac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-flac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-flac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let check = Flac_ogg.Decoder.check_packet

let decoder os =
  let ogg_dec = ref None in
  let packet = ref None in
  let decoder = ref None in
  let os = ref os in
  let dummy_c = Flac_ogg.Decoder.get_callbacks (fun _ -> ()) in
  let init () =
    match !decoder with
      | None ->
          let packet =
            match !packet with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet := Some p;
                  p
              | Some p -> p
          in
          let ogg_dec =
            match !ogg_dec with
              | None ->
                  let dec = Flac_ogg.Decoder.create packet !os in
                  ogg_dec := Some dec;
                  dec
              | Some dec -> dec
          in
          let dec, info, m = Flac.Decoder.init ogg_dec dummy_c in
          let meta =
            match m with None -> ("Unknown vendor", []) | Some x -> x
          in
          decoder := Some (dec, info, meta);
          (dec, info, meta)
      | Some d -> d
  in
  let info () =
    let _, info, m = init () in
    ( {
        Ogg_decoder.channels = info.Flac.Decoder.channels;
        sample_rate = info.Flac.Decoder.sample_rate;
      },
      m )
  in
  let decode feed =
    let decoder, _, _ = init () in
    let c = Flac_ogg.Decoder.get_callbacks (fun ret -> feed ret) in
    match Flac.Decoder.state decoder c with
      | `Search_for_metadata | `Read_metadata | `Search_for_frame_sync
      | `Read_frame ->
          Flac.Decoder.process decoder c
      (* Ogg decoder is responsible for detecting end of stream vs. end of track. *)
      | _ -> raise Ogg.Not_enough_data
  in
  let restart new_os =
    os := new_os;
    let d, _, _ = init () in
    Flac_ogg.Decoder.update_ogg_stream d new_os;
    (* Flush error are very unlikely. *)
    let c = Flac_ogg.Decoder.get_callbacks (fun _ -> ()) in
    assert (Flac.Decoder.flush d c)
  in
  Ogg_decoder.Audio
    {
      Ogg_decoder.name = "flac";
      info;
      decode;
      restart;
      samples_of_granulepos = (fun x -> x);
    }

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "flac" (check, decoder)
