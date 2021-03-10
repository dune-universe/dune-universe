(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-vorbis.
 *
 * Ocaml-vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *)

let check = Vorbis.Decoder.check_packet
let buflen = 1024

let decoder os =
  let decoder = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
  let packet3 = ref None in
  let os = ref os in
  let init () =
    match !decoder with
      | None ->
          let packet1 =
            match !packet1 with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet1 := Some p;
                  p
              | Some p -> p
          in
          let packet2 =
            match !packet2 with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet2 := Some p;
                  p
              | Some p -> p
          in
          let packet3 =
            match !packet3 with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet3 := Some p;
                  p
              | Some p -> p
          in
          let d = Vorbis.Decoder.init packet1 packet2 packet3 in
          let info = Vorbis.Decoder.info d in
          (* This buffer is created once. The call to Array.sub
           * below makes a fresh array out of it to pass to
           * liquidsoap. *)
          let chan _ = Array.make buflen 0. in
          let buf = Array.init info.Vorbis.audio_channels chan in
          let meta = Vorbis.Decoder.comments d in
          decoder := Some (d, info, buf, meta);
          (d, info, buf, meta)
      | Some d -> d
  in
  let info () =
    let _, info, _, meta = init () in
    ( {
        Ogg_decoder.channels = info.Vorbis.audio_channels;
        sample_rate = info.Vorbis.audio_samplerate;
      },
      meta )
  in
  let restart new_os =
    os := new_os;
    let d, _, _, _ = init () in
    Vorbis.Decoder.restart d
  in
  let decode feed =
    let decoder, _, buf, _ = init () in
    try
      let ret = Vorbis.Decoder.decode_pcm decoder !os buf 0 buflen in
      feed (Array.map (fun x -> Array.sub x 0 ret) buf)
    with (* Apparently, we should hide this one.. *)
    | Vorbis.False ->
      raise Ogg.Not_enough_data
  in
  Ogg_decoder.Audio
    {
      Ogg_decoder.name = "vorbis";
      info;
      decode;
      restart;
      samples_of_granulepos = (fun x -> x);
    }

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "vorbis" (check, decoder)
