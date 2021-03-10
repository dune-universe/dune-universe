(*
 * Copyright 2008 Savonet team
 *
 * This file is part of OCaml-speex.
 *
 * OCaml-speex is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OCaml-speex is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-speex; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * A speex to wav converter using OCaml-speex.
  *
  * @author Samuel Mimram
  * @author Romaib Beauxis
  *)

let bufsize = 16 * 1024
let src = ref ""
let dst = ref ""

open Unix
open Speex

let output_int chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff));
  output_char chan (char_of_int ((n lsr 16) land 0xff));
  output_char chan (char_of_int ((n lsr 24) land 0xff))

let output_short chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff))

let usage = "usage: speex2wav [options] source destination"
let float = ref false

let _ =
  Arg.parse
    [("--float", Arg.Bool (fun b -> float := b), "Use floats for encoding")]
    (let pnum = ref (-1) in
     fun s ->
       incr pnum;
       match !pnum with
         | 0 -> src := s
         | 1 -> dst := s
         | _ ->
             Printf.eprintf "Error: too many arguments\n";
             exit 1)
    usage;
  if !src = "" || !dst = "" then (
    Printf.printf "%s\n" usage;
    exit 1);
  let dec, fd = Speex.Wrapper.Decoder.open_file !src in
  let header = Speex.Wrapper.Decoder.header dec in
  let comments = Speex.Wrapper.Decoder.comments dec in
  let chans = header.Header.nb_channels in
  let rate = header.Header.rate in
  let mode = header.Header.mode in
  let print_comment (k, v) = Printf.printf "%s: %s\n" k v in
  Printf.printf "Comments:\n";
  List.iter print_comment comments;
  Printf.printf "\n";
  let s_of_m x =
    match x with
      | Speex.Narrowband -> "narrowband"
      | Speex.Wideband -> "wideband"
      | Speex.Ultra_wideband -> "ultra_wideband"
  in
  Printf.printf
    "Input file characteristics: speex codec, %d channels, %d Hz, %s mode\n"
    chans rate (s_of_m mode);
  (* Using speex to decode the ogg. *)
  Printf.printf "\nDecoding...\n";
  flush_all ();
  let tmpdst, oc =
    Filename.open_temp_file ~mode:[Open_binary] "speex2wav" ".raw"
  in
  (try
     while true do
       (* Convert mono to two identical channels.. *)
       let frames =
         if !float then
           if chans = 2 then
             List.map
               (Array.map (fun x -> Array.map int_of_float x))
               (Wrapper.Decoder.decode_stereo dec)
           else (
             let f =
               List.map (Array.map int_of_float) (Wrapper.Decoder.decode dec)
             in
             List.map (fun f -> Array.init 2 (fun _ -> f)) f)
         else if chans = 2 then Wrapper.Decoder.decode_int_stereo dec
         else (
           let f = Wrapper.Decoder.decode_int dec in
           List.map (fun f -> Array.init 2 (fun _ -> f)) f)
       in
       let put frame =
         let s1 = frame.(0) in
         let s2 = frame.(1) in
         Array.iteri
           (fun n _ ->
             output_short oc s1.(n);
             output_short oc s2.(n))
           s1
       in
       List.iter put frames
     done
   with Ogg.End_of_stream -> close_out oc);
  Printf.printf "Decoding finished, writing WAV..\n";
  Unix.close fd;
  (* Do the wav stuff. *)
  let datalen = (stat tmpdst).st_size in
  let ic = open_in_bin tmpdst in
  let oc = open_out_bin !dst in
  output_string oc "RIFF";
  output_int oc (4 + 24 + 8 + datalen);
  output_string oc "WAVE";
  output_string oc "fmt ";
  output_int oc 16;
  output_short oc 1;
  (* WAVE_FORMAT_PCM *)
  output_short oc 2;
  (* channels *)
  output_int oc rate;
  (* freq *)
  output_int oc (rate * 2 * 2);
  (* bytes / s *)
  output_short oc (2 * 2);
  (* block alignment *)
  output_short oc 16;
  (* bits per sample *)
  output_string oc "data";
  output_int oc datalen;
  (let buflen = 256 * 1024 in
   let buf = Bytes.create buflen in
   let r = ref 1 in
   let pos = ref 0 in
   while !r <> 0 do
     r := input ic buf 0 buflen;
     output oc buf 0 !r;
     pos := !pos + !r
   done);
  close_in ic;
  close_out oc;
  Unix.unlink tmpdst;
  Printf.printf "Done !\n";
  Gc.full_major ()
