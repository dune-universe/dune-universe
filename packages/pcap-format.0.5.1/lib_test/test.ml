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

open Pcap
open OUnit

let example_file = "dhcp.pcap"

(* Note this will leak fds and memory *)

let open_file filename =
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  let ba = Bigarray.(Array1.map_file fd Bigarray.char c_layout false (-1)) in
  Cstruct.of_bigarray ba

let read_header filename =
  let buf = open_file filename in
  match Pcap.detect buf with
  | Some h -> h, buf
  | None ->
    failwith (Printf.sprintf "failed to parse pcap header from %s" filename)

let header () =
  let h, buf = read_header example_file in
  let module H = (val h: HDR) in
  assert_equal ~msg:"endian"        ~printer:string_of_endian H.endian                             Little;
  assert_equal ~msg:"version_major" ~printer:string_of_int   (H.get_pcap_header_version_major buf) 2;
  assert_equal ~msg:"version_minor" ~printer:string_of_int   (H.get_pcap_header_version_minor buf) 4;
  assert_equal ~msg:"thiszone"      ~printer:Int32.to_string (H.get_pcap_header_thiszone buf)      0l;
  assert_equal ~msg:"sigfigs"       ~printer:Int32.to_string (H.get_pcap_header_sigfigs buf)       0l;
  assert_equal ~msg:"snaplen"       ~printer:Int32.to_string (H.get_pcap_header_snaplen buf)       65535l;
  assert_equal ~msg:"network"       ~printer:Int32.to_string (H.get_pcap_header_network buf)       1l

let packets () =
  (* Check that we can correctly split the buffer into packets *)
  let h, buf = read_header example_file in
  let _, body = Cstruct.split buf sizeof_pcap_header in
  let num_packets = Cstruct.fold (fun a _ -> a + 1) (packets h body) 0 in
  assert_equal ~msg:"num_packets" ~printer:string_of_int num_packets 4

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test pcap parsing code";

  let suite = "pcap" >:::
              [
                "header" >:: header;
                "packets" >:: packets;
              ] in
  run_test_tt ~verbose:!verbose suite
