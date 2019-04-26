(*
 * Copyright (c) 2012-2017 Richard Mortier <mort@cantab.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

let _ =
  (* global packet counter *)
  let npackets = ref 0 in

  (* open file, create buf *)
  let fn = Sys.argv.(1) in
  let fd = Unix.(openfile fn [O_RDONLY] 0) in
  let buf =
    let ba = Bigarray.(Array1.map_file fd Bigarray.char c_layout false (-1)) in
    Cstruct.of_bigarray ba
  in
  printf "file length %d\n%!" (Cstruct.len buf);

  (* generate packet iterator *)
  let packets = Mrt.parse buf in

  (* recursively iterate over packet iterator, printing as we go *)
  let rec print_packet () = match packets () with
    | None -> ()
    | Some packet ->
        incr npackets;
        printf "#%d|%s\n%!" !npackets (Mrt.to_string packet);
        print_packet ()
  in
  print_packet ();

  (* done! *)
  printf "num packets %d\n%!" !npackets
