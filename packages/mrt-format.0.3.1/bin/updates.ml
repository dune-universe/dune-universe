(*
 * Copyright (c) 2012 Richard Mortier <mort@cantab.net>
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

let npackets = ref 0

let updates = function
  | h, Mrt.Bgp4mp (_, Message (Some (Update u)))
    -> (printf "%d\t%s\t%s\n%!"
          !npackets (Mrt.header_to_string h) (Mrt__Bgp.update_to_string u)
    )
  | _ -> ()

let _ =
  let fn = Sys.argv.(1) in
  let fd = Unix.(openfile fn [O_RDONLY] 0) in
  let buf =
    let ba = Bigarray.(Array1.map_file fd Bigarray.char c_layout false (-1)) in
    Cstruct.of_bigarray ba
  in
  printf "file length %d\n%!" (Cstruct.len buf);

  let rec packets next = match next () with
    | None -> ()
    | Some p -> incr npackets; updates p; packets next
  in  buf |> Mrt.parse |> packets;

  printf "num packets %d\n%!" !npackets
