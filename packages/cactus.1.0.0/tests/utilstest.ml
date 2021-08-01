(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
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

module Utils = Btree.Private.Utils

let test_binary () =
  for tot = 1 to 10 do
    for i = 0 to tot - 1 do
      let compare k = compare i k in
      Alcotest.(check int)
        (Format.sprintf "Binary search for %i in list of length %i" i tot)
        i
        (Utils.binary_search 0 tot ~compare)
    done
  done

let test_offsets () =
  let sizes = [ 3; 1; 6; 4; 9; 11 ] in
  let offsets = [ 0; 3; 4; 10; 14; 23 ] in
  Alcotest.(check (list int)) "Offsets from sizes" offsets (Utils.sizes_to_offsets sizes)

let test_sorted () =
  let is_sorted = [ "0"; "1"; "2"; "3" ] in
  let is_not_sorted = [ "0"; "1"; "2"; "1" ] in
  Alcotest.(check bool) "Check that the list is indeed sorted" true (Utils.is_sorted is_sorted);
  Alcotest.(check bool)
    "Check that the list is indeed not sorted" false (Utils.is_sorted is_not_sorted)

let test_conversion () =
  for i = 1 to 1000 do
    Alcotest.(check bool)
      (Fmt.str "Check from_b256(to_b256(%i)) = %i" i i)
      true
      (Utils.(from_b256 @@ to_b256 @@ i) = i)
  done;
  Alcotest.(check string) "Check that to_b256(256) = \"\\001\\000\"" "\001\000" (Utils.to_b256 256)

let suite =
  ( "Utils",
    [
      ("Binary search", `Quick, test_binary);
      ("Sizes to offsets", `Quick, test_offsets);
      ("Is sorted", `Quick, test_sorted);
      ("Conversion", `Quick, test_conversion);
    ] )
