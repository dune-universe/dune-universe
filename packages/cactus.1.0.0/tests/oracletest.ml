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

module V = struct
  include (val Oracle.stringv ~encode_sz:15)

  let decode = encode
end

module H = Oracle.Default
module Sort = Oracle.Make (V) (H)

let () = Printexc.record_backtrace true

let () = Random.init 42

let random_char _seed = char_of_int (33 + Random.int 94)

let random_v () = String.init (V.encode_sz - 1) random_char ^ "\n"

let is_sorted l =
  let rec aux last = function [] -> true | h :: t -> if last > h then false else aux h t in
  match l with [] -> true | h :: t -> aux h t

let run ~root n () =
  let pool = Array.init n (fun _ -> random_v ()) in
  let off = ref 0 in
  let oracle () =
    let ret = pool.(!off) in
    incr off;
    ret
  in
  if not (Sys.file_exists root) then Unix.mkdir root 0o777;
  let out = root ^ "/" ^ "out" in
  Sort.sort ~oracle ~out n;

  let open Unix in
  let fd = openfile out [ O_RDONLY ] 0o655 in
  let buff = Bytes.create V.encode_sz in
  let sortpool = Array.make n "" in
  off := 0;
  for _i = 1 to n do
    let rd_sz = read fd buff 0 V.encode_sz in
    assert (rd_sz = V.encode_sz);
    sortpool.(!off) <- Bytes.to_string buff;
    incr off
  done;
  Array.sort String.compare pool;
  (pool, sortpool)

let test_sorted n () =
  let root = Fmt.str "test_sort_%i" n in
  let _pool, sortpool = run ~root n () in
  Alcotest.(check bool) "Is the final list sorted ?" true (sortpool |> Array.to_list |> is_sorted)

let test_preserve n () =
  let root = Fmt.str "test_preserve_%i" n in
  let pool, sortpool = run ~root n () in
  Array.iter2 (Alcotest.(check string) "Comparing keys") pool sortpool

let suite =
  ( "Merge sort",
    [
      ("Sorted small", `Quick, test_sorted 100);
      ("Sorted big", `Quick, test_sorted 10_000);
      ("Sorted huge", `Slow, test_sorted 100_000);
      ("Preserve small", `Quick, test_preserve 100);
      ("Preserve big", `Quick, test_preserve 10_000);
    ] )
