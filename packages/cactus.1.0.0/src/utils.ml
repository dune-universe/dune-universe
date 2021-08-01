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

open Stats.Func
open Stats.Utils
open Stats.Store

let rec binary_search ?(safe = false) ~compare i j =
  (* Finds [k] such that [compare k >= 0] and [compare (k+1) < 0]. If no such
     [k] it returns Not_found. If [safe] is false then it finds [k] such that
     [compare k = 0] and raises Not_found otherwise. *)
  (* The loop invariant is that [compare i >= 0] and [compare j < 0]. If [safe]
     and if [i] is the last element then return it. If [safe] and if [i=0] and
     [j=1] then it returns [i]. *)
  if j = i then raise Not_found;
  if j - i < 2 then if safe || compare i = 0 then i else raise Not_found
  else
    let k = i + ((j - i) / 2) in
    if compare k < 0 then binary_search ~compare ~safe i k else binary_search ~safe ~compare k j

let binary_search ?(safe = false) ~compare i j =
  tic stat_binary_search;
  let ret = binary_search ~safe ~compare i j in
  tac stat_binary_search;
  ret

let rec pow base = function
  | 0 -> 1
  | 1 -> base
  | n ->
      let res = pow base (n / 2) in
      res * res * if n mod 2 = 0 then 1 else base

let is_sorted l =
  let rec aux last = function [] -> true | h :: t -> if last > h then false else aux h t in
  match l with
  | [] -> true
  | h :: t ->
      let ret = aux h t in
      if not ret then
        Fmt.pr "%a@." Fmt.(list string) (List.map (fun s -> s |> Hex.of_string |> Hex.show) l);
      ret

let sizes_to_offsets sizes =
  let rec aux acc remaining =
    match (remaining, acc) with
    | [], _ | _, [] -> failwith "sizes_to_offsets"
    | [ _size ], _ -> List.rev acc
    | size :: sizes, off :: _ -> aux ((size + off) :: acc) sizes
  in
  aux [ 0 ] sizes

let min_key length =
  (* Creates the smallest key of length [length]. *)
  String.make length '\000'

let max_key length =
  (* Creates the largest key of length [length]. *)
  String.make length '\255'

let assert_pread fd buffer off length =
  tic stat_io_r;
  let read_sz =
    Syscalls.pread ~fd ~fd_offset:(off |> Optint.Int63.of_int) ~buffer ~buffer_offset:0 ~length
  in
  if read_sz <> length then (
    Log.err @@ fun reporter ->
    reporter "Tried reading %i bytes but read only %i bytes" length read_sz;
    assert false);
  Index_stats.add_read length;
  increment stat_io_r "nb_bytes" length;
  tac stat_io_r

let assert_pwrite fd buffer off length =
  tic stat_io_w;
  let write_sz =
    Syscalls.pwrite ~fd ~fd_offset:(off |> Optint.Int63.of_int) ~buffer ~buffer_offset:0 ~length
  in
  if write_sz <> length then (
    Log.err @@ fun reporter ->
    reporter "Tried writing %i bytes but wrote only %i bytes" length write_sz;
    assert false);
  Index_stats.add_write length;
  increment stat_io_w "nb_bytes" length;
  tac stat_io_w

let b256size n =
  let rec aux acc n = match n with 0 -> acc | _ -> aux (1 + acc) (n / 256) in
  aux 0 n

let to_b256 n =
  let buff = Bytes.create 8 in
  let rec aux l n =
    match n with
    | 0 -> Bytes.sub_string buff (8 - l) l
    | _ ->
        let q, r = (n mod 256, n / 256) in
        Bytes.set buff (7 - l) (Char.chr q);
        aux (l + 1) r
  in
  aux 0 n

let from_b256 s =
  let rep = ref 0 in
  String.iter (fun c -> rep := Char.code c + (!rep * 256)) s;
  !rep

let nop () = ()
