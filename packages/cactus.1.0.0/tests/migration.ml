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

include Common

let ( // ) a b = a ^ "/" ^ b

let maxN = 10_000

let generate_key i =
  string_of_int i |> fun s -> String.make (MyKey.encoded_size - String.length s) '\000' ^ s

let keys = Array.init maxN generate_key

let values = Array.init maxN (fun i -> (i, i, i) |> MyValue.encode)

let entry_size = MyKey.encoded_size + MyValue.encoded_size

let with_progress_bar ~message ~n ~unit =
  let open Progress in
  let w = if n = 0 then 1 else float_of_int n |> log10 |> floor |> int_of_float |> succ in
  let w_pp = Printer.int ~width:w in
  let bar =
    Line.(
      list
        [
          const message;
          count_to ~pp:w_pp n;
          const unit;
          elapsed ();
          bar ~style:`UTF8 ~color:(`magenta |> Color.ansi) n;
          eta n |> brackets;
        ])
  in
  Progress.with_reporter bar

let mkdir dirname =
  let rec aux dir k =
    if Sys.file_exists dir && Sys.is_directory dir then k ()
    else (
      if Sys.file_exists dir then Unix.unlink dir;
      (aux [@tailcall]) (Filename.dirname dir) (fun () ->
          Unix.mkdir dir 0o755;
          k ()))
  in
  aux dirname (fun () -> ())

let batchinit version n () =
  mkdir (v_to_s version // "batch_init");
  let name = v_to_s version // "batch_init" // "index" in
  if not (Sys.file_exists name) then (
    Array.sort String.compare keys;
    let fd = Unix.(openfile name [ O_CREAT; O_WRONLY ] 0o600) in
    for i = 0 to maxN - 1 do
      let kv = keys.(i) ^ values.(i) in
      let write_size = Unix.write_substring fd kv 0 String.(length kv) in
      assert (write_size = String.length kv)
    done;
    Unix.close fd);
  let fd = Unix.(openfile name [ O_RDONLY ] 0o600) in
  let module MyBtree = (val get_migrate_tree version) in
  with_progress_bar ~message:"Migrating" ~n ~unit:"bindings" @@ fun prog ->
  Unix.lseek fd 0 Unix.SEEK_SET |> ignore;
  let buff = Bytes.create (entry_size * Input.fanout * 2) in
  let read n =
    let read_size = Unix.read fd buff 0 (n * entry_size) in
    assert (read_size = n * entry_size);
    prog n;
    Bytes.(sub_string buff 0 (n * entry_size))
  in
  let root = v_to_s version // "batch_init" // Fmt.str "%i" n in
  let tree = MyBtree.init ~root ~read n in
  MyBtree.snapshot tree;
  for i = 0 to n - 1 do
    Alcotest.(check bool)
      (Format.sprintf "Checking that key %s is indeed there" keys.(i))
      true
      (MyBtree.mem tree keys.(i))
  done;
  Unix.close fd

let suite version =
  ( Fmt.str "%s batch initialisation" (v_to_s version),
    [ ("Small batch", `Quick, batchinit version 100); ("Big batch", `Slow, batchinit version maxN) ]
  )
