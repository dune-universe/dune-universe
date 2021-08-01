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

let test_single_creation version () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "tree_single_creation" in
  MyBtree.create root |> ignore

let test_creation version () =
  let module MyBtree = (val get_tree version) in
  let root1 = v_to_s version // "tree_single_creation_1" in
  let root2 = v_to_s version // "tree_single_creation_2" in
  MyBtree.create root1 |> ignore;
  MyBtree.create root2 |> ignore

let test_addition version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // Format.sprintf "tree_add_%i" n in
  let tree = MyBtree.create root in
  for _i = 1 to n do
    let key = generate_key () in
    MyBtree.add tree key (0, 0, 0)
  done

let test_removal version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // Format.sprintf "tree_remove_%i" n in
  let tree = MyBtree.create root in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to n do
    MyBtree.add tree keys.(i) (0, 0, 0);
    if i mod 2 = 1 then MyBtree.remove tree keys.(i)
  done;
  for i = 1 to n / 2 do
    Fmt.pr "[%i] %s@." (2 * i) keys.(2 * i);
    if i = 357 then MyBtree.snapshot tree;
    MyBtree.remove tree keys.(2 * i)
  done;
  Alcotest.(check int) "Checking that the tree is empty" 0 (MyBtree.length tree)

let test_mem version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // Format.sprintf "tree_mem_%i" n in
  let tree = MyBtree.create root in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to n do
    MyBtree.add tree keys.(i) (i, 2 * i, 3 * i)
  done;
  for i = 1 to n do
    Alcotest.(check bool)
      (Format.sprintf "Checking that key %s is indeed there" keys.(i))
      true
      (MyBtree.mem tree keys.(i));
    let absent = generate_key () in
    Alcotest.(check bool)
      (Format.sprintf "Checking that key %s is indeed not there" absent)
      false (MyBtree.mem tree absent)
  done

let test_clear version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // Format.sprintf "tree_clear_%i" n in
  let tree = MyBtree.create root in
  let keys = Array.init (2 * n) (fun _ -> generate_key ()) in
  Array.iter (fun key -> MyBtree.add tree key (0, 0, 0)) (Array.sub keys 0 n);
  MyBtree.clear tree;
  Array.iter (fun key -> MyBtree.add tree key (0, 0, 0)) (Array.sub keys n n);
  Array.iteri
    (fun i key ->
      Alcotest.(check bool) (Format.sprintf "Checking key %i" i) (i >= n) (MyBtree.mem tree key))
    keys

let testable_repr t = Alcotest.testable (Repr.pp t) Repr.(unstage (equal t))

let check_repr t = Alcotest.check (testable_repr t)

let test_retrieval version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // Format.sprintf "tree_find_%i" n in
  let tree = MyBtree.create root in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to n do
    MyBtree.add tree keys.(i) (i, i, i);
    if i = 6648 then assert (MyBtree.mem tree keys.(i))
  done;
  MyBtree.snapshot tree;
  for i = 1 to n do
    check_repr
      (Repr.triple Repr.int Repr.int Repr.int)
      (Format.sprintf "Retrieving from tree [%i]" i)
      (i, i, i)
      (MyBtree.find tree keys.(i))
  done

let test_redundant version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // Format.sprintf "tree_redundant_%i" n in
  let tree = MyBtree.create root in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for _ = 1 to 2 do
    for i = 1 to n do
      MyBtree.add tree keys.(i) (i, 2 * i, 3 * i)
    done
  done;
  for i = 1 to n do
    Alcotest.(check bool)
      (Format.sprintf "Checking that key %s is indeed there" keys.(i))
      true
      (MyBtree.mem tree keys.(i));
    let absent = generate_key () in
    Alcotest.(check bool)
      (Format.sprintf "Checking that key %s is indeed not there" absent)
      false (MyBtree.mem tree absent)
  done

let test_snapshot version () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "tree_snapshot" in
  let tree = MyBtree.create root in
  let keys = Array.init 5001 (fun _ -> generate_key ()) in

  for i = 1 to 5000 do
    MyBtree.add tree keys.(i) (i, i, i)
  done;
  MyBtree.snapshot tree;
  let key_to_analyse = generate_key () in
  let path_to_leaves = MyBtree.Private.go_to_leaf tree key_to_analyse in
  Format.fprintf Format.std_formatter "@[<v 2>Mem path:@,Key %s@ -> %a@ -> %b@]"
    (key_to_analyse |> Repr.to_string MyKey.t)
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ -> ")
       (fun ppf -> Format.fprintf ppf "Page %i"))
    (path_to_leaves |> List.rev) (MyBtree.mem tree key_to_analyse)

let test_length version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "tree_length" in
  let tree = MyBtree.create root in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to n do
    MyBtree.add tree keys.(i) (i, i, i)
  done;
  Alcotest.(check int) "Retrieving the number of bindings" n (MyBtree.length tree)

let test_replay version () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "tree_replay" in
  let record = root // "trace.repr" in
  let tree = MyBtree.create root ~record in
  let keys = Array.init 101 (fun _ -> generate_key ()) in
  for i = 0 to 99 do
    MyBtree.add tree keys.(i) (i, i, i);
    let before = Random.int (i + 1) in
    let after = i + 1 + Random.int (99 - i + 1) in
    assert (MyBtree.mem tree keys.(before));
    try
      MyBtree.find tree keys.(after) |> ignore;
      assert false
    with Not_found -> ()
  done

let suite version =
  ( Fmt.str "%s tree" (v_to_s version),
    [
      ("Single creation", `Quick, test_single_creation version);
      ("Creation", `Quick, test_creation version);
      ("Small addition", `Quick, test_addition version 100);
      ("Small mem", `Quick, test_mem version 100);
      ("Small retrieval", `Quick, test_retrieval version 100);
      ("Addition", `Quick, test_addition version 1000);
      ("Mem", `Quick, test_mem version 1000);
      ("Retrieval", `Quick, test_retrieval version 1000);
      ("Length", `Quick, test_length version 1_415);
      ("Removal", `Quick, test_removal version 1000);
      ("Clear", `Quick, test_clear version 1000);
      ("Snapshot", `Quick, test_snapshot version);
      ("Huge addition", `Slow, test_addition version 100_000);
      ("Huge retrieval", `Slow, test_retrieval version 100_000);
      ("Redundant additions", `Quick, test_redundant version 1000);
      ("Replay", `Quick, test_replay version);
    ] )
