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

let test_creation version () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "integrity_creation" in
  for _ = 1 to 10 do
    MyBtree.create root |> ignore
  done

let test_addition version n () =
  let module MyBtree = (val get_tree version) in
  let increment = n / 10 in
  let root = v_to_s version // Format.sprintf "integrity_addition_%i" n in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to 10 do
    let tree = MyBtree.create root in
    MyBtree.snapshot tree;
    for _j = 1 to i * increment do
      MyBtree.add tree keys.(i) (0, 0, 0)
    done
  done

let test_mem version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "integrity_mem" in
  let increment = n / 10 in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  for i = 1 to 10 do
    let tree = MyBtree.create root in
    for j = ((i - 1) * increment) + 1 to i * increment do
      MyBtree.add tree keys.(j) (0, 0, 0)
    done;
    MyBtree.flush tree;
    let tree = MyBtree.create root in
    for j = 1 to i * increment do
      Alcotest.(check bool)
        (Format.sprintf "Checking that key %s is indeed there" keys.(j))
        true
        (MyBtree.mem tree keys.(j))
    done
  done

let test_reconstruct version n () =
  let module MyBtree = (val get_tree version) in
  let root = v_to_s version // "integrity_reconstruct" in
  let tree = MyBtree.create root in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in
  let () = keys |> Array.iteri @@ fun i key -> MyBtree.add tree key (i, i, i) in
  MyBtree.close tree;
  let tree = MyBtree.reconstruct root in
  MyBtree.snapshot tree;
  let () =
    keys
    |> Array.iteri @@ fun i key ->
       Alcotest.(check (triple int int int))
         (Format.sprintf "Checking key %s" key)
         (i, i, i) (MyBtree.find tree key)
  in
  ()

let _test_recovery version n () =
  Random.init 42;
  let module MyBtree = (val get_tree ~random_failure:true version) in
  let root = v_to_s version // "integrity_recovery" in
  Logs.set_reporter Log.app_reporter;
  Logs.set_level (Some Logs.Warning);
  let tree = ref (MyBtree.create root) in
  let keys = Array.init (n + 1) (fun _ -> generate_key ()) in

  let rec safe_reconstruct safety_net =
    if safety_net = 0 then failwith "Too many chained failures";
    try tree := MyBtree.reconstruct root
    with MyBtree.Private.Store.RandomFailure ->
      Fmt.pr "Failure...@.";
      safe_reconstruct (safety_net - 1);
      Fmt.pr "...restored@."
  in
  let rec safe_add key value safety_net =
    if safety_net = 0 then failwith "Too many chained failures";
    try
      MyBtree.add !tree key value;
      MyBtree.find !tree key |> ignore
    with MyBtree.Private.Store.RandomFailure ->
      Fmt.pr "Failure...@.";
      safe_reconstruct (safety_net - 1);
      safe_add key value (safety_net - 1);
      Fmt.pr "...restored@."
  in
  let () =
    keys |> Array.iteri @@ fun i key -> safe_add key (i, i, i) 30
    (* if i = 5667 then MyBtree.snapshot !tree;
       if i = 5667 then
         Array.iteri
           (fun j key ->
             Fmt.pr "->%i %s@ " j key;
             MyBtree.find !tree key |> ignore)
           (Array.sub keys 0 i) *)
  in
  let () =
    keys
    |> Array.iteri @@ fun i key ->
       Alcotest.(check (triple int int int))
         (Format.sprintf "Checking key %s" key)
         (i, i, i) (MyBtree.find !tree key)
  in
  ()

let suite version =
  ( Fmt.str "%s integrity" (v_to_s version),
    [
      ("Creation", `Quick, test_creation version);
      ("Addition", `Quick, test_addition version 1000);
      ("Big addition", `Quick, test_addition version 10_000);
      ("Mem", `Quick, test_mem version 10);
      ("Reconstruct", `Slow, test_reconstruct version 10_000);
      (* ("Recover", `Slow, test_recovery version 100_000); *)
    ] )
