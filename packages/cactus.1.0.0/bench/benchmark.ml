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

include Benchmark_intf

module Make (Config : CONFIG) = struct
  module Size : Input.SIZE = struct
    let ({ fanout; page_sz; cache_sz; key_sz; value_sz; debug; version; _ } : Input.config) =
      Config.config

    module Debug = struct
      let random_failure = false
    end
  end

  module Input = Input.Make (Size)
  module Btree = Btree.Make (Input.Key) (Input.Value) (Size)

  let config = Config.config

  let iterate seed nb_entry iterated =
    Random.init seed;
    for i = 1 to nb_entry do
      iterated i (Input.generate_key i, Input.generate_value i)
    done

  let n = config.n

  let seed = Unix.time () |> int_of_float

  let normal_iter = iterate (seed + 0) n

  let absent_iter = iterate (seed + 1) n

  let sorted_bindings_pool = ref `Uninit

  let of_array_iter arr = function func -> Array.iteri func arr

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

  let write prog tree ?(with_flush = false) iterator =
    iterator (fun i (k, v) ->
        if i > 0 && i mod 937 = 0 then prog 937;
        Btree.add tree k v;
        if with_flush || i mod 10_000 = 0 then Btree.flush tree;
        if config.sleep && Random.int n <= 3 then (
          Logs.info (fun reporter -> reporter "Sleeping%s" (String.make 30 ' '));
          Unix.sleep 5));
    prog (n mod 937);
    Btree.flush tree

  let read prog tree iterator =
    iterator (fun _i (key, _) ->
        prog 1;
        Btree.find tree key |> ignore)

  let read_absent prog tree iterator =
    iterator (fun i (key, _) ->
        if (i + 1) mod 987 = 0 then prog 987;
        try
          Btree.find tree key |> ignore;
          assert false
        with Not_found -> ())

  let write_random tree () =
    with_progress_bar ~message:"replace_random" ~n ~unit:"insertions" @@ fun prog ->
    write prog tree normal_iter

  let write_seq tree () =
    with_progress_bar ~message:"replace_increasing_keys" ~n ~unit:"insertions" @@ fun prog ->
    let arr =
      match !sorted_bindings_pool with
      | `Uninit ->
          let arr = Array.init n (fun i -> (Input.generate_key i, Input.generate_value i)) in
          sorted_bindings_pool := `Init arr;
          arr
      | `Init arr -> arr
    in
    Array.sort (fun a b -> String.compare (fst a) (fst b)) arr;
    write prog tree (of_array_iter arr)

  let write_rev_seq tree () =
    with_progress_bar ~message:"replace_increasing_keys" ~n ~unit:"insertions" @@ fun prog ->
    let arr =
      match !sorted_bindings_pool with
      | `Uninit ->
          let arr = Array.init n (fun i -> (Input.generate_key i, Input.generate_value i)) in
          sorted_bindings_pool := `Init arr;
          arr
      | `Init arr -> arr
    in
    Array.sort (fun a b -> -String.compare (fst a) (fst b)) arr;
    write prog tree (of_array_iter arr)

  let write_sync tree () =
    with_progress_bar ~message:"replace_random_sync" ~n ~unit:"insertions" @@ fun prog ->
    write ~with_flush:true prog tree normal_iter

  let iter tree () =
    with_progress_bar ~message:"iter_rw" ~n ~unit:"bindings" @@ fun prog ->
    Btree.iteri (fun i _ _ -> if (i + 1) mod 987 = 0 then prog 987) tree

  let find_random tree () =
    with_progress_bar ~message:"find_random" ~n ~unit:"lookups" @@ fun prog ->
    read prog tree normal_iter

  let find_absent tree () =
    with_progress_bar ~message:"find_absent" ~n ~unit:"lookups" @@ fun prog ->
    read_absent prog tree absent_iter

  type t = {
    name : string;
    synopsis : string;
    exec : Btree.t -> unit -> unit;
    dependency : string option;
    kind : [ `RW | `R ];
    speed : [ `Quick | `Slow ];
  }

  type suite = t list

  let name t = t.name

  let run benchmark root cache =
    let writer = benchmark.kind = `RW in
    let start_sz = config.start_sz in
    let tree =
      if (not writer) || start_sz = 0 then Btree.create ~cache root
      else
        let read = Input.batch_initialiser start_sz in
        Btree.init ~root start_sz ~read
    in
    let res =
      Results.run ~entry_sz:(config.key_sz + config.value_sz) ~nb_entries:n (benchmark.exec tree)
    in
    if writer then (
      Logs.app (fun reporter ->
          reporter "Used cache size is %i MB" (Btree.Private.cache_size tree / 1_000_000));
      Btree.flush tree);
    res

  let suite =
    [
      {
        name = "replace_random";
        synopsis = "Insertions in random order";
        exec = write_random;
        dependency = None;
        kind = `RW;
        speed = `Quick;
      };
      {
        name = "replace_random_sync";
        synopsis = "Random insertions with continuous flushing";
        exec = write_sync;
        dependency = None;
        kind = `RW;
        speed = `Slow;
      };
      {
        name = "replace_increasing_keys";
        synopsis = "Insertions in increasing key order";
        exec = write_seq;
        dependency = None;
        kind = `RW;
        speed = `Slow;
      };
      {
        name = "replace_decreasing_keys";
        synopsis = "Insertions in decreasing key order";
        exec = write_rev_seq;
        dependency = None;
        kind = `RW;
        speed = `Slow;
      };
      {
        name = "iter_rw";
        synopsis = "Single pass iteration over all bindings";
        exec = iter;
        dependency = Some "replace_random";
        kind = `R;
        speed = `Slow;
      };
      {
        name = "find_random";
        synopsis = "Random lookups";
        exec = find_random;
        dependency = Some "replace_random";
        kind = `R;
        speed = `Quick;
      };
      {
        name = "find_absent";
        synopsis = "Random lookups for unbound keys";
        exec = find_absent;
        dependency = Some "replace_random";
        kind = `R;
        speed = `Slow;
      };
    ]

  let split suite =
    (* splits suites into chunks corresponding to the connex subgraph in the suite dependency graph.
        More over, each chunk is sorted in topological order *)
    let n = List.length suite in
    let name_to_i = List.mapi (fun i elem -> (elem.name, i)) suite in
    let i_to_elem = List.map (fun elem -> elem) suite |> Array.of_list in
    let i_mode = Array.make n `Unseen in
    let dependants = Array.make n [] in
    let rec find_root i =
      match i_mode.(i) with
      | `Waiting -> failwith "Cyclic dependency"
      | `Done root -> root
      | `Unseen -> (
          i_mode.(i) <- `Waiting;
          let elem = i_to_elem.(i) in
          match elem.dependency with
          | None ->
              i_mode.(i) <- `Done i;
              dependants.(i) <- [ elem ];
              i
          | Some name ->
              let root = find_root (List.assoc name name_to_i) in
              i_mode.(i) <- `Done root;
              dependants.(root) <- elem :: dependants.(root);
              root)
    in
    List.iteri (fun i _ -> find_root i |> ignore) suite;
    dependants
    |> Array.to_list
    |> List.filter (( <> ) []) (* remove empty deps *)
    |> List.map List.rev

  let minimal_filter elem = match elem.speed with `Slow -> false | `Quick -> true
end
