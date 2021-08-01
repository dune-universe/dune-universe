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

module Stats = Btree.Private.Stats

let _ = Memtrace.trace_if_requested ()

let stable_v = 2 (* last stable version number *)

let init with_profiling =
  if with_profiling then (
    Stats.Btree.setup_log [ "add"; "find"; "mem" ];
    Stats.Nodes.setup_log [ "load"; "split"; "add"; "find" ];
    Stats.Store.setup_log [ "flush"; "io read"; "io write" ];
    Stats.Utils.setup_log [ "binary-search" ])
  else Stats.Store.setup_log [ "io read"; "io write"; "flush" ];
  flush stdout;
  Printexc.record_backtrace true;
  Utils.chdir "_bench";
  Random.init 42

let pp_config ppf config =
  let ({ fanout; page_sz; cache_sz; key_sz; value_sz; start_sz; n; debug; version; sleep }
        : Input.config) =
    config
  in
  Fmt.pf ppf
    "@[<v 2>Configuration@;\
     Key size: %i bytes@;\
     Value size: %i bytes@;\
     Cache size: %i MB@;\
     Page size: %i bytes@;\
     Start size : %i bindings@;\
     Fanout: %i@;\
     Debug: %b@;\
     Version: %i@;\
     Random sleeps: %b@;\
     Number of bindings: %i@]" key_sz value_sz cache_sz page_sz start_sz fanout debug version sleep
    n

type args = {
  version : bool;
  key_sz : int;
  value_sz : int;
  fanout : int;
  cache_size : int;
  page_size : int;
  nb_entries : int;
  minimal : bool;
  with_profiling : bool;
  json : bool;
  clear : bool;
  switch : int;
  start_sz : int;
  force : bool;
  random_sleep : bool;
}

module Run (Args : sig
  val args : args
end) =
struct
  let args = Args.args

  let config : Input.config =
    {
      fanout = args.fanout;
      cache_sz = args.cache_size;
      page_sz = args.page_size;
      key_sz = args.key_sz;
      value_sz = args.value_sz;
      start_sz = args.start_sz;
      n = args.nb_entries;
      debug = false;
      version = args.switch;
      sleep = args.random_sleep;
    }

  module Benchmark = Benchmark.Make (struct
    let config = config
  end)

  open Utils

  let run_benchmark main cache (benchmark : Benchmark.t) =
    mkdir benchmark.name;
    let logchan =
      open_out_gen [ Open_append; Open_creat; Open_trunc ] 0o600 (benchmark.name // "b.log")
    in
    let logppf = Format.formatter_of_out_channel logchan in
    let statschan =
      open_out_gen [ Open_append; Open_creat; Open_trunc ] 0o600 (benchmark.name // "stats.log")
    in
    let statsppf = Format.formatter_of_out_channel statschan in
    Logs.set_reporter (Log.reporter logppf statsppf);
    let ret =
      (benchmark.name, Benchmark.run benchmark main cache)
      (* use main/b.tree as reference for the tree *)
    in
    close_out logchan;
    close_out statschan;
    ret

  let run_suite cache suite =
    let names = List.map Benchmark.name suite in
    let main = List.hd names in

    let filter_benchmark (benchmark : Benchmark.t) =
      args.clear || args.force || benchmark.kind <> `RW || not (Sys.file_exists (main // "b.tree"))
    in

    if args.clear then List.iter Utils.clean names;

    suite |> List.filter filter_benchmark |> List.map (run_benchmark main cache)

  let run () =
    if args.version then Fmt.pr "Btree version %i@." args.switch
    else (
      init args.with_profiling;
      Fmt.pr "%a@." pp_config config;
      let suite =
        if args.minimal then Benchmark.suite |> List.filter Benchmark.minimal_filter
        else Benchmark.suite
      in
      let suites = Benchmark.split suite in
      let cache = Benchmark.Btree.empty_cache () in
      let name_results = suites |> List.map (run_suite cache) |> List.fold_left ( @ ) [] in
      if args.json then
        let pp_name_res ppf (name, res) = Fmt.pf ppf "\"%s\":%a" name Results.pp_json res in
        Fmt.pr "{%a}" Fmt.(list ~sep:comma pp_name_res) name_results
      else
        let pp_name_res ppf (name, res) =
          Fmt.pf ppf "@[<v 2>%s:@;%a@]" name
            (if args.with_profiling then Results.pp_detailed else Results.pp)
            res
        in
        let sep ppf () = Fmt.pf ppf "@.@." in
        Fmt.pr "%a@." (Fmt.list ~sep pp_name_res) name_results)
end

let run version key_sz value_sz fanout cache_size page_size nb_entries minimal with_profiling json
    clear switch start_sz force random_sleep () =
  let module Args = struct
    let args : args =
      {
        version;
        key_sz;
        value_sz;
        fanout;
        cache_size;
        page_size;
        nb_entries;
        minimal;
        with_profiling;
        json;
        clear;
        switch;
        start_sz;
        force;
        random_sleep;
      }
  end in
  let module Run = Run (Args) in
  Run.run ()

open Cmdliner

let env_var s = Arg.env_var ("BTREE_BENCH_" ^ s)

let key_sz =
  let doc = "The size of a key." in
  let env = env_var "KEY_SIZE" in
  Arg.(value & opt int 32 & info [ "k"; "key-size" ] ~env ~doc)

let value_sz =
  let doc = "The size of a value." in
  let env = env_var "VALUE_SIZE" in
  Arg.(value & opt int 13 & info [ "value-size" ] ~env ~doc)

let fanout =
  let doc = "Btree fanout" in
  let env = env_var "FANOUT" in
  Arg.(value & opt int 64 & info [ "f"; "fanout" ] ~env ~doc)

let cache_size =
  let doc = "The size in MB of available memory" in
  let env = env_var "CACHE_SIZE" in
  Arg.(value & opt int 500 & info [ "c"; "cache" ] ~env ~doc)

let page_size =
  let doc = "The size in bytes of a single disk page." in
  let env = env_var "PAGE_SIZE" in
  Arg.(value & opt int 8192 & info [ "p"; "page" ] ~env ~doc)

let nb_entries =
  let doc = "The number of bindings." in
  let env = env_var "NB_ENTRIES" in
  Arg.(value & opt int 100_000 & info [ "n"; "nb-entries" ] ~env ~doc)

let minimal =
  let doc = "Only run quick benchmarks" in
  let env = env_var "MINIMAL" in
  Arg.(value & flag & info [ "minimal" ] ~env ~doc)

let with_profiling =
  let doc = "Show detailed profiling metrics." in
  let env = env_var "WITHPROFILING" in
  Arg.(value & flag & info [ "with-profiling" ] ~env ~doc)

let json =
  let doc = "Dump metrics in a json format." in
  let env = env_var "JSON" in
  Arg.(value & flag & info [ "json" ] ~env ~doc)

let setup_log = Term.(const Log.setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let clear =
  let doc =
    "Clears past index and rerun the \"write-kind\" benchmarks. Otherwise only run the \
     \"read-kind\" benchmarks if a past-run index is present."
  in
  let env = env_var "CLEAR" in
  Arg.(value & flag & info [ "clear" ] ~env ~doc)

let version =
  let doc = "Show the last stable version number." in
  Arg.(value & flag & info [ "version" ] ~doc)

let switch =
  let doc = "Specify the version to use." in
  let env = env_var "VERSION" in
  Arg.(value & opt int stable_v & info [ "switch" ] ~doc ~env)

let start_sz =
  let doc =
    "Specify a starting size for btrees. Btrees will be initialised with that many bindings before \
     proceeding to benchmarks."
  in
  let env = env_var "SIZE" in
  Arg.(value & opt int 0 & info [ "size" ] ~doc ~env)

let random_sleep =
  let doc = "Randomly (but rarely) sleep between operations" in
  let env = env_var "SLEEP" in
  Arg.(value & flag & info [ "sleep" ] ~doc ~env)

let force =
  let doc =
    "Force the run of all writing benchmarks, even if a b.tree file already exists. This may write \
     additional values on top of the already existing tree."
  in
  Arg.(value & flag & info [ "force" ] ~doc)

let cmd =
  let doc = "Run all benchmarks." in
  ( Term.(
      const run
      $ version
      $ key_sz
      $ value_sz
      $ fanout
      $ cache_size
      $ page_size
      $ nb_entries
      $ minimal
      $ with_profiling
      $ json
      $ clear
      $ switch
      $ start_sz
      $ force
      $ random_sleep
      $ setup_log),
    Term.info "run" ~doc ~exits:Term.default_exits )

let () = Term.(exit @@ eval cmd)
