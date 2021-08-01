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

module Span = Mtime.Span

module Func = struct
  type t = {
    mutable counter : int;
    mutable histo : Bentov.histogram;
    mutable span : Mtime.span;
    mutable last_counter : int;
    mutable last_log : Mtime.t;
    mutable tic : [ `CanTac of Mtime.t | `CantTac ];
    mutable logger : [ `NoLog | `Log of Bentov.histogram -> unit ];
    counters : (string, int) Hashtbl.t; (* optional additional named counters *)
  }

  let v ~counter_names =
    let counters = Hashtbl.create (List.length counter_names) in
    List.iter (fun name -> Hashtbl.add counters name 0) counter_names;
    {
      counter = 0;
      histo = Bentov.create 30;
      last_counter = 0;
      last_log = Mtime_clock.now ();
      span = Mtime.Span.zero;
      tic = `CantTac;
      logger = `NoLog;
      counters;
    }

  let increment t name n =
    let current = Hashtbl.find t.counters name in
    Hashtbl.replace t.counters name (current + n)

  let clear_histo t = t.histo <- Bentov.create 30

  let reset t =
    t.counter <- 0;
    clear_histo t;
    t.last_counter <- 0;
    t.last_log <- Mtime_clock.now ();
    t.span <- Mtime.Span.zero;
    t.tic <- `CantTac

  let copy t = { t with counter = t.counter } (* dirty trick *)

  let tic t =
    (* TODO : use counters instead and trust ocaml gc ? *)
    match t.logger with `NoLog -> () | `Log _ -> t.tic <- `CanTac (Mtime_clock.now ())

  let tac t =
    match t.logger with
    | `NoLog -> ()
    | `Log logger -> (
        match t.tic with
        | `CantTac -> failwith "tac must follow tic"
        | `CanTac before ->
            t.tic <- `CantTac;
            let after = Mtime_clock.now () in
            let add_span = Mtime.span before after in
            t.span <- Mtime.Span.add t.span add_span;

            let point = add_span |> Mtime.Span.to_us |> Float.log in
            (* use logarithm to spread values *)
            t.histo <- Bentov.add point t.histo;

            t.counter <- t.counter + 1;
            if
              t.counter > t.last_counter + 1_000
              && Mtime.span t.last_log after |> Mtime.Span.to_s > 0.1
            then (
              logger t.histo;
              clear_histo t;
              t.last_counter <- t.counter;
              t.last_log <- after))

  let setup_log t logger = t.logger <- `Log logger

  let get_count ?name t =
    match name with None -> t.counter | Some name -> Hashtbl.find t.counters name

  let get_span t = t.span

  let pp_counters ppf counters =
    let pp_assoc ppf (name, count) = Fmt.pf ppf "%s : %i" name count in
    counters |> Hashtbl.to_seq |> List.of_seq |> Format.pp_print_list pp_assoc ppf

  let pp ppf t =
    let duration = t.span |> Span.to_ms in
    let count = t.counter |> float_of_int in
    let avg_tictac = if count > 0. then duration /. count |> string_of_float else "-" in
    let op_rate = if count > 0. then 1000. *. count /. duration |> string_of_float else "-" in
    Fmt.pf ppf
      "@[<v>Number of calls : %i@;\
       Total time : %f s@;\
       Average time/call : %s ms@;\
       Operations per second : %s op/s@;\
       %a@]"
      t.counter (duration /. 1000.) avg_tictac op_rate pp_counters t.counters

  type json_counter = { name : string; counter : int } [@@deriving repr]

  type json_t = {
    mutable counter : int;
    mutable span : float;
    counters : json_counter list; (* optional additional named counters *)
  }
  [@@deriving repr]

  let of_t (t : t) =
    let json_of_counter (name, counter) = { name; counter } in
    {
      counter = t.counter;
      span = t.span |> Mtime.Span.to_ms;
      counters = t.counters |> Hashtbl.to_seq |> List.of_seq |> List.map json_of_counter;
    }
end

module Btree_ = struct
  let stat_add = Func.v ~counter_names:[]

  let stat_find = Func.v ~counter_names:[]

  let stat_mem = Func.v ~counter_names:[]

  let name_stats =
    let names = [ "add"; "find"; "mem" ] in
    let stats = [ stat_add; stat_find; stat_mem ] in
    List.map2 (fun a b -> (a, b)) names stats

  let name = "Btree"
end

module Nodes_ = struct
  let stat_create = Func.v ~counter_names:[]

  let stat_load = Func.v ~counter_names:[]

  let stat_split = Func.v ~counter_names:[]

  let stat_shift = Func.v ~counter_names:[]

  let stat_add = Func.v ~counter_names:[]

  let stat_find = Func.v ~counter_names:[]

  let stat_mem = Func.v ~counter_names:[]

  let name_stats =
    let names = [ "create"; "load"; "split"; "shift"; "add"; "find"; "mem" ] in
    let stats = [ stat_create; stat_load; stat_split; stat_shift; stat_add; stat_find; stat_mem ] in
    List.map2 (fun a b -> (a, b)) names stats

  let name = "Nodes"
end

module Store_ = struct
  let stat_flush = Func.v ~counter_names:[]

  (** syscalls *)
  let stat_io_r = Func.v ~counter_names:[ "nb_bytes" ]

  let stat_io_w = Func.v ~counter_names:[ "nb_bytes" ]

  let stat_fsync = Func.v ~counter_names:[]

  let name_stats =
    let names = [ "flush"; "io read"; "io write"; "fsync" ] in
    let stats = [ stat_flush; stat_io_r; stat_io_w; stat_fsync ] in
    List.map2 (fun a b -> (a, b)) names stats

  let name = "Store"
end

module Utils_ = struct
  let stat_binary_search = Func.v ~counter_names:[]

  let name_stats =
    let names = [ "binary-search" ] in
    let stats = [ stat_binary_search ] in
    List.map2 (fun a b -> (a, b)) names stats

  let name = "Utils"
end

let pp_assoc ppf (name, fstat) = Fmt.pf ppf "@[<v 2>%s@;%a@]" name Func.pp fstat

let pp_histo ppf histo =
  histo
  |> Bentov.bins
  |> Fmt.pf ppf "[%a]"
       (Fmt.list
          ~sep:(fun ppf () -> Fmt.pf ppf ",")
          (fun ppf ({ center; count } : Bentov.bin) -> Fmt.pf ppf "(%f,%i)" center count))

type module_stats = (string * Func.t) list

module type Common = sig
  val name : string

  val reset : unit -> unit

  val setup_log : string list -> unit

  val get : unit -> module_stats

  val pp : Format.formatter -> module_stats -> unit
end

module MakeCommon (M : sig
  val name_stats : (string * Func.t) list

  val name : string
end) : Common = struct
  let name = M.name

  let setup_log names =
    let logger (name, _fstat) = function
      | histo ->
          Log.app (fun reporter ->
              reporter "%s;%s;%a" M.name name pp_histo histo ~tags:Log.Tag.(v () |> kind_it Stats))
    in
    let name_stats = List.filter (fun (name, _stat) -> List.mem name names) M.name_stats in
    let loggers = List.map logger name_stats in
    let fstats = List.map snd name_stats in
    List.iter2 Func.setup_log fstats loggers;
    List.iter (fun (name, _stat) -> Fmt.pr "Log setup for %s.%s@\n" M.name name) name_stats

  let copy name_stats = List.map (fun (name, fstat) -> (name, Func.copy fstat)) name_stats

  let get () = M.name_stats |> copy

  let pp ppf name_stats =
    Fmt.pf ppf "@[<v 2>%s__@;%a@]" M.name Fmt.(list ~sep:(Fmt.sps 1) pp_assoc) name_stats

  let reset () = List.iter (fun (_name, stats) -> Func.reset stats) M.name_stats
end

module Btree = struct
  include Btree_
  include MakeCommon (Btree_)
end

module Nodes = struct
  include Nodes_
  include MakeCommon (Nodes_)
end

module Store = struct
  include Store_
  include MakeCommon (Store_)
end

module Utils = struct
  include Utils_
  include MakeCommon (Utils_)
end

type t = (string * (string * Func.t) list) list

let modules = [ (module Btree : Common); (module Nodes); (module Store); (module Utils) ]

let module_names = List.map (fun (module M : Common) -> M.name) modules

let reset () = modules |> List.map (fun (module M : Common) -> M.reset) |> List.iter (fun f -> f ())

let get () =
  let from_module name (module M : Common) = (name, M.get ()) in
  List.map2 from_module module_names modules

let get_by_name t ~modul ~stat = t |> List.assoc modul |> List.assoc stat

let rec pp_print_assoclist ?pp_sep ppf pps_and_args =
  (* pps_and_args is an association list which specifies a pp for each arg,
     instead of a global pp as in [pp_print_list] *)
  let pp_sep = match pp_sep with None -> Fmt.cut | Some pp_sep -> pp_sep in
  match pps_and_args with
  | [] -> ()
  | [ (pp, arg) ] -> Fmt.pf ppf "%a" pp arg
  | (pp, arg) :: pps_and_args ->
      Fmt.pf ppf "%a%a%a" pp arg pp_sep () (pp_print_assoclist ~pp_sep) pps_and_args

let make_assoc l1 l2 = List.map2 (fun a b -> (a, b)) l1 l2

let pp ppf t =
  let stats_without_names = List.map (fun (_name, mstats) -> mstats) t in
  let pps = modules |> List.map (fun (module M : Common) -> M.pp) in
  let pps_and_args = make_assoc pps stats_without_names in
  let pp_sep ppf () = Format.pp_print_break ppf 1 0 in
  Fmt.pf ppf "%a" (pp_print_assoclist ~pp_sep) pps_and_args

type json_stat = { name : string; stat : Func.json_t } [@@deriving repr]

type json_module_stats = { modul : string; t : json_stat list } [@@deriving repr]

type json_stats = json_module_stats list [@@deriving repr]

let pp_json = Repr.pp_json json_stats_t

let pp_json ppf t =
  let json_of_stat (name, stat) = { name; stat = Func.of_t stat } in
  let json_stats =
    List.map (fun (modul, name_stats) -> { modul; t = List.map json_of_stat name_stats }) t
  in
  pp_json ppf json_stats

module Miscellaneous = struct
  type density = { mutable n_samples : int; mutable average : float }

  type t = { density : float }

  let dens = { n_samples = 0; average = 0. }

  let add_density_sample d =
    let avg =
      let n = Float.of_int dens.n_samples in
      ((n *. dens.average) +. d) /. (n +. 1.)
    in
    dens.average <- avg;
    dens.n_samples <- succ dens.n_samples

  let get () = { density = dens.average }
end
