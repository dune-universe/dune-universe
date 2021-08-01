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

include Cache_intf

module Make
    (K : Hashtbl.HashedType) (V : sig
      type t
    end) =
struct
  type key = K.t

  type value = V.t

  module Lru = struct
    module V = struct
      type t = V.t

      let weight _ = 1
    end

    include Lru.M.Make (K) (V)

    let clear t =
      let cap = capacity t in
      resize 0 t;
      trim t;
      resize cap t
  end

  module Hashtbl = Hashtbl.Make (K)

  type t = {
    california : value Hashtbl.t;
    lru : Lru.t;
    volatile : value Hashtbl.t;
    flush : key -> value -> unit;
    load : ?available:value -> key -> value;
    mutable filter : value -> [ `California | `Lru | `Volatile ];
  }

  let v ~flush ~load ~filter lru_cap =
    {
      flush;
      load;
      california = Hashtbl.create 4096;
      lru = Lru.create lru_cap;
      volatile = Hashtbl.create 16;
      filter;
    }

  (* Emit a warning when the lru if filled. *)
  let lru_filled =
    let flag = ref true in
    fun () ->
      if !flag then (
        Log.warn (fun reporter -> reporter "LRU is filled");
        flag := false)

  module Queue = struct
    include Queue

    let push =
      let lost_count = ref 0 in
      fun v q ->
        if length q < 128 then push v q
        else (
          incr lost_count;
          if !lost_count mod 1_000 = 0 then
            Log.warn (fun reporter -> reporter "%i buffers lost" !lost_count))
  end

  let reusable_buffer_pool = Queue.create ()

  let length t = Hashtbl.length t.california + Hashtbl.length t.volatile + Lru.size t.lru

  (* Remove the least recently used value, flush it and reuse the buffer. *)
  let remove_lru_and_reuse t =
    match Lru.lru t.lru with
    | Some (key, value) ->
        t.flush key value;
        Queue.push value reusable_buffer_pool;
        Lru.drop_lru t.lru
    | None -> failwith "Empty LRU"

  let find t key =
    match
      (Hashtbl.find_opt t.california key, Lru.find key t.lru, Hashtbl.find_opt t.volatile key)
    with
    | Some value, None, None | None, None, Some value -> value
    | None, Some value, None ->
        Lru.promote key t.lru;
        value
    | None, None, None ->
        let value =
          match Queue.is_empty reusable_buffer_pool with
          | true -> t.load key
          | false -> t.load ~available:(Queue.pop reusable_buffer_pool) key
        in
        (match t.filter value with
        | `California -> Hashtbl.add t.california key value
        | `Lru ->
            Lru.add key value t.lru;
            while Lru.weight t.lru > Lru.capacity t.lru do
              lru_filled ();
              remove_lru_and_reuse t
            done
        | `Volatile ->
            Hashtbl.add t.volatile key value;
            if Hashtbl.length t.volatile > 64 then (
              Log.warn (fun reporter -> reporter "Not enough release");
              assert false));
        value
    | _ -> failwith "Key loaded in several caches"

  let reload t key =
    match
      (Hashtbl.find_opt t.california key, Lru.find key t.lru, Hashtbl.find_opt t.volatile key)
    with
    | Some value, None, None -> (
        match t.filter value with
        | `California -> ()
        | `Lru ->
            Hashtbl.remove t.california key;
            Lru.add key value t.lru
        | `Volatile ->
            Hashtbl.remove t.california key;
            Hashtbl.add t.volatile key value)
    | None, Some value, None -> (
        match t.filter value with
        | `California ->
            Lru.remove key t.lru;
            Hashtbl.add t.california key value
        | `Lru -> Lru.promote key t.lru
        | `Volatile ->
            Lru.remove key t.lru;
            Hashtbl.add t.volatile key value)
    | None, None, Some value -> (
        match t.filter value with
        | `California ->
            Hashtbl.remove t.volatile key;
            Hashtbl.add t.california key value
        | `Lru ->
            Hashtbl.remove t.volatile key;
            Lru.add key value t.lru
        | `Volatile -> ())
    | None, None, None -> failwith "Key is not loaded"
    | _ -> failwith "Key loaded in several caches"

  let update_filter t ~filter =
    t.filter <- filter;
    Hashtbl.filter_map_inplace
      (fun key value ->
        if filter value = `California then Some value
        else (
          t.flush key value;
          None))
      t.california;
    Lru.iter (fun k v -> t.flush k v) t.lru;
    Lru.clear t.lru

  let release t =
    Hashtbl.iter
      (fun k v ->
        Queue.push v reusable_buffer_pool;
        t.flush k v)
      t.volatile;
    Hashtbl.clear t.volatile

  let deallocate t key =
    Hashtbl.remove t.california key;
    Lru.remove key t.lru;
    Hashtbl.remove t.volatile key

  let clear t =
    Hashtbl.clear t.volatile;
    Hashtbl.clear t.california;
    Lru.clear t.lru

  let flush t =
    Hashtbl.iter t.flush t.california;
    Lru.iter t.flush t.lru;
    release t
end
