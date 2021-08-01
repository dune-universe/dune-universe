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

(* No overflow is allowed in this implementation *)

include Vertex_intf

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Value : VALUE)
  ->
  struct
    type store = Store.t

    type address = Store.address

    module Common = Field.MakeCommon (Params)
    module Page = Store.Page
    module Header = Vertex_header.Make (Params) (Store) (Common)

    module Isdead = Field.MakeBool (struct
      let size = 1
    end)

    (* STAT WRAPPERS *)
    open Stats.Func
    open Stats.Nodes

    type t = { store : store; header : Header.t; buff : bytes; marker : unit -> unit }

    let depth =
      match Value.kind with
      | `Leaf -> ( function _ -> 0)
      | `Node -> ( function t -> Header.g_kind t.header |> Common.Kind.to_depth)

    (* [nentry t] includes the number of dead entries. *)
    let nentry t = Header.g_nentry t.header |> Header.Nentry.from_t

    let ndeadentry t = Header.g_ndeadentry t.header |> Header.Ndeadentry.from_t

    let flag_sz, key_sz, value_sz = (Isdead.size, Params.key_sz, Value.size)

    let entry_sizes = [ flag_sz; key_sz; value_sz ]

    let entry_size = List.fold_left ( + ) 0 entry_sizes

    type offsets = { flag : int; key : int; value : int }

    let offsets =
      match Utils.sizes_to_offsets entry_sizes with
      | [ flag; key; value ] -> { flag; key; value }
      | _ -> failwith "Incorrect offsets"

    let available_size = Params.page_sz - Header.size

    let () =
      if Params.version = 2 && available_size / entry_size < 2 * Params.fanout then (
        Fmt.pr "Page size must be at least %i@." (Header.size + (2 * Params.fanout * entry_size));
        assert false)

    (* The nth_* functions below assume that [n] is not greater than [nentry t]. *)
    let nth_key t n = Key.get t.buff ~off:(Header.size + (n * entry_size) + offsets.key)

    let nth_dead t n =
      Isdead.get t.buff ~off:(Header.size + (n * entry_size) + offsets.flag) |> Isdead.from_t

    let nth_value t n = Value.get t.buff ~off:(Header.size + (n * entry_size) + offsets.value)

    let density t =
      let n = nentry t - ndeadentry t |> Float.of_int in
      let scale = Params.fanout |> Float.of_int in
      (n /. scale) -. 1.

    let load store address =
      tic stat_load;
      let page = Store.load store address in
      let buff = Page.buff page in
      let marker = Page.marker page in
      let header = Header.load ~marker buff in
      tac stat_load;
      let t = { store; buff; header; marker } in
      let d = density t in
      if d > 0. (* don't sample on the root *) then
        Stats.Miscellaneous.add_density_sample (density t);
      t

    module PP = struct
      open Fmt

      let pp_entry ppf ~off buff =
        let color = match Value.kind with `Leaf -> `Blue | `Node -> `Cyan in
        pf ppf "@[<hov 1>dead:@ %a%,%a@]@;@[<hov 1>key:@ %a@]@;@[<hov 1>value:@ %a@]"
          (Isdead.pp_raw ~off:(off + offsets.flag) |> styled (`Bg `Red))
          buff
          (Isdead.pp |> styled (`Bg `Red) |> styled `Reverse)
          (Isdead.get buff ~off:(off + offsets.flag))
          (*-*)
          Key.pp
          (Key.get buff ~off:(off + offsets.key))
          (*-*)
          (Value.pp |> styled (`Bg color) |> styled `Reverse)
          (Value.get buff ~off:(off + offsets.value))

      let pp ppf t =
        let offs = List.init (nentry t) (fun i -> Header.size + (i * entry_size)) in
        let pp_entries ppf offs =
          List.iteri (fun i off -> Fmt.pf ppf "@[<v 2>%i:@;%a@]@;" i (pp_entry ~off) t.buff) offs
        in
        Fmt.pf ppf "@[<v 2>Header:@;%a@]@;@[<v 2>Content:@;%a@]" Header.pp t.header pp_entries offs
    end

    include PP

    let create store kind address =
      tic stat_create;
      (* Initialises the header of a new vertex. We need the kind to recover the
         depth in the node. *)
      assert (
        match (kind : Field.kind) with Node _ -> Value.kind = `Node | Leaf -> Value.kind = `Leaf);
      let page = Store.load store address in
      let buff = Page.buff page in
      let marker = Page.marker page in
      let header = Header.load ~marker buff in
      Header.init header kind;
      (* Now that the page is marked as node or leaf, we reload it in the
         correct cache. *)
      Store.reload store address;
      tac stat_create;
      { store; header; buff; marker }

    let compare t key n = Key.compare key (nth_key t n)

    (* Returns 0 if key belong to interval [n, n+1[, <0 if its in a lower
       interval and >0 if it's in a higher interval. Use this when looking for
       an interval for the key. *)
    let compare_interval t key n =
      let comp = Key.compare key (nth_key t n) in
      if comp <= 0 then comp
      else if n = nentry t - 1 then 0
      else if Key.compare key (nth_key t (n + 1)) < 0 then 0
      else comp

    let keys_sorted t =
      let ret = List.init (nentry t) (fun i -> nth_key t i |> Key.dump) in
      Utils.is_sorted ret

    (* [shrink t] launches a garbage collection process that shrinks the size of
       [t] to a minimum, by reclaiming the space of all dead entries in the
       vertex. *)
    let shrink t =
      let n = nentry t in
      let rec aux src dst =
        if src < n then
          match nth_dead t src with
          | true -> aux (src + 1) dst
          | false ->
              Bytes.blit t.buff
                (Header.size + (src * entry_size))
                t.buff
                (Header.size + (dst * entry_size))
                entry_size;
              aux (src + 1) (dst + 1)
      in
      if ndeadentry t > 0 then (
        aux 0 0;
        Header.s_nentry t.header (nentry t - ndeadentry t |> Header.Nentry.to_t);
        Header.s_ndeadentry t.header (0 |> Header.Ndeadentry.to_t))

    let split t address =
      (* TODO : we cannot reconstruct the tree if the system crashes between
         flushing the node and its split. *)
      tic stat_split;
      shrink t;

      (* The promoted key [promoted] here acts as a pivot to separate the keys
         remaining in the current vertex from those that are moved to a newly
         allocated vertex. *)
      let promoted_rank = nentry t / 2 in
      let promoted = nth_key t promoted_rank in

      let mv_t = create t.store (Header.g_kind t.header |> Common.Kind.from_t) address in

      let mv_nentry = nentry t - promoted_rank in
      Bytes.blit t.buff
        (Header.size + (promoted_rank * entry_size))
        mv_t.buff Header.size
        (Header.size + (mv_nentry * entry_size));

      mv_nentry |> Header.Nentry.to_t |> Header.s_nentry mv_t.header;
      promoted_rank |> Header.Nentry.to_t |> Header.s_nentry t.header;

      if Params.debug then (
        assert (keys_sorted mv_t);
        assert (keys_sorted t));

      tac stat_split;
      (promoted, mv_t)

    let leftmost t = Key.get t.buff ~off:(Header.size + offsets.key)

    let find_alive_neighbour t n =
      let top = nentry t - 1 in
      let bottom = 0 in
      let rec aux current direction =
        if current < bottom || current > top then None
        else if not (nth_dead t current) then Some current
        else aux (current + direction) direction
      in
      (aux (n - 1) (-1), aux (n + 1) 1)

    let find_n t key =
      let compare =
        match Value.kind with `Leaf -> compare t key | `Node -> compare_interval t key
      in
      let n = Utils.binary_search ~compare 0 (nentry t) in
      if nth_dead t n then
        match Value.kind with
        | `Leaf -> raise Not_found
        | `Node -> (
            (* find the nearest left alive neighbour *)
            match find_alive_neighbour t n |> fst with None -> raise Not_found | Some n -> n)
      else n

    let find t key =
      tic stat_find;
      let n = find_n t key in
      tac stat_find;
      nth_value t n

    type with_neighbour = {
      main : Key.t * Value.t;
      neighbour : (Key.t * Value.t) option;
      order : [ `Lower | `Higher ];
    }

    let find_with_neighbour t key =
      tic stat_find;
      let n = find_n t key in
      let m =
        match find_alive_neighbour t n with
        | None, None -> None
        | None, Some m | Some m, None -> Some m
        | Some _left, Some right -> Some right
        (* TODO : use a good heuristic for choosing neighbour*)
      in
      let neighbour = match m with None -> None | Some m -> Some (nth_key t m, nth_value t m) in
      let order = match m with Some m when m < n -> `Lower | _ -> `Higher in
      tac stat_find;
      { main = (nth_key t n, nth_value t n); neighbour; order }

    let mem t key =
      tic stat_mem;
      match Value.kind with
      | `Node -> invalid_arg "Operation not supported for nodes"
      | `Leaf ->
          let ret =
            if nentry t = 0 then false
            else
              let compare = compare t key in
              let n = Utils.binary_search ~safe:true ~compare 0 (nentry t) in
              Key.equal (nth_key t n) key && not (nth_dead t n)
          in
          tac stat_mem;
          ret

    let find_position t key =
      let compare = compare t key in
      if nentry t = 0 || compare (nentry t - 1) > 0 then nentry t
      else
        let position = Utils.binary_search ~safe:true ~compare 0 (nentry t) in
        if compare position <= 0 then position else position + 1

    let shift t position =
      tic stat_shift;
      let length = (nentry t - position) * entry_size in
      Bytes.blit t.buff
        (Header.size + (entry_size * position))
        t.buff
        (Header.size + (entry_size * (position + 1)))
        length;
      tac stat_shift

    let add t key value =
      tic stat_add;
      let position = find_position t key in
      let shadow = Key.equal (nth_key t position) key in
      let append = position >= nentry t in
      if not (shadow || append) then shift t position;

      let off = Header.size + (position * entry_size) in
      Isdead.to_t false |> Isdead.set ~marker:t.marker t.buff ~off:(off + offsets.flag);
      key |> Key.set ~marker:t.marker t.buff ~off:(off + offsets.key);
      value |> Value.set ~marker:t.marker t.buff ~off:(off + offsets.value);

      if append || not shadow then Header.s_nentry t.header (nentry t + 1 |> Header.Nentry.to_t);
      if nentry t > 2 * Params.fanout then shrink t;
      if Params.debug then assert (keys_sorted t);
      tac stat_add

    (* This function is only used in the context of a deletion, after a merge,
       to update the separator key. *)
    let replace t k1 k2 =
      let n = find_n t k1 in
      k2 |> Key.set ~marker:t.marker t.buff ~off:(Header.size + (n * entry_size) + offsets.key);
      if
        Key.compare k1 k2 < 0 && n < nentry t - 1 && Key.compare k2 (nth_key t (n + 1)) > 0
        (* Sorted invariant is broken. [shrink] is fixing the invariant if key
           n+1 is dead. *)
      then shrink t;
      if
        Key.compare k1 k2 > 0 && n > 0 && Key.compare k2 (nth_key t (n - 1)) < 0
        (* Sorted invariant is broken. [shrink] is fixing the invariant if key
           n-1 is dead. *)
      then shrink t;
      if Params.debug then assert (keys_sorted t)

    let remove t key =
      (* Because we want to remove exactly [key] we are using [compare] instead of
         [compare_interval] here. *)
      let compare = compare t key in
      let n = Utils.binary_search ~compare 0 (nentry t) in
      if nth_dead t n then raise Not_found;
      let off = Header.size + (n * entry_size) in
      Isdead.to_t true |> Isdead.set ~marker:t.marker t.buff ~off:(off + offsets.flag);
      Header.s_ndeadentry t.header (ndeadentry t + 1 |> Header.Ndeadentry.to_t)

    let merge t1 t2 mode =
      shrink t1;
      shrink t2;
      let n1 = nentry t1 in
      let n2 = nentry t2 in
      let () =
        match mode with
        | `Total ->
            Header.s_nentry t1.header (n1 + n2 |> Header.Nentry.to_t);
            Header.s_nentry t2.header (0 |> Header.Nentry.to_t);
            Bytes.blit t2.buff Header.size t1.buff
              (Header.size + (n1 * entry_size))
              (n2 * entry_size)
        | `Partial ->
            let diff = (n2 - n1) / 2 in
            Header.s_nentry t1.header (n1 + diff |> Header.Nentry.to_t);
            Header.s_nentry t2.header (n2 - diff |> Header.Nentry.to_t);
            if n2 > n1 then (
              Bytes.blit t2.buff Header.size t1.buff
                (Header.size + (n1 * entry_size))
                (diff * entry_size);
              Bytes.blit t2.buff
                (Header.size + (diff * entry_size))
                t2.buff Header.size
                ((n2 - diff) * entry_size))
            else (
              Bytes.blit t2.buff Header.size t2.buff
                (Header.size - (diff * entry_size))
                (n2 * entry_size);
              Bytes.blit t1.buff
                (Header.size + ((n1 + diff) * entry_size))
                t2.buff Header.size (-diff * entry_size))
      in
      if Params.debug then assert (keys_sorted t1 && keys_sorted t2)

    let length t = nentry t - ndeadentry t

    let header_of_depth i =
      let open Header in
      let buff = Bytes.make size '\000' in
      let header = load ~marker:Utils.nop buff in
      let kind : Field.kind = if i = 0 then Leaf else Node i in
      init header kind;
      s_nentry header @@ Nentry.to_t @@ Params.fanout;
      Bytes.to_string buff

    let migrate_headers = Array.init 10 (fun i -> header_of_depth (1 + i))

    let migrate_leaf_header = header_of_depth 0

    let header_of_depth depth =
      if depth = 0 then migrate_leaf_header
      else if depth <= 10 then migrate_headers.(depth - 1)
      else header_of_depth depth

    let migrate_header kind n =
      let depth = kind |> Common.Kind.to_t |> Common.Kind.to_depth in
      if n = Params.fanout then header_of_depth depth
      else
        let open Header in
        let buff = Bytes.make size '\000' in
        let header = load ~marker:Utils.nop buff in
        init header kind;
        s_nentry header @@ Nentry.to_t @@ n;
        Bytes.to_string buff

    let migrate kvs kind =
      assert (
        match (kind : Field.kind) with Leaf -> Value.kind = `Leaf | Node _ -> Value.kind = `Node);
      let not_dead = Bytes.create Isdead.size in
      Isdead.to_t false |> Isdead.set ~marker:Utils.nop not_dead ~off:0;
      let not_dead = Bytes.to_string not_dead in
      let kvs = List.map (( ^ ) not_dead) kvs in
      let header = migrate_header kind (List.length kvs) in
      String.concat "" (header :: kvs)

    let reconstruct t kind kvs =
      List.iteri
        (fun i (key, value) ->
          let off = Header.size + (i * entry_size) in
          Isdead.to_t false |> Isdead.set ~marker:t.marker t.buff ~off:(off + offsets.flag);
          key |> Key.set ~marker:t.marker t.buff ~off:(off + offsets.key);
          value |> Value.set ~marker:t.marker t.buff ~off:(off + offsets.value))
        kvs;
      Header.init t.header kind;
      Header.s_nentry t.header (List.length kvs |> Header.Nentry.to_t);
      Header.s_ndeadentry t.header (0 |> Header.Ndeadentry.to_t)

    let iter t func =
      for i = 0 to nentry t - 1 do
        if not (nth_dead t i) then func (nth_key t i) (nth_value t i)
      done

    let fold_left func acc t =
      List.init (nentry t) (fun i -> (nth_key t i, nth_value t i)) |> List.fold_left func acc
  end

module LeafMake (Params : Params.S) (Store : Store.S) (Key : Data.K) (Value : Data.V) = struct
  include
    Make (Params) (Store) (Key)
      (struct
        include Value

        let kind = `Leaf
      end)
end

module NodeMake (Params : Params.S) (Store : Store.S) (Key : Data.K) = struct
  module CommonFields = Field.MakeCommon (Params)

  include
    Make (Params) (Store) (Key)
      (struct
        include CommonFields.Address

        let kind = `Node
      end)
end
