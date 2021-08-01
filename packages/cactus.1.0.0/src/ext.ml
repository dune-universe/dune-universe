include Ext_intf

let ( // ) a b = a ^ "/" ^ b

module Make_ext (InKey : Input.Key) (InValue : Input.Value) (Size : Input.Size) :
  S with type key = InKey.t and type value = InValue.t = struct
  type key = InKey.t

  type value = InValue.t

  module Params = Params.Make (Size) (InKey) (InValue)
  module Common = Field.MakeCommon (Params)
  module Entry = Data.Make (InKey) (InValue)
  module Key = Entry.Key
  module Value = Entry.Value
  module Store = Store.Make (Params) (Common)
  module Page = Store.Page
  module Leaf = Leaf.Make (Params) (Store) (Key) (Value)
  module Node = Node.Make (Params) (Store) (Key)
  module Recorder = Recorder.Make (InKey) (InValue)

  (** STAT WRAPPERS **)

  open Stats.Func
  open Stats.Btree

  let () = if Params.debug then Log.warn (fun reporter -> reporter "Debug mode is set.")

  let min_key = Key.min

  type t = { store : Store.t; mutable instances : int; recorder : Recorder.t option }

  type cache = (string, t) Hashtbl.t

  let caches = ref []

  let empty_cache () : cache =
    let cache = Hashtbl.create 10 in
    (* Use a list of caches to allow [empty_cache ()] to return a fresh
       cache. *)
    caches := cache :: !caches;
    cache

  let record t op =
    match t.recorder with Some recorder -> Recorder.record recorder (op ()) | None -> ()

  let flush t =
    record t (fun () -> Flush);
    Store.flush t.store

  let clear t =
    Log.debug (fun reporter -> reporter "clearing");
    Store.clear t.store;
    Leaf.init t.store (Store.root t.store) |> ignore;
    flush t

  let close t =
    Log.debug (fun reporter ->
        reporter "Closing a btree instance at root %s" (Store.Private.dir t.store));
    t.instances <- t.instances - 1;
    if t.instances = 0 then (
      Log.info (fun reporter -> reporter "Closing %s/b.tree" (Store.Private.dir t.store));
      Store.close t.store;
      Option.iter Recorder.close t.recorder;
      List.iter (fun cache -> Hashtbl.remove cache (Store.Private.dir t.store)) !caches)

  let snapshot ?(depth = 0) t =
    let rec snap_page path address =
      let page = Store.load t.store address in
      let kind = Page.kind page in
      if Common.Kind.to_depth kind >= depth then
        match Common.Kind.from_t kind with
        | Leaf ->
            let leaf = Leaf.load t.store address in
            let out_file = open_out (path // Fmt.str "leaf_%i.ansi" address) in
            let formatter = out_file |> Format.formatter_of_out_channel in
            Fmt.set_style_renderer formatter `Ansi_tty;
            Fmt.pf formatter "%a@." Leaf.pp leaf;
            close_out out_file;
            Store.release t.store
        | Node _n ->
            let node = Node.load t.store address in
            let out_file = open_out (path // Fmt.str "node_%i.ansi" address) in
            let formatter = out_file |> Format.formatter_of_out_channel in
            Fmt.set_style_renderer formatter `Ansi_tty;
            Fmt.pf formatter "%a@." (Node.pp |> Fmt.vbox) node;
            close_out out_file;
            Unix.mkdir (path // Fmt.str "%i" address) 0o777;
            Node.iter node (fun _key addr -> snap_page (path // Fmt.str "%i" address) addr)
    in
    snap_page (Store.Private.dir t.store) (Store.root t.store);
    let out_header = open_out (Store.Private.dir t.store // "header.ansi") in
    let formatter = out_header |> Format.formatter_of_out_channel in
    Fmt.set_style_renderer formatter `Ansi_tty;
    Fmt.pf formatter "%a@." Store.pp_header t.store;
    close_out out_header

  let length t =
    let rec aux address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf ->
          let leaf = Leaf.load t.store address in
          let ret = Leaf.length leaf in
          Store.release t.store;
          ret
      | Node _depth ->
          let node = Node.load t.store address in
          Node.fold_left (fun acc _key address -> acc + aux address) 0 node
    in
    let root = Store.root t.store in
    aux root

  let create ?cache ?record root =
    Log.info (fun reporter -> reporter "Btree version %i" Size.version);
    Log.debug (fun reporter -> reporter "Btree at root %s" root);
    let t =
      match cache with
      | Some cache when Hashtbl.mem cache root ->
          Log.debug (fun reporter -> reporter "root found in cache");
          let t = Hashtbl.find cache root in
          t.instances <- t.instances + 1;
          t
      | _ ->
          let just_load = Sys.file_exists (root ^ "/" ^ "b.tree") in
          let store = Store.init ~root in
          let recorder = match record with None -> None | Some path -> Some (Recorder.v path) in
          let t = { store; instances = 1; recorder } in
          if just_load then Log.debug (fun reporter -> length t |> reporter "Loading %i bindings")
          else (
            Leaf.init t.store (Store.root t.store) |> ignore;
            flush t);
          t
    in
    match cache with
    | Some cache ->
        Hashtbl.add cache root t;
        t
    | None -> t

  let rec go_to_leaf t key address =
    let page = Store.load t.store address in
    match Page.kind page |> Common.Kind.from_t with
    | Leaf -> address
    | Node _depth ->
        let node = Node.load t.store address in
        go_to_leaf t key (Node.find node key)

  let find t inkey =
    tic stat_find;
    let key = Key.of_input inkey in
    let go_to_leaf = go_to_leaf t key in
    let address = go_to_leaf (Store.root t.store) in
    let leaf = Leaf.load t.store address in
    let ret =
      try
        let ret = Leaf.find leaf key |> Value.to_input in
        record t (fun () -> Find (inkey, true));
        ret
      with Not_found ->
        record t (fun () -> Find (inkey, false));
        raise Not_found
    in
    Store.release t.store;
    tac stat_find;
    ret

  let mem t inkey =
    tic stat_mem;
    let key = Key.of_input inkey in
    let go_to_leaf = go_to_leaf t key in
    let address = go_to_leaf (Store.root t.store) in
    let leaf = Leaf.load t.store address in
    let ret = Leaf.mem leaf key in
    Store.release t.store;
    tac stat_mem;
    record t (fun () -> Mem (inkey, ret));
    ret

  let path_to_leaf t key =
    let rec aux path address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf -> address :: path
      | Node _depth ->
          let node = Node.load t.store address in
          aux (address :: path) (Node.find node key)
    in
    aux [] (Store.root t.store)

  (* The address of the neighbour of a vertex is its neighbour in the
     parent mapping. *)
  let path_to_leaf_with_neighbour t key =
    let rec aux path_with_neighbour address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf -> (address, path_with_neighbour)
      | Node _depth ->
          let node = Node.load t.store address in
          let neighbour = Node.find_with_neighbour node key in
          let next = neighbour.main |> snd in
          aux ((address, neighbour) :: path_with_neighbour) next
    in
    aux [] (Store.root t.store)

  let add t inkey invalue =
    tic stat_add;
    record t (fun () -> Add (inkey, invalue));
    Index_stats.incr_nb_replace ();
    let key = Key.of_input inkey in
    let value = Value.of_input invalue in
    let path = path_to_leaf t key in
    let leaf_address = List.hd path in

    let rec split_nodes nodes promoted allocated_address =
      match nodes with
      | [] ->
          (* This case happens only when no nodes are there in the first place
             *and* the leaf has overflowed. This means that the tree is a single
             leaf, and we have to create a new root on top of it. *)
          let root = Node.create t.store Common.Kind.(of_depth 1 |> from_t) in
          Node.add root min_key leaf_address;
          Node.add root promoted allocated_address;
          Store.reroot t.store (Node.self_address root);
          Log.info (fun reporter -> reporter "Btree height increases to 1")
      | [ address ] ->
          (* There are no nodes above : we are at the root. *)
          let root = Node.load t.store address in
          Node.add root promoted allocated_address;
          if Node.overflow root then (
            let promoted, allocated = Node.split root in
            let new_root =
              Node.create t.store Common.Kind.(of_depth (1 + Node.depth root) |> from_t)
            in
            Node.add new_root min_key address;
            Node.add new_root promoted (Node.self_address allocated);
            Store.reroot t.store (Node.self_address new_root);
            Log.info (fun reporter -> reporter "Btree height increases to %i" (Node.depth new_root)))
      | address :: nodes ->
          let node = Node.load t.store address in
          Node.add node promoted allocated_address;
          if Node.overflow node then
            let promoted, allocated = Node.split node in
            split_nodes nodes promoted (Node.self_address allocated)
    in

    let leaf = Leaf.load t.store leaf_address in
    Leaf.add leaf key value;
    (if Leaf.overflow leaf then
     let promoted, allocated = Leaf.split leaf in
     split_nodes (List.tl path) promoted (Leaf.self_address allocated));
    Store.release t.store;
    tac stat_add

  module type MERGER = sig
    type t

    val load : Store.t -> Store.address -> t

    val leftmost : t -> Key.t

    val merge : t -> t -> [ `Partial | `Total ]
  end

  let choose_kind t address =
    match Store.load t.store address |> Page.kind |> Common.Kind.from_t with
    | Leaf -> (module Leaf : MERGER)
    | Node _ -> (module Node)

  let remove t inkey =
    let rec merges path =
      match path with
      | [] -> ()
      | (address, ({ main; neighbour; order } : Node.neighbour)) :: path -> (
          (* [main] and [neighbour] are entries of the node at address
             [address]. *)
          match neighbour with
          | None ->
              if not (path = []) then failwith "No neighbour";
              (* We are at the root, which contains only a single key and acts
                 as a mere redirection. We want to remove it and make its only
                 child the new root. *)
              Store.reroot t.store (snd main);
              Log.info (fun reporter ->
                  reporter "Btree height decreases to %i"
                    (Store.load t.store (snd main) |> Page.kind |> Common.Kind.to_depth))
          | Some neighbour ->
              let node = Node.load t.store address in
              let k1, address1 = main in
              let k2, address2 = neighbour in
              let module Merger = (val choose_kind t address1) in
              let v1, v2 = Merger.(load t.store address1, load t.store address2) in
              (match order with
              | `Lower -> (
                  match Merger.merge v2 v1 with
                  | `Partial -> Node.replace node k1 (Merger.leftmost v1)
                  | `Total -> Node.remove node k1)
              | `Higher -> (
                  match Merger.merge v1 v2 with
                  | `Partial -> Node.replace node k2 (Merger.leftmost v2)
                  | `Total -> Node.remove node k2));
              if Node.underflow node then merges path)
    in
    let key = Key.of_input inkey in
    let leaf_address, path = path_to_leaf_with_neighbour t key in
    let leaf = Leaf.load t.store leaf_address in
    Leaf.remove leaf key;
    if Leaf.underflow leaf then merges path;
    Store.release t.store

  let iter func t =
    let func key value = func (key |> Key.to_input) (value |> Value.to_input) in
    let rec aux address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf ->
          let leaf = Leaf.load t.store address in
          Leaf.iter leaf func;
          Store.release t.store
      | Node _depth ->
          let node = Node.load t.store address in
          Node.iter node (fun _key address -> aux address)
    in
    let root = Store.root t.store in
    aux root

  let iteri func t =
    let counter = ref 0 in
    let f key value =
      incr counter;
      func !counter key value
    in
    iter f t

  type total = { mutable add : int; mutable mem : int; mutable flush : int; mutable find : int }

  let bar ~message ~max total =
    let open Progress in
    let w = if max = 0 then 1 else float_of_int max |> log10 |> floor |> int_of_float |> succ in
    Line.(
      list
        [
          const message |> rpad 6;
          count_to total |> lpad ((2 * w) + 1);
          percentage_of total |> brackets;
          elapsed ();
          bar ~style:`UTF8 ~color:(`magenta |> Color.ansi) total;
          eta total |> brackets;
        ])

  let noop _ = ()

  let replay_ops ops t reporters =
    let r_add, r_find, r_mem, r_flush =
      match reporters with
      | [] -> (noop, noop, noop, noop)
      | [ reporter ] -> (reporter, reporter, reporter, reporter)
      | [ r1; r2; r3; r4 ] -> (r1, r2, r3, r4)
      | _ -> failwith "Unexpected number of reporters"
    in
    let off_add, off_find, off_mem, off_flush = (ref 0, ref 0, ref 0, ref 0) in
    let delta = 437 in
    Seq.iter
      (function
        | (op : Recorder.op) -> (
            match op with
            | Add (k, v) ->
                add t k v;
                incr off_add;
                if !off_add mod delta = 0 then (
                  r_add !off_add;
                  off_add := 0)
            | Mem (k, b) ->
                let b' = mem t k in
                assert (b' = b);
                incr off_mem;
                if !off_mem mod delta = 0 then (
                  r_mem !off_mem;
                  off_mem := 0)
            | Flush ->
                flush t;
                incr off_flush;
                if !off_flush mod delta = 0 then (
                  r_flush !off_flush;
                  off_flush := 0)
            | Find (k, b) ->
                (try
                   find t k |> ignore;
                   assert b
                 with Not_found -> assert (not b));
                incr off_find;
                if !off_find mod delta = 0 then (
                  r_find !off_find;
                  off_find := 0)))
      ops;
    r_add !off_add;
    r_mem !off_mem;
    r_flush !off_flush;
    r_find !off_find

  let count_ops seq =
    let tot = { add = 0; find = 0; mem = 0; flush = 0 } in
    let incr (op : Recorder.op) =
      match op with
      | Add _ -> tot.add <- succ tot.add
      | Find _ -> tot.find <- succ tot.find
      | Mem _ -> tot.mem <- succ tot.mem
      | Flush -> tot.flush <- succ tot.flush
    in
    Seq.iter incr seq;
    tot

  let replay path ?(prog = `None) t =
    let tot = Recorder.operations path |> count_ops in
    let seq = Recorder.operations path in
    let reporters =
      match prog with
      | `None -> Progress.Multi.lines []
      | `Single ->
          let sum = tot.add + tot.find + tot.mem + tot.flush in
          Progress.Multi.lines [ bar ~message:"Operations" ~max:sum sum ]
      | `Multiple ->
          let max = max tot.add @@ max tot.find @@ max tot.mem tot.flush in
          Progress.Multi.lines
            [
              bar ~message:"Add" ~max tot.add;
              bar ~message:"Find " ~max tot.find;
              bar ~message:"Mem  " ~max tot.mem;
              bar ~message:"Flush" ~max tot.flush;
            ]
    in
    let config = Progress.(Config.v ~min_interval:(Some (Duration.of_int_ms 200)) ()) in
    Progress.with_reporters ~config reporters (replay_ops seq t)

  let depth_of n =
    let rec aux h n = if n = 0 then h else aux (h + 1) (n / Params.fanout) in
    aux (-1) n

  let init ~root n ~read =
    Log.info (fun reporter -> reporter "Btree version %i" Size.version);
    let store = Store.init ~root in
    Log.info (fun reporter -> reporter "Initialising btree with %i bindings" n);

    let rec nvertices depth =
      match depth with
      | 0 -> 1
      | 1 -> Params.fanout
      | n -> (
          let sqrt = nvertices (depth / 2) in
          let sqrt2 = sqrt * sqrt in
          match n mod 2 with 0 -> sqrt2 | _ -> Params.fanout * sqrt2)
    in

    let sequentiate n depth =
      let step = nvertices depth in
      let steps = List.init (n / step) (fun _ -> step) in
      match n mod step with 0 -> steps | m -> steps @ [ m ]
    in

    let add content = Store.Private.write store content in

    let depth = depth_of n in

    let address = ref (Store.root store - 1) in
    Store.Private.init_migration store;

    let get_address () =
      let open Common.Address in
      let buff = Bytes.create size in
      !address |> to_t |> set ~marker:Utils.nop buff ~off:0;
      Bytes.to_string buff
    in
    let rec create leftmost depth n =
      match depth with
      | 0 ->
          incr address;
          let kvs = List.init n (fun _ -> read 1) in
          let k_dump = String.sub (List.hd kvs) 0 Params.key_sz in
          let content = Leaf.migrate kvs in
          let pad = Params.page_sz - String.length content in
          if pad < 0 then (
            Fmt.pr "Assertion error : [page size] must be at least %i@." (String.length content);
            assert false);
          add (content ^ String.make pad '\000');
          k_dump
      | _ ->
          let ns = sequentiate n depth in
          assert (n = List.fold_left ( + ) 0 ns);
          let kvs =
            List.mapi
              (fun i n ->
                let k_dump = create (leftmost && i = 0) (depth - 1) n in
                let address_dump = get_address () in
                (if leftmost && i = 0 then min_key |> Key.dump else k_dump) ^ address_dump)
              ns
          in
          let content = Node.migrate kvs Common.Kind.(of_depth depth |> from_t) in
          let pad = Params.page_sz - String.length content in
          incr address;
          add (content ^ String.make pad '\000');
          String.sub (List.hd kvs) 0 Params.key_sz
    in

    create true depth n |> ignore;
    Store.Private.end_migration store (!address + 1) !address;
    incr address;
    { store; instances = 1; recorder = None }

  type reconstruct_elem = { address : Common.Address.t; min_key : Key.t }

  let reconstruct root =
    (* Reconstruct the btree, assuming that only the leaves are not
       corrupted. *)
    let prepare_store t =
      (* Deallocate all non-leaf page and evaluates to the list of leaf
         addresses. *)
      let leaves = ref [] in
      let () =
        Store.iter t.store @@ fun address page ->
        match Store.Page.kind page |> Common.Kind.from_t with
        | Leaf -> leaves := address :: !leaves
        | Node _ -> Store.deallocate t.store address
      in
      !leaves
    in

    let leaf_arr t leaves =
      let ret =
        Array.of_list leaves
        |> Array.map @@ fun address ->
           {
             address = Common.Address.to_t address;
             min_key =
               (let key = Leaf.load t.store address |> Leaf.leftmost in
                Store.release t.store;
                key);
           }
      in
      Array.sort (fun e1 e2 -> Key.compare e1.min_key e2.min_key) ret;
      ret
    in

    let rec reconstruct t depth arr =
      (* Reconstruct the level [depth] of the tree, given the sorted array [arr]
         of elements at the level below. *)
      let kind = Common.Kind.of_depth depth |> Common.Kind.from_t in
      let width =
        max 1 ((Array.length arr / Params.fanout) + min 1 (Array.length arr mod Params.fanout))
        (* This is ceil (length / fanout). *)
      in
      let next_arr =
        Array.init width @@ fun i ->
        let nentry = min Params.fanout (Array.length arr - (Params.fanout * i)) in
        let kvs =
          List.init nentry (fun j ->
              let elem = arr.((Params.fanout * i) + j) in
              let key = if i = 0 && j = 0 then min_key else elem.min_key in
              (key, elem.address |> Common.Address.from_t))
        in
        let address = Store.allocate t.store in
        let node = Node.load t.store address in
        Node.reconstruct node kind kvs;
        let ret = { address = Common.Address.to_t address; min_key = Node.leftmost node } in
        Store.release t.store;
        ret
      in
      match width with
      | 1 -> Store.reroot t.store (Common.Address.from_t next_arr.(0).address)
      | _ -> reconstruct t (depth + 1) next_arr
    in

    Log.info (fun reporter -> reporter "Btree version %i" Size.version);
    Log.debug (fun reporter -> reporter "Btree at root %s" root);
    let just_load = Sys.file_exists (root ^ "/" ^ "b.tree") in
    let store = Store.init ~root in
    let t = { store; instances = 1; recorder = None } in
    if just_load then (
      Log.debug (fun reporter -> reporter "Launching the recovery process");
      prepare_store t |> leaf_arr t |> reconstruct t 1;
      Log.debug (fun reporter -> length t |> reporter "Loading %i bindings"))
    else (
      Leaf.init t.store (Store.root t.store) |> ignore;
      flush t);
    t

  let pp ppf t =
    Fmt.pf ppf "@[<hov 2>ROOT OF THE TREE:@;%a@]" Leaf.pp (Leaf.load t.store (Store.root t.store))

  module Private = struct
    let dir t = Store.Private.dir t.store

    let root t = Store.root t.store

    let store t = t.store

    let cache_size t = Store.Private.cache_size t.store

    let pp t ppf address =
      let page = Store.load t.store address in
      match Page.kind page |> Common.Kind.from_t with
      | Leaf ->
          let leaf = Leaf.load t.store address in
          Fmt.set_style_renderer ppf `Ansi_tty;
          Fmt.pf ppf "%a@." Leaf.pp leaf
      | Node _n ->
          let node = Node.load t.store address in
          Fmt.set_style_renderer ppf `Ansi_tty;
          Fmt.pf ppf "%a@." (Node.pp |> Fmt.vbox) node

    let go_to_leaf t inkey =
      let key = Key.of_input inkey in
      let rec aux t key address acc =
        let page = Store.load t.store address in
        match Page.kind page |> Common.Kind.from_t with
        | Leaf -> address :: acc
        | Node _depth ->
            let node = Node.load t.store address in
            aux t key (Node.find node key) (address :: acc)
      in
      aux t key (Store.root t.store) []

    module Params = Params
    module Common = Common
    module Entry = Entry
    module Key = Key
    module Value = Value
    module Store = Store
    module Page = Page
    module Leaf = Leaf
    module Node = Node
  end
end
