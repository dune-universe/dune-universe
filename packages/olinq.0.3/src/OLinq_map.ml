
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 Polymorphic Maps and Multimaps} *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a iter = ('a -> unit) -> unit

type ('a, +'b) t = {
  is_empty : unit -> bool;
  size : unit -> int; (* Number of keys *)
  get_exn : 'a -> 'b;
  iter : ('a -> 'b -> unit) -> unit;
  fold : 'c. ('c -> 'a -> 'b -> 'c) -> 'c -> 'c;
  choose: (unit -> ('a * 'b) option);
}

type ('a, +'b) map = ('a, 'b) t

let get_exn m x = m.get_exn x

let get m x =
  try Some (m.get_exn x)
  with Not_found -> None

let mem m x =
  try ignore (m.get_exn x); true
  with Not_found -> false

let to_iter m yield = m.iter (fun k v -> yield (k,v))

let to_iter_multimap m yield =
  m.iter (fun k vs -> List.iter (fun v -> yield (k,v)) vs)

let get_seq key m yield = match get m key with
  | None -> ()
  | Some x -> yield x

let iter m = m.iter

let fold f acc m = m.fold f acc

let fold_multimap f acc m =
  m.fold (fun acc x l -> List.fold_left (fun acc y -> f acc x y) acc l) acc

let size m = m.size ()

module Build = struct
  type ('a, 'b) t = {
    cur : unit -> ('a, 'b) map;
    add : 'a -> 'b -> unit;
    update : 'a -> f:('b -> 'b) -> or_:'b -> unit;
  }

  let get b = b.cur ()
  let add b x y = b.add x y
  let update b f = b.update f

  let add_multimap b x y = update b x ~f:(fun l -> y::l) ~or_:[y]

  let add_count b x = update b x ~f:succ ~or_:1

  (* careful to use this map linearly *)
  let of_hash (type key) ?(eq=(=)) ?(hash=Hashtbl.hash) ?(size=128) () =
    let module H = Hashtbl.Make(struct
        type t = key
        let equal = eq
        let hash = hash
      end) in
    (* build table *)
    let tbl = H.create size in
    let cur () = {
      is_empty = (fun () -> H.length tbl = 0);
      size = (fun () -> H.length tbl);
      get_exn = (fun k -> H.find tbl k);
      fold = (fun f acc -> H.fold (fun k v acc -> f acc k v) tbl acc);
      iter = (fun k -> H.iter (fun key v -> k key v) tbl);
      choose = (fun () ->
        let r = ref None in
        (try H.iter (fun k v -> r := Some (k,v); raise Exit) tbl with Exit -> ());
        !r);
    } in
    { cur;
      add = (fun k v -> H.replace tbl k v);
      update =
        (fun k ~f ~or_ ->
          try
            let v = f (H.find tbl k) in
            H.replace tbl k v
          with Not_found -> H.add tbl k or_);
    }

  let of_cmp (type key) ?(cmp=Pervasives.compare) () =
    let module M = Map.Make(struct
        type t = key
        let compare = cmp
      end) in
    let of_map map = {
      is_empty = (fun () -> M.is_empty map);
      size = (
        let size = lazy (M.cardinal map) in
        fun () -> Lazy.force size);
      get_exn = (fun k -> M.find k map);
      fold =
        (fun f acc ->
          M.fold
            (fun key set acc -> f acc key set) map acc);
      iter = (fun k -> M.iter k map);
      choose = (fun () -> try Some (M.choose map) with Not_found -> None);
    } in
    let map = ref M.empty in
    let cur () = of_map !map in
    {
      cur;
      add = (fun k v -> map := M.add k v !map);
      update =
        (fun k ~f ~or_ ->
          try
            let v = f (M.find k !map) in
            map := M.add k v !map
          with Not_found ->
            map := M.add k or_ !map);
    }

  type 'a src =
    | Cmp of 'a ord
    | Hash of 'a equal * 'a hash * int
    | Default

  let of_src = function
    | Default -> of_hash ()
    | Cmp cmp -> of_cmp ~cmp ()
    | Hash (eq,hash,size) -> of_hash ~eq ~hash ~size ()

  (* choose a build method from the optional arguments *)
  let src_of_args ?cmp ?eq ?hash () =
    let _maybe default o = match o with
      | Some x -> x
      | None -> default
    in
    match eq, hash with
    | Some _, _
    | _, Some _ ->
        Hash ( _maybe (=) eq, _maybe Hashtbl.hash hash, 128)
    | _ ->
        match cmp with
        | Some f -> Cmp f
        | _ -> Default

  let make ?cmp ?eq ?hash () =
    let src = src_of_args ?cmp ?eq ?hash () in
    of_src src
end

let of_iter ?(src=Build.Default) seq =
  let build = Build.of_src src in
  seq (fun (k,v) -> Build.add_multimap build k v);
  Build.get build

let of_list ?(src=Build.Default) l =
  let build = Build.of_src src in
  List.iter (fun (k,v) -> Build.add_multimap build k v) l;
  Build.get build

let count_seq ?(src=Build.Default) seq =
  let build = Build.of_src src in
  seq (fun x -> Build.add_count build x);
  Build.get build

(* map values *)
let map f m = {
  is_empty = m.is_empty;
  size = m.size;
  get_exn = (fun k -> f (m.get_exn k));
  iter = (fun k -> m.iter (fun x y -> k x (f y)));
  fold = (fun f' acc ->
      m.fold (fun acc x y -> f' acc x (f y)) acc
    );
  choose = (fun () ->
    match m.choose () with None -> None | Some (k,v) -> Some (k, f v)
  );
}

let choose m = m.choose()

let to_rev_list m = m.fold (fun acc k v -> (k,v) :: acc) []

let to_list m = List.rev (to_rev_list m)

let reverse ?(src=Build.Default) m =
  let build = Build.of_src src in
  m.iter (fun k v -> Build.add_multimap build v k);
  Build.get build

let reverse_multimap ?(src=Build.Default) m =
  let build = Build.of_src src in
  m.iter
    (fun k vs ->
      List.iter
        (fun v -> Build.add_multimap build v k)
        vs);
  Build.get build

let flatten m yield =
  m.iter (fun k vs -> vs (fun v -> yield (k,v)))

let flatten_l = to_iter_multimap
