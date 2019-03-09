
(* This file is free software, part of OLinq. See file "license" for more details. *)

(** {1 LINQ-like operations on collections} *)

type 'a iter = ('a -> unit) -> unit
type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int
type 'a hash = 'a -> int
type 'a or_error = [`Ok of 'a | `Error of string ]
type 'a printer = Format.formatter -> 'a -> unit

let id_ x = x
let (|>) x f = f x

exception IExit

let seq_map ~f seq yield = seq (fun x -> yield (f x))
let seq_filter ~f seq yield = seq (fun x -> if f x then yield x)
let seq_filter_map ~f seq yield =
  seq (fun x -> match f x with None -> () | Some y -> yield y)
let seq_fold f acc seq =
  let r = ref acc in
  seq (fun x -> r := f !r x);
  !r
let seq_len seq = seq_fold (fun n _ -> n+1) 0 seq
let seq_of_list l yield = List.iter yield l
let seq_to_list seq = seq_fold (fun l x->x::l) [] seq |> List.rev
let seq_exists f seq =
  try seq (fun x -> if f x then raise IExit); false
  with IExit -> true
let seq_head seq =
  let r = ref None in
  try seq (fun x -> r := Some x; raise IExit); None
  with IExit -> !r

module M = OLinq_map
module Vec = OLinq_vec

type ('a, 'b) map = ('a, 'b) OLinq_map.t

type 'a search_result =
  | SearchContinue
  | SearchStop of 'a

type ('a,'b,'key,'c) join_descr = {
  join_key1 : 'a -> 'key;
  join_key2 : 'b -> 'key;
  join_merge : 'key -> 'a -> 'b -> 'c option;
  join_build_src : 'key M.Build.src;
}

type ('a,'b,'key,'c) outer_join_descr = {
  ojoin_key1 : 'a -> 'key;
  ojoin_key2 : 'b -> 'key;
  ojoin_merge : 'key -> 'a list -> 'b list -> 'c option;
  ojoin_build_src : 'key M.Build.src;
}

type ('a,'b) group_join_descr = {
  gjoin_proj : 'b -> 'a;
  gjoin_build_src : 'a M.Build.src;
}

type ('a,'b) search_descr = {
  search_check: ('a -> 'b search_result);
  search_failure : 'b;
}

module Iterable = struct
  type 'a t =
    | I_list of 'a list
    | I_vec of 'a Vec.t
    | I_iter of 'a iter 
    | I_set : ('a, unit) M.t -> 'a t
    | I_map : ('a, 'b) M.t -> ('a * 'b) t
    | I_multimap : ('a, 'b list) M.t -> ('a * 'b) t
    | I_range : int * int -> int t
    | I_string : string -> char t

  let to_iter
    : type a. a t -> a iter
    = function
      | I_vec v -> Vec.to_iter v
      | I_list l -> (fun k -> List.iter k l)
      | I_iter s -> s
      | I_set m -> (fun k -> M.to_iter m (fun (x,()) -> k x))
      | I_map m -> M.to_iter m
      | I_multimap m -> M.to_iter_multimap m
      | I_range (i,j) ->
          (fun yield ->
             if i<=j then for k = i to j do yield k done
             else for k = i downto j do yield k done)
      | I_string s -> (fun yield -> String.iter yield s)

  let empty = I_list []
  let range i j = I_range (i,j)
  let return x = I_list [x]
  let of_list l = I_list l
  let of_vec v = I_vec v
  let of_iter s = I_iter s
  let of_set m = I_set m
  let of_map m = I_map m
  let of_multimap m = I_multimap m
  let of_string s = I_string s

  let of_opt = function
    | None -> empty
    | Some x -> return x

  let choose
    : type a. a t -> a t
    = function
      | I_list [] -> empty
      | I_list (x::_) -> return x
      | I_iter s ->
        begin match seq_head s with None -> empty | Some x -> return x end
      | I_vec v -> if Vec.is_empty v then empty else return (Vec.get v 0)
      | I_set m -> M.choose m |> (function None -> empty | Some (x,()) -> return x)
      | I_map m -> M.choose m |> of_opt
      | I_multimap m ->
          begin match M.choose m with
            | Some (_, []) -> assert false
            | Some (x,y::_) -> return (x,y)
            | None -> empty
          end
      | I_range (i,_) -> return i
      | I_string "" -> empty
      | I_string s -> return s.[0]

  let to_vec
    : type a. a t -> a Vec.t
    = function
      | I_vec v -> v
      | i -> Vec.of_iter (to_iter i)

  let to_list
    : type a. a t -> a list
    = function
      | I_list l -> l
      | I_vec v -> Vec.to_list v
      | i -> to_iter i |> seq_fold (fun l x -> x::l) [] |> List.rev

  let mem
    : type a. eq:(a -> a -> bool) -> a t -> a -> bool
    = fun ~eq c x -> match c with
    | I_range (i,j) when i<=j -> i <= x && x <= j
    | I_range (i,j) -> j <= x && x <= i
    | I_list l -> List.exists (eq x) l
    | _ -> to_iter c |> seq_exists (eq x)

  let distinct ~cmp i =
    let build = M.Build.of_cmp ~cmp () in
    to_iter i
      (fun x -> M.Build.add build x ());
    of_set (M.Build.get build)

  (* c -> seq -> f -> seq *)
  let seq_seq_ ~f c = to_iter c |> f |> of_iter

  let map f c = seq_seq_ c ~f:(seq_map ~f)
  let filter f c = seq_seq_ c ~f:(seq_filter ~f)
  let filter_map f c = seq_seq_ c ~f:(seq_filter_map ~f)

  let flat_map f c =
    let v = Vec.create () in
    to_iter c (fun x -> Vec.append_iter v (f x));
    of_vec v

  (* TODO: add [to_seq] and use it *)
  let flat_map_seq f c =
    let v = Vec.create () in
    to_iter c (fun x -> Vec.append_seq v (f x));
    of_vec v

  let fold f acc i = to_iter i |> seq_fold f acc

  let head
    : type a. a t -> a option
    = function
    | I_range (i,_) -> Some i
    | I_list [] -> None
    | I_list (x::_) -> Some x
    | I_vec v -> if Vec.is_empty v then None else Some (Vec.get v 0)
    | i -> to_iter i |> seq_head

  let take
    : type a. int -> a t -> a t
    = fun n c -> match c with
    | _ when n=0 -> empty
    | I_range (i,j) when i<=j -> I_range (i, min j (i+n-1))
    | I_range (i,j) -> I_range (i, max j (i-n+1))
    | _ ->
      let v = Vec.create () in
      let i = ref n in
      begin
        try to_iter c (fun x -> if !i=0 then raise IExit; decr i; Vec.push v x)
        with IExit -> ()
      end;
      of_vec v

  let take_while p c =
    let v = Vec.create () in
    begin
      try to_iter c (fun x -> if not (p x) then raise IExit; Vec.push v x)
      with IExit -> ()
    end;
    of_vec v

  let sort ~cmp c =
    let l = to_list c in
    List.sort cmp l |> of_list

  let length
    : type a. a t -> int
    = function
      | I_range (i,j) -> abs (i-j)+1
      | I_vec v -> Vec.length v
      | I_iter seq -> seq_len seq
      | I_list l -> List.length l
      | I_set m -> M.size m
      | I_map m -> M.size m
      | I_multimap _ as i -> to_iter i |> seq_len
      | I_string s -> String.length s

  let search obj i =
    let r = ref None in
    try
      to_iter i
        (fun x -> match obj.search_check x with
           | SearchContinue -> ()
           | SearchStop y -> r := Some y; raise IExit);
      obj.search_failure
    with IExit ->
      match !r with
        | None -> assert false
        | Some x -> x

  let do_join ~join c1 c2 =
    let build1 =
      to_iter c1
      |> seq_map ~f:(fun x -> join.join_key1 x, x)
      |> M.of_iter ~src:join.join_build_src
    in
    let v = Vec.create () in
    to_iter c2
      (fun y ->
         let key = join.join_key2 y in
         match M.get build1 key with
           | None -> ()
           | Some l1 ->
               List.iter
                 (fun x -> match join.join_merge key x y with
                    | None -> ()
                    | Some res -> Vec.push v res)
                 l1);
    of_vec v

  (* all the pairs from left and right for a given key *)
  type ('a, 'b) ojoin_cell = {
    ojoin_left: 'a list;
    ojoin_right: 'b list;
  }

  let do_outer_join ~ojoin c1 c2 =
    let build = M.Build.of_src ojoin.ojoin_build_src in
    (* build the map [key -> cell] *)
    to_iter c1
      (fun x ->
         let k = ojoin.ojoin_key1 x in
         M.Build.update build k ~or_:{ojoin_left=[x]; ojoin_right=[]}
           ~f:(fun c -> {c with ojoin_left =x::c.ojoin_left }));
    to_iter c2
      (fun x ->
         let k = ojoin.ojoin_key2 x in
         M.Build.update build k ~or_:{ojoin_left=[]; ojoin_right=[x]}
           ~f:(fun c -> {c with ojoin_right=x::c.ojoin_right}));
    let m = M.Build.get build in
    let v = Vec.create () in
    M.to_iter m
      (fun (k,cell) ->
         match ojoin.ojoin_merge k cell.ojoin_left cell.ojoin_right with
           | None -> ()
           | Some res -> Vec.push v res);
    of_vec v

  let do_group_join ~gjoin c1 c2 =
    let build = M.Build.of_src gjoin.gjoin_build_src in
    to_iter c1 (fun x -> M.Build.add build x []);
    to_iter c2
      (fun y ->
         (* project [y] into some element of [c1] *)
         let x = gjoin.gjoin_proj y in
         M.Build.update build x ~or_:[] ~f:(fun l -> y::l));
    M.Build.get build

  let do_group_by ~src f c =
    let m = M.Build.of_src src in
    to_iter c
      (fun x ->
         let key = f x in
         M.Build.add_multimap m key x);
    M.Build.get m

  let do_count ~src c =
    let m = M.Build.of_src src in
    to_iter c
      (fun x -> M.Build.add_count m x);
    M.Build.get m

  let product a b =
    let v = Vec.create () in
    to_iter a
      (fun x -> to_iter b (fun y -> Vec.push v (x,y)));
    of_vec v

  let append a b =
    let v = to_vec a in
    to_iter b (Vec.push v);
    of_vec v

  let app a b =
    let v = Vec.create () in
    to_iter a
      (fun f -> to_iter b (fun x -> Vec.push v (f x )));
    of_vec v

  let do_union ~src c1 c2 =
    let build = M.Build.of_src src  in
    to_iter c1 (fun x -> M.Build.add build x ());
    to_iter c2 (fun x -> M.Build.add build x ());
    let m = M.Build.get build in
    of_set m

  type inter_status =
    | InterLeft
    | InterDone  (* already output *)

  let do_inter ~src c1 c2 =
    let build = M.Build.of_src src in
    let v = Vec.create() in
    to_iter c1 (fun x -> M.Build.add build x InterLeft);
    to_iter c2
      (fun x ->
        M.Build.update build x
          ~or_:InterDone
          ~f:(function
            | InterDone -> InterDone
            | InterLeft -> Vec.push v x; InterDone)
      );
    of_vec v

  let do_diff ~src c1 c2 =
    let build = M.Build.of_src src in
    to_iter c2 (fun x -> M.Build.add build x ());
    let map = M.Build.get build in
    (* output elements of [c1] not in [map] *)
    to_iter c1
      |> seq_filter ~f:(fun x -> not (M.mem map x))
      |> Vec.of_iter
      |> of_vec

  let do_subset ~src c1 c2 =
    let build = M.Build.of_src src in
    to_iter c2 (fun x -> M.Build.add build x ());
    let map = M.Build.get build in
    let res =
      try
        to_iter c1 (fun x -> if not (M.mem map x) then raise IExit);
        true
      with IExit -> false
    in
    return res
end

(** {2 Query operators} *)

type (_, _) unary =
  | Map : ('a -> 'b) -> ('a, 'b) unary
  | Filter : ('a -> bool) -> ('a, 'a) unary
  | Fold : ('b -> 'a -> 'b) * 'b -> ('a, 'b) unary
  | Size : ('a, int) unary
  | Choose : ('a, 'a) unary
  | FilterMap : ('a -> 'b option) -> ('a, 'b) unary
  | FlatMapSeq : ('a -> 'b Seq.t) -> ('a, 'b) unary
  | FlatMap : ('a -> 'b iter) -> ('a, 'b) unary
  | Take : int -> ('a, 'a) unary
  | TakeWhile : ('a -> bool) -> ('a, 'a) unary
  | Sort : 'a ord -> ('a, 'a) unary
  | SortBy : 'b ord * ('a -> 'b) -> ('a, 'a) unary
  | Distinct : 'a ord -> ('a, 'a) unary
  | Search : ('a, 'b) search_descr -> ('a, 'b) unary
  | Contains : 'a equal * 'a -> ('a, bool) unary
  | GroupBy :
      'b M.Build.src
      * ('a -> 'b)
      -> ('a, 'b * 'a list) unary
  | GroupBy_reflect :
      'b M.Build.src
      * ('a -> 'b)
      -> ('a, ('b, 'a list) M.t) unary
  | Count : 'a M.Build.src -> ('a, 'a * int) unary
  | Count_reflect : 'a M.Build.src -> ('a, ('a, int) M.t) unary
  | Lazy : ('a lazy_t, 'a) unary

type (_,_) set_op =
  | Union : ('a,'a) set_op
  | Inter : ('a,'a) set_op
  | Diff : ('a,'a) set_op
  | Subset : ('a, bool) set_op

type (_, _, _) binary =
  | App : ('a -> 'b, 'a, 'b) binary
  | Join :
      ('a, 'b, 'key, 'c) join_descr
      -> ('a, 'b, 'c) binary
  | OuterJoin :
      ('a, 'b, 'key, 'c) outer_join_descr
      -> ('a, 'b, 'c) binary
  | GroupJoin :
      ('a, 'b) group_join_descr
      -> ('a, 'b, 'a * 'b list) binary
  | GroupJoin_refl :
      ('a, 'b) group_join_descr
      -> ('a, 'b, ('a, 'b list) M.t) binary
  | Product : ('a, 'b, ('a*'b)) binary
  | Append : ('a, 'a, 'a) binary
  | SetOp :
      ('a, 'b) set_op * 'a M.Build.src
      -> ('a, 'a, 'b) binary

(* type of queries that return values of type ['a] *)
type 'a t_ =
  | Empty : _ t_
  | Return : 'a -> 'a t_
  | OfIterable : 'a Iterable.t -> 'a t_
  | Unary : ('a, 'b) unary * 'a t_ -> 'b t_
  | Binary : ('a, 'b, 'c) binary * 'a t_ * 'b t_ -> 'c t_
  | Bind : ('a -> 'b t_) * 'a t_ -> 'b t_
  | Reflect_vec : 'a t_ -> 'a Vec.t t_
  | Reflect_list : 'a t_ -> 'a list t_

(* type of queries, with an additional phantom parameter *)
type ('a, +'card) t = 'a t_ constraint 'card = [<`One | `AtMostOne | `Any]

type 'a t_any = ('a, [`Any]) t
type 'a t_one = ('a, [`One]) t
type 'a t_at_most_one = ('a, [`AtMostOne]) t

let of_list l = OfIterable (Iterable.of_list l)

let of_vec v = OfIterable (Iterable.of_vec v)

let of_iter s = OfIterable (Iterable.of_iter s)

let of_array a = of_vec (Vec.of_array a)

let of_array_i a =
  let v = Vec.init (Array.length a) (fun i -> i, a.(i)) in
  of_vec v

let of_hashtbl h =
  let m = M.of_iter (fun yield -> Hashtbl.iter (fun k v -> yield (k,v)) h) in
  OfIterable (Iterable.of_multimap m)

let range i j =
  OfIterable (Iterable.range i j)

let (--) = range

let of_iter seq = of_iter seq

let of_queue q = of_iter (fun yield -> Queue.iter yield q)

let of_stack s = of_iter (fun yield -> Stack.iter yield s)

let of_string s = OfIterable (Iterable.of_string s)

let of_map m = OfIterable (Iterable.of_map m)

let of_multimap m = OfIterable (Iterable.of_multimap m)

(** {6 Execution} *)

(* apply a unary operator on a collection *)
let do_unary : type a b. (a,b) unary -> a Iterable.t -> b Iterable.t
  = fun u c -> match u with
    | Map f -> Iterable.map f c
    | Filter p -> Iterable.filter p c
    | Fold (f, acc) -> Iterable.fold f acc c |> Iterable.return
    | Size -> Iterable.return (Iterable.length c)
    | Choose -> Iterable.choose c
    | FilterMap f -> Iterable.filter_map f c
    | FlatMapSeq f -> Iterable.flat_map_seq f c
    | FlatMap f -> Iterable.flat_map f c
    | Take n -> Iterable.take n c
    | TakeWhile p -> Iterable.take_while p c
    | Sort cmp -> Iterable.sort ~cmp c
    | SortBy (cmp,proj) -> Iterable.sort ~cmp:(fun a b -> cmp (proj a) (proj b)) c
    | Distinct cmp -> Iterable.distinct ~cmp c
    | Search obj -> Iterable.return (Iterable.search obj c)
    | GroupBy (src,f) -> Iterable.of_map (Iterable.do_group_by ~src f c)
    | GroupBy_reflect (src,f) -> Iterable.return (Iterable.do_group_by ~src f c)
    | Contains (eq, x) -> Iterable.return (Iterable.mem ~eq c x)
    | Count src -> Iterable.of_map (Iterable.do_count ~src c)
    | Count_reflect src -> Iterable.return (Iterable.do_count ~src c)
    | Lazy -> Iterable.map Lazy.force c

let do_binary
  : type a b c. (a, b, c) binary -> a Iterable.t -> b Iterable.t -> c Iterable.t
  = fun b c1 c2 -> match b with
    | Join join -> Iterable.do_join ~join c1 c2
    | OuterJoin ojoin -> Iterable.do_outer_join ~ojoin c1 c2
    | GroupJoin gjoin -> Iterable.of_map (Iterable.do_group_join ~gjoin c1 c2)
    | GroupJoin_refl gjoin -> Iterable.return (Iterable.do_group_join ~gjoin c1 c2)
    | Product -> Iterable.product c1 c2
    | Append -> Iterable.append c1 c2
    | App -> Iterable.app c1 c2
    | SetOp (Inter,src) -> Iterable.do_inter ~src c1 c2
    | SetOp (Union,src) -> Iterable.do_union ~src c1 c2
    | SetOp (Diff,src) -> Iterable.do_diff ~src c1 c2
    | SetOp (Subset,src) -> Iterable.do_subset ~src c1 c2

let rec run_ : type a. a t_ -> a Iterable.t
  = fun q -> match q with
    | Empty -> Iterable.empty
    | Return c -> Iterable.return c
    | Unary (u, q') -> do_unary u (run_ q')
    | Binary (b, q1, q2) -> do_binary b (run_ q1) (run_ q2)
    | OfIterable l -> l
    | Bind (f, q') ->
        let i = run_ q' in
        Iterable.flat_map
          (fun x ->
             let q'' = f x in
             run_ q'' |> Iterable.to_iter)
          i
    | Reflect_vec q ->
        Iterable.return (run_ q |> Iterable.to_vec)
    | Reflect_list q ->
        Iterable.return (run_ q |> Iterable.to_list)

let apply_limit ?limit q = match limit with
  | None -> q
  | Some l -> Unary (Take l, q)

(* safe execution *)
let run ?limit q =
  let q = apply_limit ?limit q in
  run_ q

let run_head q =
  apply_limit ~limit:1 q
  |> run_
  |> Iterable.head

let run1_exn q = match run_head q with
  | None -> raise Not_found
  | Some x -> x

(* never raises, by typing *)
let run1 = run1_exn

let run_list ?limit q = run ?limit q |> Iterable.to_list

let run_vec ?limit q = run ?limit q |> Iterable.to_vec

let run_array ?limit q =
  run ?limit q
  |> Iterable.to_vec
  |> Vec.to_array

(** {6 Basics} *)

let empty = Empty

let rec map
: type a b. (a -> b) -> a t_ -> b t_
= fun f q -> match q with
  | Binary (Append, q1, q2) -> Binary (Append, map f q1, map f q2)
  | Unary (Map f', q) -> map (fun x -> f (f' x)) q
  | Unary (Filter p, q) ->
      filter_map (fun x -> if p x then Some (f x) else None) q
  | _ -> Unary (Map f, q)

and filter_map
: type a b. (a -> b option) -> a t_ -> b t_
= fun f q -> match q with
  | Unary (Map f', q) -> filter_map (fun x -> f (f' x)) q
  | _ -> Unary (FilterMap f, q)

let (>|=) q f = map f q

let rec filter
: type a. (a -> bool) -> a t_ -> a t_
= fun p q -> match q with
  | Binary (Append, q1, q2) -> Binary (Append, filter p q1, filter p q2)
  | _ -> Unary (Filter p, q)

let flat_map_seq f q = Unary (FlatMapSeq f, q)
let flat_map_iter f q = Unary (FlatMap f, q)

let flat_map_l f q =
  let f' x = seq_of_list (f x) in
  flat_map_iter f' q

let flatten_seq q = flat_map_seq id_ q
let flatten_iter q = flat_map_iter id_ q

let flatten_list q = flat_map_iter seq_of_list q

let flatten_map q = flat_map_iter M.to_iter q

let flatten_multimap q = flat_map_iter M.to_iter_multimap q

let rec take
: type a. int -> a t_ -> a t_
  = fun n q ->
    if n<0 then invalid_arg "take";
    match q with
    | Unary (Map f, q) -> map f (take n q)
    | _ -> Unary (Take n, q)

let take1 q = take 1 q

let take_while p q = Unary (TakeWhile p, q)

let sort ?(cmp=Pervasives.compare) () q = Unary (Sort cmp, q)

let sort_by ?(cmp=Pervasives.compare) proj q = Unary (SortBy (cmp, proj), q)

let distinct ?(cmp=Pervasives.compare) () q = Unary (Distinct cmp, q)

let group_by_reflect ?cmp ?eq ?hash f q =
  let src = M.Build.src_of_args ?cmp ?eq ?hash () in
  Unary (GroupBy_reflect (src, f), q)

let group_by ?cmp ?eq ?hash f q =
  let src = M.Build.src_of_args ?cmp ?eq ?hash () in
  Unary (GroupBy (src, f), q)

let count_reflect ?cmp ?eq ?hash () q =
  let src = M.Build.src_of_args ?cmp ?eq ?hash () in
  Unary (Count_reflect src, q)

let count ?cmp ?eq ?hash () q =
  let src = M.Build.src_of_args ?cmp ?eq ?hash () in
  Unary (Count src, q)

let rec fold
: type a b. (a -> b -> a) -> a -> b t_ -> a t_
= fun f acc q -> match q with
  | Unary (Map f', q) -> fold (fun acc x -> f acc (f' x)) acc q
  | _ -> Unary (Fold (f, acc), q)

let rec size
: type a. a t_ -> int t_
= function
  | Unary (Choose, _) -> Return 1
  | Unary (Sort _, q) -> size q
  | Unary (SortBy _, q) -> size q
  | Unary (Map _, q) -> size q
  | q -> Unary (Size, q)

let sum q = Unary (Fold ((+), 0), q)

let _lift_some f x y = match y with
  | None -> Some x
  | Some y -> Some (f x y)

let max q = Unary (Fold (Pervasives.max, min_int), q)
let min q = Unary (Fold (Pervasives.min, max_int), q)
let average q =
  q
  |> fold (fun (sum,num) x -> x+sum, num+1) (0,0)
  |> map (fun (sum,num) -> sum/num)

let is_empty q =
  Unary
    (Search {
      search_check = (fun _ -> SearchStop false); (* stop in case there is an element *)
      search_failure = true;
    }, q)

let contains ?(eq=(=)) x q =
  Unary (Contains (eq, x), q)

let for_all p q =
  Unary
    (Search {
      search_check = (fun x -> if p x then SearchContinue else SearchStop false);
      search_failure = true;
    }, q)

let exists p q =
  Unary
    (Search {
      search_check = (fun x-> if p x then SearchStop true else SearchContinue);
      search_failure = false;
    }, q)

let find p q =
  Unary
    (Search {
      search_check = (fun x -> if p x then SearchStop (Some x) else SearchContinue);
      search_failure = None;
    }, q)

let find_map f q =
  Unary
    (Search {
      search_check =
        (fun x -> match f x with
          | Some y -> SearchStop (Some y)
          | None -> SearchContinue);
      search_failure = None;
    }, q)

(** {6 Binary Operators} *)

let join ?cmp ?eq ?hash join_key1 join_key2 ~merge q1 q2 =
  let join_build_src = M.Build.src_of_args ?eq ?hash ?cmp () in
  let j = {
    join_key1;
    join_key2;
    join_merge=merge;
    join_build_src;
  } in
  Binary (Join j, q1, q2)

let outer_join ?cmp ?eq ?hash ojoin_key1 ojoin_key2 ~merge q1 q2 =
  let ojoin_build_src = M.Build.src_of_args ?eq ?hash ?cmp () in
  let j = {
    ojoin_key1;
    ojoin_key2;
    ojoin_merge=merge;
    ojoin_build_src;
  } in
  Binary (OuterJoin j, q1, q2)

let group_join ?cmp ?eq ?hash gjoin_proj q1 q2 =
  let gjoin_build_src = M.Build.src_of_args ?eq ?hash ?cmp () in
  let j = {
    gjoin_proj;
    gjoin_build_src;
  } in
  Binary (GroupJoin j, q1, q2)

let group_join_reflect ?cmp ?eq ?hash gjoin_proj q1 q2 =
  let gjoin_build_src = M.Build.src_of_args ?eq ?hash ?cmp () in
  let j = {
    gjoin_proj;
    gjoin_build_src;
  } in
  Binary (GroupJoin_refl j, q1, q2)

let product q1 q2 = Binary (Product, q1, q2)

let append q1 q2 = Binary (Append, q1, q2)

let inter ?cmp ?eq ?hash q1 q2 =
  let build = M.Build.src_of_args ?cmp ?eq ?hash () in
  Binary (SetOp (Inter, build), q1, q2)

let union ?cmp ?eq ?hash q1 q2 =
  let build = M.Build.src_of_args ?cmp ?eq ?hash () in
  Binary (SetOp (Union, build), q1, q2)

let diff ?cmp ?eq ?hash q1 q2 =
  let build = M.Build.src_of_args ?cmp ?eq ?hash () in
  Binary (SetOp (Diff, build), q1, q2)

let subset ?cmp ?eq ?hash q1 q2 =
  let build = M.Build.src_of_args ?cmp ?eq ?hash () in
  Binary (SetOp (Subset, build), q1, q2)

let map_fst f q = map (fun (x,y) -> f x, y) q
let map_snd f q = map (fun (x,y) -> x, f y) q

let flatten_opt q = filter_map id_ q

exception UnwrapNone

let opt_unwrap_exn q =
  Unary
    (Map
       (function
         | Some x -> x
         | None -> raise UnwrapNone),
     q)

(** {6 Applicative} *)

let pure x = Return x

let app f x = match f, x with
  | Return f, Return x -> Return (f x)
  | Return f, _ -> map f x
  | f, Return x -> map (fun f -> f x) f
  | _ -> Binary (App, f, x)

let (<*>) = app

(** {6 Monadic stuff} *)

let return x = Return x

let flat_map f q = Bind (f,q)

let (>>=) x f = Bind (f, x)

(** {6 Misc} *)

let lazy_ q = Unary (Lazy, q)

(** {6 Others} *)

let rec choose
: type a. a t_ -> a t_
= function
  | Unary (Map f, q) -> map f (choose q)
  | Unary (Lazy, q) -> Unary (Lazy, choose q)
  | Binary (Product, q1, q2) ->
      let q1 = choose q1 and q2 = choose q2 in
      app (map (fun x y -> x,y) q1) q2
  | Binary (App, f, x) ->
      let q_f = choose f and q_x = choose x in
      app (map (fun f x -> f x) q_f) q_x
  | Unary (Fold _, _) as q -> q (* one solution *)
  | q -> Unary (Choose, q)

(** {6 Infix} *)

module Infix = struct
  let (>>=) = (>>=)
  let (>|=) = (>|=)
  let (<*>) = (<*>)
  let (--) = (--)
end

(** {6 Adapters} *)

let reflect_vec q = Reflect_vec q

let reflect_list q = Reflect_list q

let reflect_hashtbl q =
  let vec_to_tbl v =
    let h = Hashtbl.create (Pervasives.min 16 (Vec.length v)) in
    Vec.to_iter v (fun (k,v) -> Hashtbl.add h k v);
    h
  in
  map vec_to_tbl (reflect_vec q)

let reflect_queue q =
  let vec_to_q v =
    let q = Queue.create () in
    Vec.to_iter v (fun x -> Queue.push x q);
    q
  in
  map vec_to_q (reflect_vec q)

let reflect_stack q =
  let vec_to_s v =
    let s = Stack.create () in
    Vec.to_iter v (fun x -> Stack.push x s);
    s
  in
  map vec_to_s (reflect_vec q)

module AdaptSet(S : Set.S) = struct
  let of_set set = of_iter (fun k -> S.iter k set)

  let reflect q =
    let f c = Vec.fold (fun set x -> S.add x set) S.empty c in
    map f (reflect_vec q)

  let run q = run1 (reflect q)
end

module AdaptMap(M : Map.S) = struct
  let _to_iter m k = M.iter (fun x y -> k (x,y)) m

  let of_map map = of_iter (_to_iter map)

  let reflect q =
    let f c =
      Vec.fold (fun m (x,y) -> M.add x y m) M.empty c
    in
    map f (reflect_vec q)

  let run q = run1 (reflect q)
end

module IO = struct
  let read_all_ ~size ic =
    let buf = ref (Bytes.create size) in
    let len = ref 0 in
    try
      while true do
        (* resize *)
        if !len = Bytes.length !buf then (
          buf := Bytes.extend !buf 0 !len;
        );
        assert (Bytes.length !buf > !len);
        let n = input ic !buf !len (Bytes.length !buf - !len) in
        len := !len + n;
        if n = 0 then raise Exit;  (* exhausted *)
      done;
      assert false (* never reached*)
    with Exit -> Bytes.sub_string !buf 0 !len

  let slurp_ with_input =
    let l = lazy (with_input (fun ic -> read_all_ ~size:2048 ic)) in
    lazy_ (return l)

  let read_chan ic = slurp_ (fun f -> f ic)

  let finally_ f x ~h =
    try
      let res = f x in
      h();
      res
    with e ->
      h();
      raise e

  let with_in filename f =
    let ic = open_in filename in
    finally_ f ic ~h:(fun () -> close_in ic)

  let with_out filename f =
    let oc = open_out filename in
    finally_ f oc ~h:(fun () -> close_out oc)

  let read_file filename = slurp_ (with_in filename)

  (* find [c] in [s], starting at offset [i] *)
  let rec _find s c i =
    if i >= String.length s then None
    else if s.[i] = c then Some i
    else _find s c (i+1)

  let rec _lines s i k = match _find s '\n' i with
    | None ->
        if i<String.length s then k (String.sub s i (String.length s-i))
    | Some j ->
        let s' = String.sub s i (j-i) in
        k s';
        _lines s (j+1) k

  let lines q =
    (* sequence of lines *)
    let f s = _lines s 0 in
    flat_map_iter f q

  let lines_l q =
    let f s = lazy (seq_to_list (_lines s 0)) in
    lazy_ (map f q)

  let concat_ ~sep ?(stop="") v =
    let buf = Buffer.create 128 in
    Vec.iteri v
      ~f:(fun i x ->
         if i>0 then Buffer.add_string buf sep;
         Buffer.add_string buf x);
    Buffer.add_string buf stop;
    Buffer.contents buf

  let unlines q =
    let f l = lazy (concat_ ~sep:"\n" ~stop:"\n" l) in
    lazy_ (map f (reflect_vec q))

  let concat sep q =
    let f l = lazy (concat_ ~sep l) in
    lazy_ (map f (reflect_vec q))

  let out oc q =
    output_string oc (run1 q)

  let out_lines oc q =
    let i = run q in
    Iterable.to_iter i (fun l -> output_string oc l; output_char oc '\n')

  let to_file_exn filename q =
    with_out filename (fun oc -> out oc q)

  let to_file filename q =
    try `Ok (with_out filename (fun oc  -> out oc q))
    with Failure s -> `Error s

  let to_file_lines_exn filename q =
    with_out filename (fun oc -> out_lines oc q)

  let to_file_lines filename q =
    try `Ok (with_out filename (fun oc  -> out_lines oc q))
    with Failure s -> `Error s
end

let print ?(sep=", ") pp out q =
  let rec pp_list l = match l with
    | x::((_::_) as l) ->
      Format.fprintf out "%a%s@," pp x sep;
      pp_list l
    | x::[] -> pp out x
    | [] -> ()
  in
  let l = run_list q in
  pp_list l
