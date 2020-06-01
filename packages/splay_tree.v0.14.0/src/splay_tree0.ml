open Core_kernel
open Int.Replace_polymorphic_compare
include Splay_tree0_intf

module Make_with_reduction
    (Key : Key)
    (Data : Data)
    (R : Reduction_operation with type key = Key.t and type data = Data.t) =
struct
  type key = Key.t [@@deriving sexp]
  type data = Data.t [@@deriving sexp]
  type accum = R.accum

  (* [Kernel] ensures that all [Node]s
     - are annotated with the correct size, and
     - are annotated with the right accumulator, based on their children
  *)
  module Kernel : sig
    (** tree size *)
    type size

    type t = private
      | Empty
      | Node of
          { left : t
          ; key : key
          ; data : data
          ; right : t
          ; size : size
          ; accum : accum
          }

    val length : t -> int
    val node : t -> key -> data -> t -> t
    val empty : t
    val accum : t -> accum
  end = struct
    type size = int

    type t =
      | Empty
      | Node of
          { left : t
          ; key : key
          ; data : data
          ; right : t
          ; size : int
          ; accum : accum
          }


    let length = function
      | Empty -> 0
      | Node { size; _ } -> size
    ;;

    let accum = function
      | Empty -> R.identity
      | Node { accum; _ } -> accum
    ;;

    let node left key data right =
      Node
        { left
        ; key
        ; data
        ; right
        ; size = length left + length right + 1
        ; accum =
            R.combine (R.combine (accum left) (R.singleton ~key ~data)) (accum right)
        }
    ;;

    let empty = Empty
  end

  module Tree = struct
    include Kernel

    let is_empty = function
      | Empty -> true
      | Node _ -> false
    ;;

    (* [ctx] represents the positions of a subtree within its containing tree. You can
       also think of the context like a phantom node placeholder within the larger tree.
    *)
    type ctx =
      | Top
      | Fst of ctx * key * data * t
      | Snd of t * key * data * ctx

    (* [plug t ctx] restores the overall tree from the subtree [t] and its context [ctx].

       NOTE: this definition is used nowhere in the remainder of this file. It serves
       only to indicate what a context /means/.
    *)
    let rec _plug t = function
      | Top -> t
      | Fst (ctx, k, v, r) -> _plug (node t k v r) ctx
      | Snd (l, k, v, ctx) -> _plug (node l k v t) ctx
    ;;

    (* Traverse tree downwards, converting parents into ctx *)
    (* The downward tree traversal methods find a subtree and its [ctx] (position with the
       larger tree. If no subtree is returned, it is assumed to be the empty tree. One can
       reconstruct the original tree by [_plug t ctx] or splay the found node to the root
       with [splay_to_tree (ctx, t)]. *)

    let find_ctx : t -> key -> ctx * t =
      fun t x ->
        let rec loop ctx this =
          match this with
          | Empty -> ctx, this
          | Node { left; key; data; right; _ } ->
            let cmp = Key.compare x key in
            if cmp < 0
            then loop (Fst (ctx, key, data, right)) left
            else if cmp > 0
            then loop (Snd (left, key, data, ctx)) right
            else ctx, this
        in
        loop Top t
    ;;

    let find_leftmost_ctx t =
      let rec loop t ctx =
        match t with
        | Empty -> ctx
        | Node { left; key; data; right; _ } -> loop left (Fst (ctx, key, data, right))
      in
      loop t Top
    ;;

    let find_rightmost_ctx t =
      let rec loop ctx t =
        match t with
        | Empty -> ctx
        | Node { left; key; data; right; _ } -> loop (Snd (left, key, data, ctx)) right
      in
      loop Top t
    ;;

    let nth_ctx t i =
      let rec loop ctx this i =
        match this with
        | Empty -> ctx, this
        | Node { left; key; data; right; _ } ->
          let lsize = length left in
          if i < lsize
          then loop (Fst (ctx, key, data, right)) left i
          else if i = lsize
          then ctx, this
          else (* i > lsize *)
            loop (Snd (left, key, data, ctx)) right (i - lsize - 1)
      in
      loop Top t i
    ;;

    let search_ctx t ~f =
      let rec loop ctx this ~left:left_ctx ~right:right_ctx =
        match this with
        | Empty -> ctx, this
        | Node { left; key; data; right; _ } ->
          let left_combined = R.combine left_ctx (accum left) in
          let right_combined = R.combine right_ctx (accum right) in
          let right_with_node = R.combine (R.singleton ~key ~data) right_combined in
          (match f ~left:left_combined ~right:right_with_node with
           | `Left ->
             loop (Fst (ctx, key, data, right)) left ~left:left_ctx ~right:right_with_node
           | `Right ->
             let left_with_node = R.combine left_combined (R.singleton ~key ~data) in
             (match f ~left:left_with_node ~right:right_combined with
              | `Left -> ctx, this
              | `Right ->
                loop
                  (Snd (left, key, data, ctx))
                  right
                  ~left:left_with_node
                  ~right:right_ctx))
      in
      loop Top t ~left:R.identity ~right:R.identity
    ;;

    (* Traverse tree upwards, converting ctx to parents *)
    (* [splay l r ctx = (l', r')] performs the splay operation.

       It pulls a phantom node [x] from its position at [ctx] up to the
       top of the tree by doing double and single rotations.

       :  (ctx)       (top)
       :   ...
       :   [x]   ==>   [x]
       :   / \         / \
       :  l   r       l'  r'
    *)
    let rec splay l r = function
      | Top -> l, r
      | Fst (Top, y, yv, c) ->
        let a = l in
        let b = r in
        (*
           :      y         [x]
           :     / \        / \
           :   [x]  c  =>  a   y
           :   / \            / \
           :  a   b          b   c
        *)
        a, node b y yv c
      | Snd (a, y, yv, Top) ->
        let b = l in
        let c = r in
        (*
           :      y             [x]
           :     / \            / \
           :    a  [x]   =>    y   c
           :   / \        / \
           :  b   c      a   b
        *)
        node a y yv b, c
      | Fst (Fst (ctx, z, zv, d), y, yv, c) ->
        let a = l in
        let b = r in
        (*
           :        z         [x]
           :       / \        / \
           :      y   d      a   y
           :     / \     =>     / \
           :   [x]  c          b   z
           :   / \                / \
           :  a   b              c   d
        *)
        splay a (node b y yv (node c z zv d)) ctx
      | Snd (b, y, yv, Snd (a, z, zv, ctx)) ->
        let c = l in
        let d = r in
        (*
           :        z                 [x]
           :       / \                / \
           :      a   y              y   d
           :     / \     =>     / \
           :    b  [x]         z   c
           :   / \        / \
           :  c   d      a   b
        *)
        splay (node (node a z zv b) y yv c) d ctx
      | Snd (a, y, yv, Fst (ctx, z, zv, d)) | Fst (Snd (a, y, yv, ctx), z, zv, d) ->
        let b = l in
        let c = r in
        (*
           :        z                             y
           :       / \           [x]             / \
           :      y   d         /   \           a   z
           :     / \     =>    y     z    <=       / \
           :    a  [x]        / \   / \          [x]  d
           :   / \       a   b c   d         / \
           :  b   c                         b   c
        *)
        splay (node a y yv b) (node c z zv d) ctx
    ;;

    (* Splay a node up to the root *)
    let splay_node l k v r ctx =
      let l, r = splay l r ctx in
      node l k v r
    ;;

    (* Splay an empty leaf up, letting its parent become the root *)
    let splay_empty ctx =
      match ctx with
      | Top -> empty
      | Fst (ctx, k, v, r) -> splay_node empty k v r ctx
      | Snd (l, k, v, ctx) -> splay_node l k v empty ctx
    ;;

    (* Splay the phantom node to the root, splitting the tree into the left side with
       lesser keys and the right side with greater keys *)
    let splay_to_triple found =
      let inject v (left, right) = left, v, right in
      match found with
      | ctx, Node { left; key; data; right; _ } ->
        inject (Some (key, data)) (splay left right ctx)
      | ctx, Empty -> inject None (splay empty empty ctx)
    ;;

    (* Splay the phantom node to the root, splitting off the greater keys as the right
       side *)
    let splay_split_leq_gt = function
      | ctx, Node { left; key; data; right; _ } ->
        let l, r = splay left right ctx in
        node l key data empty, r
      | ctx, Empty -> splay empty empty ctx
    ;;

    (* Splay the phantom node to the root, splitting off the lesser keys as the left side
    *)
    let splay_split_lt_geq = function
      | ctx, Node { left; key; data; right; _ } ->
        let l, r = splay left right ctx in
        l, node empty key data r
      | ctx, Empty -> splay empty empty ctx
    ;;

    (* Splay the tree back together with the found node at the root *)
    let splay_to_tree = function
      | ctx, Node { left; key; data; right; _ } -> splay_node left key data right ctx
      | ctx, Empty -> splay_empty ctx
    ;;

    (* Splay the tree back together, but keep aside a reference to the node contents, if
       any *)
    let splay_to_result = function
      | ctx, Node { left; key; data; right; _ } ->
        splay_node left key data right ctx, Some (key, data)
      | ctx, Empty -> splay_empty ctx, None
    ;;

    (* Point-wise mutation operations *)

    let set t ~key ~data =
      let l, _, r = find_ctx t key |> splay_to_triple in
      node l key data r
    ;;

    let add t ~key ~data =
      match find_ctx t key |> splay_to_triple with
      | l, None, r -> Some (node l key data r)
      | _, Some _, _ -> None
    ;;

    let remove_min t =
      match find_leftmost_ctx t with
      | Top -> None
      | Snd _ ->
        (* find_leftmost_ctx only accumulates Top and Fst constructors *)
        assert false
      | Fst (ctx, x, xv, right) ->
        (match splay empty right ctx with
         | Empty, r -> Some (x, xv, r)
         | Node _, _ ->
           (* when [ctx] contains only Top and Fst constructors, as it
              does here since it was returned by [find_leftmost_ctx], then
              [fst (splay Empty t ctx)] will always be [Empty] for all [t]. *)
           assert false)
    ;;

    let remove_max t =
      match find_rightmost_ctx t with
      | Top -> None
      | Fst _ ->
        (* find_rightmost_ctx only accumulates Top and Snd constructors *)
        assert false
      | Snd (left, x, xv, ctx) ->
        (match splay left empty ctx with
         | l, Empty ->
           (* order reversed here to give the same type as [remove_min] *)
           Some (x, xv, l)
         | _, Node _ ->
           (* when [ctx] contains only Top and Snd constructors, as it
              does here since it was returned by [find_rightmost_ctx], then
              [snd (splay Empty t ctx)] will always be [Empty] for all [t]. *)
           assert false)
    ;;

    let concat_unchecked left right =
      match remove_min right with
      | None -> left
      | Some (x, xv, right) -> node left x xv right
    ;;

    let concat_value_unchecked left key data right =
      match data with
      | None -> concat_unchecked left right
      | Some data -> node left key data right
    ;;

    let concat_triple_unchecked left kv right =
      match kv with
      | None -> concat_unchecked left right
      | Some (k, v) -> node left k v right
    ;;

    let remove t k =
      match find_ctx t k with
      | ctx, Empty -> splay_empty ctx
      | ctx, Node { left; right; _ } ->
        (* Remove the node before splaying to reduce unnecessary node churn *)
        splay_to_tree (ctx, concat_unchecked left right)
    ;;

    (* [remove_after] and [remove_before] return an [Either.t] so that even if the removal
       fails, the tree can still benefit from the splay operation *)

    let remove_before t k =
      let before, at, after = find_ctx t k |> splay_to_triple in
      match remove_max before with
      | Some (res_k, res_v, before) ->
        First (res_k, res_v, concat_triple_unchecked before at after)
      | None -> Second (concat_triple_unchecked before at after)
    ;;

    let remove_after t k =
      let before, at, after = find_ctx t k |> splay_to_triple in
      match remove_min after with
      | Some (res_k, res_v, after) ->
        First (res_k, res_v, concat_triple_unchecked before at after)
      | None -> Second (concat_triple_unchecked before at after)
    ;;

    (* Folding *)

    let fold_right : 'b. t -> init:'b -> f:(key:key -> data:data -> 'b -> 'b) -> 'b =
      fun t ~init ~f ->
        let rec loop acc = function
          | [] -> acc
          | `Elem (key, data) :: to_visit -> loop (f ~key ~data acc) to_visit
          | `Tree Empty :: to_visit -> loop acc to_visit
          | `Tree (Node { left; key; data; right; _ }) :: to_visit ->
            loop acc (`Tree right :: `Elem (key, data) :: `Tree left :: to_visit)
        in
        loop init [ `Tree t ]
    ;;

    (* Querying *)

    let data t = fold_right t ~init:[] ~f:(fun ~key:_ ~data acc -> data :: acc)
    let keys t = fold_right t ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)

    let mem t x =
      let t, res = find_ctx t x |> splay_to_result in
      t, Option.is_some res
    ;;

    let find t x =
      let t, res = find_ctx t x |> splay_to_result in
      t, Option.map ~f:snd res
    ;;

    let nth t n = nth_ctx t n |> splay_to_result

    let rank t key =
      let t = find_ctx t key |> splay_to_tree in
      let result =
        match t with
        | Empty -> 0
        | Node { left; key = root; _ } ->
          if Key.compare root key < 0 then length left + 1 else length left
      in
      t, result
    ;;

    let search t ~f = search_ctx t ~f |> splay_to_result

    (* Conversions *)

    let to_alist t = fold_right t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc)

    let of_alist l =
      List.fold_result l ~init:empty ~f:(fun t (key, data) ->
        match add t ~key ~data with
        | None -> error_s [%message "Duplicate key" (key : Key.t)]
        | Some t -> Ok t)
    ;;

    let of_alist_exn l = Or_error.ok_exn (of_alist l)
    let t_of_sexp sexp = of_alist_exn ([%of_sexp: (key * data) list] sexp)

    let sexp_of_t t =
      Sexp.List
        (fold_right t ~init:[] ~f:(fun ~key ~data acc ->
           Sexp.List [ sexp_of_key key; sexp_of_data data ] :: acc))
    ;;

    (* Range mutation operations *)

    module Partition = struct
      type nonrec t =
        { lt : t
        ; mid : t
        ; gt : t
        }
    end

    let partition ?min_key ?max_key t =
      let lt, geq =
        match min_key with
        | None -> empty, t
        | Some min_key -> find_ctx t min_key |> splay_split_lt_geq
      in
      let mid, gt =
        match max_key with
        | None -> geq, empty
        | Some max_key -> find_ctx geq max_key |> splay_split_leq_gt
      in
      { Partition.lt; mid; gt }
    ;;

    let subrange ?min_key ?max_key t = (partition ?min_key ?max_key t).mid


    let rec merge left right ~f =
      match left, right with
      | Empty, Empty -> empty
      | Empty, Node { left; key; data; right; _ } ->
        let l' = merge ~f empty left in
        let v' = f ~key (`Right data) in
        let r' = merge ~f empty right in
        concat_value_unchecked l' key v' r'
      | Node { left; key; data; right; _ }, Empty ->
        let l' = merge ~f left empty in
        let v' = f ~key (`Left data) in
        let r' = merge ~f right empty in
        concat_value_unchecked l' key v' r'
      | Node { left = l1; key; data = v1; right = r1; _ }, Node _ ->
        let l2, kv2, r2 = find_ctx right key |> splay_to_triple in
        let v =
          match kv2 with
          | None -> `Left v1
          | Some (k2, v2) ->
            assert (Key.compare key k2 = 0);
            (* Sanity check *)
            `Both (v1, v2)
        in
        let l' = merge ~f l1 l2 in
        let v' = f ~key v in
        let r' = merge ~f r1 r2 in
        concat_value_unchecked l' key v' r'
    ;;

    let split t k =
      let l, v, r = find_ctx t k |> splay_to_triple in
      l, Option.map ~f:snd v, r
    ;;

    let join l r =
      match find_rightmost_ctx l, remove_min r with
      | _, None -> Ok l
      | Top, _ -> Ok r
      | Fst _, _ ->
        (* [find_rightmost_ctx] never creates [Fst] constructors *)
        assert false
      | Snd (_, k1, _, _), Some (k2, v2, r) ->
        if Key.compare k1 k2 >= 0
        then
          error_s
            [%message
              "Trees were overlapping" ~left_max:(k1 : key) ~right_min:(k2 : key)]
        else Ok (node l k2 v2 r)
    ;;

    let join_exn l r = Or_error.ok_exn (join l r)

    (* Mapping *)
    (* this is in CPS so that it is tail-recursive *)
    let rec map_cps : 'r. t -> f:(data -> data) -> (t -> 'r) -> 'r =
      fun t ~f k ->
      match t with
      | Empty -> k empty
      | Node { left; key; data; right; _ } ->
        map_cps left ~f (fun l ->
          let data = f data in
          map_cps right ~f (fun r -> k (node l key data r)))
    ;;

    let map t ~f = map_cps t ~f Fn.id

    let map_range t ~min_key ~max_key ~f =
      let old_range, t =
        let { Partition.lt; mid; gt } = partition ~min_key ~max_key t in
        to_alist mid, concat_unchecked lt gt
      in
      let new_range = f old_range in
      List.fold new_range ~init:t ~f:(fun t (key, data) -> set t ~key ~data)
    ;;
  end

  module T : sig
    type t [@@deriving sexp]

    val create : Tree.t -> t
    val update : t -> Tree.t * 'a -> 'a
    val pack : t -> Tree.t -> t
    val unpack : t -> Tree.t
  end = struct
    type t = { mutable tree : Tree.t } [@@deriving sexp]

    let create tree = { tree }
    let pack (_ : t) tree = { tree }

    let update t (tree, res) =
      t.tree <- tree;
      res
    ;;

    let unpack t = t.tree
  end

  open Tree
  include T

  let empty = create empty
  let of_alist l = Or_error.map ~f:create (of_alist l)
  let of_alist_exn l = create (of_alist_exn l)
  let to_alist t = to_alist (unpack t)
  let is_empty t = is_empty (unpack t)
  let length t = length (unpack t)
  let accum t = accum (unpack t)
  let keys t = keys (unpack t)
  let data t = data (unpack t)
  let mem t k = update t (mem (unpack t) k)
  let find t k = update t (find (unpack t) k)
  let set t ~key ~data = pack t (set (unpack t) ~key ~data)
  let remove t k = pack t (remove (unpack t) k)

  let pack_remove t = function
    | None -> None
    | Some (a, b, tree) -> Some (a, b, pack t tree)
  ;;

  let remove_min t = pack_remove t (remove_min (unpack t))
  let remove_max t = pack_remove t (remove_max (unpack t))

  let pack_remove_either t = function
    | First (a, b, tree) -> Some (a, b, pack t tree)
    | Second tree -> update t (tree, None)
  ;;

  let remove_after t k = pack_remove_either t (remove_after (unpack t) k)
  let remove_before t k = pack_remove_either t (remove_before (unpack t) k)
  let map t ~f = pack t (map (unpack t) ~f)

  let map_range t ~min_key ~max_key ~f =
    pack t (map_range (unpack t) ~min_key ~max_key ~f)
  ;;

  let nth t idx = update t (nth (unpack t) idx)
  let rank t key = update t (rank (unpack t) key)
  let search t ~f = update t (search (unpack t) ~f)

  module Partition = struct
    type nonrec t =
      { lt : t
      ; mid : t
      ; gt : t
      }
  end

  let partition ?min_key ?max_key t =
    let { Tree.Partition.lt; mid; gt } = partition ?min_key ?max_key (unpack t) in
    { Partition.lt = pack t lt; mid = pack t mid; gt = pack t gt }
  ;;

  let subrange ?min_key ?max_key t = pack t (subrange ?min_key ?max_key (unpack t))

  let split t k =
    let l, v, r = split (unpack t) k in
    pack t l, v, pack t r
  ;;

  let merge a b ~f = create (merge ~f (unpack a) (unpack b))
  let join a b = Or_error.map ~f:create (join (unpack a) (unpack b))
  let join_exn a b = create (join_exn (unpack a) (unpack b))
end

module Make_without_reduction (Key : Key) (Data : Data) :
  S with type key = Key.t and type data = Data.t and type accum = unit =
  Make_with_reduction (Key) (Data)
    (struct
      type key = Key.t
      type data = Data.t
      type accum = unit

      let identity = ()
      let singleton ~key:_ ~data:_ = ()
      let combine () () = ()
    end)

module Reduction_operations = struct
  let reduce2
        (type k d a b)
        (module R1 : Reduction_operation
          with type key = k
           and type data = d
           and type accum = a)
        (module R2 : Reduction_operation
          with type key = k
           and type data = d
           and type accum = b)
    =
    (module struct
      type key = k
      type data = d
      type accum = a * b

      let identity = R1.identity, R2.identity
      let singleton ~key ~data = R1.singleton ~key ~data, R2.singleton ~key ~data
      let combine (l1, l2) (r1, r2) = R1.combine l1 r1, R2.combine l2 r2
    end : Reduction_operation
      with type key = k
       and type data = d
       and type accum = a * b)
  ;;
end
