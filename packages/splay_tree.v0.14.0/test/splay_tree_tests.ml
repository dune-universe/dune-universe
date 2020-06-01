open Core_kernel
open Poly
open Splay_tree.Std

module Sum = struct
  type key = int
  type data = int
  type accum = int

  let identity = 0
  let singleton ~key:_ ~data = data
  let combine = ( + )
end

module T = Splay_tree.Make_with_reduction (Int) (Int) (Sum)


let alist_gen =
  List.quickcheck_generator
    (Quickcheck.Generator.tuple2 Int.quickcheck_generator Int.quickcheck_generator)
;;

let make_tree = List.fold ~init:T.empty ~f:(fun t (key, data) -> T.set t ~key ~data)

let tree_and_map =
  List.fold ~init:(T.empty, Int.Map.empty) ~f:(fun (t, m) (key, data) ->
    T.set t ~key ~data, Map.set m ~key ~data)
;;

let tree_and_map_gen = Quickcheck.Generator.map alist_gen ~f:tree_and_map

let tree_and_map_and_key_gen =
  Quickcheck.Generator.tuple3
    Bool.quickcheck_generator
    Int.quickcheck_generator
    tree_and_map_gen
  |> Quickcheck.Generator.map ~f:(fun (should_floor, key, (t, m)) ->
    let key =
      (* using [should_floor] makes it likely that we sometimes pick a key that belongs to
         the splay_tree/map *)
      if should_floor
      then
        Map.closest_key m `Less_or_equal_to key
        |> Option.value_map ~f:fst ~default:key
      else key
    in
    t, m, key)
;;

let range_gen =
  Quickcheck.Generator.tuple2 Int.quickcheck_generator Int.quickcheck_generator
  |> Quickcheck.Generator.map ~f:(fun (a, b) -> if a <= b then a, b else b, a)
;;

let%test "is_empty" = T.is_empty T.empty

let%test "not is_empty" =
  let t = T.empty in
  let t = T.set t ~key:0 ~data:0 in
  not (T.is_empty t)
;;

let examples =
  let a = 0, 0 in
  let b = 1, 1 in
  let c = 2, 2 in
  List.map
    ~f:tree_and_map
    [ []
    ; [ a ]
    ; [ a; b ]
    ; [ b; a ]
    ; [ a; b; c ]
    ; [ a; c; b ]
    ; [ b; a; c ]
    ; [ b; c; a ]
    ; [ c; a; b ]
    ; [ c; b; a ]
    ]
;;

let%test_unit "length; keys; data; to_alist" =
  Quickcheck.test tree_and_map_gen ~examples ~f:(fun (t, m) ->
    [%test_result: int] ~expect:(Map.length m) (T.length t);
    [%test_result: int list] ~expect:(Map.keys m) (T.keys t);
    [%test_result: int list] ~expect:(Map.data m) (T.data t);
    [%test_result: (int * int) list] ~expect:(Map.to_alist m) (T.to_alist t))
;;

let%test_unit "accum" =
  Quickcheck.test alist_gen ~f:(fun l ->
    List.fold ~init:(T.empty, 0) l ~f:(fun (t, total) (key, data) ->
      let total =
        Option.fold (T.find t key) ~init:total ~f:(fun acc data -> acc - data)
      in
      let t = T.remove t key in
      [%test_result: int] ~expect:total (T.accum t);
      let t = T.set t ~key ~data in
      let total = total + data in
      [%test_result: int] ~expect:total (T.accum t);
      t, total)
    |> ignore)
;;

let%test_unit "accum subrange" =
  let t, map = Quickcheck.random_value tree_and_map_gen in
  let map_accum_subrange min max =
    Map.fold_range_inclusive map ~min ~max ~init:0 ~f:(fun ~key:_ ~data acc ->
      data + acc)
  in
  Quickcheck.test range_gen ~f:(fun (min_key, max_key) ->
    [%test_result: int]
      ~expect:(map_accum_subrange min_key max_key)
      (T.accum (T.subrange ~min_key ~max_key t)))
;;

let%test_unit "of_alist" =
  let examples =
    let a = 0, 0 in
    let b = 1, 1 in
    let c = 2, 2 in
    [ []; [ a ]; [ a; a ]; [ a; b; a ]; [ a; b; c ]; [ c; b; c; b; a ]; [ b; c; a; c ] ]
  in
  Quickcheck.test alist_gen ~examples ~f:(fun l ->
    let t = T.of_alist l in
    match Int.Map.of_alist l with
    | `Duplicate_key _ -> [%test_result: bool] ~expect:false (Result.is_ok t)
    | `Ok _ ->
      [%test_result: bool] ~expect:true (Result.is_ok t);
      let t = Or_error.ok_exn t in
      List.iter l ~f:(fun (k, v) ->
        [%test_result: int option] ~expect:(Some v) (T.find t k)))
;;

let%test_unit "sexp" =
  Quickcheck.test (Quickcheck.Generator.map alist_gen ~f:make_tree) ~f:(fun t ->
    let t' = T.t_of_sexp (T.sexp_of_t t) in
    [%test_result: (int * int) list] ~expect:(T.to_alist t) (T.to_alist t'))
;;

let%test_unit "mem; find" =
  let list_gen = List.quickcheck_generator (Quickcheck.Generator.geometric ~p:0.05 0) in
  Quickcheck.test list_gen ~f:(fun l ->
    let t, map = T.empty, Int.Map.empty in
    let t, map, _ =
      List.fold l ~init:(t, map, 0) ~f:(fun (t, map, data) key ->
        [%test_result: bool] ~expect:(Map.mem map key) (T.mem t key);
        [%test_result: int option] ~expect:(Map.find map key) (T.find t key);
        let t = T.set t ~key ~data in
        let map = Map.set map ~key ~data in
        assert (T.mem t key);
        [%test_result: int option] ~expect:(Some data) (T.find t key);
        t, map, data + 1)
    in
    let t, map =
      List.fold l ~init:(t, map) ~f:(fun (t, map) key ->
        [%test_result: bool] ~expect:(Map.mem map key) (T.mem t key);
        [%test_result: int option] ~expect:(Map.find map key) (T.find t key);
        let t = T.remove t key in
        let map = Map.remove map key in
        assert (not (T.mem t key));
        [%test_result: int option] ~expect:None (T.find t key);
        t, map)
    in
    [%test_result: bool] ~expect:true (Map.is_empty map);
    (* sanity check *)
    [%test_result: bool] ~expect:true (T.is_empty t))
;;

let%test_unit "remove_min; remove_max" =
  List.iter [ `Min; `Max ] ~f:(fun extreme ->
    Quickcheck.test tree_and_map_gen ~examples ~f:(fun (t, m) ->
      let t_obs =
        Option.map
          ~f:(fun (k, v, t) -> k, v, T.to_alist t)
          (match extreme with
           | `Min -> T.remove_min t
           | `Max -> T.remove_max t)
      and m_obs =
        Option.map
          ~f:(fun (k, v) -> k, v, Map.to_alist (Map.remove m k))
          (match extreme with
           | `Min -> Map.min_elt m
           | `Max -> Map.max_elt m)
      in
      [%test_result: (int * int * (int * int) list) option] ~expect:m_obs t_obs))
;;

let%test_unit "remove_before; remove_after" =
  List.iter [ `Before; `After ] ~f:(fun placement ->
    Quickcheck.test tree_and_map_and_key_gen ~f:(fun (t, m, key) ->
      let t_obs =
        Option.map
          ~f:(fun (k, v, t) -> k, v, T.to_alist t)
          (match placement with
           | `Before -> T.remove_before t key
           | `After -> T.remove_after t key)
      and m_obs =
        Option.map
          ~f:(fun (k, v) -> k, v, Map.to_alist (Map.remove m k))
          (Map.closest_key
             m
             (match placement with
              | `Before -> `Less_than
              | `After -> `Greater_than)
             key)
      in
      [%test_result: (int * int * (int * int) list) option] ~expect:m_obs t_obs))
;;

let%test_unit "map" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 Int.quickcheck_generator tree_and_map_gen)
    ~f:(fun (amnt, (t, m)) ->
      let f x = x * amnt in
      let t = T.map t ~f
      and m = Map.map m ~f in
      [%test_result: (int * int) list] ~expect:(Map.to_alist m) (T.to_alist t))
;;

let%test_unit "map_range" =
  Quickcheck.test
    (Quickcheck.Generator.tuple3 range_gen tree_and_map_gen alist_gen)
    ~f:(fun ((min, max), (t, m), replacement_alist) ->
      let t =
        T.map_range t ~min_key:min ~max_key:max ~f:(fun lmap ->
          [%test_result: (int * int) list]
            ~expect:(Map.range_to_alist m ~min ~max)
            lmap;
          replacement_alist)
      and m =
        List.fold
          replacement_alist
          ~init:(Map.filter_keys m ~f:(fun x -> x < min || x > max))
          ~f:(fun m (key, data) -> Map.set m ~key ~data)
      in
      [%test_result: (int * int) list] ~expect:(Map.to_alist m) (T.to_alist t))
;;

let%test_unit "nth in range" =
  let t, m = Quickcheck.random_value tree_and_map_gen in
  let indices = List.init (T.length t) ~f:Fn.id in
  Quickcheck.test (List.gen_permutations indices) ~f:(fun indices ->
    List.iter indices ~f:(fun idx ->
      [%test_result: (int * int) option] ~expect:(Map.nth m idx) (T.nth t idx)))
;;

let%test_unit "nth out of range" =
  let size = 100 in
  let t =
    Sequence.fold ~init:T.empty (Sequence.range 0 size) ~f:(fun t v ->
      T.set t ~key:v ~data:v)
  in
  Quickcheck.test Int.quickcheck_generator ~f:(fun idx ->
    let expect = if 0 <= idx && idx < size then Some (idx, idx) else None in
    [%test_result: (int * int) option] ~expect (T.nth t idx))
;;

let%test_unit "rank" =
  Quickcheck.test (Quickcheck.Generator.tuple2 alist_gen alist_gen) ~f:(fun (l, l') ->
    let t, m = tree_and_map l in
    List.iter (l @ l') ~f:(fun (key, _) ->
      let idx =
        Option.value
          ~default:(Map.length m)
          (let open Option.Let_syntax in
           let%bind key, _ = Map.closest_key m `Greater_or_equal_to key in
           Map.rank m key)
      in
      [%test_result: int * int] ~expect:(key, idx) (key, T.rank t key)))
;;

let%test_unit "search" =
  (* Ensure that we have only small positive integers *)
  let pos_gen = Int.gen_incl 0 99_999 in
  let alist_gen =
    List.quickcheck_generator
      (Quickcheck.Generator.tuple2 Int.quickcheck_generator pos_gen)
  in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 pos_gen alist_gen)
    ~sexp_of:[%sexp_of: int * (int * int) list]
    ~f:(fun (pos, l) ->
      let t, _ = tree_and_map l in
      let f ~left ~right =
        assert (T.accum t = left + right);
        if pos < left then `Left else `Right
      in
      let kv = T.search ~f t in
      match kv with
      | None -> assert (T.accum t <= pos)
      | Some (key, data) ->
        let l = (T.partition ~min_key:key t).lt in
        assert (T.accum l <= pos);
        assert (pos < T.accum l + data))
;;

let%test_unit "partition; subrange" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 range_gen tree_and_map_gen)
    ~sexp_of:[%sexp_of: (int * int) * (T.t * int Int.Map.t)]
    ~f:(fun ((min, max), (t, m)) ->
      let { T.Partition.lt; mid; gt } = T.partition ~min_key:min ~max_key:max t in
      let mid_subrange = T.subrange ~min_key:min ~max_key:max t in
      [%test_result: (int * int) list]
        ~expect:
          (Map.subrange m ~lower_bound:Unbounded ~upper_bound:(Excl min) |> Map.to_alist)
        (T.to_alist lt);
      [%test_result: (int * int) list]
        ~expect:(Map.range_to_alist ~min ~max m)
        (T.to_alist mid);
      [%test_result: (int * int) list]
        ~expect:(Map.range_to_alist ~min ~max m)
        (T.to_alist mid_subrange);
      [%test_result: (int * int) list]
        ~expect:
          (Map.subrange m ~lower_bound:(Excl max) ~upper_bound:Unbounded |> Map.to_alist)
        (T.to_alist gt))
;;

let%test_unit "merge" =
  let mergef ~key:_ = function
    | `Left a -> Some a
    | `Right b -> Some (17 * b)
    | `Both (a, b) -> Some (a + (17 * b))
  in
  Quickcheck.test
    (Quickcheck.Generator.tuple2 tree_and_map_gen tree_and_map_gen)
    ~f:(fun ((t1, m1), (t2, m2)) ->
      let t' = T.merge ~f:mergef t1 t2 in
      let m' = Map.merge ~f:mergef m1 m2 in
      [%test_result: (int * int) list] ~expect:(Map.to_alist m') (T.to_alist t'))
;;

let%test_unit "split" =
  Quickcheck.test
    tree_and_map_and_key_gen
    ~sexp_of:[%sexp_of: T.t * int Int.Map.t * int]
    ~f:(fun (t, m, key) ->
      let left, data, right = T.split t key in
      [%test_result: int option] ~expect:(Map.find m key) data;
      [%test_result: (int * int) list]
        (T.to_alist left)
        ~expect:
          (Map.subrange ~lower_bound:Unbounded ~upper_bound:(Excl key) m |> Map.to_alist);
      [%test_result: (int * int) list]
        (T.to_alist right)
        ~expect:
          (Map.subrange ~lower_bound:(Excl key) ~upper_bound:Unbounded m |> Map.to_alist))
;;

let%test_unit "join disjoint" =
  Quickcheck.test
    (Quickcheck.Generator.tuple2 Int.quickcheck_generator alist_gen)
    ~f:(fun (key, l) ->
      let t, m = tree_and_map l in
      let m = Map.remove m key in
      let expect = Map.to_alist m in
      let left, _, right = T.split t key in
      let joined = T.join left right |> Or_error.ok_exn in
      let joined_exn = T.join_exn left right in
      [%test_result: (int * int) list] ~expect (T.to_alist joined);
      [%test_result: (int * int) list] ~expect (T.to_alist joined_exn))
;;

let%test_unit "join non-disjoint" =
  Quickcheck.test (Quickcheck.Generator.tuple2 alist_gen alist_gen) ~f:(fun (l1, l2) ->
    let t1, m1 = tree_and_map l1 in
    let t2, m2 = tree_and_map l2 in
    if Option.both (Map.max_elt m1) (Map.min_elt m2)
       |> Option.value_map ~default:true ~f:(fun (a, b) -> a < b)
       |> not
    then
      [%test_result: (int * int) list option]
        ~expect:None
        (T.join t1 t2 |> Result.ok |> Option.map ~f:T.to_alist))
;;

open! Expect_test_helpers_core

let%expect_test _ =
  let t = T.empty in
  let t = T.set t ~key:0 ~data:9990 in
  let t = T.set t ~key:1 ~data:9991 in
  (let alist = T.to_alist t in
   print_s [%message "before" (alist : (int * int) list)]);
  let remove before_or_after =
    let before_or_after, removed =
      match before_or_after with
      | `before -> "before", T.remove_before t 1
      | `after -> "after", T.remove_after t 0
    in
    let k, v, t = Option.value_exn removed in
    let removed = k, v in
    let leftover = T.to_alist t in
    print_s
      [%message
        ("after remove_" ^ before_or_after)
          (removed : int * int)
          (leftover : (int * int) list)]
  in
  remove `before;
  remove `after;
  [%expect
    {|
    (before (
      alist (
        (0 9990)
        (1 9991))))
    ("after remove_before" (removed (0 9990)) (leftover ((1 9991))))
    ("after remove_after" (removed (1 9991)) (leftover ((0 9990))))
  |}]
;;
