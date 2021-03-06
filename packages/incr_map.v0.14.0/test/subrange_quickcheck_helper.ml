open Core
open Import
open Quickcheck
open Quickcheck.Generator.Let_syntax
module Key = Int
module Value = Int

type map = Value.t Key.Map.t [@@deriving sexp_of, compare]

type map_op =
  [ `Add_nearby of Key.t * Value.t
  | `Remove of Key.t
  | `Update of Key.t * Value.t
  | `No_change
  ]
[@@deriving sexp_of]

let map_with_length_gen ?key_range length : map Generator.t =
  let f, t = Option.value key_range ~default:(-10 * length, 10 * length) in
  let%map l =
    Generator.list_with_length
      (2 * length)
      (Generator.both (Int.gen_uniform_incl f t) (Int.gen_uniform_incl (-10) 20000))
  in
  List.fold l ~init:Int.Map.empty ~f:(fun map (key, data) ->
    if Map.length map < length then Map.set map ~key ~data else map)
;;

let map_gen : map Generator.t =
  let%bind l1 = Generator.small_non_negative_int in
  let%bind l2 = Generator.small_non_negative_int in
  map_with_length_gen ((4 * l1) + l2)
;;

let map_op_gen ?key_range ?(none_ratio = 0.0) () : map_op Generator.t =
  let key_gen =
    match key_range with
    | None -> Generator.small_non_negative_int
    | Some (from, to_) -> Int.gen_incl from to_
  in
  let val_gen = Int.gen_uniform_incl (-10) 20000 in
  Generator.weighted_union
    [ ( 1.0 -. none_ratio
      , Generator.union
          [ (let%map k = key_gen
             and v = val_gen in
             `Add_nearby (k, v))
          ; (let%map k = key_gen
             and v = val_gen in
             `Update (k, v))
          ; (let%map k = key_gen in
             `Remove k)
          ] )
    ; none_ratio, Generator.singleton `No_change
    ]
;;

let rec apply_map_op ~search_length map = function
  | `Add_nearby (key, data) ->
    if Map.mem map key && Int.( > ) search_length 0
    then apply_map_op map (`Add_nearby (key + 1, data)) ~search_length:(search_length - 1)
    else Map.set map ~key ~data
  | `Update (key, data) ->
    (match Map.closest_key map `Greater_or_equal_to key with
     | None -> map
     | Some (key, _) -> Map.set map ~key ~data)
  | `Remove key ->
    (match Map.closest_key map `Greater_or_equal_to key with
     | None -> map
     | Some (key, _) -> Map.remove map key)
  | `No_change -> map
;;

let apply_map_op = apply_map_op ~search_length:10

let apply_map_op_incr map_var op =
  let map = Incr.Var.value map_var in
  Incr.Var.set map_var (apply_map_op map op)
;;

type range = int Maybe_bound.t * int Maybe_bound.t [@@deriving sexp_of]

type range_op =
  [ `Move_start of int
  | `Move_end of int
  | `Next_page
  | `Prev_page
  | `Set_range of range (* not generated by default *)
  | `No_change
  ]
[@@deriving sexp_of]

let range_gen : range Generator.t =
  let%map a = Generator.small_non_negative_int
  and b = Generator.small_non_negative_int in
  if a < b then Incl a, Incl b else Incl b, Incl a
;;

let range_op_gen ?(none_ratio = 0.0) () : range_op Generator.t =
  let offset_gen =
    let%map ch = Generator.small_positive_int
    and positive = Bool.quickcheck_generator in
    if positive then ch else Int.(-ch)
  in
  Generator.weighted_union
    [ ( 1.0 -. none_ratio
      , Generator.weighted_union
          [ ( 0.4
            , let%map diff = offset_gen in
              `Move_start diff )
          ; ( 0.4
            , let%map diff = offset_gen in
              `Move_end diff )
          ; 0.1, Generator.singleton `Next_page
          ; 0.1, Generator.singleton `Prev_page
          ] )
    ; none_ratio, Generator.singleton `No_change
    ]
;;

let bounds_contradictory ~lower ~upper =
  (* Returns true if there are provably no elements included in the given range. *)
  match lower, upper with
  | _, Unbounded | Unbounded, _ -> false
  | Incl l, Incl u -> l > u
  | Excl l, Incl u | Incl l, Excl u -> l >= u
  | Excl l, Excl u -> l >= u - 1
;;

let some_upper_bound_so_that_non_empty ~lower =
  match lower with
  | Unbounded ->
    (* arbitrary *)
    Incl 0
  | Incl l ->
    (* Smallest possible *)
    Incl l
  | Excl l ->
    (* Smallest possible *)
    Incl (l + 1)
;;

let replace_upper_bound_if_contradictory ~lower ~upper =
  let new_upper =
    if bounds_contradictory ~lower ~upper
    then some_upper_bound_so_that_non_empty ~lower
    else upper
  in
  lower, new_upper
;;

let apply_range_op (start, end_) = function
  | `Move_start d ->
    let new_start =
      Maybe_bound.map start ~f:(fun start ->
        let new_start = Int.max 0 (start + d) in
        new_start)
    in
    replace_upper_bound_if_contradictory ~lower:new_start ~upper:end_
  | `Move_end d ->
    let new_end = Maybe_bound.map end_ ~f:(( + ) d) in
    replace_upper_bound_if_contradictory ~lower:start ~upper:new_end
  | `Next_page ->
    let shift =
      match start, end_ with
      | (Incl s | Excl s), (Incl e | Excl e) -> ( + ) (e - s + 1)
      | _ -> Fn.id
    in
    Maybe_bound.map start ~f:shift, Maybe_bound.map end_ ~f:shift
  | `Prev_page ->
    let shift =
      match start, end_ with
      | (Incl s | Excl s), (Incl e | Excl e) -> fun x -> Int.max 0 (x - (e - s + 1))
      | _ -> Fn.id
    in
    Maybe_bound.map start ~f:shift, Maybe_bound.map end_ ~f:shift
  | `Set_range range -> range
  | `No_change -> start, end_
;;

let apply_range_op_incr range_var op =
  let range = Incr.Var.value range_var in
  Incr.Var.set range_var (apply_range_op range op)
;;

let map_and_range_op_gen ?key_range () : (map_op * range_op) Generator.t =
  let%map m = map_op_gen ?key_range ~none_ratio:0.5 ()
  and r = range_op_gen ~none_ratio:0.5 () in
  m, r
;;
