open Core
open Gnuplot
open List_utils

let () = Random.init 100  (* For reproducability. *)

(* The type of a bid in an auction. *)
type bid =
  { action : buy_or_sell;
    price  : int;
    volume : int;
  } and buy_or_sell = Buy | Sell

(* Generates random auction data. *)
let generate_auction_data () =
  List.map3_exn
    ([Buy; Sell] |> sample ~size:100)
    (List.range 1 100 ~stop:`inclusive |> sample ~size:100)
    (List.range 1  10 ~stop:`inclusive |> sample ~size:100)
    ~f:(fun action price volume -> { action; price; volume })

(* Compute total volume sold / bought for each price level. *)
let asks, bids =
  let group_by_price = group_by ~f:(fun q -> q.price) in
  let group_by_action = group_by ~f:(fun q -> q.action) in
  let flat_map l ~f = List.bind l f in
  let aggregate bids =
    List.reduce_exn bids ~f:(fun b1 b2 ->
      { b1 with volume = b1.volume + b2.volume })
  in
  generate_auction_data ()
  |> group_by_price
  |> flat_map ~f:group_by_action
  |> List.map ~f:aggregate
  |> List.partition_tf ~f:(fun b -> b.action = Sell)
  |> Tuple.T2.map_snd ~f:List.rev

let () =
  let aggregate bids =
    begin match bids with
    | [] | [_] as x -> x
    | bid :: bids ->
      scan bids ~init:bid ~f:(fun b1 b2 ->
        { b1 with price = b2.price; volume = b1.volume + b2.volume })
    end |> List.map ~f:(fun b -> float b.volume, float b.price)
  in
  let gp = Gp.create () in
  (* Plot supply and demand curve. *)
  Gp.set gp ~title:"Supply and Demand";
  Gp.plot_many gp ~labels:(Labels.create ~x:"Volume" ~y:"Price" ())
    [ Series.steps_xy (aggregate asks) ~title:"Sell" ~color:`Green
    ; Series.steps_xy (aggregate bids) ~title:"Buy"  ~color:`Red ];
  Gp.close gp
