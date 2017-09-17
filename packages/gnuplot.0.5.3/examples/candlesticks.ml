open Core
open Gnuplot

let () =
  (* Generate random candlestick bars. *)
  let gen_bars ~num_bars =
    let next_bar cl =
      let op = cl +. (Random.float 1. -. 0.5) /. 2. in
      let hi = op +. Random.float 1. /. 5. in
      let lo = op -. Random.float 1. /. 5. in
      let cl = (lo +. hi) /. 2. in
      op, hi, lo, cl
    in
    let rec loop n_bars bars (( _, _, _, cl) as bar) =
      if n_bars = 0 then
        bars
      else
        loop (n_bars - 1) (bar :: bars) (next_bar cl)
    in
    let op = 100. in
    let hi = op +. Random.float 1. /. 5. in
    let lo = op -. Random.float 1. /. 5. in
    let cl = (lo +. hi) /. 2. in
    List.rev (loop num_bars [] (op, hi, lo, cl))
  in
  let gen_data ~start ~stop =
    let date_range = Date.dates_between ~min:start ~max:stop in
    List.zip_exn date_range (gen_bars ~num_bars:(Date.diff stop start + 1))
  in
  let num_days = 100 in
  let stop = Date.today ~zone:(Lazy.force Time.Zone.local) in
  let start = Date.add_days stop (-num_days) in
  let gp = Gp.create () in
  (* Plot a random candlestick chart. *)
  Gp.plot gp ~range:(Range.Date (Date.add_days start (-1), Date.add_days stop 1))
    ~format:"%b %d'%y" (Series.candles_date_ohlc (gen_data ~start ~stop));
  Gp.close gp
