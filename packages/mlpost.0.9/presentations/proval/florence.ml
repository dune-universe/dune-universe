open Mlpost
open Command
open Path
open Point
open Plot
module T = Transform

let f1 i =
  let aux = function
    | 0 -> 1
    | 1 | 2 -> 2
    | 3 | 4 -> 3
    | 5 -> 4
    | 6 | 7 -> 5
    | 8 | 9 -> 6
    | 10 -> 7
    | 11 | 12 -> 8
    | 13 | 14 -> 9
    | 15 -> 10
    | 16 | 17 -> 11
    | 18 | 19 -> 12
    | 20 -> 13
    | _ -> 0
  in
  float_of_int (aux i)

let f2 i =
  let aux = function
    | 0 | 1 | 2 -> 0
    | 3 -> 1
    | 4 -> 2
    | 5 | 6 | 7 -> 3
    | 8 -> 4
    | 9 -> 5
    | 10 | 11 | 12 -> 6
    | 13 -> 7
    | 14 -> 8
    | 15 | 16 | 17 -> 9
    | 18 -> 10
    | 19 -> 11
    | 20 -> 12
    | _ -> 0
  in
  float_of_int (aux i)

let f3 i = float_of_int ((i + 3) / 5)

let flab i =
  ( Picture.transform [ Transform.scaled 1.7 ]
      (Picture.tex (Printf.sprintf "$f_{\\omega_%d}$" i)),
    Command.Ptop,
    19 )

let instants =
  let pen = Pen.default ~tr:[ Transform.scaled 2.5 ] () in
  let base =
    Command.draw ~pen (Path.path ~style:JLine [ (0., -65.); (280., -65.) ])
  in
  let tick i =
    let xi = float_of_int i *. 14. in
    let yi = if f1 i = f1 (i - 1) then -60. else -45. in
    let p = Path.path ~style:JLine [ (xi, -65.); (xi, yi) ] in
    Command.draw ~pen p
  in
  Command.seq
    [
      base;
      Command.iter 0 20 tick;
      Command.label
        (Picture.transform [ Transform.scaled 2. ] (Picture.tex "$\\omega_1$"))
        (pt (-20., -55.));
    ]

let florence =
  let sk = mk_skeleton 20 14 14. 20. in
  let pen = Pen.default ~tr:[ Transform.scaled 4. ] () in
  let pen2 = Pen.default ~tr:[ Transform.scaled 3. ] () in
  let dash _ = Dash.scaled 0.5 Dash.withdots in
  let dash2 = Dash.scaled 0.66 Dash.withdots in
  let dash3 = Dash.scaled 0.9 Dash.evenly in
  let vcaption, hcaption =
    let tr = [ Transform.scaled 1.5 ] in
    ( Picture.transform tr (Picture.tex "\\textsf{Number of ones}"),
      Picture.transform tr (Picture.tex "\\textsf{Instants}") )
  in
  let plot = draw_func ~drawing:Stepwise ~style:JLine in
  [
    draw_grid ~hdash:dash ~vdash:dash sk;
    draw_axes ~closed:true ~hcaption ~vcaption sk;
    plot ~pen ~label:(flab 1) f1 sk;
    plot ~pen:pen2 ~dashed:dash2 ~label:(flab 2) f2 sk;
    plot ~pen ~dashed:dash3 ~label:(flab 3) f3 sk;
    instants;
  ]

let _ = Metapost.emit "florence" florence
