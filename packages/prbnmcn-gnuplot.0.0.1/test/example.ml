let normal =
  let pi = 4. *. atan 1. in
  let saved = ref None in
  fun st ->
    match !saved with
    | Some (r, t) ->
        saved := None ;
        r *. sin t
    | None ->
        let u1 = Random.State.float st 1. in
        let u2 = Random.State.float st 1. in
        let r = sqrt (-2. *. log u1) in
        let t = 2. *. pi *. u2 in
        saved := Some (r, t) ;
        r *. cos t

let normal2 (mu_x, mu_y) st = Plot.r2 (normal st +. mu_x) (normal st +. mu_y)

let normal3 (mu_x, mu_y, mu_z) st =
  Plot.r3 (normal st +. mu_x) (normal st +. mu_y) (normal st +. mu_z)

let scatter ~mu st =
  Plot.Scatter.points_2d
    ~points:(Plot.Data.of_array (Array.init 100 (fun _ -> normal2 mu st)))
    ()

let st = Random.State.make_self_init ()

let scatter2d_example =
  Plot.plot2
    ~xaxis:"x"
    ~yaxis:"y"
    ~title:"Clouds"
    [scatter ~mu:(3., 2.) st; scatter ~mu:(10., 8.) st]

let () =
  let target = Plot.png ~pixel_size:(1024, 1024) ~png_file:"scatter.png" () in
  Plot.(run ~plot:scatter2d_example ~target exec)

let discretize f =
  Array.init 100 (fun i ->
      let x = float_of_int i *. 0.1 in
      Plot.r2 x (f x))

let sine_with_points =
  let open Plot in
  Line.line_2d
    ~points:(Data.of_array (discretize sin))
    ~style:
      Style.(default |> set_color Color.red |> set_point ~ptyp:Pointtype.box)
    ~with_points:true
    ~legend:"sin"
    ()

let line2d_example =
  Plot.plot2 ~xaxis:"x" ~yaxis:"y" ~title:"Lines" [sine_with_points]

let () =
  let target = Plot.png ~pixel_size:(1024, 1024) ~png_file:"line.png" () in
  Plot.(run ~plot:line2d_example ~target exec)

let gaussian =
  let open Plot in
  Histogram.hist
    ~points:(Data.of_array (Array.init 100 (fun _ -> r1 @@ normal st)))
    ()

let histogram_example =
  Plot.plot2 ~xaxis:"x" ~yaxis:"freq" ~title:"Histogram" [gaussian]

let () =
  let target = Plot.png ~pixel_size:(1024, 1024) ~png_file:"histogram.png" () in
  Plot.(run ~plot:histogram_example ~target exec)
