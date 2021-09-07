(* Some features were tested to work with Gnuplot 5.4 but do not work with 5.2.
   (Namely, fillstyles) *)
let recent_enough_version =
  try
    let ic = Unix.open_process_in "gnuplot --version" in
    let version_string = input_line ic in
    match String.split_on_char ' ' version_string with
    | "gnuplot" :: version :: _ ->
        let v = float_of_string version in
        v >= 5.4
    | _ -> false
  with _ -> exit 0

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

let data ~style ~mu st =
  Plot.Scatter.points_2d
    ~points:(Plot.Data.of_array (Array.init 100 (fun _ -> normal2 mu st)))
    ~style
    ()

let rng_state = Random.State.make [| 0x1337 |]

let scatter_2d =
  let open Plot in
  let open Pointtype in
  let open Color in
  Plot.plot2
    ~xaxis:"x"
    ~yaxis:"y"
    ~title:"nice 2d plot"
    [ data
        ~style:
          Style.(default |> set_point ~psize:0.5 ~ptyp:box |> set_color red)
        ~mu:(3., 2.)
        rng_state;
      data
        ~style:Style.(default |> set_point ~ptyp:delta_solid)
        ~mu:(10., 8.)
        rng_state;
      data
        ~style:
          (if recent_enough_version then
           Style.(
             default
             |> set_circle
                  ~fill:
                    Fill.(default |> set_solid ~density:0.35 ~transparent:true)
                  ~radius:0.2)
          else Style.default)
        ~mu:(10., 3.)
        rng_state ]

let data ~style ~mu st =
  Plot.Scatter.points_3d
    ~points:(Plot.Data.of_array (Array.init 100 (fun _ -> normal3 mu st)))
    ~style
    ()

let scatter_3d =
  let open Plot in
  let open Pointtype in
  let open Color in
  Plot.plot3
    ~xaxis:"x"
    ~yaxis:"y"
    ~zaxis:"z"
    ~title:"nice 3d plot"
    [ data
        ~style:Style.(default |> set_point ~ptyp:box |> set_color red)
        ~mu:(3., 2., 1.0)
        rng_state;
      data
        ~style:Style.(default |> set_point ~ptyp:delta_solid)
        ~mu:(10., 8., 5.0)
        rng_state;
      data
        ~style:
          (if recent_enough_version then
           Style.(
             default
             |> set_circle
                  ~fill:
                    Fill.(default |> set_solid ~density:0.35 ~transparent:true)
                  ~radius:0.2)
          else Style.default)
        ~mu:(2., 10., 12.0)
        rng_state ]

let histogram =
  (* testing absence of title *)
  Plot.plot2 ~xaxis:"x" ~yaxis:"frequency"
  @@ [ Plot.Histogram.hist
         ~points:
           (Plot.Data.of_array
              (Array.init 2000 (fun _ -> Plot.r1 @@ normal rng_state)))
         ~bins:100
         ~legend:"nice legend"
         () ]

let line ~length =
  Array.init length (fun i ->
      Plot.r2 (float_of_int i) (Random.State.float rng_state 10.0))

let lines =
  let open Plot in
  plot2 ~xaxis:"x" ~yaxis:"y" ~title:"lines!"
  @@ [ Line.line_2d
         ~points:(Data.of_array (line ~length:20))
         ~style:Style.(default |> set_color Color.red)
         ~legend:"red curve"
         ();
       Line.line_2d
         ~points:(Data.of_array (line ~length:10))
         ~style:Style.(default |> set_color Color.blue)
         ~legend:"blue curve"
         ();
       Line.line_2d
         ~points:(Data.of_array (line ~length:20))
         ~style:
           Style.(
             default |> set_color Color.black |> set_point ~ptyp:Pointtype.box)
         ~legend:"black curve"
         ~with_points:true
         () ]

let error_bars =
  let open Plot in
  plot2 ~xaxis:"x" ~yaxis:"y" ~title:"error bars 1"
  @@ [ Scatter.points_2d
         ~points:(Data.of_array (line ~length:20))
         ~style:Style.(default |> set_color Color.red)
         ~error_bars:(Data.of_array (line ~length:20))
         ~legend:"red curve"
         () ]

let error_lines =
  let open Plot in
  plot2 ~xaxis:"x" ~yaxis:"y" ~title:"error bars 2"
  @@ [ Line.line_2d
         ~points:(Data.of_array (line ~length:20))
         ~style:
           Style.(
             default |> set_color Color.red |> set_point ~ptyp:Pointtype.box)
         ~error_bars:(Data.of_array (line ~length:20))
         ~legend:"red curve"
         () ]

let () =
  Plot.(
    run_matrix
      ~target:
        (Plot.png ~pixel_size:(1280, 960) ~png_file:"figure.png" ())
        (* (Plot.pdf ~cm_size:(20., 20.) ~pdf_file:"figure.pdf" ()) *)
        (* ~target:(Plot.qt ~pixel_size:(1280, 1280) ()) *)
      ~title:"meta-title"
      ~plots:
        [| [| Some scatter_2d; Some scatter_3d |];
           [| Some histogram; Some lines |];
           [| Some error_bars; Some error_lines |]
        |]
      exec)
