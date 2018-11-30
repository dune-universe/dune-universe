open Printf
open Gg

let () =
  let fh = open_out "horror.gp" in
  fprintf fh "set terminal pdfcairo\n\
              set output \"horror.pdf\"\n\
              set grid\n";
  let n_dat = ref 0 in
  let plot ?(xmin = -5.) ?(xmax = 5.) ?(ymin = -5.) ?(ymax = 5.) ?(n=100)
        ~title f =
    let vp = Box2.v (P2.v xmin ymin) (Size2.v (xmax -. xmin) (ymax -. ymin)) in
    let s = Curve_sampling.fn f xmin xmax ~viewport:vp ~n in
    incr n_dat;
    let fname = sprintf "horror%d.dat" !n_dat in
    Curve_sampling.to_file s fname;
    let fname_p = sprintf "horror%d_p.dat" !n_dat in
    Curve_sampling.Internal.write_points s fname_p;
    let fname_s = sprintf "horror%d_s.dat" !n_dat in
    Curve_sampling.Internal.write_segments s fname_s;
    fprintf fh "unset title\n\
                unset y2tics\n\
                plot [%f:%f] \"%s\" with l lt 5 title \"%s\", \
                \"%s\" with p lt 1 pt 6 ps 0.2 title \"n=%d\"\n"
      xmin xmax fname title fname n;
    fprintf fh "set title \"Restricted to viewport [%g:%g]×[%g:%g]\"\n\
                set y2tics\n\
                set y2range [-1e-6: %f]\n\
                plot [%f:%f] [%f:%f] \"%s\" with l lt 5 title \"%s\", \
                \"%s\" with p lt 3 pt 7 ps 0.2 title \"n=%d\", \
                \"%s\" using 1:3  with lp ps 0.2 lt rgb \"#737373\" \
                  title \"cost points\", \
                \"%s\" using 1:8 with lp ps 0.2 lt rgb \"#760b0b\" \
                  axes x1y2 title \"cost segments\"\n"
      xmin xmax ymin ymax (Curve_sampling.Internal.cost_max s +. 1e-6)
      xmin xmax ymin ymax fname title fname n fname_p fname_s;
  in
  (* Tests from
     https://github.com/soegaard/bracket/blob/master/plotting/adaptive-plotting.rkt#L225 *)
  plot (fun _x -> 2.) ~title:"x ↦ 2" ~n:10;
  plot (fun x -> x) ~title:"x ↦ x";
  plot (fun x -> 5. *. x) ~title:"x ↦ 5x";
  plot (fun x -> 1. /. x) ~title:"1/x"; (* check singularity *)
  plot (fun x -> 1. /. x) ~title:"1/x"  (* singularity at starting point *)
    ~xmin:0. ~xmax:5. ~ymax:100.;
  plot sqrt ~title:"√x" ~xmin:(-0.3) ~xmax:2. ~ymin:0. ~ymax:1.6;
  plot tan ~title:"tan" ~n:200; (* many singularities *)
  plot (fun x -> 1. /. (abs_float x)) ~title:"1/|x|";
  plot (fun x -> 1e6 *. x) ~title:"10⁶ x"; (* high slope *)
  plot (fun x -> 1e50 *. x) ~title:"10⁵⁰ x"; (* high slope *)
  plot (fun x -> log(1. +. sin (cos x))) ~title:"1 + sin(cos x)"
    ~xmin:(-6.) ~xmax:6. ~ymin:(-2.) ~ymax:2.;
  plot (fun x -> sin(x**3.) +. cos(x**3.)) ~title:"sin x³ + cos x³" ~n:400
    ~xmin:0. ~xmax:6.28 ~ymin:(-1.5) ~ymax:1.5;
  plot sin ~title:"sin" ~n:400
    ~xmin:(-5.) ~xmax:200. ~ymin:(-1.) ~ymax:1.;
  (* Examples from R. Avitzur, O. Bachmann, N. Kajler, "From Honest to
     Intelligent Plotting", proceedings of ISSAC' 95, pages 32-41, July 1995. *)
  plot (fun x -> sin(300. *. x)) ~title:"sin(300 x)"
    ~xmin:(-4.) ~xmax:4. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(300. *. x)) ~title:"sin(300 x)" ~n:1000
    ~xmin:(-4.) ~xmax:4. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(300. *. x)) ~title:"sin(310 x)"
    ~xmin:(-4.) ~xmax:4. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> 1. +. x *. x +. 0.0125 *. log(abs_float(1. -. 3. *. (x -. 1.))))
    ~title:"1 + x² + 0.0125 log|1 - 3(x-1)|"
    ~xmin:(-2.) ~xmax:2. ~ymin:0. ~ymax:3.;
  plot (fun x -> x *. sin(1. /. x)) ~title:"x sin(1/x)"
    ~xmin:(-0.5) ~xmax:0.5 ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> x *. sin(1. /. x)) ~title:"x sin(1/x)" ~n:200
    ~xmin:(-0.5) ~xmax:0.5 ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(1. /. x)) ~title:"sin(1/x)"
    ~xmin:(-2.) ~xmax:2. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(1. /. x)) ~title:"sin(1/x)" ~n:400
    ~xmin:(-2.) ~xmax:2. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(x**4.)) ~title:"sin(x⁴)"
    ~xmin:(-4.) ~xmax:4. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(x**4.)) ~title:"sin(x⁴)" ~n:600
    ~xmin:(-4.) ~xmax:4. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(exp x)) ~title:"sin(exp x)"
    ~xmin:(-6.) ~xmax:6. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> sin(exp x)) ~title:"sin(exp x)" ~n:500
    ~xmin:(-6.) ~xmax:6. ~ymin:(-1.) ~ymax:1.;
  plot (fun x -> 1. /. sin x) ~title:"1 / sin x"
    ~xmin:(-10.) ~xmax:10. ~ymin:0. ~ymax:10.;
  plot (fun x -> sin x /. x) ~title:"(sin x)/x"
    ~xmin:(-6.) ~xmax:6. ~ymin:0. ~ymax:2.;
  plot (fun x -> tan(x**3. -. x +. 1.) +. 1. /. (x +. 3. *. exp x))
    ~title:"tan(x³ - x + 1) + 1/(x + 3 eˣ)"
    ~xmin:(-2.) ~xmax:2. ~ymin:(-15.) ~ymax:15.;
  close_out fh
