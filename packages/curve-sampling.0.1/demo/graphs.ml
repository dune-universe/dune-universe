open Printf

let () =
  let fh = open_out "graphs.gp" in
  fprintf fh "set terminal pngcairo\n\
              set grid\n";
  let n = ref 0 in
  let save t ~title =
    incr n;
    let fname = sprintf "graph%d.dat" !n in
    Curve_sampling.to_file t fname;
    fprintf fh "set output \"graph%d.png\"\n\
                plot %S with l lt 1 lw 2 title %S\n" !n fname title;
    fprintf fh "set output \"graph%d_p.png\"\n\
                plot %S with l lt 5 lw 2 title %S, \
                %S with p lt 1 pt 5 ps 0.2 title \"points\"\n"
      !n fname title fname
  in

  let f x = x *. sin(1. /. x) in
  let t = Curve_sampling.fn f (-0.4) 0.4 ~n:227 in
  save t ~title:"x sin(1/x)";
  let t = Curve_sampling.fn f (-0.4) 0.4 ~n:391 in
  save t ~title:"x sin(1/x)";

  close_out fh
