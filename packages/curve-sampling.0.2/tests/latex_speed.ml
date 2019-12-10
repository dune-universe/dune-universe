open Printf

let () =
  let fh = open_out "latex_speed.tex" in
  fprintf fh "\\documentclass[12pt,a4paper]{article}\n\
              \\usepackage{tikz}\n\
              \\begin{document}\n\
              \\begin{tikzpicture}\n";
  let n = 40_000 in
  printf "🛈 Will measure LaTeX speed with %d points.\n%!" n;
  let t = Curve_sampling.fn sin (-6.) 6. ~n in
  Curve_sampling.to_latex_channel t fh;
  fprintf fh "\\end{tikzpicture}\n\
              \\end{document}";
  close_out fh


(* Local Variables: *)
(* compile-command: "dune build latex_speed.exe" *)
(* End: *)
