(* this module handles the latex generation *)

let tikz_only = ref true

type internal = string (* output file *)

let output = ref None

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  at_exit (fun () -> close_out oc);
  output := Some fmt

let get_output () =
  try Option.get !output
  with Invalid_argument _ -> failwith "tex output should be set before tex generation"

let emit s = Format.fprintf (get_output ()) s

let name = Format.asprintf "col_%i_%i_%i"

let header out =
  set_output out;
  emit "\\usepackage{tikz}\n\n";
  emit "\\begin{tikzpicture}\n";
  emit "\\def\\sx{1}\n";
  emit "\\def\\sy{1}\n";
  emit "\\definecolor{%s}{RGB}{%i,%i,%i}\n" (name 0 0 0) 0 0 0

let footer () =
  emit "\\end{tikzpicture}\n%!"

let header_full out =
  set_output out;
  emit "\\documentclass{article}\n\\usepackage{fullpage}\n\\usepackage{tikz}\n\n";
  emit "\\begin{document}\n\n\\centering\n\n";
  emit "\\begin{figure}[t]\n\\centering\n\\begin{tikzpicture}\n";
  emit "\\def\\sx{1}\n";
  emit "\\def\\sy{1}\n";
  emit "\\definecolor{%s}{RGB}{%i,%i,%i}\n" (name 0 0 0) 0 0 0

let footer_full () =
  emit "\\end{tikzpicture}\n\\end{figure}\n%!";
  emit "\\end{document}\n%!"

let init f =
  if !tikz_only then
    header f
  else
    header_full f

let ending () =
  if !tikz_only then
    footer ()
  else
    footer_full ()

let normalize (x_min, x_max) (y_min, y_max) (x, y) =
  let x = Tools.projection (x_min,x_max) (0.,1.) x in
  let y = Tools.projection (y_min,y_max) (0.,1.) y in
  x, y

let width () = int_of_float 1.
let height () = int_of_float 1.

type color = int * int * int
let rgb r g b : color = (r,g,b)

(* outputs the tikz definition of a color
on the std outpout and returns the color name as a string *)
let define_color =
  let already_defined = Hashtbl.create 10 in
  fun ((r,g,b) as col :color) ->
  try Hashtbl.find already_defined col
  with Not_found ->
    let id = name r g b in
    Hashtbl.add already_defined col id;
    emit "\\definecolor{%s}{RGB}{%i,%i,%i};\n" id r g b;
    id

let draw_text c p (x,y) text =
  let text = String.escaped text in
  let p = match p with
    | `Center -> "center"
    | `Left | `Right -> failwith "draw text left right not implemented"
  in
  let c = define_color c in
  emit "\\node[align=%s,text=%s,font=\\tiny] at (%f*\\sx, %f*\\sy) { %s };\n" p c x y text

let draw_line col (x1,y1) (x2,y2) =
  let col = define_color col in
  emit "\\draw[%s] (%f*\\sx,%f*\\sy) -- (%f*\\sx, %f*\\sy);\n" col x1 y1 x2 y2

let circle filldraw (col:color) (x,y) rad =
  let col = define_color col in
  emit "\\%s[%s] (%f*\\sx, %f*\\sy) circle (%f);\n" filldraw col x y rad

let fill_circle = circle "fill"

let draw_circle = circle "draw"

let poly filldraw col vertices =
  let col = define_color col in
  emit "\\%s[%s] " filldraw col;
	List.iter (fun (x,y) -> emit "(%f*\\sx, %f*\\sy) -- " x y) vertices;
  emit "cycle;@."

let fill_poly = poly "fill"

let draw_poly = poly "draw"
