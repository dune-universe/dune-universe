(* Convert verbose chase output into an XHTML document that displays
   its tree structure. *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

(* Read chase output *)

(* Read a character ignoring comments *)
let rec get_char i =
  let c = input_char i in
  if c = '%' then
    get_comment i
  else
    c

and get_comment i =
  let c = input_char i in
  if c = '\n' then
    get_char i
  else
    get_comment i

(* Read the contents of a structure.  Convert newlines into spaces and
   eliminate duplicate spaces. *)
let rec read_to_end i buf space =
  let c = input_char i in
  if c = ' ' || c = '\n' then
    read_to_end i buf true
  else
    begin
      if space then
        Buffer.add_char buf ' ';
      Buffer.add_char buf c;
      if c = ']' then
        Buffer.contents buf
      else
        read_to_end i buf false
    end

(* Read a structure *)
let rec read_struct i =
  let c = get_char i in
  if c = ' ' || c = '\n' then
    read_struct i
  else if c = '(' then
    let buf = Buffer.create 320 in
    Buffer.add_char buf c;
    try read_to_end i buf false with
    | End_of_file ->
       failwith "End of file within structure"
  else
    failwith (Printf.sprintf "Expecting '(' but got '%c'" c)

let read_structs i =
  let rec loop stcs =
    try
      let stc = read_struct i in
      loop (stc :: stcs)
    with
    | End_of_file ->
       if stcs = [] then
         failwith "Found no structures"
       else
         List.rev stcs in
  loop []

(* Add bold to a fact in a structure when it doesn't occur in the parent. *)

(* Find the extent of a fact *)
let comma stc n i =
  let rec loop p i =
    if i >= n then
      None
    else
      let c = stc.[i] in
      match c with
      | ']' -> Some i            (* End of structure *)
      | ',' when p = 0 -> Some i (* Parens are balanced *)
      | '(' -> loop (p + 1) (i + 1)
      | ')' -> loop (p - 1) (i + 1)
      | _ -> loop p (i + 1) in
  loop 0 i

(* Is pattern in the structure? *)
let match_stc pattern stc =
  let regex = Str.regexp_string pattern in
  try
    ignore @@ Str.search_forward regex stc 0;
    true
  with
  | Not_found ->
     false

(* Add strong annotation to facs in a structure that are not in the
   parent. *)
let strong parent stc =
  let n = String.length stc in
  let buf = Buffer.create (2 * n) in
  match String.index_opt stc '[' with
  | None -> stc                 (* This should never happen *)
  | Some i ->                   (* Add prelude *)
     Buffer.add_string buf (String.sub stc 0 (i + 1));
     let rec loop i =           (* For each fact *)
       if i >= n then
         stc                    (* This should never happen *)
       else
         match stc.[i] with
         | ' ' ->
            Buffer.add_char buf ' ';
            loop (i + 1)
         | ',' ->
            Buffer.add_char buf ',';
            loop (i + 1)
         | ']' ->
            Buffer.add_char buf ']';
            Buffer.contents buf
         | _ ->
            match comma stc n i with
            | None -> stc       (* This should never happen *)
            | Some j ->         (* Found a fact *)
               let fact = String.sub stc i (j - i) in
               if match_stc fact parent then (* Fact in parent? *)
                 Buffer.add_string buf fact
               else
                 begin          (* Add strong annotation *)
                   Buffer.add_string buf "<strong>";
                   Buffer.add_string buf fact;
                   Buffer.add_string buf "</strong>"
                 end;
               loop j in
     loop (i + 1)

(* Construct an internal representation of a structure called an item *)

type status = Unsat | Sat | Aborted

type item = {
    label : int;
    parent : int option;
    status : status;
    stc : string
  }

(* Is structure a model or aborted? *)
let status stc =
  if String.contains stc '!' then
    Sat
  else if String.contains stc '?' then
    Aborted
  else
    Unsat

(* Convert a structure as text into an item. *)
let get_item stc =
  try
    let i = String.index stc ')' in
    let s = String.sub stc 1 (i - 1) in
    match String.index_opt s ',' with
    | None -> {
        label = int_of_string s;
        parent =  None;
        status = status stc;
        stc }
    | Some i ->
       let t = String.sub s 0 i in
       let u = String.sub s (i + 1) (String.length s - i - 1) in {
           label = int_of_string t;
           parent = Some (int_of_string u);
           status = status stc;
           stc } with
  | Not_found -> failwith "Bad structure"
  | Failure _ -> failwith "Bad structure"

let get_label stc =
  try
    let i = String.index stc ')' in
    let s = String.sub stc 1 (i - 1) in
    match String.index_opt s ',' with
    | None -> int_of_string s
    | Some i ->
       let t = String.sub s 0 i in
       int_of_string t with
  | Not_found -> failwith "Bad structure"
  | Failure _ -> failwith "Bad structure"

(* Construct a tree from structure items *)

type 'a node = {
    item : 'a;
    kids : 'a node list;
    alive : bool;
    width : int;
    height : int
  }

let chlds_width = function
  | [] -> 1
  | _ :: _ as xs ->
     let f a n = a + n.width in
     List.fold_left f 0 xs

let chlds_height xs =
  let f a n = max a n.height in
  1 + List.fold_left f 0 xs

let mk_tree stcs =
  let items = Array.of_list (List.map get_item stcs) in
  for i = Array.length items - 1 downto 0 do
    let item = items.(i) in
    match item.parent with
    | None -> ()
    | Some p ->                 (* Add strong annotations *)
       items.(i) <- { item with stc = strong items.(p).stc item.stc }
  done;
  let kids = Array.make (Array.length items) [] in
  let f i item =
    if i = item.label then
      match item.parent with
      | None -> ()
      | Some p ->
         kids.(p) <- item.label :: kids.(p)
    else
      let msg =
        Printf.sprintf "Bad structure numbering: %d %d" i item.label in
      failwith msg in
  Array.iteri f items;
  let nodes = Array.make (Array.length items) {
                  item = items.(0);
                  kids = [];
                  alive = false;
                  width = 0;
                  height = 0 } in
  for i = Array.length nodes - 1 downto 0 do
    let item = items.(i) in
    let kids = List.map (fun i -> nodes.(i)) (List.rev kids.(i)) in
    nodes.(i) <- {
      item = item;
      kids = kids;
      alive = not (item.status = Unsat) || List.exists (fun i -> i.alive) kids;
      width = chlds_width kids;
      height = chlds_height kids
    }
  done;
  let stcs = List.map (fun i -> i.stc) @@ Array.to_list items in
  stcs, nodes.(0)

(* XML output *)

let quote str =
  let buf = Buffer.create (String.length str + 30) in
  let put_char c =
    match c with
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '&' -> Buffer.add_string buf "&amp;"
    | '"' -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&apos;"
    | _ -> Buffer.add_char buf c in
  String.iter put_char str;
  Buffer.contents buf

type attribute = string * string

type element =
  | M of string * attribute list * string (* Mixed content *)
  | E of string * attribute list * element list (* Element content *)

let mc name attrs body =
  M (name, attrs, body)

let ec name attrs body =
  E (name, attrs, body)

let print_attribute f (name, value) =
  Format.pp_print_string f name;
  Format.pp_print_string f " = '";
  Format.pp_print_string f (quote value);
  Format.pp_print_string f "'"

let rec print_attributes f = function
  | [] -> ()
  | attr :: attrs ->
     Format.pp_print_space f ();
     print_attribute f attr;
     print_attributes f attrs

let print_start f name attrs tail =
  Format.pp_print_string f "<";
  Format.pp_print_string f name;
  (match attrs with
   | [] -> ()
   | attr :: attrs ->
      Format.pp_print_string f " ";
      Format.pp_open_box f 0;
      print_attribute f attr;
      print_attributes f attrs;
      Format.pp_close_box f ());
  Format.pp_print_string f tail

let print_close f name =
  Format.pp_print_string f "</";
  Format.pp_print_string f name;
  Format.pp_print_string f ">"

let rec print_element f = function
  | M (name, attrs, body) when body = "" ->
     print_start f name attrs "/>"
  | E (name, attrs, []) ->
     print_start f name attrs "/>"
  | M (name, attrs, body) ->
     print_start f name attrs ">";
     Format.pp_print_string f (quote body);
     print_close f name
  | E (name, attrs, body) ->
     Format.pp_open_box f 1;
     print_start f name attrs ">";
     print_rest f body;
     Format.pp_close_box f ();
     Format.pp_print_cut f ();
     print_close f name

and print_rest f = function
  | [] -> ()
  | e :: es ->
     Format.pp_print_cut f ();
     print_element f e;
     print_rest f es

(* Support for CSS properties *)

let prop buf (name, value) =
  Buffer.add_string buf name;
  Buffer.add_string buf ": ";
  Buffer.add_string buf value;
  Buffer.add_char buf ';'

let props = function
  | [] -> ""
  | attr :: attrs ->
     let buf = Buffer.create 60 in
     prop buf attr;
     let rec loop = function
       | [] -> Buffer.contents buf
       | attr :: attrs ->
          Buffer.add_char buf ' ';
          prop buf attr;
          loop attrs in
     loop attrs

let show_float f =
  Printf.sprintf "%.3f" f

(* Printing configuration *)

type conf = {
    units : string;              (* Unit of length *)
    font : float;                (* Font size *)
    stroke : float;              (* Stroke width *)
    mx : float;                  (* Width of margin *)
    my : float;                  (* Height of margin *)
    tx : float;                  (* Distance between tree leaves *)
    ty : float;                  (* Distance between tree levels *)
    ta : float;                  (* Font ascent *)
    td : float                   (* Font descent *)
  }

let font = 12.0

let conf = {
    units = "pt";
    font = font;
    stroke = 0.08 *. font;
    mx = 3.33 *. font;
    my = 3.33 *. font;
    tx = 4.16 *. font;
    ty = 6.25 *. font;
    ta = 1.75 *. font;
    td = 1.16 *. font
  }

let button x y color label =
  let style = [("text-anchor", "middle"); ("fill", color)] in
  let action = Printf.sprintf "show(evt, \"s%d\")" label in
  let attrs = [
      ("x", show_float x);
      ("y", show_float y);
      ("style", props style);
      ("onclick", action)] in
  mc "text" attrs (string_of_int label)

let line x1 y1 x2 y2 =
  let style = [("stroke-width", show_float conf.stroke);
               ("stroke", "gray")] in
  let attrs = [("x1", show_float x1); ("y1", show_float y1);
               ("x2", show_float x2); ("y2", show_float y2);
               ("style", props style)] in
  ec "line" attrs []

let header ratio ifile o =
  let ratio =                   (* Adjust the ratio between *)
    if ratio < 5 then           (* the tree window and *)
      5                         (* the structure window *)
    else if ratio > 50 then
      50
    else
      ratio in
  let title =                   (* Title for document *)
    if ifile = "" then
      "Chase Tree"
    else
      Printf.sprintf "Chase Tree for %s" ifile in
  Printf.fprintf o
    "<?xml version=\"1.0\"?>\n\
     <!-- chase %s -->\n\
     <html xmlns=\"http://www.w3.org/1999/xhtml\">\n\
     <head>\n\
     \ <title>%s</title>\n\
     \ <meta http-equiv=\"content-type\"\n\
     \       content=\"application/xhtml+xml;\
     \ charset=UTF-8\" />\n\
     \ <style>\n\
     \  div.diagram { position: absolute; width: 100%%; top: 0;\n\
     \                left: 0; height: %d%%; overflow: scroll }\n\
     \  div.structs { position: absolute; width: 100%%; bottom: 0;\n\
     \                height: %d%%; overflow: scroll }\n\
     \  div.struct { left: 2em; right: 2em }\n\
     \ </style>\n\
     \ <script type=\"application/ecmascript\">\n\
     <![CDATA[\n\
     var s;\n\
     \n\
     function init() {\n\
     \  s = document.getElementById(\"s0\");\n\
     \  s.setAttribute(\"style\", \"display: block\");\n\
     \  var t = document.getElementById(\"tree\");\n\
     \  t.scrollTop = (t.scrollHeight - t.clientHeight/2) / 2;\n\
     }\n\
     \n\
     function show(evt, struct) {\n\
     \  s.setAttribute(\"style\", \"display: none\");\n\
     \  s = document.getElementById(struct);\n\
     \  s.setAttribute(\"style\", \"display: block\");\n\
     }\n\
     ]]>\n\
     \ </script>\n\
     </head>\n\
     <body onload=\"init()\">\n"
    Version.version title (100 - ratio) ratio

let color t =
  match t.item.status with
  | Sat -> "blue"
  | Aborted -> "orange"
  | Unsat ->
     if t.alive then
       "black"
     else
       "red"

let tbutton x y t =
  button x y (color t) t.item.label

let tree t =
  let tw = conf.tx *. float_of_int (t.height - 1) in (* Tree width *)
  let th = conf.ty *. float_of_int (t.width - 1) in (* Tree height *)
  let x = conf.mx in
  let y = conf.my +. th /. 2.0 in
  let top = [tbutton x (y -. conf.td) t] in
  let w = 2.0 *. conf.mx +. tw in (* Diagram width *)
  let h = 2.0 *. conf.my +. th in (* Diagram height *)
  let rec loop x1 y1 (h, es) t =
    let x2 = x1 +. conf.tx in
    let th = conf.ty *. float_of_int (t.width - 1) in
    let y2 = h +. th /. 2.0 in
    let es1 = tbutton x2 (y2 -. conf.td) t ::
                line x1 y1 x2 y2 :: es in
    let es2 = snd (List.fold_left (loop x2 y2) (h, es1) t.kids) in
    (h +. conf.ty +. th, es2) in
  (w, h, snd (List.fold_left (loop x y) (conf.my, top) t.kids))

let svg w h es =
  let viewbox = Printf.sprintf "0 0 %s %s" (show_float w) (show_float h) in
  let attrs = [
      ("width", Printf.sprintf "%s%s" (show_float w) conf.units);
      ("height", Printf.sprintf "%s%s" (show_float h) conf.units);
      ("xmlns", "http://www.w3.org/2000/svg");
      ("version", "1.1");
      ("viewBox", viewbox);
      ("font-size", show_float conf.font)] in
  ec "div" [("class", "diagram"); ("id", "tree")]
    [ec "svg" attrs es]

let diagram o t =
  let (w, h, es) = tree t in
  let root = svg w h es in
  let f = Format.formatter_of_out_channel o in
  print_element f root;
  Format.pp_print_newline f ()

let run ratio ifile o stcs tree =
  header ratio ifile o;
  flush o;
  diagram o tree;
  Printf.fprintf o "<div class='structs'>\n";
  let f s =
    Printf.fprintf o
      " <p id='s%d' class='struct' style='display: none'>%s</p>\n"
      (get_label s) s in
  List.iter f stcs;
  Printf.fprintf o "</div>\n</body>\n</html>\n"

(* Main entry point *)

let go ratio ifile i ofile =
  let stcs = read_structs i in
  close_in i;
  let stcs, tree = mk_tree stcs in
  let o =
    if ofile = "" then
      stdout
    else
      open_out ofile in
  run ratio ifile o stcs tree

let default_ratio = 25

(* Command-line processing *)
let usage =
  Printf.sprintf
    "Usage: %s [OPTIONS] [INPUT]\n\
    Options:\n\
    \  -o FILE  --output=FILE  output to file (default is standard output)\n\
    \  -r INT   --ratio=INT    set ratio between window heights \
                               (default %d%%)\n\
    \  -v       --version      print version number\n\
    \  -h       --help         print this message"
    Sys.argv.(0) default_ratio

let output  = ref ""
let ratio   = ref default_ratio
let version = ref false
let help    = ref false

let getint option place string =
  match int_of_string_opt string with
  | Some i -> place := i
  | None -> failwith ("Bad value for option " ^ option)

let specs =
[
 ('o', "output",  None, Getopt.atmost_once output
                          (Getopt.Error "only one output"));
 ('r', "ratio", None, Some (getint "ratio" ratio));
 ('v', "version", Getopt.set version true, None);
 ('h', "help", Getopt.set help true, None);
]

let cmdline v specs =
  let args = ref [] in
  let add_arg arg = args := arg :: !args in
  (try
     Getopt.parse_cmdline specs add_arg
   with
   | Getopt.Error s ->
      prerr_endline s;
      prerr_endline "";
      prerr_endline usage;
      exit 1);
  if !help then
    begin
      print_endline usage;
      exit 0
    end
  else if !version then
    begin
      print_endline v;
      exit 0
    end
  else
    List.rev !args

let start version go =
  let (in_name, in_ch) =
    match cmdline version specs with
    | [] -> ("", stdin)
    | [f] -> (try (f, open_in f)
              with Sys_error s -> failwith s)
    | _ ->
	prerr_endline "Bad command-line argument count\n";
	prerr_endline usage;
	exit 1 in
  go !ratio in_name in_ch !output

let filter version go =
  try
    start version go
  with
  | Failure s ->
      prerr_endline s;
      exit 1

let () = filter Version.version go
