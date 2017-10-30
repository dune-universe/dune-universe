(* Generate the layout dependent files by textual substitution.

   This script is supposed to be run before oasis.  The tarballs
   must contain the already generated files. *)

let string_of_file fn =
  let buf = Buffer.create 4096 in
  let fh = open_in fn in
  let s = Bytes.create 4096 in
  let read = ref 1 in (* enter the loop *)
  while !read > 0 do
    read := input fh s 0 4096;
    Buffer.add_substring buf (Bytes.unsafe_to_string s) 0 !read;
  done;
  Buffer.contents buf

let write_ro ?orig_path fn s =
  (try Sys.remove fn with _ -> ());
  let fh = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o444 fn in
  (match orig_path with
   | Some fn -> Printf.fprintf fh "# 1 %S\n" fn
   | None -> ());
  output_string fh s;
  close_out fh


let include_re = Str.regexp "INCLUDE(\\([^()]+\\))"

let rec tr_include s =
  let inc s = tr_include (string_of_file (Str.matched_group 1 s)) in
  Str.global_substitute include_re inc s

let arg =
  "\\([!#.A-Za-z0-9_ !]+\\|\
   [!#.A-Za-z0-9_ ]*([!#.A-Za-z0-9_ ]*)[#.A-Za-z0-9_ ]*\\)"

let tr_fortran =
  let tr = [
    "\\([a-z]+\\)FC\\b", "\\1F";
    "LAYOUT", "fortran_layout";
    "DEFAULT_SWITCHES", "";
    "FST", "1";
    "SND", "2";
    "THIRD", "3";
    "TO_FORTRAN(\\([^()]*\\))", "\\1";
    (* FORTRAN view taken as default: *)
    ("CREATE_VEC(" ^ arg ^ ", *" ^ arg ^ ")",
     "Array1.create (\\1) fortran_layout (\\2)");
    ("CREATE_MAT(" ^ arg ^ ", *" ^ arg ^ ", *\\(" ^ arg ^ "\\))",
     "Array2.create (\\1) fortran_layout (\\2) (\\3)");
    "GET(\\([^,]*\\), *\\([^,]*\\), *\\([^()]*\\))", "\\1.{\\2,\\3}";
    "LINE_COL(\\([^,]*\\), *\\([^()]*\\))", "(\\1)(\\2)";
    "NCOLS(\\([^()]+\\))", "Array2.dim2(\\1)";
    "NROWS(\\([^()]+\\))", "Array2.dim1(\\1)";
    "LASTCOL(\\([^()]+\\))", "Array2.dim2(\\1)";
    "LASTROW(\\([^()]+\\))", "Array2.dim1(\\1)";
    "LASTEL(\\([^()]+\\))", "Array1.dim(\\1)";
    "COLS", "dim2";
    "ROWS", "dim1";
    "OF_IDX(\\([^()]+\\))", "\\1 - 1"; (* Fortran -> Easymesh indexes *)
    "TO_IDX(\\([^()]+\\))", "\\1 + 1"; (* Easymesh -> Fortran *)
    "CHOOSE_FC(\\([^,]*\\), *\\([^()]*\\))", "\\1";
  ] in
  List.map (fun (re, s) -> (Str.regexp re, s)) tr

let tr_c =
  let tr = [
    "\\([a-z]+\\)FC\\b", "\\1C";
    "LAYOUT", "c_layout";
    "DEFAULT_SWITCHES", "z";
    "FST", "0";
    "SND", "1";
    "THIRD", "2";
    "TO_FORTRAN(\\([^()]*\\))", "(\\1) + 1";
    (* C matrices are transpose of FORTRAN: *)
    ("CREATE_VEC(" ^ arg ^ ", *" ^ arg ^ ")",
     "Array1.create (\\1) c_layout (\\2)");
    ("CREATE_MAT(" ^ arg ^ ", *" ^ arg ^ ", *\\(" ^ arg ^ "\\))",
     "Array2.create (\\1) c_layout (\\3) (\\2)");
    "GET(\\([^,]*\\), *\\([^,]*\\), *\\([^()]*\\))", "\\1.{\\3,\\2}";
    "LINE_COL(\\([^,]*\\), *\\([^()]*\\))", "(\\2)(\\1)";
    "NCOLS(\\([^()]+\\))", "Array2.dim1(\\1)";
    "NROWS(\\([^()]+\\))", "Array2.dim2(\\1)";
    "LASTCOL(\\([^()]+\\))", "Array2.dim1(\\1) - 1";
    "LASTROW(\\([^()]+\\))", "Array2.dim2(\\1) - 1";
    "LASTEL(\\([^()]+\\))", "Array1.dim(\\1) - 1";
    "COLS", "dim1";
    "ROWS", "dim2";
    "OF_IDX(\\([^()]+\\))", "\\1"; (* Easymesh *)
    "TO_IDX(\\([^()]+\\))", "\\1";
    "CHOOSE_FC(\\([^,]*\\), *\\([^()]*\\))", "\\2";
  ] in
  List.map (fun (re, s) -> (Str.regexp re, s)) tr


let gen_FC ~mod_name ~fn_base ~orig_path =
  let s = string_of_file (fn_base ^ "FC.ml") in
  let s = tr_include s in
  let replace s (re,tr) = Str.global_replace re tr s in
  let tr = [(Str.regexp "MOD", mod_name)] in
  write_ro (fn_base ^ "F.ml") ~orig_path
    (List.fold_left replace s (tr @ tr_fortran));
  write_ro (fn_base ^ "C.ml") ~orig_path
    (List.fold_left replace s (tr @ tr_c))

let rm_gen fn_base =
  (try Sys.remove (fn_base ^ "F.ml") with _ -> ());
  (try Sys.remove (fn_base ^ "C.ml") with _ -> ())


let filenames =
  (* Filename, module name *)
  [ "mesh_triangle", "Mesh";
    "mesh", "Mesh";
    "mesh_display", "Mesh_display";
    "easymesh", "Easymesh";
  ]

(** Working directory relative to the base of the project.  This is
   important for the file of the error to be clickable from, say, Emacs. *)
let get_relwd () =
  let d = ref (Sys.getcwd()) in
  let path = ref [] in
  let b = ref(Filename.basename !d) in
  while !b <> "_build" && !b <> "/" do
    path := !b :: !path;
    d := Filename.dirname !d;
    b := Filename.basename !d;
  done;
  match !path with
  | _ :: path -> String.concat Filename.dir_sep path
  | [] -> ""

let () =
  List.iter (fun (fn_base, mod_name) ->
      let d = get_relwd() in
      let fn_orig = fn_base ^ "FC.ml" in
      if Sys.file_exists fn_orig then
        gen_FC ~mod_name ~fn_base ~orig_path:(Filename.concat d fn_orig)
    ) filenames
