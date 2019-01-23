open Printf
module C = Configurator.V1

module String = struct
  include String

  (* Make sure this exists even for OCaml < 4.04.0 *)
  let split_on_char sep s =
    let r = ref [] in
    let j = ref (length s) in
    for i = length s - 1 downto 0 do
      if unsafe_get s i = sep then begin
        r := sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    sub s 0 !j :: !r

  (* Naive substring detection *)
  let rec is_substring_pos j p lenp i s lens =
    if j >= lenp then true
    else if i >= lens then false
    else if p.[j] = s.[i] then is_substring_pos (j+1) p lenp (i+1) s lens
    else false
  let rec is_substring_loop p lenp i s lens =
    if is_substring_pos 0 p lenp i s lens then true
    else if i >= lens then false
    else is_substring_loop p lenp (i+1) s lens
  let is_substring ~sub:p s =
    is_substring_loop p (String.length p) 0 s (String.length s)
end

module List = struct
  include List

  let rec find_map l ~f =
    match l with
    | [] -> None
    | x :: l -> match f x with
                | None -> find_map l ~f
                | Some _ as res -> res
end

(* Adapted from Configurator — until it is exported. *)
module Find_in_path = struct
  let path_sep = if Sys.win32 then ';' else ':'

  let get_path () =
    match Sys.getenv "PATH" with
    | exception Not_found -> []
    | s -> String.split_on_char path_sep s

  let exe = if Sys.win32 then ".exe" else ""

  let prog_not_found prog =
    C.die "Program %s not found in PATH" prog

  let best_prog dir prog =
    let fn = Filename.concat dir (prog ^ ".opt" ^ exe) in
    if Sys.file_exists fn then
      Some fn
    else
      let fn = Filename.concat dir (prog ^ exe) in
      if Sys.file_exists fn then
        Some fn
      else
        None

  let find_ocaml_prog prog =
    match
      List.find_map (get_path ()) ~f:(fun dir ->
          best_prog dir prog)
    with
    | None -> prog_not_found prog
    | Some fn -> fn

  let find prog =
    List.find_map (get_path ()) ~f:(fun dir ->
        let fn = Filename.concat dir (prog ^ exe) in
        if Sys.file_exists fn then Some fn else None)
end

let read_file fname =
  let buf = Buffer.create 4096 in
  let fh = open_in_bin fname in
  Buffer.add_channel buf fh (in_channel_length fh);
  close_in fh;
  Buffer.contents buf

(* Inspired from Dune/Jbuilder — until it is exported. *)
let run cmd =
  let stdout_fn = Filename.temp_file "stdout-" ".bin" in
  let stderr_fn = Filename.temp_file "stderr-" ".bin" in
  let exit_code =
    Printf.ksprintf
      Sys.command "cd %s && %s > %s 2> %s"
      (Filename.quote (Filename.get_temp_dir_name()))
      cmd
      (Filename.quote stdout_fn)
      (Filename.quote stderr_fn)
  in
  let stdout = read_file stdout_fn in
  let stderr = read_file stderr_fn in
  Sys.remove stdout_fn;
  Sys.remove stderr_fn;
  if exit_code <> 0 then
    C.die "Command %s terminated with code %d." cmd exit_code;
  (stdout, stderr)

let has_header c h =
  try ignore(C.C_define.import c ~includes:[h] []); true
  with _ (* Fail to compile *) -> false

let fortran_compilers c =
  let fortran = ["gfortran"; "g95"; "g77"] in
  match C.ocaml_config_var c "target" with
  | Some target ->
     let arch, os, toolset = match String.split_on_char '-' target with
       | [arch; _; os; toolset] -> (* Linux, example: x86_64-pc-linux-gnu *)
          arch, os, toolset
       | [arch; mach; toolset] -> (* Windows, example: x86_64-w64-mingw32 *)
          arch, mach, toolset
       | _ -> C.die "target %S not understood" target in
     let ext = if Sys.os_type = "Win32" then ".exe" else "" in
     sprintf "%s-%s-%s-gfortran%s" arch os toolset ext
     :: fortran
  | None -> fortran

let fortran c =
  let fortran_compilers = fortran_compilers c in
  match List.find_map fortran_compilers ~f:Find_in_path.find with
  | Some fortran -> fortran
  | None -> C.die "Please install one of these fortran compilers: %s.\n\
                   If you use a different compiler, send its name to the \
                   author (see `opam show lbfgs`).\n%!"
              (String.concat ", " fortran_compilers)

let conf c =
  let fortran = fortran c in
  let is_gfortran = String.is_substring ~sub:"gfortran" fortran in
  let system = C.ocaml_config_var_exn c "system" in
  let clibs =
    if system = "macosx" && is_gfortran then
      let stdout, _ = run "gfortran --print-file-name libgfortran.dylib" in
      ["-L" ^ Filename.dirname stdout]
    else [] in
  let clibs = if is_gfortran then "-lgfortran" :: clibs else clibs in
  let cflags = [] in
  fortran, cflags, clibs

let () =
  let c = C.create "odepack" in
  let fortran, cflags, clibs = conf c in
  C.Flags.write_lines "fortranc.txt" [fortran];
  C.Flags.write_sexp "c_flags.sexp" cflags;
  C.Flags.write_sexp "c_library_flags.sexp" clibs
