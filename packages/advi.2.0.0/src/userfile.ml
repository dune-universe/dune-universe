(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* Part I

File names manipulations: path normalization, tilde substitution,
file name directory (and sub directories) creation, etc.

*)
let is_absolute path = not (Filename.is_relative path);;

let normalize path =
  let full = is_absolute path in
  let final_slash = path.[String.length path - 1] = '/' in
  let tokens = Misc.split_string path (function '/' -> true | _ -> false) 0 in
  let rec remove = function
    | x :: xs ->
      begin match x :: remove xs with
      | x :: [] -> [x]
      | "." :: xs -> xs (* remove "." *)
      | x :: ".." :: xs when x <> ".." -> xs (* remove "dir/.." *)
      | l -> l
      end
    | [] -> [] in
  let path = String.concat "/" (remove tokens) in
  Printf.sprintf "%s%s%s"
    (if full then "/" else "") path (if final_slash then "/" else "")
;;

let fullpath from_dir path =
  (* get full path (weaker than Junix.realpath) *)
  if is_absolute path then path else normalize (Filename.concat from_dir path)
;;

(* Tilde substitution *)

(* skip to next / *)
let rec next_slash s n =
  if n >= String.length s || s.[n] = '/' then n else
  next_slash s (succ n)
;;

let user_home_dir = try Sys.getenv "HOME" with _ -> "~";;

let tilde_subst fname =
  try
    if fname = "" || fname.[0] <> '~' then fname else
    let len = String.length fname in
    if len = 1 then user_home_dir else
    match fname.[1] with
    | '/' ->
      Filename.concat user_home_dir (String.sub fname 2 (len - 2))
    | _ ->
      let final = next_slash fname 1 in
      let user = String.sub fname 1 (pred final) in
      let pwnam = Unix.getpwnam user in
      if succ final >= len then pwnam.Unix.pw_dir else
       Filename.concat pwnam.Unix.pw_dir
         (String.sub fname (succ final) (len - succ final))
  with
  | Unix.Unix_error (_, _, _) | Sys_error _ | Not_found -> fname
;;

let rec digdir dir perm =
  (* try to create the directory dir *)
  if not (Sys.file_exists dir) then
    let pdir = Filename.dirname dir in
    Misc.debug_endline ("Creating directory " ^ pdir ^ "... " );
    digdir pdir perm;
    Unix.mkdir dir perm;
    Misc.debug_endline "done"
;;

(* cautious_perm_digdir digs a directory with cautious permissions,
   i.e. drwx------ *)
let cautious_perm_digdir dir = digdir dir 0o0700;;

(* Prepare a file access: create its directory and sub-directories if any. *)
let prepare_file fname =
  let dirname = Filename.dirname fname in
  if not (Sys.file_exists dirname) then begin
    try cautious_perm_digdir dirname with
    | Unix.Unix_error (e, _, _) ->
      Misc.warning (Unix.error_message e)
  end
;;

(* Prepare a file access and clear it before use. *)
let prepare_and_clear_file fname =
  prepare_file fname;
  try
    let oc = open_out_gen [Open_creat; Open_trunc; Open_binary] 0o0600 fname in
    close_out oc
  with
  | Unix.Unix_error (e, _, _) ->
    Misc.warning (Unix.error_message e)
;;

(* Part II

File names definitions: DVI file to treat, user preferences files,
cache directory name, etc.

*)
(* The DVI file currently read. *)
(* first file is master, other files are clients *)
let dvi_filenames = ref [];;
let dvi_mastername = ref None;;
let set_dvi_filename fname =
  dvi_filenames :=  fname :: !dvi_filenames;
  if !dvi_mastername = None then dvi_mastername := Some fname

let get_dvi_filenames () = List.rev !dvi_filenames

(* Its directory name. *)
let get_dvi_file_dirname () =
  match !dvi_mastername with
  | None -> raise Not_found
  | Some dirname -> Filename.dirname dirname;;

(* User preferences. *)
(* User options files. *)
let default_user_advi_dir = Filename.concat user_home_dir ".advi";;

let user_advi_dir =
  let dir =
    try Sys.getenv "ADVIDIR" with _ -> default_user_advi_dir in
  try tilde_subst dir with
  | _ -> ".advi";;

let default_init_file0 = "/etc/advirc";;
let default_init_file1 = Filename.concat user_home_dir ".advirc";;
let default_init_file2 =
  tilde_subst (Filename.concat default_user_advi_dir "advirc");;
let default_init_file3 = ".advirc";;

let init_files = [
  default_init_file0;
  default_init_file1;
  default_init_file2;
  default_init_file3;
];;

let load_options_file options set_dvi_filename usage_msg fname =
 Rc.cautious_parse_file fname options set_dvi_filename usage_msg;;

let load_init_files options set_dvi_filename usage_msg =
 List.iter
   (fun fname ->
      if Sys.file_exists fname
      then load_options_file options set_dvi_filename usage_msg fname)
   init_files;;

(* Cache directory *)

(* test is the directory dirname can serve as a cache directory:
   it should be readable writable and executable. *)
let can_be_cache_directory dirname =
   Sys.file_exists dirname &&
   try
     Unix.access dirname [Unix.R_OK; Unix.W_OK; Unix.X_OK; Unix.F_OK];
     true
   with
   | Unix.Unix_error _ -> false;;

(* Try to dig a directory to serve as cache,
   Raise Not_found if the directory still cannot be
   used as a cache directory. *)
let mk_user_advi_cache_dir dirname =
  cautious_perm_digdir dirname;
  if can_be_cache_directory dirname then dirname else raise Not_found;;

(* try to figure out a reasonable default cache directory:
   - if user's environment variable TMPDIR is set and the corresponding
     directory is writtable, use it,
   - otherwise, if /tmp can serve as a cache use it,
   - otherwise, if the DVI file directory is possible use it,
   - otherwise, if $HOME/.advi is available use it,
   - otherwise, if none of those is possible, try to use the current
     working directory as a cache. *)
let get_user_advi_cache_dir () =
  let add_user_defined_tmp dirs =
    try Sys.getenv "TMPDIR" :: dirs with
    | Not_found -> dirs in
  let add_system_tmp_dir dirs =
    Filename.dirname (Filename.temp_file "" "") :: dirs in
  let add_current_dvi_dirname dirs =
    try get_dvi_file_dirname () :: dirs with
    | Not_found -> dirs in
  let add_user_tmp_dir dirs =
    Filename.concat user_home_dir ".advi" :: dirs in
  let dirs =
    add_user_defined_tmp
     (add_system_tmp_dir
       (add_current_dvi_dirname
         (add_user_tmp_dir []))) in
  try List.find can_be_cache_directory dirs with
  | Not_found ->
      let rec find_cache_dir = function
      | [] ->
          Misc.warning "Cannot find a cache directory, try to use .";
          Unix.getcwd ()
      | d :: ds ->
          try mk_user_advi_cache_dir d with
          | _ -> find_cache_dir ds in
      find_cache_dir dirs;;

let advi_cache_dir = ref None;;

let set_advi_cache_dir d =
  let d =
    if d.[0] = '~' then
      mk_user_advi_cache_dir (tilde_subst d)
    else
      d
  in
  if can_be_cache_directory d then begin
    Misc.debug_endline (Printf.sprintf "Using %s as cache directory." d);
    advi_cache_dir := Some d end else
  Misc.warning (Printf.sprintf "Cannot use %s as a cache directory" d);;

Options.add
  "-cache-dir"
  (Arg.String set_advi_cache_dir)
  "<dir>: set the cache directory to <dir>,\
  \n\t (the default cache directory is \"/tmp\").";;

(* Get the actual advi cache directory.
   If it has not yet been set (i.e. it was not specified on the command
   line or in init files), try to figure out a reasonable default using
   get_user_advi_cache_dir above. *)
let get_advi_cache_dir () =
  match !advi_cache_dir with
  | Some d -> d
  | None ->
      let d = get_user_advi_cache_dir () in
      set_advi_cache_dir d;
      d;;

let init_advi_cache_dir () = ignore (get_user_advi_cache_dir ());;

Rc.at_init init_advi_cache_dir;;

(* Part III

 Writing information about the current page on a designated file,
 for synchronisation purposes with other tools.
 We write page number to the file advi_page_number_file
 and page number + (absolute) time of visualisation to the file
 advi_page_timing_file.
 *)

(* Writing page current number to the file advi_page_number_file. *)
let write_page_number =
 Options.flag false "-page-number"
  "  ask Active-DVI to write the current page number in a file,\
  \n\t (the default is to skip writing page number).";;

let advi_page_number_file = ref None;;

let set_page_number_file fname = advi_page_number_file := Some fname;;

let get_page_number_file () =
  match !advi_page_number_file with
  | Some fname -> fname
  | None ->
      let fname =
        Filename.concat (get_advi_cache_dir ()) "advi_page_number" in
      set_page_number_file fname;
      fname;;

Options.add "-page-number-file"
 (Arg.String set_page_number_file)
 "<file>: set the name of the file where \
  \n\t Active-DVI could write the current page number,\
  \n\t (the default file is \"advi_page_number\" in the cache directory).";;

let open_append fname =
  let oc =
    open_out_gen [Open_append; Open_binary; Open_creat] 0o0600 fname in
  oc;;

let my_open_out = function
  | "-" -> stdout
  | fname -> open_out fname;;

let my_open_append = function
  | "-" -> stdout
  | fname -> open_append fname;;

let my_close_out oc = if oc == stdout then flush stdout else close_out oc;;

let save_page_number n =
 if !write_page_number then
 try
   let oc = my_open_out (get_page_number_file ()) in
   output_string oc (Printf.sprintf "%i\n" n);
   my_close_out oc
 with _ ->
   Misc.warning
     (Printf.sprintf
        "Cannot write file %s to record page number."
        (get_page_number_file ()));;

(* Initialization of advi_page_number_file. *)
let init_advi_page_number_file () =
 if !write_page_number then
  let fname = get_page_number_file () in
  if fname <> "-" then prepare_file fname;;

Rc.at_init init_advi_page_number_file;;

(* Writing slides timing to the file advi_page_timing_file.
  Useful to any kind of post synchronization,
  for instance the clock or a sound track. *)
let write_page_timing =
 Options.flag false "-page-timing"
  "Ask Active-DVI to write the current page timing in a file,\
  \n\t (the default is not to write timings).";;

let advi_page_timing_file = ref None;;

let set_page_timing_file fname =
 Misc.debug_endline ("timing file is " ^ fname);
 advi_page_timing_file := Some fname;;

let get_page_timing_file () =
  match !advi_page_timing_file with
  | Some fname -> fname
  | None ->
      let fname =
        Filename.concat (get_advi_cache_dir ()) "advi_page_timing" in
      set_page_timing_file fname;
      fname;;

Options.add "-page-timing-file"
 (Arg.String set_page_timing_file)
 "<file>: set the name of the file where \
  \n\t Active-DVI could write the page timings,\
  \n\t (the default file is \"advi_page_timing\" in the cache directory).";;

let save_page_timing n =
 if !write_page_timing then
 try
   let oc = my_open_append (get_page_timing_file ()) in
   output_string oc (Printf.sprintf "#PAGE %i AT %f\n" n (Unix.time ()));
   my_close_out oc
 with _ ->
   Misc.warning
     (Printf.sprintf
        "Cannot write file %s to record page timing."
        (get_page_timing_file ()));;

(* Initialization of advi_page_timing_file. *)
let init_advi_page_timing_file () =
 if !write_page_timing then
   let fname = get_page_timing_file () in
   if fname <> "-" then prepare_and_clear_file fname;;

Rc.at_init init_advi_page_timing_file;;
