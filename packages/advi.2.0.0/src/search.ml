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

let database_mtime = ref 0.0;;
let database_table = Hashtbl.create 257;;

let unwind_protect f x g y =
  try let v = f x in let _ = g y in v with
  | exc -> let _ = g y in raise exc;;

let open_process_in cmd =
  let (in_read, in_write) = Unix.pipe () in
  let pid =
    Unix.create_process "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
      Unix.stdin in_write Unix.stderr in
  Unix.close in_write;
  let inchan = Unix.in_channel_of_descr in_read in
  pid, inchan;;

let close_process_in (pid, inchan) =
  close_in inchan;
  let rec wait () =
    try snd (Unix.waitpid [] pid) with
    | Unix.Unix_error(Unix.EINTR, _, _) -> wait () in
  wait ();;

exception Command;;

let input_lines chan =
  let lines = ref [] in
  try while true do lines :=  (input_line chan) :: !lines done; []
  with  End_of_file ->
    List.rev !lines

let command_strings com opt =
  let t = ref 0.0 in
  Misc.debug_endline (t := Unix.gettimeofday(); "BEGIN");
  let command = Printf.sprintf "%s %s" com opt in
  Misc.debug_endline (Printf.sprintf "command_string: launching %s" command);
  try
    let pid, chan as pidchan = open_process_in command in
    let s = unwind_protect input_lines chan close_process_in pidchan in
    Misc.debug_endline (Printf.sprintf "END %f" (Unix.gettimeofday() -. !t));
    s
  with
  | Unix.Unix_error (c, _, _) ->
     Misc.warning
        (Printf.sprintf "Error %s while executing %s %s"
          (Unix.error_message c) com opt);
     raise Command
  | End_of_file ->
     []
;;

let command_string cmd opt =
  match command_strings cmd opt with h:: t -> h | [] -> ""

(* Add local path to search environment. *)
let addpath elem var kind =
  (* let newv = 
   *   try Unix.getenv var
   *   with Not_found ->
   *         try command_string Config.kpsewhich_path
   *             (Printf.sprintf "-show-path='%s'" kind) with
   *       | Command -> "" in
   * let newv = oldv ^ ":" ^ elem in *)
  let newv = 
    try
      let oldv = Unix.getenv var in
      let suff = 
        (* if String.ends_with ~suffix:":" oldv *)
        if String.get oldv (String.length oldv -1) = ':'
        then oldv ^ elem ^ ":"
        else ":" ^ elem in
      oldv ^ suff 
    with Not_found ->
      elem ^ ":" in
  Unix.putenv var newv
;;

let () =
  addpath Config.advi_latexdir "TEXINPUTS" Config.psheaders_kind

  (* addpath Config.advi_texdir "PSHEADERS" Config.psheaders_kind;
   * addpath Config.advi_texdir "TEXPICTS"  Config.texpicts_kind;; *)

let is_space = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false
;;

let remove_spaces line =
  let len = String.length line in
  let p = ref 0 and q = ref len in
  while !p < len && is_space line.[!p] do incr p done;
  while !q > !p && is_space line.[!q - 1] do decr q done;
  if !p = 0 && !q = len then line else
  String.sub line !p (!q - !p)
;;

let data_base_path_delim = Str.regexp ":";;
let last_modified path =
  try
    let stats = Unix.stat path in
    Some stats.Unix.st_mtime
  with _ ->
    None

let data_base_paths =
  lazy 
    begin
      let args = String.concat " " ["-show-path"; Config.database_name ] in
      let paths = command_string Config.kpsewhich_path args in
      let path_list = Str.split_delim data_base_path_delim paths in
      let database_name path = Filename.concat path Config.database_name in
      let bases = List.map database_name path_list in
      List.map (fun p -> p, ref None) bases
    end

let load_data_base path =
  try
    let ch = open_in path in
    let dir = Filename.dirname path in
    let curr_dir = ref "." in
    try while true do
      let line = remove_spaces (input_line ch) in
      let len = String.length line in
      if len > 0 && line.[0] <> '%' then begin
        if line.[len - 1] = ':'
        then curr_dir := Filename.concat dir (String.sub line 0 (len - 1))
        else
          let fullpath = Filename.concat !curr_dir line in 
          Hashtbl.add database_table line fullpath
      end
    done with
    | End_of_file -> close_in ch
    | e -> close_in ch; raise e
  with Sys_error _ -> ()
;;

let reload_database paths =
  Hashtbl.clear database_table; 
  List.iter load_data_base (List.rev_map fst paths)
;;

let reload_if_changed_database () =
  let lazy paths = data_base_paths in
  if List.exists (function path, m -> last_modified path <> !m) paths then
    begin
      List.iter (function path, m -> m := last_modified path) paths;
      reload_database paths
    end

let font_filename dpi fontname = 
  Printf.sprintf "%s.%dpk" fontname dpi

let database_font_path fontname dpi =
  reload_if_changed_database ();
  let name = font_filename dpi fontname in
  Hashtbl.find database_table name 
;;

let true_file_name options file =
  let args = String.concat " " (options @ [file]) in
  try 
    let s = command_string Config.kpsewhich_path args in
    if s = "" then raise Command; s
  with
  | Command ->
      Misc.warning (Printf.sprintf "file %s is not found" file);
      raise Not_found
;;

let slow_true_file_names options files =
  List.fold_right
    (fun file st ->
      try true_file_name options file :: st with Not_found -> st)
    files []
;;

let true_file_names options files =
  match files with [] -> [] | _ ->
    let arg_string = String.concat " " (options @ files) in
    let truenames =
      try command_strings Config.kpsewhich_path arg_string
      with Command -> [] in
    if List.length truenames = List.length files then truenames
    else slow_true_file_names options files
;;

let rec iter2_safe f l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | h1::t1, h2::t2 -> iter2_safe f t1 t2; f h1 h2
  | _, _ -> ()

let kpsewhich_cache = Hashtbl.create 13
let prefetch filenames dpi =
  let filenames = List.map (font_filename dpi) filenames in
  let uncached = 
    List.fold_left
      (fun all filename ->
        try let _ = Hashtbl.find kpsewhich_cache filename in all
        with Not_found -> filename :: all) [] filenames in
  let truenames =
    true_file_names  ["-dpi=" ^ string_of_int dpi; "-mktex=pk"] uncached in
  let cache filename fullname =
    Hashtbl.add kpsewhich_cache filename fullname in
  (* assert failed for CB... ??? *)
  assert (List.length uncached = List.length truenames);
  iter2_safe cache uncached truenames
;;

let kpsewhich_font_path fontname dpi =
  let fontname =  (font_filename dpi fontname) in
  let truename =
    try Hashtbl.find  kpsewhich_cache fontname
    with Not_found -> 
      true_file_name ["-dpi=" ^ string_of_int dpi; "-mktex=pk"] fontname in
  if truename = "" then raise Not_found
  else truename
;;

let unix_font_path fontname dpi =
  (* Not sound---as "." is not "ls-R". We should not reimplement
     kpsewhich, but instead cache its results cross-session. *)
  (* try database_font_path fontname dpi *)
  (* with _ -> *)
    kpsewhich_font_path fontname dpi
;;

let msdos_font_path fontname dpi =
  let filename = Printf.sprintf "%s.%dpk" fontname dpi in
  try Filename.concat (Sys.getenv "PKFONTS") filename
  with _ -> filename
;;

let font_path =
  match Sys.os_type with
  | "Unix" | "Cygwin" -> unix_font_path
  | "Win32" -> msdos_font_path
  | _ -> raise Not_found
;;
