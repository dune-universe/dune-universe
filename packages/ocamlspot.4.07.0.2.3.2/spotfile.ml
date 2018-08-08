(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Format
open Utils

(* Keep the original modules *)
module Ident0 = Ident

open Spot
open Spoteval

module Load : sig
  exception Old_cmt of string (* cmt *) * string (* source *)
  val load : load_paths:string list -> string -> Unit.t
  val load_module : ?spit:bool -> cwd:string -> load_paths:string list -> string -> Unit.t
  val load_directly_with_cache : string -> Unit.t
end = struct

  let check_time_stamp ~cmt source =
    (* CR jfuruse: aaa.mll creates cmt with aaa.ml as source, but
       aaa.ml is often removed by the build system.
    *)
    let stat_cmt = try Unix.stat cmt with _ -> assert false in
    try
      let stat_source = Unix.stat source in
        (* Needs = : for packed modules, .cmt and the source .cmo are written 
           almost at the same moment. *)
      stat_cmt.Unix.st_mtime >= stat_source.Unix.st_mtime
    with
    | Unix.Unix_error(_, "stat", _) ->
        (* CR jfuruse: Camlp4.cmt created from Camlp4.cmx but Camlp4.cmx
           is not installed!!! In such a case, we cannot check the time
           stamp check... (still we can try against Camlp4.cmi installed)
        *)
        eprintf "Warning: source %s does not exist. Time stamp check was skipped.@." source;
        true

  let load_cmt_file file = snd (Cmt_format.read file)

  let load_directly path : Unit.t =
    Debug.format "cmt loading from %s@." path;
    match load_cmt_file path with
    | Some cmt -> Spot.Unit.of_cmt path cmt
    | None -> failwithf "load_directly failed: %s" path

  exception Old_cmt of string (* cmt *) * string (* source *)

  (* CR jfuruse: exception *)
  (* CRv2 jfuruse: add and check cache time stamp *)
  let load_directly_with_cache : string -> Unit.t = 
    Hashtbl.memoize (Hashtbl.create 17) (fun path ->
      try
        let file = load_directly path in
        if not (check_time_stamp ~cmt:path file.Unit.path) then 
          if Spotconfig.strict_time_stamp then 
            raise (Old_cmt (path, file.Unit.path))
          else
            eprintf "Warning: source %s is newer than the cmt@." file.Unit.path;
        file
      with
      | Not_found ->
          failwithf "failed to find cmt file %s" path)

  let find_in_path load_paths body ext =
    let body_ext = body ^ ext in
    let find_in_path load_paths name = 
      Debug.format "@[<2>find_in_path: searching %s in@ pwd=%s@ paths=[@[%a@]]@]@." 
        name
        (Sys.getcwd ())
        (Format.list "; " (fun ppf x -> fprintf ppf "%S" x)) 
        load_paths;
      try Misc.find_in_path load_paths name with Not_found ->
        Misc.find_in_path_uncap load_paths name
    in
    try find_in_path load_paths body_ext with Not_found ->
    (* We do not give up yet.
       .cmt file is not found, 
       but we still find a .cmi which is sym-linked to the original directory with .cmt
    *)
    let cminame = body ^ ".cmi" in
      try
      let cmipath = find_in_path load_paths cminame in
      let stat = Unix.lstat cmipath in
      if stat.Unix.st_kind = Unix.S_LNK then begin
        let cmipath = Filename.dirname cmipath ^/ Unix.readlink cmipath in
        let cmtpath = Filename.chop_extension cmipath ^ ext in
        if Sys.file_exists cmtpath then begin
          Debug.format "Found an alternative %s: %s@." ext cmtpath;
            cmtpath 
          end else failwithf "cmt file not found: %s, neither in %s" body_ext cmtpath
        end else raise Not_found
      with
      | (Failure _ as e) -> raise e
      | _ -> failwithf "cmt file not found: %s" body_ext
    

  let load ~load_paths cmtname : Unit.t =
    let body, ext = Filename.split_extension cmtname in
    let path = find_in_path load_paths body ext in (* XXX cmtname is abspath. not required? *)
    load_directly_with_cache path

  (* ocamlbuild tweak *)
  (* for  /.../a/b/c/x.ml
     if   /.../a/_build exists
     then look for
          /.../a/_build/b/c/x.cm*
  *)
  (* seek ocamlbuild _build destination directory *)      
  let ocamlbuild_path_tweak cmtname =
    if Filename.is_relative cmtname then None
    else
      let basename = Filename.basename cmtname in
      let dirname = Filename.dirname cmtname in
      let rec loop postfix dir = 
        let dir_build = dir ^/ "_build" in
        if Unix.is_dir dir_build then dir_build ^/ postfix
        else 
          if dir = "/" then raise Exit
          else loop (Filename.basename dir ^/ postfix) (Filename.dirname dir)
      in
      try 
        let cmtname = loop "" dirname ^/ basename in
        Debug.format "Trying ocamlbuild destination %s@." cmtname;
        Some cmtname
      with Exit -> None

  (* .ocamlspot file tweak *)        
  let dot_ocamlspot_tweak cmtname = 
    if Filename.is_relative cmtname then None
    else
      Option.bind (Dotfile.find_and_load (Filename.dirname cmtname)) 
        (fun (_postfix, found_dir, dotfile) ->
          Option.map dotfile.Dotfile.build_dir ~f:(fun build_dir ->
            let length_found_dir = String.length found_dir in
            let found_dir' = 
              String.sub cmtname 0 length_found_dir
            in
            let rel_cmtname =
              String.sub cmtname 
                (length_found_dir + 1)
                (String.length cmtname - length_found_dir - 1)
            in
            assert (found_dir = found_dir');
            let dir = 
              if Filename.is_relative build_dir then found_dir ^/ build_dir
              else build_dir
            in
            let cmtname = dir ^/ rel_cmtname in
            Debug.format "Trying .ocamlspot destination %s@." cmtname;
            cmtname
          ))

  let load ~load_paths cmtname : Unit.t =
    Format.eprintf "cmtname fix : %s@." cmtname;
    try load ~load_paths cmtname with
    | e -> 
        let load_alternative f =
          match f cmtname with
          | None -> None
          | Some cmtname ->
              try Some (load ~load_paths cmtname) with _ -> None
        in
        match load_alternative dot_ocamlspot_tweak with
        | Some v -> v
        | None -> 
            match load_alternative ocamlbuild_path_tweak with
            | Some v -> v
            | None -> raise e

  let with_cwd cwd f = 
    let d = Sys.getcwd () in
    protect ~f:(fun () -> Sys.chdir cwd; f ()) () 
      ~finally: (fun _ -> Sys.chdir d)

  (* CR jfuruse: searching algorithm must be reconsidered *)        
  let load_module ?(spit=false) ~cwd ~load_paths name =
    let cmtname = name ^ if spit then ".cmti" else ".cmt" in
    try
      with_cwd cwd (fun () -> load ~load_paths cmtname)
    with
    | Failure s ->
        let spitname = name ^ if spit then ".cmt" else ".cmti" in
        Format.printf "%s load failed. Try to load %s@."
          cmtname spitname;
        try
          load ~load_paths spitname
        with
        | Failure s' -> failwithf "%s\n%s" s s'
end

include Load

let initial_env file =
  { Env.path   = file.Unit.path;
    cwd        = file.Unit.builddir;
    load_paths = file.Unit.loadpath;
    binding    = Binding.predef }

let invalid_env file =
  { Env.path   = file.Unit.path;
    cwd        = file.Unit.builddir;
    load_paths = file.Unit.loadpath;
    binding    = Binding.invalid }
    
type result =
    | File_itself
    | Found_at of string * Region.t
    | Predefined

let find_path_in_flat file path : PIdent.t * result =
  let env = 
    let env = invalid_env file in
    let str = Eval.structure env !!(file.Unit.flat) in
    Binding.set env.Env.binding str; (* dirty hack *)
    env
  in
  let find_loc pid =
    match  pid.PIdent.path with
    | "" -> Predefined
    | path ->
        (* CR jfuruse: loading twice... *)
        Debug.format "Finding %a@." PIdent.format pid;
        let file = Load.load ~load_paths:[] (Cmt.of_path path) in
        match pid.PIdent.ident with
        | None -> File_itself (* the whole file *)
        | Some id -> 
            try
              let path, r = 
                Hashtbl.find !!(file.Unit.id_def_regions) id
              in
              Found_at (file.Unit.builddir ^/ path, r)
            with
            | Not_found ->
                eprintf "Error: find location of id %a failed@."
                  PIdent.format pid;
                raise Not_found
  in
  
  let eval_and_find path =
    (* we need evaluate the path *)
    let module V = Value in
    let v = !!(Eval.find_path env path) in
    Debug.format "Value=%a@." V.Format.t v;
    match v with
    | V.Ident id                      -> id, find_loc id
    | V.Parameter id                  -> id, find_loc id
    | V.Structure (id, _, _)          -> id, find_loc id
    | V.Closure (id, _, _, _, _)      -> id, find_loc id
    | V.Error (Failure _ as e)        -> raise e
    | V.Error (Load.Old_cmt _ as exn) -> raise exn
    | V.Error exn                     -> raise exn
  in
  eval_and_find path

let str_of_global_ident ~cwd ~load_paths id =
  assert (Ident.global id);
  let file = Load.load_module ~spit:Spotconfig.print_interface ~cwd ~load_paths (Ident0.name id) in
  file.Unit.path,
  Eval.structure (initial_env file) file.Unit.top

let _ = Eval.str_of_global_ident := str_of_global_ident

let eval_packed env file =
  let f = Load.load ~load_paths:[""] (Cmt.of_path (env.Env.cwd ^/ file)) in
  Value.Structure ({ PIdent.path = f.Unit.path; ident = None },
                  Eval.structure (initial_env f) f.Unit.top,
                  None (* packed has no .mli *))

let _ = Eval.packed := eval_packed
