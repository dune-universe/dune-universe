
(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Make(M : sig
    type path

    val mkdir : path -> int -> unit
    val stat : path -> MinUnix.stats
    val lstat : path -> MinUnix.stats
    val readdir : path -> string array
    val rmdir : path -> unit

    val remove : path -> unit

    val basename : path -> string
    val dirname : path -> path
    val add_basename : path -> string -> path

    val to_string : path -> string
  end) : FileSig.DIRECTORY_OPERATIONS with type t := M.path
= struct

  let is_directory t = (M.stat t).MinUnix.st_kind = MinUnix.S_DIR
  let is_link t = (M.lstat t).MinUnix.st_kind = MinUnix.S_LNK

  exception NotADirectory of M.path

  let mkdir dir perm = M.mkdir dir perm
  let readdir dir =
    if not (is_directory dir) then raise (NotADirectory dir);
    let array = M.readdir dir in
    Array.sort compare array;
    array

  let rmdir dir =
    if not (is_directory dir) then raise (NotADirectory dir);
    M.rmdir dir

  let rec make_dir ?(mode=0o755) ?(p=false) filename =
    try
      if not (is_directory filename) then
        raise (NotADirectory filename)
    with
    | MinUnix.Unix_error (MinUnix.ENOENT, _, _) ->
      if p then begin
        let dirname = M.dirname filename in
        make_dir ~mode ~p dirname;
      end;
      let basename = M.basename filename in
      match basename with
      | "." | ".." -> ()
      | _ ->
        try
          M.mkdir filename mode
        with MinUnix.Unix_error (MinUnix.EEXIST, _, _) -> ()

  let safe_mkdir = make_dir ~p:true ~mode:0o755

  let select = FileSelector.create
  let onedir = FileSelector.create ()

  let recurse select file path filename =
    match M.lstat filename with
    | exception exn ->
      if select.FileSelector.filter true file path then
        select.FileSelector.error exn path filename;
      false
    | st ->
      select.FileSelector.deep &&
      (match st.MinUnix.st_kind with
       | MinUnix.S_DIR -> true
       | MinUnix.S_LNK ->
         select.FileSelector.follow_links && is_directory filename
       | _ -> false) &&
      select.FileSelector.filter true file path

  let iter_dir
      ?(select=onedir)
      f file =
    if not (is_directory file) then
      raise (NotADirectory file);
    let localpath = "/" in
    match select.FileSelector.dft with
    | None ->
      let queue = Queue.create () in
      Queue.add (file, localpath) queue;
      while not (Queue.is_empty queue) do
        let (file, localpath) = Queue.take queue in
        let array = try readdir file with exn ->
          select.FileSelector.error exn localpath file;
          [||] in
        for i = 0 to Array.length array - 1 do
          let basename = array.(i) in
          let file = M.add_basename file basename in
          let localpath = Filename.concat localpath basename in
          if select.FileSelector.filter false basename localpath then
            f ~basename ~localpath ~file;
          let recurse = recurse select basename localpath file in
          if recurse then
            Queue.add (file,localpath) queue
        done;
      done
    | Some dft ->
      let rec iter file localpath =
        let array = try readdir file with exn ->
          select.FileSelector.error exn localpath file;
          [||] in
        for i = 0 to Array.length array - 1 do
          let basename = array.(i) in
          let file = M.add_basename file basename in
          let localpath = Filename.concat localpath basename in
          let recurse = recurse select basename localpath file in
          match dft with
          | `Before ->
            if select.FileSelector.filter false basename localpath then
              f ~basename ~localpath ~file;
            if recurse then iter file localpath;
          | `After ->
            if recurse then iter file localpath;
            if select.FileSelector.filter false basename localpath then
              f ~basename ~localpath ~file;
        done;
      in
      iter file localpath

  let read_dir_to_revlist ?select filename =
    let files = ref [] in
    iter_dir ?select (fun ~basename:_ ~localpath:_ ~file ->
        files := file :: !files) filename;
    !files

  let read_dir ?select filename =
    let res = read_dir_to_revlist ?select filename in
    let files = Array.of_list res in
    EzArray.rev files;
    files

  let read_dir_to_list ?select filename =
    let res = read_dir_to_revlist ?select filename in
    List.rev res

  let iterator ?(select=onedir) filename =
    if not (is_directory filename) then
      raise (NotADirectory filename);
    let path = "/" in
    match select.FileSelector.dft with
    | None ->
      let dirs = Queue.create () in
      let files = ref None in
      Queue.add (filename, path) dirs;
      let rec iter () =
        match !files with
        | None ->
          if Queue.is_empty dirs then None
          else
            let (filename, path) = Queue.take dirs in
            let array = try readdir filename with exn ->
              select.FileSelector.error exn path filename;
              [||] in
            files := Some (filename, path, array, ref 0);
            iter ()
        | Some (filename, path, array, i) ->
          if !i = Array.length array then begin
            files := None;
            iter ()
          end else begin
            let file = array.(!i) in
            let filename = M.add_basename filename file in
            let path = Filename.concat path file in
            incr i;
            let recurse = recurse select file path filename in
            if recurse then
              Queue.add (filename,path) dirs;
            if select.FileSelector.filter false file path then
              Some (path, filename)
            else
              iter ()
          end
      in
      iter
    | Some dft ->
      let dirs = ref [] in
      let enter_dir ?file filename path =
        let array = try readdir filename with exn ->
          select.FileSelector.error exn path filename;
          [||] in
        dirs := (filename, path, array, ref 0, file) :: !dirs;
        ()
      in
      let rec iter () =
        match !dirs with
        | [] -> None
        | (filename, path, array, i, maybe_file) :: rem ->
          if !i = Array.length array then begin
            dirs := rem;
            match maybe_file with
            | None -> iter ()
            | Some file ->
              match dft with
              | `Before -> iter ()
              | `After ->
                if select.FileSelector.filter false file path then
                  Some (path, filename)
                else
                  iter ()
          end
          else
            let file = array.(!i) in
            let filename = M.add_basename filename file in
            let path = Filename.concat path file in
            incr i;
            let recurse = recurse select file path filename in
            if recurse then enter_dir ~file filename path;
            match dft with
            | `Before ->
              if select.FileSelector.filter false file path then
                Some (path, filename)
              else
                iter ()
            | `After -> iter ()
      in
      enter_dir filename path;
      iter

  let remove_dir ?(all=false) ?glob dir =
    let filter = match glob with
      | None -> (fun _ -> true)
      | Some glob ->
        fun s ->
          FileSelector.globber glob s
    in
    let rec iter ~all ~filter ~glob filename =
      if all then
        iter_dir (fun ~basename ~localpath:_ ~file ->
            if not (is_link file) && is_directory file then begin
              iter ~all ~filter ~glob file
            end else begin
              if filter basename then
                M.remove file
            end
          ) filename;
      match glob with
      | None -> rmdir filename
      | Some _ -> ()
    in
    iter ~all ~filter ~glob dir

  let _ = M.to_string
end
