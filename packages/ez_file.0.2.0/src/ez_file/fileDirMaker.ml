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

  type selector = M.path FileSelector.t

  let select = FileSelector.create
  let make_select = FileSelector.make_select
  let onedir = FileSelector.create ()
  let recdir = FileSelector.create ~deep:true ()

  let check select ~filepath ~filename =
    match M.lstat filename with
    | exception exn ->
        if select.FileSelector.filter true filepath then
          select.FileSelector.error exn filename;
        ( false, false )
    | st ->
        let recurse =
          select.FileSelector.deep &&
          (match st.MinUnix.st_kind with
           | MinUnix.S_DIR -> true
           | MinUnix.S_LNK ->
               select.FileSelector.follow_links && is_directory filename
           | _ -> false) &&
          select.FileSelector.filter true filepath
        in
        let keep =
          ( match select.FileSelector.kinds with
            | None -> true
            | Some kinds -> List.mem st.st_kind kinds) &&
          select.FileSelector.filter false filepath
        in
        (keep, recurse)


  let iter_dir ?(select=onedir) dir ~f =
    if not (is_directory dir) then raise (NotADirectory dir);
    match select.FileSelector.dft with
    | None ->
        let queue = Queue.create () in
        Queue.add (dir, "") queue;
        while not (Queue.is_empty queue) do
          let (dirname, dirpath) = Queue.take queue in
          let array = try readdir dirname with exn ->
            select.FileSelector.error exn dirname;
            [||] in
          for i = 0 to Array.length array - 1 do
            let basename = array.(i) in
            let filename = M.add_basename dirname basename in
            let filepath = Filename.concat dirpath basename in
            let (keep, recurse) = check select ~filepath ~filename in
            if keep then f filepath;
            if recurse then
              Queue.add (filename,filepath) queue
          done;
        done
    | Some dft ->
        let rec iter dirname dirpath =
          let array = try readdir dirname with exn ->
            select.FileSelector.error exn dirname;
            [||] in
          for i = 0 to Array.length array - 1 do
            let basename = array.(i) in
            let filename = M.add_basename dirname basename in
            let filepath = Filename.concat dirpath basename in
            let ( keep, recurse ) = check select ~filepath ~filename in
            match dft with
            | `Before ->
                if keep then f filepath;
                if recurse then iter filename filepath
            | `After ->
                if recurse then iter filename filepath;
                if keep then f filepath
          done;
        in
        iter dir ""

  let read_dir_to_revlist ?select filename =
    let files = ref [] in
    iter_dir ?select ~f:(fun file ->
        files := file :: !files) filename;
    !files

  let read_dir ?select filename =
    let res = read_dir_to_revlist ?select filename in
    let files = Array.of_list res in
    EzArray.rev files;
    files
  ;;

  let read_dir_to_list ?select filename =
    let res = read_dir_to_revlist ?select filename in
    List.rev res

  let iterator ?(select=onedir) dirname =
    if not (is_directory dirname) then
      raise (NotADirectory dirname);
    match select.FileSelector.dft with
    | None ->
        let dirs = Queue.create () in
        let files = ref None in
        Queue.add (dirname, "") dirs;
        let rec iter () =
          match !files with
          | None ->
              if Queue.is_empty dirs then None
              else
                let (dirname, dirpath) = Queue.take dirs in
                let array = try readdir dirname with exn ->
                  select.FileSelector.error exn dirname;
                  [||] in
                files := Some (dirname, dirpath, array, ref 0);
                iter ()
          | Some (dirname, dirpath, array, i) ->
              if !i = Array.length array then begin
                files := None;
                iter ()
              end else begin
                let basename = array.(!i) in
                let filename = M.add_basename dirname basename in
                let filepath = Filename.concat dirpath basename in
                incr i;
                let (keep, recurse) = check select ~filepath ~filename in
                if recurse then Queue.add (filename,filepath) dirs;
                if keep then Some filepath
                else iter ()
              end
        in
        iter
    | Some dft ->
        let dirs = ref [] in
        let enter_dir filename filepath after =
          let array = try readdir filename with exn ->
            select.FileSelector.error exn filename;
            [||] in
          dirs := (filename, filepath, array, ref 0, after) :: !dirs;
          ()
        in
        let rec iter () =
          match !dirs with
          | [] -> None
          | (dirname, dirpath, array, i, after) :: rem ->
              if !i = Array.length array then begin
                dirs := rem;
                match dft with
                | `Before -> iter ()
                | `After ->
                    if after then Some dirpath
                    else iter ()
              end
              else
                let basename = array.(!i) in
                let filename = M.add_basename dirname basename in
                let filepath = Filename.concat dirpath basename in
                incr i;
                let ( keep, recurse ) = check select ~filepath ~filename in
                if recurse then enter_dir filename filepath keep;
                match dft with
                | `Before ->
                    if keep then Some filepath
                    else iter ()
                | `After ->
                    if not recurse && keep then Some filepath
                    else iter ()
        in
        enter_dir dirname "" false;
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
        iter_dir ~f:(fun basename ->
            let file = M.add_basename filename basename in
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
