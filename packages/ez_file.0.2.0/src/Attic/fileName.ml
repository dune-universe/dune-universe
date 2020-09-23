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

open OcpCompat

  let copy_file f1 f2 =
    let s = ReentrantBuffers.get default_buffer_size in
    let ic = open_in_bin f1 in
    let oc = open_out_bin f2 in
    let rec copy s ic oc =
      let n = input ic s 0 default_buffer_size in
      if n = 0 then () else (output oc s 0 n; copy s ic oc)
    in copy s ic oc;
    close_in ic;
    close_out oc;
    ReentrantBuffers.free s

  let iter_blocks f file =
    let s = ReentrantBuffers.get 32768 in
    let ic = open_in_bin file in
    let rec iter f ic s =
      let nread = input ic s 0 32768 in
      if nread > 0 then begin
        f s 0 nread;
        iter f ic s
      end
    in
    iter f ic s;
    close_in ic;
    ReentrantBuffers.free s

  let iter_dir f dirname =
    let files = Sys.readdir dirname in
    Array.iter f files

  let iter_files ?(recursive=false) f dirname =
    let rec iter dirname dir =
      let files = Sys.readdir (Filename.concat dirname dir) in
      Array.iter (fun file ->
        let file = Filename.concat dir file in
        if Sys.is_directory (Filename.concat dirname file) then begin
          if recursive then iter dirname file
        end else
          f file
      ) files
    in
    iter dirname ""


  let rec safe_mkdir ?(mode=0o755) filename =
    try
      let st = MinUnix.stat filename in
      match st.MinUnix.st_kind with
        MinUnix.S_DIR -> ()
      | _ ->
        failwith (Printf.sprintf
                    "File.safe_mkdir: %S exists, but is not a directory"
                    filename)
    with MinUnix.Unix_error (MinUnix.ENOENT, _, _) ->
      let dirname = Filename.dirname filename in
      safe_mkdir ~mode dirname;
      let basename = Filename.basename filename in
      match basename with
      | "." | ".." -> ()
      | _ ->
        MinUnix.mkdir filename mode

  (* [dst] must be the target file name, not the name of its directory *)
  let rec copy_rec src dst =
    (*    Printf.eprintf "copy_rec: %S -> %S\n%!" src dst; *)
    match (MinUnix.stat src).MinUnix.st_kind with
    | MinUnix.S_DIR ->
      safe_mkdir dst;
      iter_dir (fun basename ->
        copy_rec (Filename.concat src basename)
          (Filename.concat dst basename)) src
    | MinUnix.S_REG ->
      copy_file src dst
    | _ ->
      failwith (Printf.sprintf
                  "File.copy_rec: cannot copy unknown kind file %S"
                  src)

  (* [dst] must be the target file name, not the name of its directory *)
  let rec uncopy_rec src dst =
    match
      (try Some (MinUnix.stat src).MinUnix.st_kind with _ -> None),
      (try Some (MinUnix.stat dst).MinUnix.st_kind with _ -> None)
    with
    | _, None -> ()
    | Some MinUnix.S_DIR, Some MinUnix.S_DIR ->
      iter_dir (fun basename ->
        uncopy_rec (Filename.concat src basename)
          (Filename.concat dst basename)) src;
      (try MinUnix.rmdir dst with _ -> ())
    | Some MinUnix.S_REG, Some MinUnix.S_REG ->
      Sys.remove dst
    | _ ->
      failwith (Printf.sprintf
                  "File.uncopy_rec: inconsistent kinds between %S and %S"
                  src dst)


let output_line chan string =
  output_string chan (string ^ line_separator)

let lines_of_file file =
  let ic = open_in file in
  let lines = ref [] in
  begin try
          while true do
            lines := input_line ic :: !lines
          done
    with End_of_file -> ()
  end;
  close_in ic;
  List.rev !lines

let file_of_lines filename lines =
  let oc = open_out filename in
  List.iter (fun l -> output_line oc l) lines;
  close_out oc
