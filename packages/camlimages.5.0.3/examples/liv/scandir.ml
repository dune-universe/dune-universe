(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: scandir.ml,v 1.10 2008/06/16 22:35:42 furuse Exp $ *)

open Unix

let ignore_dirs = ref [".xvpics"; ".livpics"; "0000HEADER"; "0000HEADERS"]

let scan_dir f fn =
  let scanned = ref [] in
  let rec sub fn =
    let st = stat fn in
    match st.st_kind with
    | S_DIR when not (List.mem st.st_ino !scanned) ->
      scanned := st.st_ino :: !scanned;
      let dh = opendir fn in
      let files = ref [] in
      begin
        try while true do files := readdir dh :: !files done with
        | End_of_file -> ()
        | e -> prerr_endline ("readdir: " ^ Printexc.to_string e)
      end;
      closedir dh;
      let files = List.sort (fun x y -> compare y x) !files in
      let subdirs = ref [] in
      let treat fn' =
        if not (fn' = ".." || fn' = ".") then begin
          let fn'' = Filename.concat (if fn = "." then "" else fn) fn' in
          try
            let st = Unix.stat fn'' in
            match st.st_kind with
            | S_DIR -> (* sub-dir check is delayed *)
               if not (List.mem fn' !ignore_dirs) then
                 subdirs := fn'' :: !subdirs
            | _ ->
              f fn'' (* or sub fn', but it's more efficient *)
          with
          | Unix.Unix_error(_,"stat",_) ->
              prerr_endline (fn'' ^ ": stat failed")
          | e -> prerr_endline (fn'' ^ ": " ^ Printexc.to_string e) end in
      List.iter treat files;
      List.iter sub (List.rev !subdirs)
    | S_DIR -> ()
    | _ -> f fn in
  sub fn
