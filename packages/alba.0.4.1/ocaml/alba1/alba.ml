(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)


let platform =
  object
    method readdir (path:string): string array =
      Sys.readdir path
    method is_directory (path:string): bool =
      Sys.is_directory path
    method mkdir (path:string) (perm:int): unit =
      try
        Unix.mkdir path perm
      with Unix.Unix_error _ ->
        raise (Sys_error ("Cannot create directory \"" ^ path ^ "\""))
    method getcwd (): string =
      Unix.getcwd ()
    method getenv (str:string): string =
      Sys.getenv str
    method path_separator (): char = ':'
    method directory_separator (): char =
      let str = Filename.dir_sep in
      assert (String.length str = 1);
      str.[0]
    method modification_time (str:string): float =
      let open Unix in
      try
        (stat str).st_mtime
      with Unix_error _ ->
        raise (Sys_error ("Cannot stat file \" ^ str ^ \""))
  end

let _ =
  Platform.set platform;
  Alba_generic.run ()
