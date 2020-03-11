(* Copyright (C) 2017 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

type t = <
    readdir: string -> string array;
    is_directory: string -> bool;
    mkdir: string -> int -> unit;
    getcwd: unit -> string;
    getenv: string -> string;
    path_separator: unit -> char;
    directory_separator: unit -> char;
    modification_time: string -> float
>


module Dummy = struct end

let obj: t option ref = ref None


let set (p:t): unit =
  obj := Some p


let get (): t =
  match !obj with
    None ->
      assert false (* Illegal use, not initialized *)
  | Some p ->
      p

let readdir (path:string): string array =
  (get ())#readdir path

let is_directory (path:string): bool =
  (get ())#is_directory path


let path_exists (path:string): bool =
  try
    ignore(is_directory path);
    true
  with Sys_error _ ->
    false

let mkdir (path:string) (perm:int): unit =
  (get())#mkdir path perm

let getcwd (): string =
  (get())#getcwd ()

let getenv (str:string): string =
  (get())#getenv str

let path_separator (): char =
  (get())#path_separator ()

let directory_separator (): char =
  (get())#directory_separator ()


let modification_time (str:string): float =
  (get())#modification_time str


let write_dummy (path:string): unit =
  close_out (open_out path)


module Filename =
  struct
    let concat (dir:string) (name:string): string =
      Filename.concat dir name
  end



let system (cmd:string): Unix.process_status =
  (* Execute the command [cmd] by the shell and return the exit status. *)
  Unix.system cmd

let system_with_output (cmd:string): string list =
  (* Execute the command [cmd] by the shell and return the output of the
     command as a list of lines (i.e. strings).
   *)
  let chin,chout,cherr =
    Unix.open_process_full cmd (Unix.environment ()) in
  let rec call ch lst =
    try
      call ch ((input_line ch) :: lst)
    with
      End_of_file ->
      lst
  in
  let lst_in  = call chin [] in
  let lst_err = call cherr [] in
  (* Note: There is a possible deadlock if the system command writes to stderr
     and afterward to stdout and the pipe buffer is not large enough to store
     all the outout to stdout. *)
  begin
    match List.rev lst_err with
    | [] ->
       ()
    | hd::_ ->
       raise (Sys_error hd)
  end;
  close_in chin;
  close_in cherr;
  close_out chout;
  List.rev lst_in
