(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

let id x = x

let print_option start printer fmt = function
  | None -> ()
  | Some o -> Format.fprintf fmt "%s%a " start printer o

let date_string () =
  let tm = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%d/%d/%d, %d:%d:%d" (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    (tm.Unix.tm_year + 1900) tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let rec print_list sep prf fmt = function
  | [] -> ()
  | [ x ] -> prf fmt x
  | x :: xs ->
      prf fmt x;
      sep fmt ();
      print_list sep prf fmt xs

let print_iter1 iter sep print fmt l =
  let first = ref true in
  iter
    (fun x ->
      if !first then first := false else sep fmt ();
      print fmt x)
    l

let print_iter2 iter sep1 sep2 print1 print2 fmt l =
  let first = ref true in
  iter
    (fun x y ->
      if !first then first := false else sep1 fmt ();
      print1 fmt x;
      sep2 fmt ();
      print2 fmt y)
    l

let space fmt () = Format.fprintf fmt "@ "

let comma fmt () = Format.fprintf fmt ",@ "

let semicolon fmt () = Format.fprintf fmt ";@ "

let newline fmt () = Format.fprintf fmt "@\n "

let nothing _fmt _ = ()

let print_int32 fmt i = Format.fprintf fmt "%ld" i

let rec fold_from_to f acc a b =
  if a <= b then fold_from_to f (f acc a) (a + 1) b else acc

let sprintf s =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Format.kfprintf
    (fun _ ->
      Format.pp_print_flush fmt ();
      Buffer.contents buf)
    fmt s

(*Filename.generic_quote*)
let generic_quote whatquote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  for i = 0 to l - 1 do
    if s.[i] = whatquote then Buffer.add_string b quotequote
    else Buffer.add_char b s.[i]
  done;
  Buffer.contents b

let generic_quote_list lwqq s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  for i = 0 to l - 1 do
    if List.mem_assoc s.[i] lwqq then
      Buffer.add_string b (List.assoc s.[i] lwqq)
    else Buffer.add_char b s.[i]
  done;
  Buffer.contents b

let call_cmd ?(inv = false) ?(outv = false) ?(verbose = false) cmd =
  (* inv = true -> print command line
   * outv = true -> print command output
   * verbose = true -> both
   *)
  if inv || verbose then Format.printf "+ %s@." cmd;
  let ((outc, inc, errc) as proc) =
    Unix.open_process_full cmd (Unix.environment ())
  in
  close_out inc;
  let out_descr = Unix.descr_of_in_channel outc
  and err_descr = Unix.descr_of_in_channel errc in
  (* using Unix.select as in OCaml PLEAC *)
  let selector = ref [ out_descr; err_descr ] in
  let buf = Bytes.create 1024 in
  while !selector <> [] do
    let can_read, _, _ = Unix.select !selector [] [] 1.0 in
    List.iter
      (fun fh ->
        let ret = Unix.read fh buf 0 1024 in
        if ret = 0 then selector := List.filter (fun fh' -> fh <> fh') !selector;
        if outv || verbose then
          if fh = err_descr then ignore (Unix.write Unix.stderr buf 0 ret)
          else ignore (Unix.write Unix.stdout buf 0 ret))
      can_read
  done;
  let status = Unix.close_process_full proc in
  flush stdout;
  flush stderr;
  match status with
  | Unix.WEXITED n ->
      if n > 124 then (
        Printf.eprintf "command %s has aborted with exit code: %d@\n" cmd n;
        exit n );
      n
  | _ -> exit 1

(* persistent queues *)
module Q = struct
  type 'a t = 'a list * 'a list

  exception Empty

  let empty = ([], [])

  let push x (i, o) = (x :: i, o)

  let pop = function
    | [], [] -> raise Empty
    | i, x :: o -> (x, (i, o))
    | i, [] -> (
        match List.rev i with x :: o -> (x, ([], o)) | [] -> assert false )

  let of_list l = List.fold_left (fun q c -> push c q) empty l
end

module StringMap = Map.Make (String)

module IntMap = Map.Make (struct
  type t = int

  let compare = Stdlib.compare
end)
