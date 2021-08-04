(******************************************************************************)
(*               OCamlrss                                                     *)
(*                                                                            *)
(*   Copyright (C) 2004-2013 Institut National de Recherche en Informatique   *)
(*   et en Automatique. All rights reserved.                                  *)
(*                                                                            *)
(*   This program is free software; you can redistribute it and/or modify     *)
(*   it under the terms of the GNU Lesser General Public License version      *)
(*   3 as published by the Free Software Foundation.                          *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU Library General Public License for more details.                     *)
(*                                                                            *)
(*   You should have received a copy of the GNU Library General Public        *)
(*   License along with this program; if not, write to the Free Software      *)
(*   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                 *)
(*   02111-1307  USA                                                          *)
(*                                                                            *)
(*   Contact: Maxence.Guesdon@inria.fr                                        *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

(* Testing *)

let fatal msg = prerr_endline msg; exit 1;;

let main () =
  if Array.length Sys.argv < 2 then
    fatal (Printf.sprintf "Usage: %s <rss file>" Sys.argv.(0));
  try
    let opts = Rss.make_opts
      ~read_channel_data: (fun x -> Some x)
      ~read_item_data: (fun x -> Some x)
      ()
    in
    let (channel, errors) = Rss.channel_t_of_file opts Sys.argv.(1) in
    let printer xmls = xmls in
    Rss.print_channel
      ~channel_data_printer: printer
      ~item_data_printer: printer
      ~indent: 2 Format.std_formatter channel;
    List.iter prerr_endline errors
  with
    | Sys_error s | Failure s -> fatal s
;;

let () = main ();;