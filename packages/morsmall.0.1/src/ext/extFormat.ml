(******************************************************************************)
(*                                                                            *)
(*                                   RSCLI                                    *)
(*                                                                            *)
(*                  A command-line interface for RSCDS tunes                  *)
(*                Copyright (C) 2017 Nicolas "Niols" Jeannerod                *)
(*                                                                            *)
(*   This program is free software: you can redistribute it and/or modify     *)
(*   it under the terms of the GNU General Public License as published by     *)
(*   the Free Software Foundation, either version 3 of the License, or        *)
(*   (at your option) any later version.                                      *)
(*                                                                            *)
(*   This program is distributed in the hope that it will be useful,          *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*   GNU General Public License for more details.                             *)
(*                                                                            *)
(*   You should have received a copy of the GNU General Public License        *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                            *)
(******************************************************************************)

type 'a fprintf = Format.formatter -> 'a -> unit

let to_string__of__fprintf fprintf value =
  let buf = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buf in
  fprintf ppf value;
  Format.fprintf ppf "@?";
  Buffer.contents buf

let to_channel__of__fprintf fprintf channel value =
  let ppf = Format.formatter_of_out_channel channel in
  fprintf ppf value;
  Format.fprintf ppf "@?"
  
let to_file__of__fprintf fprintf filename value =
  let oc = open_out filename in
  to_channel__of__fprintf fprintf oc value;
  close_out oc
