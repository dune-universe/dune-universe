(******************************************************************************)
(*  ocaml-debian-formats: parse Debian files.                                 *)
(*                                                                            *)
(*  Copyright (C) 2010-2017, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

open ExtLib

let parse ch =
  let rec parse () =
    try
      let ln = String.trim (IO.read_line ch) in
      if String.starts_with ln "#" || String.starts_with ln "$" || ln = "" then
        parse ()
      else
        let rec cont_line str =
          if String.ends_with str "\\" then
            String.rchop str
            ^ try cont_line (IO.read_line ch) with IO.No_more_input -> ""
          else str
        in
        let full_line = cont_line ln in
        full_line :: parse ()
    with IO.No_more_input -> []
  in
  parse ()
