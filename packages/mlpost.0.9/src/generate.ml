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

(** copied from File to remove dependency *)

let write_to filename f =
  let chan = open_out filename in
  let r = f chan in
  close_out chan;
  r

let write_to_formatted filename f =
  write_to filename (fun chan ->
      let fmt = Format.formatter_of_out_channel chan in
      let r = f fmt in
      Format.fprintf fmt "@?";
      r)

open Format

let minipage fmt coef i tmpl sep suf =
  fprintf fmt "@[<hov 2>\\begin{minipage}[tb]{%f\\textwidth}@\n" coef;
  fprintf fmt "@[<hov 2>\\begin{center}@\n";
  fprintf fmt
    "\\includegraphics[width=\\textwidth,height=\\textwidth,keepaspectratio]{%s%s%i%s}"
    tmpl sep i suf;
  fprintf fmt "@]@\n\\end{center}@\n";
  fprintf fmt "@]@\n\\end{minipage}@\n"

let generate_tex ?(pdf = false) tf tmpl1 tmpl2 l =
  let suf = if pdf then ".mps" else "" in
  let sep = if pdf then "-" else "." in
  write_to_formatted tf (fun fmt ->
      fprintf fmt "\\documentclass[a4paper]{article}@.";
      fprintf fmt "\\usepackage[]{graphicx}@.";
      fprintf fmt "@[<hov 2>\\begin{document}@.";
      List.iter
        (fun (i, _) ->
          fprintf fmt "@\n %i" i;
          minipage fmt 0.5 i tmpl1 sep suf;
          minipage fmt 0.5 i tmpl2 sep suf;
          fprintf fmt "@\n \\vspace{3cm}@\n")
        l;
      fprintf fmt "@]@\n\\end{document}@.")

let generate_tex_cairo tf tmpl1 tmpl2 tmpl3 l =
  write_to_formatted tf (fun fmt ->
      fprintf fmt "\\documentclass[a4paper]{article}@.";
      fprintf fmt "\\usepackage[]{graphicx}@.";
      fprintf fmt "@[<hov 2>\\begin{document}@.";
      List.iter
        (fun (i, _) ->
          fprintf fmt "@\n %i" i;
          minipage fmt 0.3 i tmpl1 "-" ".mps";
          minipage fmt 0.3 i tmpl2 "-" ".mps";
          minipage fmt 0.3 i tmpl3 "-" ".pdf";
          fprintf fmt "@\n \\vspace{3cm}@\n")
        l;
      fprintf fmt "@]@\n\\end{document}@.")
