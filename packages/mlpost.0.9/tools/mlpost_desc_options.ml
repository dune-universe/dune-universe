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

open Format
open Arg

let pdf = ref true

let latex_file = ref None

let set_latex_file f =
  if not (Sys.file_exists f) then (
    eprintf "mlpost: %s: no such file@." f;
    exit 1 );
  latex_file := Some f

let xpdf = ref false

let interactive = ref false

let eps = ref false

let verbose = ref false

let cairo = ref false

let t1disasm = ref None

let depend = ref false

let dumpable = ref false

let dont_clean = ref false

let mp = ref false

let mps = ref false

let pgf = ref false

let png = ref false

let svg = ref false

let filename_prefix = ref ""

let required_files : string list ref = ref []

let push_required s = required_files := s :: !required_files

(* notuple please or change Tool.wrap_options *)
let spec =
  [
    ("-pdf", Set pdf, " Generate .mps files (default)");
    ("-mp", Set mp, " Generate .mp files");
    ("-png", Set png, " Generate .png files");
    ("-ps", Clear pdf, " Generate .1 files");
    ("-mps", Set mps, " Use the experimental Mps output");
    ("-pgf", Set pgf, " Use the experimental Pgf output");
    ("-svg", Set svg, " Generate .svg files (only with cairo)");
    ("-latex", String set_latex_file, "<main.tex> Scan the LaTeX prelude");
    ("-eps", Set eps, " Generate encapsulated postscript files");
    ("-xpdf", Set xpdf, " wysiwyg mode using xpdf remote server");
    ("-i", Set interactive, " wysiwyg mode using file _mlpost.pdf");
    ("-required", String push_required, " Specify files required by latex");
    ("-v", Set verbose, " be a bit more verbose");
    ( "-depend",
      Set depend,
      " output dependency lines in a format suitable for the make(1) utility" );
    ("-dumpable", Set dumpable, " output one name of dumpable file by line");
    ("-dont-clean", Set dont_clean, " Don't remove intermediate files");
    ("-cairo", Set cairo, " Use the cairo backend instead of metapost");
    ( "-prefix",
      String (( := ) filename_prefix),
      "Add to all the filename this prefix" );
    ( "-t1disasm",
      String (fun s -> t1disasm := Some s),
      " Set the program used to decrypt PostScript Type 1 font, only with \
       cairo (default built-in one)" );
  ]
