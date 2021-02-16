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

let supported = true

module M = Mps
open Icairost

let emit_pdf = emit_pdf

let emit_ps = emit_ps

let emit_png = emit_png

let emit_svg = emit_svg

let emit_pdfs = emit_pdfs

type cairo_t = Cairo.context

let emit_cairo = emit_cairo

(*let emit_cairo = fun x -> ()*)

let dump_pdf () =
  Queue.iter
    (fun (fname, fig) ->
      let pdfname = File.mk fname "pdf" in
      let pdfname_s = File.to_string pdfname in
      try emit_pdf pdfname_s fig with
      | Cairo.Error status ->
          Format.printf
            "An@ internal@ error@ occured@ during@ thegeneration@ of@ %s@ \
             with@ Cairo :@ %s@."
            pdfname_s
            (Cairo.status_to_string status)
      | error ->
          Format.printf
            "An@ internal@ error@ occured@ during@ the@ generation@ of@ %s :@ \
             %s@."
            pdfname_s (Printexc.to_string error))
    Defaults.figures

let dump_pdfs fname =
  let figs =
    List.rev (Queue.fold (fun l (_, x) -> x :: l) [] Defaults.figures)
  in
  emit_pdfs (fname ^ ".pdf") figs

let generate_pdfs pdffile figs =
  List.iter
    (fun (i, fig) ->
      emit_pdf ~msg_error:100. (Printf.sprintf "%s-%i.pdf" pdffile i) fig)
    figs

let dump_ext ext f () =
  Queue.iter
    (fun (fname, fig) ->
      let s = File.to_string (File.mk fname ext) in
      f s fig)
    Defaults.figures

let dump_ps = dump_ext "ps" emit_ps

let dump_png = dump_ext "png" emit_png

let dump_svg = dump_ext "svg" emit_svg
