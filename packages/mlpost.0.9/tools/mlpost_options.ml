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

open Mlpost
open Mlpost_desc_options

let call_cmd = Misc.call_cmd

let () =
  let user_opts = Queue.create () in
  let user_opt =
    ( "--",
      Arg.Rest (fun s -> Queue.add s user_opts),
      "The option given to the program" )
  in

  Arg.parse
    (Arg.align (user_opt :: spec))
    (fun _ ->
      raise
        (Arg.Bad
           "No anonymous option among the mlpost options, begin by -- for the \
            user options"))
    "A program compiled with mlpost";

  (* Replace the mlpost argument by the user one *)
  for i = 1 to Array.length Sys.argv - 1 do
    if Queue.is_empty user_opts then Sys.argv.(i) <- ""
    else Sys.argv.(i) <- Queue.pop user_opts
  done;
  (* And reset the current of Arg *)
  Arg.current := 0;

  let prelude =
    match !latex_file with
    | None -> None
    | Some f ->
        let s = Metapost_tool.read_prelude_from_tex_file f in
        Defaults.set_prelude s;
        Some s
  in

  let verbose = !verbose in

  Defaults.set_filename_prefix !filename_prefix;
  Defaults.set_required_files !required_files;
  Defaults.set_verbosity verbose;
  Defaults.set_t1disasm !t1disasm;

  let bn = Filename.concat (Sys.getcwd ()) (Filename.basename Sys.argv.(0)) in

  let do_at_exit () =
    if !dumpable then Metapost.dumpable ()
    else if !cairo then
      if not (!xpdf || !interactive) then
        if !png then Cairost.dump_png ()
        else if !svg then Cairost.dump_svg ()
        else if !pdf then Cairost.dump_pdf ()
        else Cairost.dump_ps ()
      else Cairost.dump_pdfs "_mlpost"
    else (
      if !mp && not (!xpdf || !interactive) then Metapost.dump_mp ?prelude bn
      else if !png && not (!xpdf || !interactive) then
        Metapost.dump_png ?prelude ~verbose ~clean:(not !dont_clean) bn
      else if !mps then Mps.dump ()
      else if !pgf then Pgf.dump ()
      else Metapost.dump ?prelude ~verbose ~clean:(not !dont_clean) bn;
      if !xpdf || !interactive then (
        Metapost.dump_tex ?prelude "_mlpost";
        (try Sys.remove "_mlpost.aux" with _ -> ());
        ignore (call_cmd ~verbose "pdflatex _mlpost.tex") ) );

    if !xpdf then
      if call_cmd ~verbose "fuser _mlpost.pdf" = 0 then
        ignore (call_cmd ~verbose "xpdf -remote mlpost -reload")
      else ignore (call_cmd ~verbose "setsid xpdf -remote mlpost _mlpost.pdf &")
  in

  (* When an exit is fired inside do_at_exit
     the function seems to be run again *)
  let done_once = ref false in
  at_exit (fun () ->
      if not !done_once then (
        done_once := true;
        do_at_exit () ))
