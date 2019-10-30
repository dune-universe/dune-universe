(* Printers for debugging.  Load the printers in ocamldebug with:

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

load_printer "printers.cma"
install_printer Printers.pform
install_printer Printers.pid
install_printer Printers.pfact
install_printer Printers.pframe
set arg ../thy.gl

Consider placing the above commands in $HOME/.ocamldebug.

 *)

let ctx = Sym.mk_ctx ()
let pform = Form_printer.print_form ctx
let pid = Fact_printer.print_sym ctx
let pfact = Fact_printer.print_fact ctx
let pframe = Fact_printer.print_frame ctx
