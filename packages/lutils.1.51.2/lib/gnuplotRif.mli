(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** Author: erwan.jahier@univ-grenoble-alpes.fr
**
*)

type terminal_kind =
    Jpg | Ps | Pdf | Cps | Eps | Latex | X11| Wxt | Qt | NoDisplay | Default
val terminal : terminal_kind ref
val window_size : int ref
val min_step:int option ref
val max_step:int option ref
val grid:bool ref
val vars_to_show:string list ref
val vars_to_hide:string list ref
val verbose:bool ref
val dynamic : bool ref

val rif_file : string ref (* The only one with no default value. *)

val ressource_file_usage : string
val read_ressource_file : unit -> unit (* parses a .gnuplot-rif resource file *)

val f: unit -> out_channel * int


