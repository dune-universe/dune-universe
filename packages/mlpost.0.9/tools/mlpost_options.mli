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

(** Provide command-line options and dump figures. *)

(** When you link with this module, command-line options of the Mlpost tool
    are added to your program. If you want to use your own options you have
    to give them after a ["--"] argument. This module will replace all arguments
    before this ["--"] (included) by [""], and then shift all remaining
    arguments to the beginning.

    This module also calls [dump] when your program exits. This produces
    the files of the figures you [emit]ed. *)
