(* File: mindstorm_lwt.mli

   Copyright (C) 2016-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Drive Lego Minsdstorm bricks with OCaml! [Lwt] interface. *)

#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2
module NXT = Mindstorm_lwt__NXT
#else
module NXT : module type of Mindstorm_lwt__NXT
#endif

