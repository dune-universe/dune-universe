(* File: mindstorm_lwt__NXT.ml

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


#define LWT
#define MODULE_ERR(err) STRINGIFY(Mindstorm_lwt.NXT: err)
#define MODULE(fn) STRINGIFY(Mindstorm_lwt.NXT.fn)

#include "../src/mindstorm__NXT.ml"
