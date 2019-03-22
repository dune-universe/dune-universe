(* File: mindstorm__EV3.mli

   Copyright (C) 2015-

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

type usb
type bluetooth

type 'a conn
(** Abstract type representing a connection to a LEGO® mindstorm
    EV3 brick.  The type parameter indicates whether this connection
    is a USB, a WIFI, or a bluetooth one. *)


val connect_bluetooth : string -> bluetooth conn
(** [connect_bluetooth bdaddr] connects through bluetooth to the brick
    with bluetooth address [bdaddr].  See the section
    "{!section:Mindstorm.connectBluetooth}" for more information.

    @raise Unix.Unix_error in case of a connection problem.  In
    particular, [Unix.Unix_error(Unix.EHOSTDOWN, _,_)] is raised if
    the brick is not turned on.  *)

val close : _ conn -> unit
(** [close conn] closes the connection [conn] to the brick. *)


module Sound : sig
  val tone : _ conn -> vol:int -> freq:int -> ms:int -> unit
  (** [tone conn vol freq dur] plays a sound with volume [vol]
      (in [0 .. 100]), frequency [freq] (in [250 .. 10000]) for the
      duration of [ms] miliseconds (in [0 .. 32767].  *)

  ;;
end

;;
