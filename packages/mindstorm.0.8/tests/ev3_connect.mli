(** Module to handle the Bluetooth and USB program arguments to
    establish a bluetooth or a USB connection with the EV3 LEGO® brick. *)

type t = {
  args : (Arg.key * Arg.spec * Arg.doc) list;
  (** Possible additional program arguments. *)

  f : 'a. 'a Mindstorm.EV3.conn -> unit;
  (** The function to execute once connected. *)
}

val and_do : t -> unit
(** [and_do d] establishes the connection, execute [d.f] and close
    the connection. *)
