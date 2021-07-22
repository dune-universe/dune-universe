(** Windows service 

    Only one service in process is supported
*)

exception Error of string

(** Signature describing service hosted in this process *)
module type Sig = sig
  (** Service identifier (unique) *)
  val name : string

  (** Service name displayed to user *)
  val display : string

  (** Service description *)
  val text : string

  (** Service arguments. *)
  val arguments : string list

  (** Callback executed when service needs to stop. *)
  val stop : unit -> unit
end

module Make (S : Sig) : sig
  (** Install current executable as Windows service, started with the given arguments *)
  val install : unit -> unit

  (** Remove service *)
  val remove : unit -> unit

  (** [run main]
    @param main function to run, stdin/stdout not available (will raise exception if used),
                when [S.stop] is called this function should return as soon as possible.
    @raise Failure if the program is being run as a console application rather than as a
                   service. *)
  val run : (unit -> unit) -> unit
end
