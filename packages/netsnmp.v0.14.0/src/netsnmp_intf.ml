open Netsnmp_raw_monad

module type S = sig
  module IO : Io_intf.S

  (** netsnmp provides high level access to SNMP functionality
  *)
  module ASN1_value = ASN1_value

  include (module type of struct include Netsnmp_types end)

  module Oid : sig

    include (module type of struct include Oid end)

    (** [of_string] - convert string to an [Oid.t], modules reference will be loaded
        if they are in the mib path *)
    val of_string : string -> t IO.t

    (** [to_string] - convert an [Oid.t] to a string *)
    val to_string : t -> string IO.t
  end

  module Mib : Mib_intf.S with module IO := IO

  module Connection : sig
    type t

    (** [connect] creates a connection to a peer based on the information
        passed in [Connection_info.t].
        This and [close] should be called as a pair.
    *)
    val connect : Connection_info.t -> t IO.t

    (** [close] - close an existing connection *)
    val close : t -> unit IO.t

    (** [with_connection] - connect to the remote agent using the supplied credentials
        and then call the function [f] with the session. Closes the session in all
        cases before returning *)
    val with_connection : Connection_info.t -> f:(t -> 'a) -> 'a IO.t

  end

  (** [add_mib_paths] adds directories to search for mibs in.  Note that this will
      be used for all subsequent mib searches. raises Failure on error *)
  val add_mib_paths : string list -> unit IO.t

  (** [get_s] - get the values of a list of symbolic oids. The function handles loading mib
      modules referenced by the supplied oids, eg passing 'SNMPv2-MIB::sysDescr.0' ensures
      SNMPv2-MIB is loaded.  *)
  val get_s : Connection.t -> string list -> (Oid.t * ASN1_value.t) list IO.t

  (** [get] - get the values of a list oids *)
  val get : Connection.t -> Oid.t list -> (Oid.t * ASN1_value.t) list IO.t

  (** [get_next] - get the value of the next node after the supplied oid *)
  val get_next : Connection.t -> Oid.t -> (Oid.t * ASN1_value.t) list IO.t


  module Raw : sig
    module Oid = Oid
    module Pdu : Pdu_intf.S with module IO := IO
    module Mib : Mib_intf.S with module IO := IO
    module Session : Session_intf.S with module IO := IO
  end
end
