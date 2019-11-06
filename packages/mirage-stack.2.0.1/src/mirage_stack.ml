module type V4 = sig

  include Mirage_device.S

  module UDPV4: Mirage_protocols.UDPV4

  module TCPV4: Mirage_protocols.TCPV4

  module IPV4: Mirage_protocols.IPV4

  val udpv4: t -> UDPV4.t
  (** [udpv4 t] obtains a descriptor for use with the [UDPV4] module,
      usually to transmit traffic. *)

  val tcpv4: t -> TCPV4.t
  (** [tcpv4 t] obtains a descriptor for use with the [TCPV4] module,
      usually to initiate outgoing connections. *)

  val ipv4: t -> IPV4.t
  (** [ipv4 t] obtains a descriptor for use with the [IPV4] module,
      which can handle raw IPv4 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen_udpv4: t -> port:int -> UDPV4.callback -> unit
  (** [listen_udpv4 t ~port cb] registers the [cb] callback on the
      UDPv4 [port] and immediately return.  If [port] is invalid (not
      between 0 and 65535 inclusive), it raises [Invalid_argument].
      Multiple bindings to the same port will overwrite previous
      bindings, so callbacks will not chain if ports clash. *)

  val listen_tcpv4: ?keepalive:Mirage_protocols.Keepalive.t
    -> t -> port:int -> (TCPV4.flow -> unit Lwt.t) -> unit
  (** [listen_tcpv4 ~keepalive t ~port cb] registers the [cb] callback
      on the TCPv4 [port] and immediatey return.  If [port] is invalid (not
      between 0 and 65535 inclusive), it raises [Invalid_argument].
      Multiple bindings to the same port will overwrite previous
      bindings, so callbacks will not chain if ports clash.
      If [~keepalive] is provided then these keepalive settings will be
      applied to the accepted connections before the callback is called. *)

  val listen: t -> unit Lwt.t
  (** [listen t] requests that the stack listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end
