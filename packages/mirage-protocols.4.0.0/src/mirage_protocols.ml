module Ethernet = struct
  type error = [ `Exceeds_mtu ]
  let pp_error ppf = function
    | `Exceeds_mtu -> Fmt.string ppf "exceeds MTU"

  type proto = [ `ARP | `IPv4 | `IPv6 ]
  let pp_proto ppf = function
    | `ARP -> Fmt.string ppf "ARP"
    | `IPv4 -> Fmt.string ppf "IPv4"
    | `IPv6 -> Fmt.string ppf "IPv6"
end

module Ip = struct
  type error = [
    | `No_route of string (** can't send a message to that destination *)
    | `Would_fragment
  ]
  let pp_error ppf = function
    | `No_route s -> Fmt.pf ppf "no route to destination: %s" s
    | `Would_fragment -> Fmt.string ppf "would fragment"

  type proto = [ `TCP | `UDP | `ICMP ]
  let pp_proto ppf = function
    | `TCP -> Fmt.string ppf "TCP"
    | `UDP -> Fmt.string ppf "UDP"
    | `ICMP -> Fmt.string ppf "ICMP"
end

(** Configuration *)
type ipv4_config = {
  address : Ipaddr.V4.t;
  network : Ipaddr.V4.Prefix.t;
  gateway : Ipaddr.V4.t option;
}

module Arp = struct
  type error = [
    | `Timeout (** Failed to establish a mapping between an IP and a link-level address *)
  ]
  let pp_error ppf = function
  | `Timeout -> Fmt.pf ppf "could not determine a link-level address for the IP address given"
end

module Tcp = struct
  type error = [ `Timeout | `Refused]
  type write_error = [ error | Mirage_flow.write_error]

  let pp_error ppf = function
  | `Timeout -> Fmt.string ppf "connection attempt timed out"
  | `Refused -> Fmt.string ppf "connection attempt was refused"

  let pp_write_error ppf = function
  | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  | #error as e                   -> pp_error ppf e
end

module type ETHERNET = sig
  type error = private [> Ethernet.error]
  val pp_error: error Fmt.t
  include Mirage_device.S
  val write: t -> ?src:Macaddr.t -> Macaddr.t -> Ethernet.proto -> ?size:int ->
    (Cstruct.t -> int) -> (unit, error) result Lwt.t
  val mac: t -> Macaddr.t
  val mtu: t -> int
  val input:
    arpv4:(Cstruct.t -> unit Lwt.t) ->
    ipv4:(Cstruct.t -> unit Lwt.t) ->
    ipv6:(Cstruct.t -> unit Lwt.t) ->
    t -> Cstruct.t -> unit Lwt.t
end

module type ARP = sig
  include Mirage_device.S
  type error = private [> Arp.error]
  val pp_error: error Fmt.t
  val pp : t Fmt.t
  val get_ips : t -> Ipaddr.V4.t list
  val set_ips : t -> Ipaddr.V4.t list -> unit Lwt.t
  val remove_ip : t -> Ipaddr.V4.t -> unit Lwt.t
  val add_ip : t -> Ipaddr.V4.t -> unit Lwt.t
  val query : t -> Ipaddr.V4.t -> (Macaddr.t, error) result Lwt.t
  val input : t -> Cstruct.t -> unit Lwt.t
end

module type IP = sig
  type error = private [> Ip.error]
  val pp_error: error Fmt.t
  type ipaddr
  val pp_ipaddr : ipaddr Fmt.t
  include Mirage_device.S
  type callback = src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit Lwt.t
  val input:
    t ->
    tcp:callback -> udp:callback -> default:(proto:int -> callback) ->
    Cstruct.t -> unit Lwt.t
  val write: t -> ?fragment:bool -> ?ttl:int ->
    ?src:ipaddr -> ipaddr -> Ip.proto -> ?size:int -> (Cstruct.t -> int) ->
    Cstruct.t list -> (unit, error) result Lwt.t
  val pseudoheader : t -> ?src:ipaddr -> ipaddr -> Ip.proto -> int -> Cstruct.t
  val src: t -> dst:ipaddr -> ipaddr
  val get_ip: t -> ipaddr list
  val mtu: t -> int
end

module type IPV4 = IP with type ipaddr = Ipaddr.V4.t
module type IPV6 = IP with type ipaddr = Ipaddr.V6.t

module type ICMP = sig
  include Mirage_device.S
  type ipaddr
  type error
  val pp_error: error Fmt.t
  val input : t -> src:ipaddr -> dst:ipaddr -> Cstruct.t -> unit Lwt.t
  val write : t -> dst:ipaddr -> ?ttl:int -> Cstruct.t -> (unit, error) result Lwt.t
end

module type ICMPV4 = ICMP with type ipaddr = Ipaddr.V4.t
module type ICMPV6 = ICMP with type ipaddr = Ipaddr.V6.t

module type UDP = sig
  type error
  val pp_error: error Fmt.t
  type ipaddr
  type ipinput
  include Mirage_device.S
  type callback = src:ipaddr -> dst:ipaddr -> src_port:int -> Cstruct.t -> unit Lwt.t
  val input: listeners:(dst_port:int -> callback option) -> t -> ipinput
  val write: ?src_port:int -> ?ttl:int -> dst:ipaddr -> dst_port:int -> t -> Cstruct.t ->
    (unit, error) result Lwt.t
end

module type UDPV4 = UDP with type ipaddr = Ipaddr.V4.t
module type UDPV6 = UDP with type ipaddr = Ipaddr.V6.t

module Keepalive = struct
  type t = {
    after: Duration.t;
    interval: Duration.t;
    probes: int;
  }
end

module type TCP = sig
  type error = private [> Tcp.error]
  type write_error = private [> Tcp.write_error]
  type ipaddr
  type ipinput
  type flow
  include Mirage_device.S
  include Mirage_flow.S with
      type flow   := flow
  and type error  := error
  and type write_error := write_error

  val dst: flow -> ipaddr * int
  val write_nodelay: flow -> Cstruct.t -> (unit, write_error) result Lwt.t
  val writev_nodelay: flow -> Cstruct.t list -> (unit, write_error) result Lwt.t
  val create_connection: ?keepalive:Keepalive.t -> t -> ipaddr * int -> (flow, error) result Lwt.t
  type listener = {
    process: flow -> unit Lwt.t;
    keepalive: Keepalive.t option;
  }
  val input: t -> listeners:(int -> listener option) -> ipinput
end

module type TCPV4 = TCP with type ipaddr = Ipaddr.V4.t
module type TCPV6 = TCP with type ipaddr = Ipaddr.V6.t

module type DHCP_CLIENT = sig
  type t = ipv4_config Lwt_stream.t
end
