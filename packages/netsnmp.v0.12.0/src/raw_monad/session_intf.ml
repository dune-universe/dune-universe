open! Import

module type S = sig
  module IO : Io_intf.S

  include module type of (struct include Session end)

  val snmp_sess_init : unit -> netsnmp_session IO.t

  val snmp_sess_open
     : netsnmp_session:netsnmp_session
    -> version:Snmp_version.t
    -> retries:int
    -> timeout:int
    -> peername:string
    -> localname:string
    -> local_port:int
    -> community:string
    -> securityName:string
    -> securityAuthProto:Snmp_sec_auth_proto.t
    -> securityAuthPassword:string
    -> unit
    -> t IO.t

  val snmp_sess_close : t -> unit IO.t

  val snmp_sess_synch_response
   :  t
   -> Pdu.t
   -> ( Oid.t * ASN1_value.t ) list IO.t
end
