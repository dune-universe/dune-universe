open Import

module type S = sig
  module IO : Io_intf.S

  include module type of (struct include Pdu end)

  val snmp_pdu_create : Pdu_type.t -> t IO.t
  val snmp_add_null_var : t -> Oid.t -> t IO.t
  val snmp_add_variable : t -> Oid.t -> ASN1_value.t -> t IO.t
end
