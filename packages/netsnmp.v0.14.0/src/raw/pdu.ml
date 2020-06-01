type t

module Pdu_type = struct
  type t = 
    | Get
    | Getnext
    | Response
    | Set
    | Trap
    | Getbulk
    | Inform
    | Trap2
    | Report
end

external snmp_pdu_create : Pdu_type.t -> t = "caml_snmp_pdu_create"
external snmp_add_null_var : t -> Oid.t -> t = "caml_snmp_add_null_var"
external snmp_add_variable : t -> Oid.t -> ASN1_value.t -> t  = "caml_snmp_add_variable"
