type t

module Pdu_type : sig
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

(** [snmp_pdu_create pdu_type] creates a PDU of the specified type, the associated
    memory is not part of the OCaml heap.

    The PDU will be freed when the OCaml memory is garbage collected
*)
val snmp_pdu_create : Pdu_type.t -> t

(** [snmp_add_null_var] adds an oid to a pdu with a null value.
    Used to build a request PDU *)
val snmp_add_null_var : t -> Oid.t -> t

(** [snmp_add_variable] adds an oid and value to a pdu
    Used to build a request PDU *)
val snmp_add_variable : t -> Oid.t -> ASN1_value.t -> t
