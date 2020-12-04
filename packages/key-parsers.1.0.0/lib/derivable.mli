module Z : sig
  type t = Z.t
  [@@deriving eq,ord,show]
end

module Cstruct : sig
  type t = Cstruct.t
  [@@deriving eq,ord,show]
end

module Asn_oid : sig
  type t = Asn.OID.t
  [@@deriving eq,ord,show]
end
