module Z : sig
  type t = Z.t
  [@@deriving bin_io,eq,ord,show,yojson]
end

module Cstruct : sig
  type t = Cstruct.t
  [@@deriving bin_io,eq,ord,show,yojson]
end

module Asn_oid : sig
  type t = Asn.OID.t
  [@@deriving eq,ord,show]

  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) Result.result
  include Bin_prot.Binable.S with type t := t
end
