let pp_of_to_string to_string fmt x = Format.pp_print_string fmt (to_string x)

module Z = struct
  type t = Z.t [@@deriving eq, ord]

  let show = Z.to_string

  let pp = pp_of_to_string show
end

module Cstruct = struct
  type t = Cstruct.t [@@deriving eq, ord]

  let to_hex_string cs =
    let (`Hex hs) = Hex.of_cstruct cs in
    hs

  let show = to_hex_string

  let pp = pp_of_to_string show
end

module Asn_oid = struct
  type t = Asn.OID.t [@@deriving eq, ord, show]
end
