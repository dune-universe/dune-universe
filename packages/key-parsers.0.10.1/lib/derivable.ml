open Bin_prot.Std

let pp_of_to_string to_string fmt x =
  Format.pp_print_string fmt (to_string x)

module Bin_string = struct
  type t = string
  [@@deriving bin_io]
end

module Z = struct
  type t = Z.t
  [@@deriving eq,ord]

  let show = Z.to_string
  let pp = pp_of_to_string show

  let of_yojson = function
    | `String s -> Result.Ok (Z.of_string s)
    | _ -> Result.Error "Cannot convert this json value to Z.t"

  let to_yojson z =
    `String (Z.to_string z)

  include Bin_prot.Utils.Make_binable
      (struct
        module Binable = Bin_string
        type t = Z.t
        let to_binable = Z.to_string
        let of_binable = Z.of_string
      end)
end

module Cstruct = struct
  type t = Cstruct.t
  [@@deriving eq,ord]

  let hex_regexp = Str.regexp "0x[0-9a-f]*"

  let is_hex s =
    Str.string_match hex_regexp s 0 && [%eq: string] (Str.matched_string s) s

  let to_hex_string cs =
    let `Hex hs = Hex.of_cstruct cs in
    hs

  let show = to_hex_string
  let pp = pp_of_to_string show

  let to_yojson cs =
    `String ("0x" ^ to_hex_string cs)

  let of_yojson = function
    | `String "" -> Result.Ok (Cstruct.of_string "")
    | `String s ->
      if is_hex s then
        let hex_s = `Hex (String.sub s 2 (String.length s - 2)) in
        Result.Ok (Hex.to_cstruct hex_s)
      else
        Result.Error "Key_parsers.Cstruct.of_yojson: expected hex encoded json string"
    | _ -> Result.Error "Key_parsers.Cstruct.of_yojson: expected json string"

  include Bin_prot.Utils.Make_binable
      (struct
        module Binable = Bin_string
        type t = Cstruct.t
        let to_binable = Cstruct.to_string
        let of_binable s = Cstruct.of_string s
      end)
end

module Asn_oid = struct
  type t = Asn.OID.t
  [@@deriving eq,ord,show]

  let of_string_exn s =
    match Asn.OID.of_string s with
    | Some oid -> oid
    | None -> invalid_arg "Key_parsers.Derivable.Asn_OID.of_string_exn"

  let of_yojson = function
    | `String s ->
      begin
        match Asn.OID.of_string s with
        | Some oid -> Result.Ok oid
        | None -> Result.Error "Key_parsers.Derivable.Asn_OID.of_yojson"
      end
    | _ -> Result.Error "Cannot convert this json value to Asn.OID.t"

  let to_yojson oid =
    `String (show oid)

  include Bin_prot.Utils.Make_binable
      (struct
        module Binable = Bin_string
        type t = Asn.OID.t
        let to_binable = show
        let of_binable = of_string_exn
      end)
end
