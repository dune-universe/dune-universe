let base_rsa_oid = Asn.OID.(base 0 4 <|| [0;127;0;7;2;2;2;1])
let base_ecdsa_oid = Asn.OID.(base 0 4 <|| [0;127;0;7;2;2;2;2])

let rsa_oids =
  let open Asn.OID in
  [ base_rsa_oid
  ; base_rsa_oid <| 1
  ; base_rsa_oid <| 2
  ; base_rsa_oid <| 3
  ; base_rsa_oid <| 4
  ; base_rsa_oid <| 5
  ; base_rsa_oid <| 6
  ]

let ecdsa_oids =
  let open Asn.OID in
  [ base_ecdsa_oid
  ; base_ecdsa_oid <| 1
  ; base_ecdsa_oid <| 2
  ; base_ecdsa_oid <| 3
  ; base_ecdsa_oid <| 4
  ; base_ecdsa_oid <| 5
  ]

type algo_typ =
  | Rsa of Asn.OID.t
  | Ecdsa of Asn.OID.t
  | Unknown of Asn.OID.t

type parser_state =
  | Init
  | Type
  | Length
  | Value of int

let cvc_object_types =
  [ 0x7F49, (`PUBLIC_KEY, true)
  ; 0x06, (`OID, false)
  ; 0x81, (`MODULUS, false)
  ; 0x82, (`EXPONENT, false)
  ; 0x82, (`COEFFICIENT_A, false)
  ; 0x83, (`COEFFICIENT_B, false)
  ; 0x84, (`BASE_POINT_G, false)
  ; 0x85, (`BASE_POINT_R_ORDER, false)
  ; 0x86, (`PUBLIC_POINT_Y, false)
  ; 0x87, (`COFACTOR_F, false)
  ]

let find_cvc_object_type tag =
  let code = Cstruct.get_uint8 tag 0 in
  try code, List.assoc code cvc_object_types
  with Not_found ->
    let code =
      let msb = code * 0x100 in
      let lsb = Cstruct.get_uint8 tag 1 in
      msb + lsb
    in
    code, List.assoc code cvc_object_types

(* utility function to parse a big-endian blob as a Z.t *)
let atoz_bigendian s =
  let reverse s =
    let n = String.length s in
    String.init n (fun i -> s.[n-1-i])
  in
  Z.of_bits @@ reverse @@ Cstruct.to_string s

let grammar =
  let open Asn.S in
  let f = function
    | oid when List.mem oid rsa_oids -> Rsa oid
    | oid when List.mem oid ecdsa_oids -> Ecdsa oid
    | oid -> Unknown oid in
  let g = function
    | Rsa oid -> oid
    | Ecdsa oid -> oid
    | Unknown oid -> oid in
  map f g oid

let decode_oid str =
  match Asn.(decode (codec ber grammar) str) with
  | Ok (t, left) when Cstruct.len left = 0 -> t
  | Ok _ -> Asn.S.parse_error "CVC: OID with leftover"
  | Error _ -> Asn.S.parse_error "Cannot parse CVC OID"

let decode bytes =
  let buffer = Cstruct.create 4_096 in
  (* FSM to produce `Type ..., `Length ..., `Value ... tokens from a blob.
   * This tries to exploit tailcall recursion as much as possible in order to
   * avoid a stack explosion
  *)
  let rec tokenize ~acc bytes i lim state = function
    | Init ->
      if i >= lim then List.rev acc
      else tokenize ~acc bytes i lim None Type
    | Type ->
      Cstruct.blit bytes i buffer 0 2;
      let cvc_type = find_cvc_object_type buffer in
      let acc = `Type cvc_type :: acc in
      begin match cvc_type with
        | tag, _ when tag <= 0xff ->
          let i = i + 1 in
          tokenize ~acc bytes i lim (Some cvc_type) Length
        | _, _ ->
          let i = i + 2 in
          tokenize ~acc bytes i lim (Some cvc_type) Length
      end
    | Length ->
      let code = Cstruct.get_uint8 bytes i in
      if code < 0x80
      then begin
        let i = i + 1 in
        tokenize ~acc:((`Length code) :: acc) bytes i lim state (Value code)
      end
      else
        begin match code with
          | 0x81 ->
            let code = Cstruct.get_uint8 bytes (i + 1) in
            let i = i + 2 in
            tokenize ~acc:((`Length code) :: acc) bytes i lim state (Value code)
          | 0x82 ->
            let code = Cstruct.BE.get_uint16 bytes (i + 1) in
            let i = i + 3 in
            tokenize ~acc:((`Length code) :: acc) bytes i lim state (Value code)
          | _ ->
            raise (Failure "Invalid LENGTH field in TLV encoded CVC data")
        end
    | Value length ->
      let is_rec =
        match state with
        | None -> false
        | Some (_, (_, x)) -> x
      in
      let acc =
        if is_rec
        then
          `Value (tokenize ~acc:[] bytes i (i + length) None Init) :: acc
        else
          let bytes' =
            Cstruct.sub bytes i length
          in
          `Bytes bytes' :: acc
      in
      (if length + i >= Cstruct.len bytes then List.rev acc else tokenize ~acc bytes (i + length) lim None Init)
  in
  let tokens = tokenize ~acc:[] bytes 0 (Cstruct.len bytes) None Init in
  let rec parse = function
    | `Type (_, (`PUBLIC_KEY, _)) :: `Length _ :: `Value ls :: _ ->
      parse ls
    | `Type (_, (`OID, _)) :: `Length _ :: `Bytes bytes :: tl ->
      let bytes =
        let prefix =
          Printf.sprintf "\006%c" (Char.chr (Cstruct.len bytes))
          |> Cstruct.of_string
        in
        Cstruct.append prefix bytes
      in
      `Oid (decode_oid bytes) :: parse tl
    | `Type (_, (`MODULUS, _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Modulus (atoz_bigendian bytes) :: parse tl
    | `Type (0x82, ((*`EXPONENT or COEFFICIENT_A *) _ , _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Exponent bytes :: parse tl
    | `Type (_, (`COEFFICIENT_B, _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Coefficient_b bytes :: parse tl
    | `Type (_, (`BASE_POINT_G, _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Base_point_g bytes :: parse tl
    | `Type (_, (`BASE_POINT_R_ORDER, _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Base_point_r_order (atoz_bigendian bytes) :: parse tl
    | `Type (_, (`PUBLIC_POINT_Y, _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Public_point_y  bytes :: parse tl
    | `Type (_, (`COFACTOR_F, _)) :: `Length _ :: `Bytes bytes :: tl ->
      `Cofactor_f (atoz_bigendian bytes) :: parse tl
    | [] ->
      []
    | `Type (_, _) :: tl
    | `Length _ :: tl
    | `Bytes _ :: tl
    | `Value _ :: tl ->
      parse tl
  in
  let symbols = parse tokens in
  let oid =
    try
      let x = List.find (function `Oid _ -> true | _ -> false) symbols in
      match x with
      | `Oid x ->
        Some x
      | _ -> None
    with Not_found -> None
  in
  let open Result in
  match oid with
  | Some (Rsa _) ->
    begin match symbols with
      | [ `Oid _
        ; `Modulus n
        ; `Exponent e
        ] ->
        Ok (`Rsa (n, (atoz_bigendian e)))
      | _ ->
        Error "Parse error: some elements are missing or are not correctly sorted"
    end
  | Some (Ecdsa _) ->
    begin match symbols with
      | [ `Oid _
        ; `Modulus modulus
        ; `Exponent (* `Coefficient_a *) coefficient_a
        ; `Coefficient_b coefficient_b
        ; `Base_point_g base_point_g
        ; `Base_point_r_order base_point_r_order
        ; `Public_point_y public_point_y
        ; `Cofactor_f cofactor_f
        ] ->
        Ok (
          `Ecdsa
            ( modulus
            , coefficient_a
            , coefficient_b
            , base_point_g
            , base_point_r_order
            , public_point_y
            , cofactor_f
            ))
      | _ ->
        Error "Parse error: some elements are missing or are not correctly sorted"
    end
  | Some (Unknown oid) ->
    Error (Printf.sprintf "unknown OID \"%s\"." (Derivable.Asn_oid.show oid))
  | None ->
    Error "invalid CVC key: OID not found"

module Rsa =
struct
  module Public =
  struct
    type t = {
      n: Derivable.Z.t;
      e: Derivable.Z.t;
    }
    [@@deriving ord,eq,yojson,eq,show,bin_io]

    let decode bytes =
      let open Result in
      match decode bytes with
      | Ok (`Rsa (n, e)) ->
        Ok {n; e}
      | Ok (`Ecdsa _)
      | Ok `Unknown ->
        Error "CVC: Algorithm OID and parameters do not match."
      | Error _ as err ->
        err
  end
end

module Ec =
struct
  module Public =
  struct
    type t =
      { modulus : Derivable.Z.t
      ; coefficient_a : Derivable.Cstruct.t
      ; coefficient_b : Derivable.Cstruct.t
      ; base_point_g : Derivable.Cstruct.t
      ; base_point_r_order : Derivable.Z.t
      ; public_point_y : Derivable.Cstruct.t
      ; cofactor_f : Derivable.Z.t
      }
    [@@deriving ord,eq,yojson,eq,show,bin_io]

    let decode bytes =
      let open Result in
      match decode bytes with
      | Ok(
          `Ecdsa(
            modulus
          , coefficient_a
          , coefficient_b
          , base_point_g
          , base_point_r_order
          , public_point_y
          , cofactor_f)) ->
        Ok
          { modulus
          ; coefficient_a
          ; coefficient_b
          ; base_point_g
          ; base_point_r_order
          ; public_point_y
          ; cofactor_f
          }
      | Ok (`Rsa _)
      | Ok `Unknown ->
        Error "CVC: Algorithm OID and parameters do not match."
      | Error _ as err ->
        err
  end
end

module RSA = Rsa
module EC = Ec
