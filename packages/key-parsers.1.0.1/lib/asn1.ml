let raise_asn f = match f () with Result.Ok x -> x | Result.Error s -> Asn.S.parse_error "%s" s

let encode_helper grammar =
  let open Asn in
  encode (codec der grammar)

let decode_helper name grammar x =
  let open Asn in
  let eprintf fmt = Printf.ksprintf (fun s -> Result.Error s) fmt in
  match decode (codec ber grammar) x with
  | Result.Ok (t, left) when Cstruct.len left = 0 -> Result.Ok t
  | Result.Ok _ -> eprintf "%s: non empty leftover" name
  | Result.Error (`Parse s) -> eprintf "%s: %s" name s

module Rsa =
struct
  module Params =
  struct
    type t = unit
    let grammar = Asn.S.null
  end

  module Public =
  struct
    type t = {
      n: Derivable.Z.t;
      e: Derivable.Z.t;
    }
    [@@deriving ord,eq,show]

    let reject_negative_modulus = function
      | Ok {n; e} ->
        if Z.gt n Z.zero then
          Ok { n; e }
        else
          (* A few tools generate keys without leading 0 bit, which gets interpreted
             as a negative integer. *)
          Error "Negative modulus"
      | e -> e


    let grammar =
      let open Asn.S in
      let f (n, e) = {n; e} in
      let g { n; e } = (n, e) in
      map f g @@ sequence2
        (required ~label:"modulus" integer)
        (required ~label:"publicExponent" integer)

    let encode = encode_helper grammar

    let decode k =
      decode_helper "PKCS1 RSA public key" grammar k
      |> reject_negative_modulus
  end

  module Private =
  struct
    type other_prime = {
      r: Derivable.Z.t;
      d: Derivable.Z.t;
      t: Derivable.Z.t;
    }
    [@@deriving ord,eq,show]

    let other_prime_grammar =
      let open Asn.S in
      let f (r, d, t) = { r; d; t } in
      let g { r; d; t } = (r, d, t) in
      map f g @@ sequence3
        (required ~label:"prime" integer)
        (required ~label:"exponent" integer)
        (required ~label:"coefficient" integer)

    type t = {
      n: Derivable.Z.t;
      e: Derivable.Z.t;
      d: Derivable.Z.t;
      p: Derivable.Z.t;
      q: Derivable.Z.t;
      dp: Derivable.Z.t;
      dq: Derivable.Z.t;
      qinv: Derivable.Z.t;
      other_primes: other_prime list;
    }
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let f = function
        | (0, (n, (e, (d, (p, (q, (dp, (dq, (qinv, None))))))))) ->
          { n; e; d; p; q; dp; dq; qinv; other_primes=[]; }
        | (1, (n, (e, (d, (p, (q, (dp, (dq, (qinv, Some other_primes))))))))) ->
          { n; e; d; p; q; dp; dq; qinv; other_primes; }
        | _ ->
          parse_error
            "PKCS#1: RSA private key version inconsistent with key data" in
      let g {n; e; d; p; q; dp; dq; qinv; _} =
        (0, (n, (e, (d, (p, (q ,(dp ,(dq, (qinv, None))))))))) in
      map f g @@ sequence
      @@ (required ~label:"version" int)
         @ (required ~label:"modulus" integer)
         @ (required ~label:"publicExponent" integer)
         @ (required ~label:"privateExponent" integer)
         @ (required ~label:"prime1" integer)
         @ (required ~label:"prime2" integer)
         @ (required ~label:"exponent1" integer)
         @ (required ~label:"exponent2" integer)
         @ (required ~label:"coefficient" integer)
           -@ (optional ~label:"otherPrimeInfo" (sequence_of other_prime_grammar))

    let encode = encode_helper grammar

    let decode = decode_helper "PKCS1 RSA private key" grammar
  end
end

module Dsa =
struct
  module Params =
  struct
    type t = {
      p: Derivable.Z.t;
      q: Derivable.Z.t;
      g: Derivable.Z.t;
    }
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let f (p, q, g) = { p; q; g } in
      let g { p; q; g } = (p, q, g) in
      map f g @@ sequence3
        (required ~label:"p" integer)
        (required ~label:"q" integer)
        (required ~label:"g" integer)

    let encode = encode_helper grammar

    let decode = decode_helper "DSA params" grammar
  end

  module Public =
  struct
    type t = Derivable.Z.t
    [@@deriving ord,eq,show]

    let grammar = Asn.S.integer

    let encode = encode_helper grammar

    let decode = decode_helper "DSA public key" grammar
  end

  module Private =
  struct
    type t = Derivable.Z.t
    [@@deriving ord,eq,show]

    let grammar = Asn.S.integer

    let encode = encode_helper grammar

    let decode = decode_helper "DSA private key" grammar
  end
end

module Ec =
struct
  type point = Derivable.Cstruct.t
  [@@deriving ord,eq,show]

  let point_grammar = Asn.S.octet_string

  module Field =
  struct
    let prime_oid = Asn.OID.(base 1 2 <|| [840;10045;1;1])
    let characteristic_two_oid = Asn.OID.(base 1 2 <|| [840;10045;1;2])

    let gn_oid = Asn.OID.(characteristic_two_oid <| 3 <| 1)
    let tp_oid = Asn.OID.(characteristic_two_oid <| 3 <| 2)
    let pp_oid = Asn.OID.(characteristic_two_oid <| 3 <| 3)

    type basis_type = | GN_typ | TP_typ | PP_typ
    let basis_type_grammar =
      let open Asn.S in
      let f = function
        | oid when oid = gn_oid -> GN_typ
        | oid when oid = tp_oid -> TP_typ
        | oid when oid = pp_oid -> PP_typ
        | _ -> parse_error "Ec: unexpected basis type OID" in
      let g = function
        | GN_typ -> gn_oid
        | TP_typ -> tp_oid
        | PP_typ -> pp_oid in
      map f g oid

    type basis =
      | GN
      | TP of Derivable.Z.t
      | PP of Derivable.Z.t * Derivable.Z.t * Derivable.Z.t
    [@@deriving ord,eq,show]

    let basis_grammar =
      let open Asn.S in
      let f = function
        | `C1 () -> GN
        | `C2 k -> TP k
        | `C3 (k1, k2, k3) -> PP (k1, k2, k3) in
      let g = function
        | GN -> `C1 ()
        | TP k -> `C2 k
        | PP (k1, k2, k3) -> `C3 (k1, k2, k3) in
      map f g @@ choice3
        null
        integer
        (sequence3
           (required ~label:"k1" integer)
           (required ~label:"k2" integer)
           (required ~label:"k3" integer))

    type characteristic_two_params = {
      m: Derivable.Z.t;
      basis: basis;
    }
    [@@deriving ord,eq,show]

    let ctwo_params_grammar =
      let open Asn.S in
      let f = function
        | (m, GN_typ, GN) -> { m; basis=GN }
        | (m, TP_typ, TP k) -> { m; basis=TP k }
        | (m, PP_typ, PP (k1, k2, k3)) -> { m; basis=PP (k1, k2, k3) }
        | _ -> parse_error "Ec: field basis type and parameters don't match" in
      let g { m; basis } =
        match basis with
        | GN -> (m, GN_typ, GN)
        | TP k -> (m, TP_typ, TP k)
        | PP (k1, k2, k3) -> (m, PP_typ, PP (k1, k2, k3)) in
      map f g @@ sequence3
        (required ~label:"m" integer)
        (required ~label:"basis" basis_type_grammar)
        (required ~label:"parameters" basis_grammar)

    type typ =
      | Prime_typ
      | C_two_typ

    let typ_grammar =
      let open Asn.S in
      let f = function
        | oid when oid = prime_oid -> Prime_typ
        | oid when oid = characteristic_two_oid -> C_two_typ
        | _ -> parse_error "Ec: unexpected field type OID" in
      let g = function
        | Prime_typ -> prime_oid
        | C_two_typ -> characteristic_two_oid in
      map f g oid

    type parameters =
      | Prime_p of Derivable.Z.t
      | C_two_p of characteristic_two_params

    let parameters_grammar =
      let open Asn.S in
      let f = function
        | `C1 p -> Prime_p p
        | `C2 params -> C_two_p params in
      let g = function
        | Prime_p p -> `C1 p
        | C_two_p params -> `C2 params in
      map f g @@ choice2
        integer
        ctwo_params_grammar

    type t =
      | Prime of Derivable.Z.t
      | C_two of characteristic_two_params
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let f = function
        | Prime_typ, Prime_p p -> Prime p
        | C_two_typ, C_two_p params -> C_two params
        | _ -> parse_error "Ec: field type and parameters don't match" in
      let g = function
        | Prime p -> Prime_typ, Prime_p p
        | C_two params -> C_two_typ, C_two_p params in
      map f g @@ sequence2
        (required ~label:"fieldType" typ_grammar)
        (required ~label:"parameters" parameters_grammar)
  end

  module Specified_domain =
  struct
    type field_element = Derivable.Cstruct.t
    [@@deriving ord,eq,show]

    let field_element_grammar = Asn.S.octet_string

    type curve = {
      a: field_element;
      b: field_element;
      seed: Derivable.Cstruct.t option;
    }
    [@@deriving ord,eq,show]

    let curve_grammar =
      let open Asn.S in
      let f (a, b, seed) = { a; b; seed } in
      let g {a; b; seed } = (a, b, seed) in
      map f g @@
      sequence3
        (required ~label:"a" field_element_grammar)
        (required ~label:"b" field_element_grammar)
        (optional ~label:"seed" bit_string_cs)

    type t = {
      field: Field.t;
      curve: curve;
      base: point;
      order: Derivable.Z.t;
      cofactor: Derivable.Z.t option;
    }
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let f (version, field, curve, base, order , cofactor) =
        if version = 1 then { field; curve; base; order; cofactor }
        else parse_error "Ec: Unknown ECParameters version" in
      let g { field; curve; base; order; cofactor } =
        (1, field, curve, base, order, cofactor) in
      map f g @@ sequence6
        (required ~label:"version" int)
        (required ~label:"fieldID" Field.grammar)
        (required ~label:"curve" curve_grammar)
        (required ~label:"base" point_grammar)
        (required ~label:"order" integer)
        (optional ~label:"cofactor" integer)
  end

  module Params =
  struct
    type t =
      | Named of Derivable.Asn_oid.t
      | Implicit
      | Specified of Specified_domain.t
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let f = function
        | `C1 oid -> Named oid
        | `C2 () -> Implicit
        | `C3 domain -> Specified domain in
      let g = function
        | Named oid -> `C1 oid
        | Implicit -> `C2 ()
        | Specified domain -> `C3 domain in
      map f g @@ choice3
        oid
        null
        Specified_domain.grammar

    let encode = encode_helper grammar

    let decode = decode_helper "EC parameters" grammar
  end

  module Public =
  struct
    type t = point
    [@@deriving ord,eq,show]

    let grammar = point_grammar

    let encode = encode_helper grammar

    let decode = decode_helper "EC public key" grammar
  end

  module Private =
  struct
    type t = {
      k: Derivable.Cstruct.t;
      params: Params.t option;
      public_key: Public.t option;
    }
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let f (version, k, params, public_key) =
        if version = 1 then { k; params; public_key }
        else parse_error "Ec: unknown private key version" in
      let g { k; params; public_key } =
        (1, k, params, public_key) in
      map f g @@ sequence4
        (required ~label:"version" int)
        (required ~label:"privateKey" octet_string)
        (optional ~label:"ECParameters" @@ explicit 0 Params.grammar)
        (optional ~label:"publicKey" @@ explicit 1 bit_string_cs)

    let encode = encode_helper grammar

    let decode = decode_helper "EC private key" grammar
  end
end

module Dh =
struct
  module Params =
  struct
    type t = {
      p: Derivable.Z.t;
      g: Derivable.Z.t;
      l: Derivable.Z.t option; (* privateValueLength *)
    }
    [@@deriving ord,eq,show]

    let grammar =
      let open Asn.S in
      let to_struct (p, g, l) = { p; g; l } in
      let of_struct {p; g; l} = ( p, g, l ) in
      map to_struct of_struct @@ sequence3
        (required ~label:"p" integer)
        (required ~label:"g" integer)
        (optional ~label:"l" integer)

    let encode = encode_helper grammar

    let decode = decode_helper "DH params" grammar
  end

  module Public =
  struct
    type t = Derivable.Z.t
    [@@deriving ord,eq,show]

    let grammar = Asn.S.integer

    let encode = encode_helper grammar

    let decode = decode_helper "DH public key" grammar
  end

  module Private =
  struct
    type t = Derivable.Z.t
    [@@deriving ord,eq,show]

    let grammar = Asn.S.integer

    let encode = encode_helper grammar

    let decode = decode_helper "DH private key" grammar
  end
end

module Algorithm_identifier =
struct
  module Algo =
  struct
    let rsa_oid = Asn.OID.(base 1 2 <|| [840;113549;1;1;1])
    let dsa_oid = Asn.OID.(base 1 2 <|| [840;10040;4;1])
    let ec_oid = Asn.OID.(base 1 2 <|| [840;10045;2;1])
    let dh_oid = Asn.OID.(base 1 2 <|| [840;113549;1;3;1])

    let ec_dh = Asn.OID.(base 1 3 <|| [132;1;12])
    let ec_mqv = Asn.OID.(base 1 3 <|| [132;1;13])

    type t =
      | Dsa
      | Rsa
      | Ec
      | Dh
      | Unknown of Asn.OID.t

    let grammar =
      let open Asn.S in
      let f = function
        | oid when oid = rsa_oid -> Rsa
        | oid when oid = dsa_oid -> Dsa
        | oid when oid = ec_oid
                || oid = ec_dh
                || oid = ec_mqv -> Ec
        | oid when oid = dh_oid -> Dh
        | oid -> Unknown oid in
      let g = function
        | Rsa -> rsa_oid
        | Dsa -> dsa_oid
        | Ec -> ec_oid
        | Dh -> dh_oid
        | Unknown oid -> oid in
      map f g oid
  end

  let rsa_grammar =
    let open Asn.S in
    let f = function
      | Algo.Rsa, _ -> ()
      | _ -> parse_error "Algorithm OID and parameters don't match" in
    let g () = Algo.Rsa, Some () in
    map f g @@ sequence2
      (required ~label:"algorithm" Algo.grammar)
      (optional ~label:"parameters" Rsa.Params.grammar)

  let dsa_grammar =
    let open Asn.S in
    let f = function
      | Algo.Dsa, params -> params
      | _, _ -> parse_error "Algorithm OID and parameters don't match" in
    let g params = Algo.Dsa, params in
    map f g @@ sequence2
      (required ~label:"algorithm" Algo.grammar)
      (required ~label:"parameters" Dsa.Params.grammar)

  let ec_grammar =
    let open Asn.S in
    let f = function
      | Algo.Ec, params -> params
      | _, _ -> parse_error "Algorithm OID and parameters don't match" in
    let g params = Algo.Ec, params in
    map f g @@ sequence2
      (required ~label:"algorithm" Algo.grammar)
      (required ~label:"parameters" Ec.Params.grammar)

  let dh_grammar =
    let open Asn.S in
    let f = function
      | Algo.Dh, params -> params
      | _, _ -> parse_error "Algorithm OID and parameters don't match" in
    let g params = Algo.Dh, params in
    map f g @@ sequence2
      (required ~label:"algorithm" Algo.grammar)
      (required ~label:"parameters" Dh.Params.grammar)

end

let map_result f = function Result.Ok x -> Result.Ok (f x) | Result.Error _ as r -> r
let default_result default = function Result.Error _ -> default () | Result.Ok _ as r -> r

module X509 =
struct
  type t =
    [ `RSA of Rsa.Public.t
    | `DSA of Dsa.Params.t * Dsa.Public.t
    | `EC of Ec.Params.t * Ec.Public.t
    | `DH of Dh.Params.t * Dh.Public.t
    ]
  [@@deriving ord,eq,show]

  let rsa_grammar =
    let open Asn.S in
    let f ((), bit_string) = raise_asn @@ fun () -> Rsa.Public.decode bit_string in
    let g key = (), Rsa.Public.encode key in
    map f g @@ sequence2
      (required ~label:"algorithm" Algorithm_identifier.rsa_grammar)
      (required ~label:"subjectPublicKey" bit_string_cs)

  let dsa_grammar =
    let open Asn.S in
    let f (params, bit_string) = params, raise_asn @@ fun () -> Dsa.Public.decode bit_string in
    let g (params, key) = params, Dsa.Public.encode key in
    map f g @@ sequence2
      (required ~label:"algorithm" Algorithm_identifier.dsa_grammar)
      (required ~label:"subjectPublicKey" bit_string_cs)

  let ec_grammar =
    let open Asn.S in
    let f (params, bit_string) = params, bit_string in
    let g (params, key) = params, key in
    map f g @@ sequence2
      (required ~label:"algorithm" Algorithm_identifier.ec_grammar)
      (required ~label:"subjectPublicKey" bit_string_cs)

  let dh_grammar =
    let open Asn.S in
    let f (params, bit_string) = params, raise_asn @@ fun () -> Dh.Public.decode bit_string in
    let g (params, key) = params, Dh.Public.encode key in
    map f g @@ sequence2
      (required ~label:"algorithm" Algorithm_identifier.dh_grammar)
      (required ~label:"subjectPublicKey" bit_string_cs)

  let encode_rsa = encode_helper rsa_grammar
  let encode_dsa = encode_helper dsa_grammar
  let encode_ec = encode_helper ec_grammar
  let encode_dh = encode_helper dh_grammar

  let encode = function
    | `RSA key -> encode_rsa key
    | `DSA key -> encode_dsa key
    | `EC key -> encode_ec key
    | `DH key -> encode_dh key

  let decode_rsa = decode_helper "X509 RSA key" rsa_grammar

  let decode_dsa = decode_helper "X509 DSA key" dsa_grammar

  let decode_ec = decode_helper "X509 EC key" ec_grammar

  let decode_dh = decode_helper "X509 DH key" dh_grammar

  let decode key : (t, string) Result.result =
    (map_result (fun x -> `RSA x) (decode_rsa key))
    |> default_result (fun () -> map_result (fun x -> `DSA x) (decode_dsa key))
    |> default_result (fun () -> map_result (fun x -> `EC x) (decode_ec key))
    |> default_result (fun () -> map_result (fun x -> `DH x) (decode_dh key))
    |> default_result @@ fun () -> Result.Error "Couldn't parse key"
end

module PKCS8 =
struct
  type t =
    [ `RSA of Rsa.Private.t
    | `DSA of Dsa.Params.t * Dsa.Private.t
    | `EC of Ec.Params.t * Ec.Private.t
    | `DH of Dh.Params.t * Dh.Private.t
    ]
  [@@deriving ord,eq,show]

  let rsa_grammar =
    let open Asn.S in
    let f (version, (), octet_string, _attributes) =
      if version = 0 then
        raise_asn @@ fun () -> Rsa.Private.decode octet_string
      else
        parse_error "PKCS8: version %d not supported" version in
    let g key = 0, (), Rsa.Private.encode key, None in
    map f g @@ sequence4
      (required ~label:"version" int)
      (required ~label:"privateKeyAlgorithm" Algorithm_identifier.rsa_grammar)
      (required ~label:"privateKey" octet_string)
      (optional ~label:"attributes" @@ implicit 0 null)

  let dsa_grammar =
    let open Asn.S in
    let f (version, params, octet_string, _attributes) =
      if version = 0 then
        params, raise_asn @@ fun () -> Dsa.Private.decode octet_string
      else
        parse_error "PKCS8: version %d not supported" version in
    let g (params, key) = 0, params, Dsa.Private.encode key, None in
    map f g @@ sequence4
      (required ~label:"version" int)
      (required ~label:"privateKeyAlgorithm" Algorithm_identifier.dsa_grammar)
      (required ~label:"privateKey" octet_string)
      (optional ~label:"attributes" @@ implicit 0 null)

  let ec_grammar =
    let open Asn.S in
    let f (version, params, octet_string, _attributes) =
      if version = 0 then
        params, raise_asn @@ fun () -> Ec.Private.decode octet_string
      else
        parse_error "PKCS8: version %d not supported" version in
    let g (params, key) = 0, params, Ec.Private.encode key, None in
    map f g @@ sequence4
      (required ~label:"version" int)
      (required ~label:"privateKeyAlgorithm" Algorithm_identifier.ec_grammar)
      (required ~label:"privateKey" octet_string)
      (optional ~label:"attributes" @@ implicit 0 null)

  let dh_grammar =
    let open Asn.S in
    let f (version, params, octet_string, _attributes) =
      if version = 0 then
        params, raise_asn @@ fun () -> Dh.Private.decode octet_string
      else
        parse_error "PKCS8: version %d not supported" version in
    let g (params, key) = 0, params, Dh.Private.encode key, None in
    map f g @@ sequence4
      (required ~label:"version" int)
      (required ~label:"privateKeyAlgorithm" Algorithm_identifier.dh_grammar)
      (required ~label:"privateKey" octet_string)
      (optional ~label:"attributes" @@ implicit 0 null)

  let encode_rsa = encode_helper rsa_grammar
  let encode_dsa = encode_helper dsa_grammar
  let encode_ec = encode_helper ec_grammar
  let encode_dh = encode_helper dh_grammar

  let encode = function
    | `RSA key -> encode_rsa key
    | `DSA key -> encode_dsa key
    | `EC key -> encode_ec key
    | `DH key -> encode_dh key

  let decode_rsa = decode_helper "PKCS8 RSA key" rsa_grammar

  let decode_dsa = decode_helper "PKCS8 DSA key" dsa_grammar

  let decode_ec = decode_helper "PKCS8 EC key" ec_grammar

  let decode_dh = decode_helper "PKCS8 DH key" dh_grammar

  let decode key : (t, string) Result.result =
    (map_result (fun x -> `RSA x) (decode_rsa key))
    |> default_result (fun () -> map_result (fun x -> `DSA x) (decode_dsa key))
    |> default_result (fun () -> map_result (fun x -> `EC x) (decode_ec key))
    |> default_result (fun () -> map_result (fun x -> `DH x) (decode_dh key))
    |> default_result @@ fun () -> Result.Error "Couldn't parse key"
end
