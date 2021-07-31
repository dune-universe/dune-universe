let ( >>= ) = Result.bind

let ( >|= ) result f = Result.map f result

module Packet_error = struct
  type t =
    | Fatal of string
    | Header of
        { skip_length : int
        ; message : string }
end

let get_z_be cs ~off ~len =
  let r = ref Z.zero in
  let base = Z.of_int 0x100 in
  for i = off to off + len - 1 do
    r := Z.add (Z.mul base !r) @@ Z.of_int @@ Cstruct.get_uint8 cs i
  done;
  !r

let decode_mpi_shift cs ~off =
  let bit_length = Cstruct.BE.get_uint16 cs off in
  let length = (bit_length / 8) + min 1 (bit_length mod 8) in
  let shifted_cs = Cstruct.shift cs (length + 2) in
  (shifted_cs, get_z_be cs ~off:(2 + off) ~len:length)

module Algo = struct
  module Public = struct
    type t =
      | Rsa_enc_sign
      | Rsa_enc_only
      | Rsa_sign_only
      | Elgamal_enc_only
      | Dsa
      | Ec
      | Ecdsa
      | Unknown
    [@@deriving ord, eq, show]

    let of_int tag =
      match tag with
      | 1 -> Rsa_enc_sign
      | 2 -> Rsa_enc_only
      | 3 -> Rsa_sign_only
      | 16 -> Elgamal_enc_only
      | 17 -> Dsa
      | 18 -> Ec
      | 19 -> Ecdsa
      | _ -> Unknown

    let name algo =
      match algo with
      | Rsa_enc_sign -> "RSA Encryption & Signature"
      | Rsa_enc_only -> "RSA Encryption only"
      | Rsa_sign_only -> "RSA Signature only"
      | Elgamal_enc_only -> "Elgamal Encryption only"
      | Dsa -> "DSA"
      | Ec -> "EC"
      | Ecdsa -> "ECDSA"
      | Unknown -> "Unknown public algorithm"
  end

  module Hash = struct
    type t =
      | Md5
      | Sha1
      | Ripe_md160
      | Sha2_256
      | Sha2_384
      | Sha2_512
      | Sha2_224
      | Sha3_256
      | Sha3_512
      | Unknown_hash_algo
    [@@deriving ord, eq, show]

    let of_int tag =
      match tag with
      | 1 -> Md5
      | 2 -> Sha1
      | 3 -> Ripe_md160
      | 8 -> Sha2_256
      | 9 -> Sha2_384
      | 10 -> Sha2_512
      | 11 -> Sha2_224
      | 12 -> Sha3_256
      | 14 -> Sha3_512
      | _ -> Unknown_hash_algo

    let name algo =
      match algo with
      | Md5 -> "MD5"
      | Sha1 -> "SHA1"
      | Ripe_md160 -> "RIPE_MD160"
      | Sha2_256 -> "SHA2 256"
      | Sha2_384 -> "SHA2 384"
      | Sha2_512 -> "SHA2 512"
      | Sha2_224 -> "SHA2 224"
      | Sha3_256 -> "SHA3 256"
      | Sha3_512 -> "SHA3 512"
      | Unknown_hash_algo -> "Unknown hash algorithm"
  end

  module Symmetric = struct
    type t =
      | Plaintext
      | Idea
      | Triple_des
      | Cast_5
      | Blowfish
      | Aes_128
      | Aes_192
      | Aes_256
      | Twofish_256
      | Unknown

    let size algo =
      match algo with
      | Plaintext -> 0
      | Idea -> 8
      | Triple_des -> 8
      | Cast_5 -> 16
      | Blowfish -> 8
      | Aes_128 -> 16
      | Aes_192 -> 24
      | Aes_256 -> 32
      | Twofish_256 -> 32
      | Unknown -> 0

    let name algo =
      match algo with
      | Plaintext -> "Plain text"
      | Idea -> "IDEA"
      | Triple_des -> "Triple DES"
      | Cast_5 -> "Cast5"
      | Blowfish -> "Blowfish"
      | Aes_128 -> "AES 128"
      | Aes_192 -> "AES 192"
      | Aes_256 -> "AES 256"
      | Twofish_256 -> "Twofish 256"
      | Unknown -> "Unknown symmetric-key algorithm"

    let of_int tag =
      match tag with
      | 0 -> Plaintext
      | 1 -> Idea
      | 2 -> Triple_des
      | 3 -> Cast_5
      | 4 -> Blowfish
      | 7 -> Aes_128
      | 8 -> Aes_192
      | 9 -> Aes_256
      | 10 -> Twofish_256
      | _ -> Unknown
  end
end

module Rsa = struct
  module Public = struct
    type t =
      { n : Derivable.Z.t
      ; e : Derivable.Z.t }
    [@@deriving ord, eq, show]

    let decode packet ~off =
      let (cs, n) = decode_mpi_shift packet ~off in
      let (shifted_cs, e) = decode_mpi_shift cs ~off in
      let public_key = {n; e} in
      (shifted_cs, public_key)
  end

  module Private = struct
    type t =
      { d : Derivable.Z.t
      ; p : Derivable.Z.t
      ; q : Derivable.Z.t
      ; u : Derivable.Z.t }
    [@@deriving ord, eq, show]

    let decode packet ~off =
      let (cs1, d) = decode_mpi_shift packet ~off in
      let (cs2, p) = decode_mpi_shift cs1 ~off in
      let (cs3, q) = decode_mpi_shift cs2 ~off in
      let (shifted_cs, u) = decode_mpi_shift cs3 ~off in
      (shifted_cs, {d; p; q; u})
  end
end

module Dsa = struct
  module Public = struct
    type t =
      { p : Derivable.Z.t
      ; q : Derivable.Z.t
      ; g : Derivable.Z.t
      ; y : Derivable.Z.t }
    [@@deriving ord, eq, show]

    let decode packet ~off =
      let (cs1, p) = decode_mpi_shift packet ~off in
      let (cs2, q) = decode_mpi_shift cs1 ~off in
      let (cs3, g) = decode_mpi_shift cs2 ~off in
      let (shifted_cs, y) = decode_mpi_shift cs3 ~off in
      (shifted_cs, {p; q; g; y})
  end

  module Private = struct
    type t = Derivable.Z.t [@@deriving ord, eq, show]

    let decode packet ~off = decode_mpi_shift packet ~off
  end
end

module Elgamal = struct
  module Public = struct
    type t =
      { p : Derivable.Z.t
      ; g : Derivable.Z.t
      ; y : Derivable.Z.t }
    [@@deriving ord, eq, show]

    let decode packet ~off =
      let (cs1, p) = decode_mpi_shift packet ~off in
      let (cs2, g) = decode_mpi_shift cs1 ~off in
      let (shifted_cs, y) = decode_mpi_shift cs2 ~off in
      let public_key = {p; g; y} in
      (shifted_cs, public_key)
  end

  module Private = struct
    type t = Derivable.Z.t [@@deriving ord, eq, show]

    let decode packet ~off = decode_mpi_shift packet ~off
  end
end

module Packet = struct
  type packet_type =
    | Session_key
    | Signature
    | Secret_key
    | Public_key
    | Secret_subkey
    | Id
    | Public_subkey
    | Unknown_packet
  [@@deriving ord, eq, show]

  let of_int tag =
    match tag with
    | 0 -> Error "Tag 0"
    | 1 -> Ok Session_key
    | 2 -> Ok Unknown_packet
    | 5 -> Ok Secret_key
    | 6 -> Ok Public_key
    | 7 -> Ok Secret_subkey
    | 13 -> Ok Id
    | 14 -> Ok Public_subkey
    | _ -> Ok Unknown_packet

  let name packet =
    match packet with
    | Session_key -> "Session key packet"
    | Signature -> "Signature packet"
    | Secret_key -> "Secret Key packet"
    | Public_key -> "Public key packet"
    | Secret_subkey -> "Secret subkey packet"
    | Id -> "Identity packet"
    | Public_subkey -> "Public subkey packet"
    | Unknown_packet -> "Unknown packet"

  module Header = struct
    type t =
      { packet_type : packet_type
      ; packet_length : int
      ; is_new : bool }
    [@@deriving ord, eq, show]

    let is_new_type header_code =
      if header_code >= 192 then
        Ok true
      else if header_code >= 128 then
        Ok false
      else
        Error (Packet_error.Fatal "Bad header code")

    let get_tag header_code =
      is_new_type header_code >|= fun is_new ->
      if is_new then
        (is_new, header_code - 192)
      else
        (is_new, (header_code - 128) / 4)

    let get_old_length_size length_tag =
      match length_tag with
      | 0 -> Ok 2
      | 1 -> Ok 3
      | 2 -> Ok 5
      | 3 -> Error "Length size not implemented"
      | _ -> Error "Bad length size"

    let get_old_length cs header_code =
      get_old_length_size (header_code mod 4) >>= fun n ->
      match n with
      | 2 -> Ok (n, Cstruct.get_uint8 cs 1)
      | 3 -> Ok (n, Cstruct.BE.get_uint16 cs 1)
      | 5 -> Ok (n, Int32.to_int (Cstruct.BE.get_uint32 cs 1))
      | _ -> Error "Bad length size"

    let get_new_length cs =
      let first_octet = Cstruct.get_uint8 cs 1 in
      if first_octet < 192 then
        Ok (2, first_octet)
      else if first_octet < 224 then
        let second_octet = Cstruct.get_uint8 cs 2 in
        let length = 192 + second_octet + (256 * (first_octet - 192)) in
        Ok (3, length)
      else if first_octet < 255 then
        Error "Partial body lengths are not treated"
      else
        let length = Cstruct.BE.get_uint32 cs 2 in
        Ok (6, Int32.to_int length)

    let decode cs =
      let header_code = Cstruct.get_uint8 cs 0 in
      get_tag header_code >>= fun (is_new, tag) ->
      let length_infos =
        if is_new then
          get_new_length cs
        else
          get_old_length cs header_code
      in
      match length_infos with
      | Error error -> Error (Fatal error)
      | Ok (header_length, packet_length) -> (
        match of_int tag with
        | Ok packet_type ->
          Ok (header_length, {packet_type; packet_length; is_new})
        | Error message ->
          Error (Header {skip_length = header_length + packet_length; message}))
  end

  module Id = struct
    type t =
      { name : string
      ; email : string }
    [@@deriving ord, eq, show]

    let decode cs =
      let id = Cstruct.to_string cs in
      let sep_id = String.split_on_char '<' id in
      let name = String.concat "<" (List.rev (List.tl (List.rev sep_id))) in
      let email = List.nth sep_id (List.length sep_id - 1) in
      {name; email = String.sub email 0 (String.length email - 1)}
  end

  module Public_key = struct
    module Value = struct
      type t =
        | Rsa of Rsa.Public.t
        | Dsa of Dsa.Public.t
        | Elgamal of Elgamal.Public.t
      [@@deriving ord, eq, show]
    end

    type t =
      { version : int
      ; creation_time : int32
      ; validity_period : int option
      ; algo : Algo.Public.t
      ; public_key : Value.t }
    [@@deriving ord, eq, show]

    let decode_public_key (algo : Algo.Public.t) packet ~version =
      let offset =
        (*A public key packet has another header*)
        match version with
        | 3 ->
          Ok 8
          (*and a version 3 public key packet also contains a validity period*)
        | 4 -> Ok 6
        | _ ->
          Error
            (Printf.sprintf "Unexpected public key packet version: %d" version)
      in
      offset >>= fun off ->
      match algo with
      | Rsa_enc_sign
      | Rsa_enc_only
      | Rsa_sign_only ->
        let (cs, key) = Rsa.Public.decode packet ~off in
        Ok (cs, Value.Rsa key)
      | Dsa ->
        let (cs, key) = Dsa.Public.decode packet ~off in
        Ok (cs, Dsa key)
      | Elgamal_enc_only ->
        let (cs, key) = Elgamal.Public.decode packet ~off in
        Ok (cs, Elgamal key)
      | Ec
      | Ecdsa
      | Unknown ->
        Error ("Unsupported algorithm: " ^ Algo.Public.name algo)

    let decode packet =
      let version = Cstruct.get_uint8 packet 0 in
      let creation_time = Cstruct.BE.get_uint32 packet 1 in
      let packet_infos =
        match version with
        | 4 -> Ok (packet, None)
        | 2
        | 3 ->
          let time = Cstruct.BE.get_uint16 packet 5 in
          let cs = Cstruct.shift packet 2 in
          Ok (cs, Some time)
        | _ -> Error "Bad version of public key packet."
      in
      let algo = Algo.Public.of_int (Cstruct.get_uint8 packet 5) in
      match packet_infos with
      | Ok (public_packet, validity_period) ->
        decode_public_key algo public_packet ~version >|= fun (cs, key) ->
        (cs, {version; creation_time; validity_period; algo; public_key = key})
      | Error error -> Error error
  end

  module Private_key_value = struct
    type t =
      | Rsa of Rsa.Private.t
      | Dsa of Dsa.Private.t
      | Elgamal of Elgamal.Private.t
    [@@deriving ord, eq, show]
  end

  module Secret_key = struct
    module S2k = struct
      type s2k_type =
        | Simple
        | Salted
        | Iterated_salted
        | Unknown

      let of_int tag =
        match tag with
        | 0 -> Simple
        | 1 -> Salted
        | 3 -> Iterated_salted
        | _ -> Unknown

      let name specifier =
        match specifier with
        | Simple -> "Simple String2Key"
        | Salted -> "Salted String2Key"
        | Iterated_salted -> "Iterated&Salted String2Key"
        | Unknown -> "Unknown String2Key"

      type t =
        | Simple of Algo.Hash.t
        | Salted of Algo.Hash.t * int64
        | Iterated_salted of Algo.Hash.t * int64 * int
      [@@deriving ord, eq, show]
    end

    type t =
      { public_key : Public_key.t
      ; s2k : S2k.t option
      ; initial_vector : string option
      ; private_key : Private_key_value.t option
      ; checksum : string option
      ; hash : string option }
    [@@deriving ord, eq, show]

    let decode_s2k packet s2k_specifier =
      let hash_tag = Cstruct.get_uint8 packet 3 in
      let hash_algo = Algo.Hash.of_int hash_tag in
      match s2k_specifier with
      | S2k.Unknown -> Error "Unknown String2key"
      | Simple -> Ok (S2k.Simple hash_algo, 4)
      | Salted ->
        let salt_value = Cstruct.BE.get_uint64 packet 4 in
        Ok (S2k.Salted (hash_algo, salt_value), 12)
      | Iterated_salted ->
        let salt_value = Cstruct.BE.get_uint64 packet 4 in
        let count = Cstruct.get_uint8 packet 12 in
        Ok (S2k.Iterated_salted (hash_algo, salt_value, count), 13)

    let decode_private_key packet (algo : Algo.Public.t) =
      match algo with
      | Rsa_enc_sign
      | Rsa_enc_only
      | Rsa_sign_only ->
        let (cs, key) = Rsa.Private.decode packet ~off:0 in
        Ok (cs, Private_key_value.Rsa key)
      | Dsa ->
        let (cs, key) = Dsa.Private.decode packet ~off:0 in
        Ok (cs, Dsa key)
      | Elgamal_enc_only ->
        let (cs, key) = Elgamal.Private.decode packet ~off:0 in
        Ok (cs, Elgamal key)
      | Ec
      | Ecdsa
      | Unknown ->
        Error ("Not implemented for algorithm:" ^ Algo.Public.name algo)

    let decode_convention (public_key : Public_key.t) packet convention =
      match convention with
      | 0 ->
        let secret_packet = Cstruct.shift packet 1 in
        decode_private_key secret_packet public_key.algo
        >|= fun (cs, private_key) ->
        let checksum_int = Cstruct.BE.get_uint16 cs 0 in
        let checksum = Z.format "0x0100" (Z.of_int checksum_int) in
        { public_key
        ; s2k = None
        ; initial_vector = None
        ; private_key = Some private_key
        ; checksum = Some checksum
        ; hash = None }
      | 254 ->
        let sym_tag = Cstruct.get_uint8 packet 1 in
        let sym_algo = Algo.Symmetric.of_int sym_tag in
        let s2k_tag = Cstruct.get_uint8 packet 2 in
        let s2k_specifier = S2k.of_int s2k_tag in
        decode_s2k packet s2k_specifier >|= fun (s2k, offset) ->
        let cs_shifted = Cstruct.shift packet offset in
        let cipher_block = Algo.Symmetric.size sym_algo in
        let initial_vector_z = get_z_be cs_shifted ~off:0 ~len:cipher_block in
        let initial_vector = Z.format "0x100" initial_vector_z in
        { s2k = Some s2k
        ; public_key
        ; initial_vector = Some initial_vector
        ; private_key = None
        ; hash = None
        ; checksum = None }
      | _ -> Error "Private key type not treated."

    let decode packet =
      Public_key.decode packet >>= fun (cs, public_key) ->
      let offset =
        match public_key.version with
        | 3 -> Ok 8
        | 4 -> Ok 6
        | _ -> Error "Bad version of Public key packet"
      in
      offset >>= fun offset ->
      let secret_packet = Cstruct.shift cs offset in
      let convention = Cstruct.get_uint8 secret_packet 0 in
      decode_convention public_key secret_packet convention
  end

  module Body = struct
    type t =
      | Id of Id.t
      | Secret_key of Secret_key.t
      | Public_key of Public_key.t
      | Signature
      | Secret_subkey of Secret_key.t
      | Public_subkey of Public_key.t
      | Unknown
    [@@deriving ord, eq, show]

    let decode packet_type packet =
      match (packet_type : packet_type) with
      | Id -> Ok (Id (Id.decode packet))
      | Secret_key -> Secret_key.decode packet >|= fun key -> Secret_key key
      | Public_key ->
        Public_key.decode packet >|= fun (_, key) -> Public_key key
      | Signature -> Ok Unknown
      | Secret_subkey ->
        Secret_key.decode packet >|= fun key -> Secret_subkey key
      | Public_subkey ->
        Public_key.decode packet >|= fun (_, key) -> Public_subkey key
      | Session_key
      | Unknown_packet ->
        Ok Unknown
  end

  type t =
    { header : Header.t
    ; packet : Body.t }
  [@@deriving ord, eq, show]

  let decode cs =
    Header.decode cs >>= fun (header_length, header) ->
    let packet_cs = Cstruct.sub cs header_length header.packet_length in
    let res = Body.decode header.packet_type packet_cs in
    match res with
    | Error message ->
      Error
        (Header {skip_length = header_length + header.packet_length; message})
    (*When a packet can't be parsed, but the header is correct*)
    | Ok packet ->
      let next_cs = Cstruct.shift cs (header_length + header.packet_length) in
      Ok (next_cs, {header; packet})
end

let rec decode_rec cs packet_list =
  if Cstruct.length cs != 0 then
    match Packet.decode cs with
    | Ok (next_cs, packet) -> decode_rec next_cs (Ok packet :: packet_list)
    | Error (Fatal message) -> List.rev (Error message :: packet_list)
    (* When the length of the header can't be parsed *)
    | Error (Header error) ->
      (*When the length of the header can be parsed*)
      let next_cs = Cstruct.shift cs error.skip_length in
      decode_rec next_cs (Error error.message :: packet_list)
  else
    List.rev packet_list

let decode cs = decode_rec cs []
