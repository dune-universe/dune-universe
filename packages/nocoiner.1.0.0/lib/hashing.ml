module Blake2B = Digestif.BLAKE2B

let hash data = Blake2B.to_hex @@ Blake2B.digest_string data

let raw_hash data = Blake2B.to_raw_string @@ Blake2B.digest_string data

let mac ~key data = Blake2B.to_hex @@ Blake2B.Keyed.mac_string ~key data
