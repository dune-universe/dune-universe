module Blake = Digestif.BLAKE2B

let hash data =
  Cstruct.of_hex @@ Blake.to_hex @@ Blake.digest_string data

let id ( ) =
  let timestamp = hash @@ string_of_float @@ Unix.gettimeofday ( ) in
  let pid = hash @@ string_of_int @@ Unix.getpid ( ) in
  let hostname = hash @@ Unix.gethostname ( ) in
  let cwd = hash @@ Unix.getcwd ( ) in
  let context = Cstruct.to_bytes @@ Cstruct.concat [
    timestamp; pid; hostname; cwd
  ] in
  Encoding.encode @@ Blake.to_raw_string @@ Blake.digest_bytes context
