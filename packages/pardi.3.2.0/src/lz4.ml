
module Log = Dolog.Log

let compress (s: string): string =
  let orig_length: int = String.length s in
  let compressed: bytes = LZ4.Bytes.compress (Bytes.unsafe_of_string s) in
  (* LZ4 doesn't store the length of the data if uncompressed, so we
     have to serialize it *)
  let res =
    (Marshal.to_string orig_length [Marshal.No_sharing]) ^
    (Bytes.unsafe_to_string compressed) in
  (* Log.debug "before: %d after: %d" orig_length (String.length res); *)
  res

let decompress (c: string): string =
  let orig_length: int = Marshal.from_string c 0 in
  let offs: int = Marshal.total_size (Bytes.unsafe_of_string c) 0 in
  let compressed = Bytes.unsafe_of_string (BatString.lchop ~n:offs c) in
  let buff = LZ4.Bytes.decompress ~length:orig_length compressed in
  Bytes.unsafe_to_string buff

(* self-test *)
let () =
  assert(decompress (compress "tototititata") = "tototititata")
