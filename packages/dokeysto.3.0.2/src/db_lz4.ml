
let compress str =
  (* LZ4 forces us to keep the length of the uncompressed string
     so that we know it at decompression time *)
  let n_str = string_of_int (String.length str) in
  let n = String.length n_str in
  let compressed = LZ4.Bytes.compress (Bytes.unsafe_of_string str) in
  let m = Bytes.length compressed in
  (* we write out: <uncompressed_length_str>:<compressed_data> *)
  let final_length = n + 1 + m in
  let bytes_res = Bytes.create final_length in
  (* [0..n-1] *)
  String.blit n_str 0 bytes_res 0 n;
  (* [n] *)
  Bytes.set bytes_res n ':';
  (* [n+1..n+m] *)
  Bytes.blit compressed 0 bytes_res (n + 1) m;
  Bytes.unsafe_to_string bytes_res

let uncompress str =
  (* first, read the uncompressed length prefix *)
  let i = String.index str ':' in
  let len = int_of_string (String.sub str 0 i) in
  (* then, uncompress the rest *)
  let n = String.length str in
  let j = i + 1 in
  let compressed = Bytes.create (n - j) in
  Bytes.blit_string str j compressed 0 (n - j);
  Bytes.unsafe_to_string (LZ4.Bytes.decompress ~length:len compressed)

module ROZ = struct

  module RO = Dokeysto.Db.RO

  type t = RO.t

  let open_existing fn =
    RO.open_existing fn

  let dummy () =
    RO.dummy ()

  let close db =
    RO.close db

  let mem db k =
    RO.mem db k

  let find db k =
    uncompress (RO.find db k)

  let raw_read db pos =
    uncompress (RO.raw_read db pos)

  let iter f db =
    RO.iter (fun k z ->
        f k (uncompress z)
      ) db

  let fold f db init =
    RO.fold (fun k z acc ->
        f k (uncompress z) acc
      ) db init

end

module RWZ = struct

  module RW = Dokeysto.Db.RW

  type t = RW.t

  let create fn =
    RW.create fn

  let open_existing fn =
    RW.open_existing fn

  let dummy () =
    RW.dummy ()

  let close db =
    RW.close db

  let sync db =
    RW.sync db

  let destroy db =
    RW.destroy db

  let mem db k =
    RW.mem db k

  let add db k str =
    RW.add db k (compress str)

  let replace db k str =
    RW.replace db k (compress str)

  let remove db k =
    RW.remove db k

  let find db k =
    uncompress (RW.find db k)

  let raw_read db pos =
    uncompress (RW.raw_read db pos)

  let iter f db =
    RW.iter (fun k z ->
        f k (uncompress z)
      ) db

  let fold f db init =
    RW.fold (fun k z acc ->
        f k (uncompress z) acc
      ) db init

end
