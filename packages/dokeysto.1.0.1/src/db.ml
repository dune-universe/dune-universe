
module Ht = Hashtbl

type filename = string

type position = { off: int;
                  len: int }

type db = { data_fn: filename;
            index_fn: filename;
            data: Unix.file_descr;
            index: (string, position) Ht.t }

module Internal = struct

  let create fn =
    let data_fn = fn in
    let index_fn = fn ^ ".idx" in
    let data =
      Unix.(openfile data_fn [O_RDWR; O_CREAT; O_EXCL] 0o600) in
    (* we just check there is not already an index file *)
    let index_file =
      Unix.(openfile index_fn [O_RDWR; O_CREAT; O_EXCL] 0o600) in
    Unix.close index_file;
    let index = Ht.create 11 in
    { data_fn; index_fn; data; index }

  let open_rw fn =
    let data_fn = fn in
    let index_fn = fn ^ ".idx" in
    let data =
      Unix.(openfile data_fn [O_RDWR] 0o600) in
    let index = Utls.restore index_fn in
    { data_fn; index_fn; data; index }

  let open_ro fn =
    let data_fn = fn in
    let index_fn = fn ^ ".idx" in
    let data =
      Unix.(openfile data_fn [O_RDONLY] 0o600) in
    let index = Utls.restore index_fn in
    { data_fn; index_fn; data; index }

  let close_simple db =
    Unix.close db.data

  let close_sync_index db =
    Unix.close db.data;
    Utls.save db.index_fn db.index

  let sync db =
    ExtUnix.All.fsync db.data;
    Utls.save db.index_fn db.index

  let destroy db =
    Ht.reset db.index;
    Unix.close db.data;
    Sys.remove db.data_fn;
    Sys.remove db.index_fn

  let mem db k =
    Ht.mem db.index k

  let add db k str =
    (* go to end of data file *)
    let off = Unix.(lseek db.data 0 SEEK_END) in
    let len = String.length str in
    let written = Unix.write_substring db.data str 0 len in
    assert(written = len);
    Ht.add db.index k { off; len }

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

  let add_z db k str =
    add db k (compress str)

  let replace db k str =
    (* go to end of data file *)
    let off = Unix.(lseek db.data 0 SEEK_END) in
    let len = String.length str in
    let written = Unix.write_substring db.data str 0 len in
    assert(written = len);
    Ht.replace db.index k { off; len }

  let replace_z db k str =
    replace db k (compress str)

  let remove db k =
    (* we just remove it from the index, not from the data file *)
    Ht.remove db.index k

  let retrieve db v_addr =
    let off = v_addr.off in
    let len = v_addr.len in
    let buff = Bytes.create len in
    let off' = Unix.(lseek db.data off SEEK_SET) in
    assert(off' = off);
    let read = Unix.read db.data buff 0 len in
    assert(read = len);
    Bytes.unsafe_to_string buff

  let retrieve_z db v_addr =
    uncompress (retrieve db v_addr)

  let find db k =
    let v_addr = Ht.find db.index k in
    retrieve db v_addr

  let find_z db k =
    let v_addr = Ht.find db.index k in
    retrieve_z db v_addr

  let iter f db =
    Ht.iter (fun k v ->
        f k (retrieve db v)
      ) db.index

  let iter_z f db =
    Ht.iter (fun k v ->
        f k (retrieve_z db v)
      ) db.index

  let fold f db init =
    Ht.fold (fun k v acc ->
        f k (retrieve db v) acc
      ) db.index init

  let fold_z f db init =
    Ht.fold (fun k v acc ->
        f k (retrieve_z db v) acc
      ) db.index init

end

module RO = struct

  type t = db

  let open_existing fn =
    Internal.open_ro fn

  let close db =
    Internal.close_simple db

  let mem db k =
    Internal.mem db k

  let find db k =
    Internal.find db k

  let iter f db =
    Internal.iter f db

  let fold f db init =
    Internal.fold f db init

end

module ROZ = struct

  type t = db

  let open_existing fn =
    RO.open_existing fn

  let close db =
    RO.close db

  let mem db k =
    RO.mem db k

  let find db k =
    Internal.find_z db k

  let iter f db =
    Internal.iter_z f db

  let fold f db init =
    Internal.fold_z f db init

end

module RW = struct

  type t = db

  let create fn =
    Internal.create fn

  let open_existing fn =
    Internal.open_rw fn

  let close db =
    Internal.close_sync_index db

  let sync db =
    Internal.sync db

  let destroy db =
    Internal.destroy db

  let mem db k =
    Internal.mem db k

  let add db k str =
    Internal.add db k str

  let replace db k str =
    Internal.replace db k str

  let remove db k =
    Internal.remove db k

  let find db k =
    Internal.find db k

  let iter f db =
    Internal.iter f db

  let fold f db init =
    Internal.fold f db init

end

module RWZ = struct

  type t = db

  let create fn =
    RW.create fn

  let open_existing fn =
    RW.open_existing fn

  let close db =
    RW.close db

  let sync db =
    RW.sync db

  let destroy db =
    RW.destroy db

  let mem db k =
    RW.mem db k

  let add db k str =
    Internal.add_z db k str

  let replace db k str =
    Internal.replace_z db k str

  let remove db k =
    RW.remove db k

  let find db k =
    Internal.find_z db k

  let iter f db =
    Internal.iter_z f db

  let fold f db init =
    Internal.fold_z f db init

end
