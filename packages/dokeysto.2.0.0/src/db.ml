
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

  let dummy () =
    { data_fn = "/dev/null";
      index_fn = "/dev/null.idx";
      data = Unix.(openfile "/dev/null" [O_RDWR] 0o600);
      index = Ht.create 0 }

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

  let replace db k str =
    (* go to end of data file *)
    let off = Unix.(lseek db.data 0 SEEK_END) in
    let len = String.length str in
    let written = Unix.write_substring db.data str 0 len in
    assert(written = len);
    Ht.replace db.index k { off; len }

  let remove db k =
    (* we just remove it from the index, not from the data file *)
    Ht.remove db.index k

  let raw_read db pos =
    let off = pos.off in
    let len = pos.len in
    let buff = Bytes.create len in
    let off' = Unix.(lseek db.data off SEEK_SET) in
    assert(off' = off);
    let read = Unix.read db.data buff 0 len in
    assert(read = len);
    Bytes.unsafe_to_string buff

  let find db k =
    let v_addr = Ht.find db.index k in
    raw_read db v_addr

  let iter f db =
    Ht.iter (fun k v ->
        f k (raw_read db v)
      ) db.index

  let fold f db init =
    Ht.fold (fun k v acc ->
        f k (raw_read db v) acc
      ) db.index init

end

module RO = struct

  type t = db

  let open_existing fn =
    Internal.open_ro fn

  let dummy () =
    Internal.dummy ()

  let close db =
    Internal.close_simple db

  let mem db k =
    Internal.mem db k

  let find db k =
    Internal.find db k

  let raw_read db pos =
    Internal.raw_read db pos

  let iter f db =
    Internal.iter f db

  let fold f db init =
    Internal.fold f db init

end

module RW = struct

  type t = db

  let create fn =
    Internal.create fn

  let open_existing fn =
    Internal.open_rw fn

  let dummy () =
    Internal.dummy ()

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

  let raw_read db pos =
    Internal.raw_read db pos

  let iter f db =
    Internal.iter f db

  let fold f db init =
    Internal.fold f db init

end
